#!/usr/bin/env python

'''
Convert a tar.gz R package distribution to a conda package

This tool takes a `tar.gz` of the SWAT package source, C extensions, and TK
files and converts it to a set of conda files using `conda build`. One conda
file is created for each supported R engine on each platform.

'''

from __future__ import print_function, division, absolute_import, unicode_literals

import argparse
import contextlib
import glob
import io
import json
import os
import platform
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
from urllib.request import urlretrieve, urlcleanup


def get_platform():
    ''' Return the Anaconda platform name for the current platform '''
    plat = platform.system().lower()
    if 'darwin' in plat:
        return 'osx-64'
    if plat.startswith('win'):
        return 'win-64'
    if 'linux' in plat:
        machine = platform.machine().lower()
        if 'x86' in machine:
            return 'linux-64'
        if 'ppc' in machine:
            return 'linux-ppc64le'
    return 'unknown'


def update_recipe(recipe, **kwargs):
    '''
    Update recipe file with parameters

    Parameters
    ----------
    recipe : basestring
        Path to the conda recipe file
    **kwargs : keyword arguments, optional
        Substitution variables for fields in recipe

    '''
    # Add recipe filename as needed
    if os.path.isdir(recipe):
        recipe = os.path.join(recipe, 'meta.yaml')

    params = kwargs.copy()
    for name, value in params.items():
        if name == 'url':
            url = value
            continue

        params[name] = value

    # Write variables to recipe
    out = []
    with open(recipe, 'r') as recipe_file:
        for line in recipe_file:

            if 'sha256' in line:
                continue

            if url:
                url = url.replace('\\', '/')
                if os.path.isdir(url):
                    line = re.sub(r'''^(\s+)(?:url|path):.*?(\s*#\s*\[.+?\]\s*)?$''',
                                  r'''\1path: %s\2''' % url, line)
                else:
                    line = re.sub(r'''^(\s+)(?:url|path):.*?(\s*#\s*\[.+?\]\s*)?$''',
                                  r'''\1url: %s\2''' % url, line)

            for key, value in params.items():
                if key == 'url':
                    continue
                line = re.sub(r'''^(\{%%\s*set\s+%s\s*=\s*)'[^']+'(\s*%%\}\s*)$''' % key,
                              r'''\1'%s'\2''' % value, line)

            out.append(line.rstrip())

    # Write recipe file
    with open(recipe, 'w') as recipe_file:
        recipe_file.write('\n'.join(out))
        recipe_file.write('\n')


def get_version(pkg_dir):
    '''  Retrieve version number from setup.py '''
    with open(os.path.join(pkg_dir, 'DESCRIPTION'), 'r') as desc_in:
        for line in desc_in:
            m = re.match(r'^Version\s*:\s*(\S+)', line)
            if m:
                return m.group(1)
    raise RuntimeError('Could not find version in DESCRIPTION file.')


@contextlib.contextmanager
def redirect_stdout(target):
    ''' Redirect stdout to given file-like object '''
    original = sys.stdout
    sys.stdout = target
    yield
    sys.stdout = original


def version_key(val):
    ''' Return normalized version number '''
    val = val.split('a')[0] + '.0.0'
    return tuple([int(x) for x in re.findall(r'(\d+)', val)][:3])


def expand_wildcards(vals):
    ''' Expand * in version numbers '''
    out = []
    for val in vals:
        if re.search(r'\.?\*$', val):
            val = re.sub(r'\.?\*$', '', val)
            next_val = [int(x) for x in val.split('.')]
            next_val[-1] += 1
            out.append('>={},<={}a0'.format(
                val,
                '.'.join('{}'.format(x) for x in next_val)))
        else:
            out.append(val)
    return out


def check_version(pkg_ver, specs):
    ''' Evaluate version expression '''
    pkg_ver = version_key(pkg_ver)
    for spec in expand_wildcards(specs):
        expr = []
        for ap in spec.split(','):
            or_expr = []
            for op in ap.split('|'):
                oper, ver = re.findall(r'^([<>=!]*)(\S+)$', op)[0]
                if oper == '=':
                    oper = '=='
                elif oper == '':
                    oper = '>='
                or_expr.append('{} {} {}'.format(pkg_ver, oper, version_key(ver)))
            expr.append('({})'.format(' or '.join(or_expr)))
        if eval(' and '.join(expr)):
            return True
    return False


def conda_search(platform, pkg):
    ''' Return information about specified package '''
    cmd = ['conda', 'search', '--json', '--platform', platform, pkg]
    try:
        out = json.loads(subprocess.check_output(cmd).decode('utf-8'))
    except subprocess.CalledProcessError as exc:
        out = json.loads(exc.output.decode('utf-8'))
        if out and out.get('exception_name', '') == 'PackagesNotFoundError':
            out = {}
        else:
            raise
    return out.get(pkg.split('::')[-1], {})


def get_supported_versions(platform, r_base):
    ''' Get the versions of R that can be used for SWAT '''
    r_base_vers = set()

    out = conda_search(platform, 'r::{}-base'.format(r_base))

    if not out:
        return []

    for item in out:
        ver = item['version']
        if (tuple([int(x) for x in ver.split('.')]) < (3, 5, 0)):
            continue
        # skip mro 3.5.1 due to build issues
        if (platform == "linux-64" and r_base == "mro"
                and tuple([int(x) for x in ver.split('.')]) == (3, 5, 1)):
            continue
        # skip 3.4.3 and 3.5.x on OSX due to build issues with libgfortran.3.dylib
        if (platform == "osx-64"
                and tuple([int(x) for x in ver.split('.')]) < (3, 6, 0)):
            continue

        r_base_vers.add(item['version'])

    for pkg in ['r::r-httr', 'r::r-jsonlite', 'r::r-testthat']:
        out = conda_search(platform, pkg)

        pkg_vers = []
        for item in out:
            rver = [x for x in item['depends'] if x.startswith('{}-base'.format(r_base))]

            if not rver:
                continue

            rver = rver[0]
            if rver == 'mro-base':
                rver = 'mro-base ==3.4.3'
            rver = rver.split(' ')[-1]

            pkg_vers.append(rver)

        for ver in list(r_base_vers):
            if not check_version(ver, pkg_vers):
                # print('Removing {}-base {} due to package {}.'.format(r_base, ver, pkg))
                r_base_vers.remove(ver)

    return list(sorted(r_base_vers))


open = io.open


def main(url, args):
    ''' Convert given tar file to conda packages '''

    args.output_folder = os.path.abspath(args.output_folder)

    os.makedirs(args.output_folder, exist_ok=True)

    args.recipe_dir = os.path.abspath(args.recipe_dir)
    if os.path.isfile(args.recipe_dir):
        args.recipe_dir = os.path.dirname(args.recipe_dir)

    download = False
    if url.startswith('http:') or url.startswith('https:'):
        print('> download %s' % url)
        download = True
        url = urlretrieve(url)[0]
        urlcleanup()
    elif os.path.exists(url):
        url = os.path.abspath(url)

    with tempfile.TemporaryDirectory() as temp:

        with tarfile.open(url, url.endswith('tar') and 'r:' or 'r:gz') as tar:
            tar.extractall(temp)

        # Clean up
        if download:
            urlcleanup()

        url = glob.glob(os.path.join(temp, 'R-swat*'))[0]

        # Report available R versions
        print('')
        print('> Available verions for {}:'.format(args.platform))
        vers = dict(r=get_supported_versions(args.platform, 'r'),
                    mro=get_supported_versions(args.platform, 'mro'))
        for key, value in vers.items():
            if value:
                print('  + {}-base'.format(key))
                for item in sorted(value):
                    print('    {}'.format(item))
        print('')

        # Make sure we aren't picking up any stray installed packages
        if 'R_LIBS_USER' in os.environ:
            del os.environ['R_LIBS_USER']

        # Create conda package for each R version
        r_base_finished = set()
        for base, versions in vers.items():

            os.environ['R_BASE'] = base

            for ver in versions:

                # Only build for major.minor of r-base
                if base == 'r':
                    minor_ver = re.match(r'^(\d+\.\d+)', ver).group(1)
                    if minor_ver in r_base_finished:
                        continue
                    r_base_finished.add(minor_ver)

                update_recipe(args.recipe_dir, url=url, version=get_version(url),
                              r_base='{}-base'.format(base), r_version=ver)

                os.environ['R_VERSION'] = ver

                cmd = ['conda', 'build', '-q']  # '--no-test'
                cmd.extend(['--R', ver])
                if args.debug:
                    cmd.append('--debug')
                if args.no_test:
                    cmd.append('--no-test')
                if args.output_folder:
                    cmd.extend(['--output-folder', args.output_folder])
                if args.override_channels:
                    cmd.append('--override-channels')
                if args.channel:
                    for chan in args.channel:
                        cmd.extend(['--channel', chan])
                cmd.append(args.recipe_dir)

                print('> ' + ' '.join(cmd))
                subprocess.check_call(cmd)


if __name__ == '__main__':

    opts = argparse.ArgumentParser(description=__doc__.strip(),
                                   formatter_class=argparse.RawTextHelpFormatter)

    opts.add_argument('url', type=str,
                      help='input file / url')

    opts.add_argument('--build', '-b', default=0, type=int,
                      help='build number')
    opts.add_argument('--channel', '-c', type=str, nargs='*',
                      help='additional chanel to search')
    opts.add_argument('--debug', action='store_true',
                      help='enable conda build debug logging')
    opts.add_argument('--no-test', action='store_true',
                      help='disable conda build tests')
    opts.add_argument('--output-folder', type=str, default='',
                      help='folder to create the output package in')
    opts.add_argument('--override-channels', action='store_true', default=False,
                      help='disable searching default or .condarc channels')
    opts.add_argument('--recipe-dir', '-r', required=True, type=str,
                      help='path to recipe file')
    opts.add_argument('-p', '--platform', type=str, metavar='<platform>',
                      default=get_platform(),
                      choices=['linux-64', 'osx-64', 'linux-ppc64le', 'win-64'],
                      help='platform libraries to install')

    args = opts.parse_args()

    main(args.url, args)
