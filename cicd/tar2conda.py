#!/usr/bin/env python

''' Convert a tar.gz R package distribution to a conda package '''

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
try:
    from urllib.request import urlretrieve, urlcleanup
except ImportError:
    from urllib import urlretrieve, urlcleanup

try:
    execfile
except NameError:
    def execfile(filename, global_vars, local_vars):
        with open(filename) as f:
            code = compile(f.read(), filename, 'exec')
            exec(code, global_vars, local_vars)


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


class TemporaryDirectory(object):
    '''
    Context manager for tempfile.mkdtemp()

    This class is available in Python 3.2+.

    '''
    def __enter__(self):
        self.dir_name = tempfile.mkdtemp()
        return self.dir_name

    def __exit__(self, exc_type, exc_value, traceback):
        import atexit
        atexit.register(shutil.rmtree, self.dir_name)


@contextlib.contextmanager
def redirect_stdout(target):
    ''' Redirect stdout to given file-like object '''
    original = sys.stdout
    sys.stdout = target
    yield
    sys.stdout = original


def get_supported_versions(platform):
    ''' Get the versions of R that can be used for SWAT '''
    vers = dict()
    exclusions = dict(r=dict(), mro=dict())

    for base in ['r::r-base', 'r::mro-base']:
        base_vers = set()

        cmd = ['conda', 'search', '--json', '--platform', platform, base]
        out = json.loads(subprocess.check_output(cmd).decode('utf-8'))

        if base.split('::')[-1] not in out:
            continue

        for item in out[base.split('::')[-1]]:
            ver = item['version']
            if tuple([int(x) for x in ver.split('.')]) < (3, 4, 3):
                continue
            base_vers.add(item['version'])

        vers[base.split('::')[-1].split('-')[0]] = base_vers

    for i, pkg in enumerate(['r::r-httr', 'r::r-jsonlite', 'r::r-testthat', 'r::r-xlsx']):
        cmd = ['conda', 'search', '--json', '--platform', platform, pkg]
        out = subprocess.check_output(cmd).decode('utf-8')

        pkg_vers = set()

        for item in json.loads(out)[pkg.split('::')[-1]]:
            rver = [x for x in item['depends']
                    if x.startswith('r-base') or x.startswith('mro-base')]
            if not rver:
                continue
            rver = rver[0]
            base = rver.split('-')[0]
            try:
                rver = re.findall(r'(\d+\.\d+(?:\.\d+)?)', rver)[0]
            except IndexError:
                # mro-base didn't include versions in the beginning
                if base == 'mro':
                    rver = '3.4.3'
                else:
                    raise

            int_rver = tuple([int(x) for x in rver.split('.')])

            # Ignore versions older than 3.4.3
            if len(int_rver) < 3 and int_rver < (3, 4):
                continue
            if int_rver < (3, 4, 3):
                continue

            if len(int_rver) < 3:
                rver += '.0'

            pkg_vers.add(rver)

        for item in vers[base].difference(pkg_vers):
            if item not in exclusions[base]:
                exclusions[base][item] = []
            exclusions[base][item].append(pkg.split('::')[-1])

        vers[base] = vers[base].intersection(pkg_vers)

    vers['r'] = list(sorted(vers['r']))
    vers['mro'] = list(sorted(vers['mro']))

    return vers, exclusions


open = io.open


def main(url, args):
    ''' Convert given tar file to conda packages '''

    cwd = os.getcwd()

    args.output_folder = os.path.abspath(args.output_folder)

    os.makedirs(args.output_folder, exist_ok=True)

    args.recipe_dir = os.path.abspath(args.recipe_dir)
    if os.path.isfile(args.recipe_dir):
        args.recipe_dir = os.path.dirname(args.recipe_dir)

    download = False
    if url.startswith('http:') or url.startswith('https:'):
        print('> download %s' % url)
        download = True
        url, headers = urlretrieve(url)
        urlcleanup()
    elif os.path.exists(url):
        url = os.path.abspath(url)

    with TemporaryDirectory() as temp:

        with tarfile.open(url, url.endswith('tar') and 'r:' or 'r:gz') as tar:
            tar.extractall(temp)

        # Clean up
        if download:
            urlcleanup()

        os.chdir(temp)

        url = os.path.join(temp, glob.glob('R-swat*')[0])

        # Report available R versions
        print('')
        print('> Available verions for {}:'.format(args.platform))
        vers, exc = get_supported_versions(args.platform)
        for key, value in vers.items():
            if value:
                print('  + {}-base'.format(key))
                for item in sorted(value):
                    print('    {}'.format(item))
        print('')

        if exc['r'] or exc['mro']:
            print('> Excluded versions:')
            for key, value in exc.items():
                if value:
                    print('  + {}-base'.format(key))
                for k, v in sorted(value.items()):
                    print('    {}: {}'.format(k, ', '.join(v)))
            print('')

        # Create conda package for each R version
        for base, versions in vers.items():

            for ver in versions:
                update_recipe(args.recipe_dir, url=url, version=get_version(url),
                              r_base='{}-base'.format(base), r_version=ver)

                cmd = ['conda', 'build', '-q', '--no-test']
                cmd.extend(['--R', ver])
                if args.debug:
                    cmd.append('--debug')
                if args.output_folder:
                    cmd.extend(['--output-folder', args.output_folder])
                if args.override_channels:
                    cmd.append('--override-channels')
                if args.channel:
                    for chan in args.channel:
                        cmd.extend(['--channel', chan])
                cmd.append(args.recipe_dir)

                print('> ' + ' '.join(cmd))
                subprocess.check_output(cmd)

    os.chdir(cwd)


if __name__ == '__main__':

    opts = argparse.ArgumentParser(description=__doc__.strip())

    opts.add_argument('url', type=str,
                      help='input file / url')

    opts.add_argument('--build', '-b', default=0, type=int,
                      help='build number')
    opts.add_argument('--channel', '-c', type=str, nargs='*',
                      help='additional chanel to search')
    opts.add_argument('--debug', action='store_true',
                      help='enable conda build debug logging')
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
