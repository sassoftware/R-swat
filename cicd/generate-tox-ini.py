#!/usr/bin/env python

'''
Generate a Tox config file for all test configurations

'''

import argparse
import glob
import json
import os
import platform
import random
import re
import subprocess
import sys


def print_err(*args, **kwargs):
    ''' Print a message to stderr '''
    sys.stderr.write(*args, **kwargs)
    sys.stderr.write('\n')


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
        if tuple([int(x) for x in ver.split('.')]) < (3, 4, 3):
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


def main(args):
    ''' Main routine '''
    info = dict(r=get_supported_versions(args.platform, 'r'),
                mro=get_supported_versions(args.platform, 'mro'))

    print('> Available versions for {}:'.format(args.platform))
    for key, value in info.items():
        if value:
            print('  + {}-base'.format(key))
        for item in sorted(value):
            print('    {}'.format(item))

    # Pick a subset of the matrix to test.
    subset = dict(r=set(), mro=set())

    print('')
    print('> Subset of versions used for test environments:')

    # Take the newest version, oldest version, and a random one.
    if info['r']:
        subset['r'].add(info['r'][0])
        subset['r'].add(info['r'][-1])
        if len(info['r']) > 2:
            subset['r'].add(random.choice(info['r'][1:-1]))
        print('  + r-base')
        for item in sorted(subset['r']):
            print('    {}'.format(item))

    if info['mro']:
        subset['mro'].add(info['mro'][0])
        subset['mro'].add(info['mro'][-1])
        if len(info['mro']) > 2:
            subset['mro'].add(random.choice(info['mro'][1:-1]))
        print('  + mro-base')
        for item in sorted(subset['mro']):
            print('    {}'.format(item))

    # Generate Tox configurations for testenvs
    for pkg in ['conda']:
        out = ['', '#', '# BEGIN GENERATED ENVIRONMENTS', '#', '']
        out.append('[testenv:empty]')
        out.append('commands =')
        out.append('deps =')
        out.append('conda_deps =')
        out.append('')

        envlist = []

        for base, vers in sorted(subset.items()):
            out.append('#')
            out.append('# {}-base'.format(base))
            out.append('#')
            out.append('')
            for ver in sorted(vers):
                out.append('# R {}'.format(ver))

                name = '{}{}-{}-cicd'.format(base, ver.replace('.', ''), pkg)
                envlist.append(name)
                out.append('[testenv:{}]'.format(name))
                out.append('commands = {{[testenv:{}]commands}}'.format(pkg))
                out.append('conda_deps =')
                out.append('    {}-base=={}'.format(base, ver))
                out.append('    {[testenv]conda_deps}')
                out.append('')

        # Write new Tox configuration
        with open(args.tox_ini, 'r') as tox_in:
            lines = iter(tox_in.readlines())

        out_file = '{}-{}.ini'.format(os.path.splitext(args.tox_ini)[0], pkg)
        with open(out_file, 'w') as tox_out:
            for line in lines:
                # Override envlist
                if line.startswith('envlist'):
                    tox_out.write('envlist =\n')
                    for item in envlist:
                        tox_out.write('    {}\n'.format(item))
                    else:
                        tox_out.write('    empty\n')
                    for line in lines:
                        if not line.startswith(' '):
                            break
                tox_out.write(line)

            # Write new environments
            for item in out:
                tox_out.write(item)
                tox_out.write('\n')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('tox_ini', type=str, metavar='ini-file',
                        help='path to tox.ini file')

    parser.add_argument('--root', type=str, metavar='<directory>', default='.',
                        help='root directory of R package')
    parser.add_argument('--platform', '-p', type=str, metavar='<platform>',
                        choices=['linux-64', 'osx-64', 'win-64', 'linux-ppc64le'],
                        default=get_platform(),
                        help='platform of the resulting package')

    args = parser.parse_args()

    sys.exit(main(args) or 0)
