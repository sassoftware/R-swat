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


def main(args):
    ''' Main routine '''
    info, exc = get_supported_versions(args.platform)

    print('> Available versions for {}:'.format(args.platform))
    for key, value in info.items():
        if value:
            print('  + {}-base'.format(key))
        for item in sorted(value):
            print('    {}'.format(item))

    if exc['r'] or exc['mro']:
        print('')
        print('> Excluded versions:')
        for key, value in exc.items():
            if value:
                print('  + {}-base'.format(key))
            for k, v in sorted(value.items()):
                print('    {}: {}'.format(k, ', '.join(v)))

    # Pick a subset of the matrix to test.
    subset = dict(r=set(), mro=set())

    print('')
    print('> Subset of versions used for test environments:')

    # Take the newest version, oldest version, and a random one.
    if info['r']:
        subset['r'].add(info['r'][0])
        subset['r'].add(info['r'][-1])
        if len(info['mro']) > 2:
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
