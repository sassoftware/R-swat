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
    vers = dict(r=set(), mro=set())

    for i, pkg in enumerate(['r::r-dplyr', 'r::r-httr', 'r::r-testthat', 'r::r-xlsx']):
        cmd = ['conda', 'search', '--json', '--platform', platform, pkg]
        out = subprocess.check_output(cmd).decode('utf-8')

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

            # Ignore versions older than 3.4.3
            if tuple([int(x) for x in rver.split('.')]) < (3, 4, 3):
                continue

            if i == 0:
                vers[base].add(rver)
            elif rver not in vers[base]:
                vers[base].remove(rver)

    vers['r'] = list(sorted(vers['r']))
    vers['mro'] = list(sorted(vers['mro']))

    return vers


def main(args):
    ''' Main routine '''
    info = get_supported_versions(args.platform)

    # Pick a subset of the matrix to test.
    subset = dict(r=set(), mro=set())

    # Take the newest version, oldest version, and a random one.
    if info['r']:
        subset['r'].add(info['r'][0])
        subset['r'].add(info['r'][-1])
        if len(info['mro']) > 2:
            subset['r'].add(random.choice(info['r'][1:-1]))

    if info['mro']:
        subset['mro'].add(info['mro'][0])
        subset['mro'].add(info['mro'][-1])
        if len(info['mro']) > 2:
            subset['mro'].add(random.choice(info['mro'][1:-1]))

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
