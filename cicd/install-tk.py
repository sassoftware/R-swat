#!/usr/bin/env python

'''
Install TK libraries and C extensions

'''

import argparse
import io
import json
import os
import platform
import re
import requests
import subprocess
import sys
import zipfile

# Name of the package that contains C extensions
TK_PACKAGE_NAME = 'casrclnt'

# Possible locations for CAS client TK packages
TK_URLS = [x + '/release/{package}/{package}_en.zip' for x in [
    '{tk_base}/sashpa/dev/mva-{release}f/cda/zippkg/{platform}',
    '{tk_base}/sashpa/day/mva-{release}/cda/zippkg/{platform}',
    '{tk_base}/sashpa/wky/mva-{release}f/cda/zippkg/{platform}',
    '{tk_base}/sashpa/wky/mva-{release}/cda/zippkg/{platform}',
]]

# TK packages to install
TK_PKGS = [
    TK_PACKAGE_NAME,
    'tk',
    'tkcore',
    'tkcas3rdclnt',
    'tknls',
    'tkformats',
    'tkl4sas',
]

# File patterns that should be excluded from the installation
TK_EXCLUDES = [re.compile('^' + x + '$', flags=re.I) for x in [
    r'mkl_.+\.(so|dll)',
    r'libmkl_custom\.(so|dll)',
    r'tkclang\.(so|dll)',
    r'xorgpkg\.(so|dll)',
    r't[0-9][a-z][0-9](de|es|ja|zh|zt|ko|it|pl|fr)\.(so|dll)',
    r'(htclient|httplogin|.*arrow.*|libpaquet|libtkcpp).(so|dll)',
    r'(tkcasl|tkconsul|tkcudajit|tkhttpc?|tkek8s|tkmgpu|tkscript).(so|dll)',
]]

# File patterns that should be included in the installation
TK_INCLUDES = [re.compile('^' + x + '$', flags=re.I) for x in [
    r'.+\.so',
    r'.+\.dll',
    r'.+\.dylib',
]]

# Map of conda platform names to SAS platform names
PLATFORM_MAP = {
    'linux-64': 'lax',
    'linux-ppc64le': 'plx',
    'osx-64': 'm64',
    'win-64': 'wx6',
}


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


def extract_zip(root, data):
    ''' Write files from zip file to directory '''
    with zipfile.ZipFile(io.BytesIO(data)) as zip_in:
        for name in zip_in.namelist():

            basename = os.path.basename(name)

            # Filter unneeded files
            exclude = True
            for item in TK_INCLUDES:
                if item.match(basename):
                    exclude = False
                    break
            if exclude:
                continue

            exclude = False
            for item in TK_EXCLUDES:
                if item.match(basename):
                    exclude = True
                    break
            if exclude:
                continue

            # Write file
            out_path = os.path.join(root, basename)
            with open(out_path, 'wb') as out_file:
                out_file.write(zip_in.read(name))


def update_tk_version(root, version):
    ''' Add the TK version to the DESCRIPTION file of the package '''
    tk_file = os.path.join(root, 'DESCRIPTION')
    with open(tk_file, 'r') as tk_file_in:
        txt = tk_file_in.read()
        txt = re.sub(r'(TKVersion\s*:\s*)(\S+)', r'\1{}'
                     .format(re.sub(r'f$', r'', version)), txt)
    with open(tk_file, 'w') as tk_file_out:
        tk_file_out.write(txt)


def get_packages(lib_root, tk_base, release, platform, pkgs):
    '''
    Retrieve all TK packages

    Parameters
    ----------
    lib_root : string
        Directory where SWAT package exists
    tk_base : string
        Base URL of TK package repository
    release : string
        TK release
    platform : string
        Platform to search for TK components
    pkgs : list
        List of TK package names to retrieve

    '''
    resp = None
    is_installed = True

    for pkg in pkgs:
        for url in TK_URLS:
            url = url.format(tk_base=tk_base,
                             release=release,
                             platform=PLATFORM_MAP[platform],
                             package=pkg)

            resp = requests.head(url, allow_redirects=True)

            if resp.status_code == 404:
                continue

            if resp.status_code != 200:
                raise RuntimeError('{} code occurred during download of {}'
                                   .format(resp.status_code, url))

            print_err(url)

            resp = requests.get(url, allow_redirects=True)

            extract_zip(lib_root, resp.content)

            break

        # Package was not found, bail out
        if resp.status_code == 404:
            is_installed = False
            break

    return is_installed


def main(args):
    ''' Main routine '''
    if args.platform == 'win-64':
        args.tk_base = args.tk_base.replace('unix', 'win')

    # Fix TK versions (Linux-only at vb015; Windows-only at vb020)
    if args.release == 'vb020' and args.platform != 'win-64':
        args.release = 'vb015'
    elif args.release == 'vb015' and args.platform == 'win-64':
        args.release = 'vb020'

    # Directory for TK libraries
    lib_root = os.path.join(args.root, 'inst', 'libs')
    if args.platform == 'win-64':
        lib_root = os.path.join(lib_root, 'x64')

    # Create output directory
    os.makedirs(lib_root, exist_ok=True)

    is_installed = get_packages(lib_root, args.tk_base, args.release,
                                args.platform, TK_PKGS)

    update_tk_version(args.root, is_installed and args.release or 'none')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('root', type=str, nargs='?', default='.',
                        help='root directory of R package')

    parser.add_argument('-p', '--platform', type=str, metavar='<platform>',
                        default=get_platform(),
                        choices=['linux-64', 'osx-64', 'linux-ppc64le', 'win-64'],
                        help='platform libraries to install')
    parser.add_argument('-r', '--release', type=str, metavar='<release>',
                        default='vbviya', help='TK release')
    parser.add_argument('--tk-base', type=str, required=True, metavar='<url>',
                        help='base URL / path for TK repository')

    args = parser.parse_args()

    sys.exit(main(args) or 0)
