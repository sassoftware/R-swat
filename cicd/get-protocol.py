#!/usr/bin/env python

'''
Return the preferred CAS protocol ('cas' if TK is available; 'http' otherwise)

This utility checks the package `DESCRIPTION` file for a `TKVersion`
parameter that indicates the packaged version of the TK libraries.
If it finds it, the `cas` protocol will be returned. If the value is
set to `None`, the `http` protocol is returned.

'''

import argparse
import glob
import os
import re
import sys


def main(args):
    ''' Main routine '''
    version = None

    init = glob.glob(os.path.join(args.root, 'DESCRIPTION'))[0]
    with open(init, 'r') as init_in:
        for line in init_in:
            m = re.match(r'''TKVersion\s*:\s*(\S+)''', line)
            if m:
                version = m.group(1)
                if version == 'none':
                    version = None
                break

    print(version and 'cas' or 'http')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('root', type=str, metavar='<directory>',
                        default='.', nargs='?',
                        help='root directory of R package')

    args = parser.parse_args()

    sys.exit(main(args) or 0)
