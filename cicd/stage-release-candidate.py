#!/usr/bin/env python

'''
Utitily for preparing a new release

This utility creates a Github release with the specified version,
title, and assets. When the `-snapshot` option is used, a draft version
of the current code is created. This is used to create daily builds
that aren't full public releases.

'''

import argparse
import datetime
import glob
import os
import re
import requests
import sys
import subprocess
import tempfile
from urllib.parse import quote


if '--help' not in sys.argv and '-h' not in sys.argv:
    try:
        GITHUB_TOKEN = os.environ['GITHUB_TOKEN']
    except KeyError:
        sys.stderr.write('ERROR: This utility requires a Github '
                         'token for accessing the Github release API.\n')
        sys.stderr.write('       The variable should be held in an '
                         'environment variable named GITHUB_TOKEN.\n')
        sys.exit(1)

RELEASE_TEMPLATE = '''
Highlights include:

{highlights}

To install the {name} package, you use a command as follows::

```
R CMD INSTALL R-swat-{version}-linux-ppc64le.tar.gz
R CMD INSTALL R-swat-{version}-linux64.tar.gz
R CMD INSTALL R-swat-{version}-win64.tar.gz
```

Some platforms do not support all CAS protocols, you can still use the REST interface by installing the REST-only package with R CMD INSTALL.
```
R CMD INSTALL R-swat-{version}-REST-only-osx.tar.gz
```

<b>NOTE:</b> The platform-specific installers bundle libraries from the SAS TK subsystem.  These bundled libraries are required for using the binary interface to CAS.
'''


def extract_highlights(version):
    '''  Extract highlights for specified version '''
    version = version.replace('v', '')
    txt = []
    with open('CHANGELOG.md', 'r') as in_file:
        for line in in_file:
            if not re.match(r'##\s+{}'.format(version), line):
                continue
            for line in in_file:
                line = line.strip()
                if not line:
                    continue
                if re.match(r'##', line):
                    break
                txt.append(line)
    return '\n'.join(txt)


def get_repo():
    ''' Retrieve the repo part of the git URL '''
    cmd = ['git', 'remote', 'get-url', 'origin']
    repo = subprocess.check_output(cmd).decode('utf-8').strip()
    repo = re.search(r'github.com[/:](.+?)\.git$', repo).group(1)
    return repo


def create_release(version, tag_name, target_commitish, name=None,
                   draft=False, prerelease=True, assets=None):
    '''
    Create new release on Github

    POST /repos/:owner/:repo/releases

    '''
    with open('DESCRIPTION', 'r') as in_file:
        pkg_name = re.search(r'^\s*Package\s*:\s*(\S+)',
                             in_file.read(), flags=re.M).group(1)

    if '.9000' in version:
        body = '(Development snapshot)'
    else:
        body = RELEASE_TEMPLATE.format(
            highlights=extract_highlights(version),
            tag=tag_name, pkg_name=pkg_name, version=version,
            name=(name or tag_name).format(tag=tag_name, version=version))

    res = requests.post(
        'https://api.github.com/repos/{}/releases'.format(get_repo()),
        headers=dict(Authorization='token {}'.format(GITHUB_TOKEN),
                     Accept='application/vnd.github.v3+json'),
        json=dict(tag_name=tag_name,
                  target_commitish=target_commitish,
                  name=(name or tag_name).format(tag=tag_name, version=version),
                  body=body, draft=draft, prerelease=prerelease)
    )

    if res.status_code >= 400:
        raise RuntimeError(res.json())

    res = res.json()

    if assets:
        upload_assets(res['upload_url'].split('{')[0], assets)


def upload_assets(url, assets):
    ''' Upload assets to the release '''
    for asset in assets:
        print(' > {}'.format(asset))
        upload_url = url + '?name={}'.format(quote(os.path.split(asset)[-1]))
        with open(asset, 'rb') as asset_file:
            requests.post(
                upload_url,
                headers={'Authorization': 'token {}'.format(GITHUB_TOKEN),
                         'Accept': 'application/vnd.github.v3+json',
                         'Content-Type': 'application/octet-stream'},
                data=asset_file
            )


def next_version(version):
    ''' Return the next dev version number '''
    return version + '.9000'


def edit_changelog(version):
    ''' Open editor on CHANGELOG.md '''
    filename = 'CHANGELOG.md'

    with open(filename, 'r') as in_file:
        txt = in_file.read()

    if '.9000' not in version and version not in txt:
        date = datetime.date.today().strftime('%Y-%m-%d')
        txt = re.sub(r'(#\s+Change\s+Log\s+)',
                     r'\1## {} - {}\n\n- \n\n'.format(version, date),
                     txt, flags=re.I)
        with open(filename, 'w') as out_file:
            out_file.write(txt)

    editor = os.environ.get('EDITOR', 'vim')
    cmd = [editor, filename]
    subprocess.check_call(cmd)

    with open(filename, 'r') as in_file:
        txt = in_file.read().strip()
        if not txt:
            raise RuntimeError('{} is empty'.format(filename))

    git_add(filename)


def set_version(version):
    ''' Set the version number in source files '''
    files = ['DESCRIPTION', 'conda.recipe/meta.yaml']

    for f in files:
        with open(f, 'r') as in_file:
            txt = in_file.read()
        with open(f, 'w') as out_file:
            out_file.write(
                re.sub(r'^(\s*Version\s*:\s*)\S+(\s*)$',
                       r'\g<1>{}\g<2>'.format(version), txt, flags=re.M))
        git_add(f)


def git_add(filename):
    ''' Add file to staging '''
    cmd = ['git', 'add', filename]
    subprocess.check_call(cmd)


def git_commit(message):
    ''' Commit changes '''
    cmd = ['git', 'diff', '--name-only', '--cached']
    txt = subprocess.check_output(cmd).decode('utf-8').strip()
    if txt:
        cmd = ['git', 'commit', '-m', message]
        subprocess.check_call(cmd)
    cmd = ['git', 'log', '-1', '--format=%H']
    return subprocess.check_output(cmd).decode('utf-8').strip()


def get_head_sha():
    ''' Return the sha of the current HEAD '''
    cmd = ['git', 'log', '-1', '--format=%H', 'HEAD']
    return subprocess.check_output(cmd).decode('utf-8').strip()


def git_tag(tag):
    ''' Add a tag '''
    cmd = ['git', 'tag', tag]
    subprocess.check_call(cmd)


def git_push(tag=None):
    ''' Push updates '''
    cmd = ['git', 'push']
    subprocess.check_call(cmd)
    if tag:
        cmd = ['git', 'push', 'origin', tag]
        subprocess.check_call(cmd)


def git_fetch():
    ''' Make sure we have all commits and tags '''
    cmd = ['git', 'fetch', '--tags']
    subprocess.check_call(cmd)


def git_diff():
    ''' Show current diffs '''
    cmd = ['git', 'diff', '--staged']
    subprocess.check_call(cmd)


def get_version():
    ''' Get package version number '''
    with open('DESCRIPTION', 'r') as in_file:
        txt = in_file.read()
        return re.search(r'^\s*Version\s*:\s*(\S+)', txt, re.M).group(1)


def delete_release(tag_name):
    ''' Remove local and remote tags for the given release '''
    # Delete release
    res = requests.get(
        'https://api.github.com/repos/{}/releases/tags/{}'.format(get_repo(), tag_name),
        headers=dict(Authorization='token {}'.format(GITHUB_TOKEN),
                     Accept='application/vnd.github.v3+json'))

    if res.status_code < 400:
        release_url = res.json()['url']
        res = requests.delete(
            release_url,
            headers=dict(Authorization='token {}'.format(GITHUB_TOKEN),
                         Accept='application/vnd.github.v3+json'))

    # Delete tags
    cmd = ['git', 'show-ref', '--tags']
    for line in subprocess.check_output(cmd).decode('utf-8').strip().split('\n'):
        sha, tag = re.split(r'\s+', line.strip())
        tag = tag.split('/')[-1]
        if tag == tag_name:
            cmd = ['git', 'tag', '-d', tag]
            subprocess.check_call(cmd)
            cmd = ['git', 'push', 'origin', ':refs/tags/{}'.format(tag)]
            subprocess.check_call(cmd)
            break


def checkout_main():
    ''' Make sure we're on the main branch '''
    subprocess.check_call(['git', 'checkout', 'main'])


def version_type(value):
    ''' Check version syntax '''
    if re.match(r'^\d+\.\d+\.\d+', value):
        return value
    raise argparse.ArgumentTypeError(value)


def main(args):
    ''' Main routine '''
    # Make sure local repo is up-to-date
    print('\nNOTE: Updating local git repo.\n')
    git_fetch()
    checkout_main()

    if args.snapshot:
        version = get_version()
        version = [int(x) for x in re.sub(r'\.9000$', r'', version).split('.')]
        version = '{}.{}.{}'.format(version[0], version[1], version[2] + 1)
        tag = 'v{}-snapshot'.format(version)
        sha = get_head_sha()

    else:
        version = args.version or get_version()
        tag = 'v{}-rc'.format(version)

        # Make file changes for release
        if args.version:
            print('\nNOTE: Updating version number in source files.\n')
            set_version(version)
        else:
            print('\nNOTE: Using {} as the release version number.\n'.format(version))

        edit_changelog(version)

        # Verify changes and push
        git_diff()
        txt = input('\nContinue with release candidate? [Y/n]: ')
        if txt.strip().lower() not in ['y', 'yes', '']:
            return 2

        print('\nNOTE: Committing changes and creating release.\n')
        sha = git_commit('Create {} release candidate.'.format(tag))

    # Push release
    delete_release(tag)
    git_tag(tag)
    git_push(tag=tag)
    create_release(version, tag, sha, name=args.title, assets=args.assets)

    # Push updates for next development release
    if not args.snapshot and args.version:
        print('\nNOTE: Incrementing development version in source and commit.\n')
        set_version(next_version(version))
        git_commit('Increment development version')
        git_push()

    return 0


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__.strip(),
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument('--version', '-v', type=version_type, metavar='version',
                        help='version of the package')
    parser.add_argument('--title', '-t', type=str, metavar='release-title',
                        default='SWAT {tag}', help='title of the release')
    parser.add_argument('--snapshot', '-s', action='store_true',
                        help='create a snapshot release (--version is ignored)')
    parser.add_argument('assets', type=str, metavar='filename', nargs='*',
                        help='assets to include with the release')

    args = parser.parse_args()

    try:
        sys.exit(main(args))
    except KeyboardInterrupt:
        sys.exit(1)
