# -*- coding: utf-8; -*-

import sys
import os
from getpass import getpass
from subprocess import Popen, PIPE, check_call

def find_password(service, username):
    proc = Popen(['security', 'find-generic-password',
                  '-s', service, '-a', username, '-w'],
                 stdout=PIPE)
    stdout, _ = proc.communicate()
    return stdout if proc.returncode == 0 else None


def add_password(service, username, password):
    check_call(['security', 'add-generic-password',
                '-a', username, '-s', service, '-w', password,
                '-j', 'OfflineImap password',
                '-T', os.path.abspath(sys.argv[0])])


def get_password(username):
    service = 'OfflineImap ' + username.split('@')[1]
    password = find_password(service, username)
    if not password:
        password = getpass('Password for {0}: '.format(username))
        add_password(service, username, password)
    return password
