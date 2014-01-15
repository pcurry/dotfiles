# -*- coding: utf-8; -*-

import keyring
from getpass import getpass


def get_password(username):
    service = 'OfflineImap ' + username.split('@')[1]
    password = keyring.get_password(service, username)
    if not password:
        password = getpass('Password for {0}: '.format(username))
        keyring.set_password(service, username, password)
    return password
