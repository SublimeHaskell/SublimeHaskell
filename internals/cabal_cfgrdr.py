
import re
import os

import SublimeHaskell.internals.utils as Utils

def cabal_config():
    cconfig = os.environ.get('CABAL_CONFIG') or \
        ('~/.cabal' if not Utils.is_windows() else r'%APPDATA%/cabal') + \
        "/config"

    # Various patterns to match...
    re_user_dirs = re.compile(r'^install-dirs\s+user')
    re_global_dirs = re.compile(r'^install-dirs\s+global')
    re_section = re.compile(r'^\w+')
    re_prefix = re.compile(r'prefix:\s+(.*)$')
    re_bindir = re.compile(r'bindir:\s+(.*)$')

    # Things to collect
    user_prefix = "$HOME/.cabal" if not Utils.is_windows() else r'%APPDATA%/cabal'
    # FIXME: Need to interrogate Shel32 for the Windows PROGRAMFILES known
    # folder path:
    global_prefix = "/usr/local" if not Utils.is_windows() else r'%PROGRAMFILES%/Haskell'
    user_bindir = "bin"
    global_bindir = "bin"
    p_state = 0

    try:
        with open(Utils.normalize_path(cconfig), 'rU') as f_cconfig:
            # You would think that the Cabal maintainers would use a
            # well known file format... But now, they didn't. And they
            # had to go with an indentation-specific format.
            #
            # This is a "cheap and dirty" scanner to pick up
            for line in f_cconfig:
                line = line.rstrip()
                # One of the sections?
                if re_user_dirs.match(line):
                    p_state = 1
                elif re_global_dirs.match(line):
                    p_state = 2
                elif re.match(r'^\s+\w', line):
                    # prefix attribute?
                    m_prefix = re_prefix.search(line)
                    if m_prefix:
                        if p_state == 1:
                            user_prefix = m_prefix.group(1)
                        elif p_state == 2:
                            global_prefix = m_prefix.group(1)
                    # bindir attribute?
                    m_bindir = re_bindir.search(line)
                    if m_bindir:
                        if p_state == 1:
                            user_bindir = m_bindir.group(1)
                        elif p_state == 2:
                            global_bindir = m_bindir.group(1)
                elif re_section.match(line):
                    p_state = 0

    except IOError:
        # Silently fail.
        pass

    return [os.path.join(user_prefix, user_bindir)
            , os.path.join(global_prefix, global_bindir)
           ]
