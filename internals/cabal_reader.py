'''Cabal project file reader.
'''

import io

class CabalFileReader(object):
    def __init__(self, cabal_file):
        self.cabal_info = {}

        try:
            with open(cabal_file, "r", encoding='utf-8') as f_cabal:
                for cline in f_cabal:
                    pass
        except OSError:
            pass