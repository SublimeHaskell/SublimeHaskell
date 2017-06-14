'''Cabal project file reader.
'''

import os.path
import pprint

#import SublimeHaskell.internals.logging as Logging

class CabalFileReader(object):
    '''
    '''
    # Parser tokens:
    TOK_ERROR = -2
    TOK_EOF = -1
    TOK_NAME = 1
    TOK_COLON = 2
    TOK_FIELDARG = 3

    TOKEN_NAMES = {TOK_ERROR: 'ERROR',
                   TOK_EOF: 'EOF',
                   TOK_NAME: 'NAME',
                   TOK_COLON: 'COLON',
                   TOK_FIELDARG: 'FIELDARG'}

    # Parser states:
    STATE_ELEMENTS = 0
    STATE_ELEMENT = 1
    STATE_FIELD_OR_SECTION = 2

    # Lexing modes:
    LEX_IN_SECTION = 1
    LEX_IN_FIELD_LAYOUT = 2

    def __init__(self, project_dir, project_name):
        self.content = ''
        self.indent_stack = [0]
        self.state_stack = [self.STATE_ELEMENTS]
        self.content_idx = 0
        self.inp_line = 0

        try:
            with open(os.path.join(project_dir, project_name + ".cabal"), "r", encoding='utf-8') as f_cabal:
                xlat_dict = {}
                self.cabal_info = self.read_cabal_file(f_cabal, xlat_dict)
        except OSError:
            self.cabal_info = {}


    def read_cabal_file(self, file, xlat):
        tok = self.get_token(file)
        while tok[0] not in [self.TOK_EOF, self.TOK_ERROR]:
            if self.state_stack[-1] == self.STATE_ELEMENTS:
                self.state_stack.append(self.STATE_ELEMENT)
            elif self.state_stack[-1] == self.STATE_ELEMENT:
                if tok[0] == self.TOK_NAME:
                    self.state_stack.append(self.STATE_FIELD_OR_SECTION)
                    tok = self.get_token(file)
                else:
                    tok = (self.TOK_ERROR, 'Expected a section or field name')
            elif self.state_stack[-1] == self.STATE_FIELD_OR_SECTION:
                if tok[0] == self.TOK_COLON:
                    # It's a field.
                    tok = self.get_token(file, lexmode=self.LEX_IN_FIELD_LAYOUT)
                    self.state_stack.pop()
                    tok = self.get_token(file)
                elif tok[0] == self.TOK_NAME:
                    # It's a section
                    print('section arg')
                    break

    # Lexical classes
    CLASS_ALPHA = set([chr(c) for c in range(ord('A'), ord('Z')+1)] + [chr(c) for c in range(ord('a'), ord('z')+1)])
    CLASS_NAMECORE = CLASS_ALPHA
    CLASS_NAMEEXTRA = CLASS_NAMECORE | set(['-', '_', '.', '\''])
    CLASS_SPACETAB = set([' ', '\t'])
    CLASS_NBSPSPACETAB = set([chr(0xa0)]) | CLASS_SPACETAB
    CLASS_OPLIKE = set([',', '.', '=', '<', '>', '+', '*', '-', '&', '|', '!', '$', '%', '^', '@', '#', '?', '/', '\\', '~'])

    def get_token(self, file, lexmode=LEX_IN_SECTION):
        indent = self.indent_stack[0]
        if self.content == '':
            indent, self.content = self.get_content(file, lexmode)
            self.content_idx = 0
            if indent < 0:
                return (self.TOK_EOF, '')

        retval = ()
        clen = len(self.content)
        if lexmode == self.LEX_IN_SECTION:
            # skip the whitespace
            i = self.content_idx
            while i < clen and self.content[i] in self.CLASS_NBSPSPACETAB:
                i += 1
            self.content_idx = i
            curc = self.content[i]
            print('curc = {0} i = {1}'.format(curc, i))
            if curc in self.CLASS_NAMEEXTRA or curc in self.CLASS_NAMECORE:
                while i < clen and self.content[i] in self.CLASS_NAMEEXTRA:
                    i += 1
                while i < clen and self.content[i] in self.CLASS_NAMECORE:
                    i += 1
                while i < clen and self.content[i] in self.CLASS_NAMEEXTRA:
                    i += 1
                retval = (self.TOK_NAME, self.content[self.content_idx:i])
                self.content_idx = i
            elif curc == ':':
                retval = (self.TOK_COLON, ':')
                self.content_idx += 1
        elif lexmode == self.LEX_IN_FIELD_LAYOUT:
            i = self.content_idx
            while i < clen and self.content[i] in self.CLASS_NBSPSPACETAB:
                i += 1
            ret_content = self.content[i:] if i < clen else ''
            indent, self.content = self.get_content(file, lexmode)
            while indent > self.indent_stack[0]:
                ret_content += '\n' + self.content
                indent, self.content = self.get_content(file, lexmode)
            retval = (self.TOK_FIELDARG, ret_content)
        else:
            retval = (self.TOK_ERROR, 'line {0}, column {1}: Unexpected input \'{2}\''.format(self.inp_line, self.content_idx,
                                                                                              self.content[self.content_idx]))

        print('get_token: ({0}, \'{1}\')'.format(self.TOKEN_NAMES.get(retval[0]), retval[1]))
        return retval

    def printable(self, char):
        '''Returns True if the character `char` is printable (not a control character)
        '''
        char_val = ord(char)
        return (char_val >= 0x20 and char_val <= 0x10ffff) and char_val != 0x7f

    def get_content(self, file, lexmode):
        got_content = False
        self.content_idx = 0
        while not got_content:
            content = file.readline()
            self.inp_line += 1
            if content != '':
                indent = len(content) - len(content.lstrip())
                content = content.strip()

                if lexmode == self.LEX_IN_SECTION:
                    cmnt = content.find('--')
                    if cmnt >= 0:
                        content = content[:cmnt]
                    if content != '':
                        got_content = True
                elif lexmode == self.LEX_IN_FIELD_LAYOUT:
                    got_content = True
            else:
                got_content = True
                indent = -1
                content = ''

        return (indent, content)
