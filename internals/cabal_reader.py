'''Cabal project file reader.
'''

import os.path
import pprint

#import SublimeHaskell.internals.logging as Logging

class CabalFileReader(object):
    '''A cabal file parser that roughly mimics the Haskell Distribution.Parsec code.
    '''
    # Parser tokens:
    TOK_ERROR = -2
    TOK_EOF = -1
    TOK_EOL = 1
    TOK_NAME = 2
    TOK_COLON = 3
    TOK_FIELDARG = 4

    TOKEN_NAMES = {TOK_ERROR: 'ERROR',
                   TOK_EOF: 'EOF',
                   TOK_EOL: 'EOL',
                   TOK_NAME: 'NAME',
                   TOK_COLON: 'COLON',
                   TOK_FIELDARG: 'FIELDARG'}

    # Parser states:
    STATE_START = -1
    STATE_ELEMENTS = 0
    STATE_ELEMENT = 1
    STATE_FIELD_OR_SECTION = 2

    STATE_NAMES = {STATE_START: 'start state',
                   STATE_ELEMENTS: 'elements',
                   STATE_ELEMENT: 'element',
                   STATE_FIELD_OR_SECTION: 'field or section'}

    # Lexing modes:
    LEX_IN_SECTION = 1
    LEX_IN_FIELD_LAYOUT = 2

    def __init__(self, project_dir, project_name):
        self.content = ''
        self.content_idx = 0
        self.inp_line = 0
        self.indent_stack = [0]
        self.state_stack = [self.STATE_START]
        self.var_stack = [{}]
        self.token_stack = []

        try:
            with open(os.path.join(project_dir, project_name + ".cabal"), "r", encoding='utf-8') as f_cabal:
                self.cabal_info = self.read_cabal_file(f_cabal)
        except OSError:
            self.cabal_info = {}


    def read_cabal_file(self, file):
        tok = self.lex(file)
        while tok[0] not in [self.TOK_EOF, self.TOK_ERROR]:
            print('state: {0} {1}'.format(self.STATE_NAMES.get(self.state_stack[-1], '<unknown>'), self.state_stack))
            if self.in_state(self.STATE_START):
                tok = self.lex(file)
                self.reduce(self.STATE_START, None)
            if self.in_state(self.STATE_ELEMENTS):
                self.shift(self.STATE_ELEMENT, {})
            elif self.in_state(self.STATE_ELEMENT):
                if tok[0] == self.TOK_NAME:
                    self.shift(self.STATE_FIELD_OR_SECTION, tok[1])
                    tok = self.lex(file)
                else:
                    tok = (self.TOK_ERROR, 'Expected a section or field name')
            elif self.in_state(self.STATE_FIELD_OR_SECTION):
                if tok[0] == self.TOK_COLON:
                    # It's a field. Consume the EOL if it's there (and it'll be on the token stack)
                    if self.token_stack and self.token_stack[0] == self.TOK_EOL:
                        self.token_stack = self.token_stack[1:]
                    tok = self.lex(file, lexmode=self.LEX_IN_FIELD_LAYOUT)
                    fld_name = self.current_var()
                    self.reduce(self.STATE_FIELD_OR_SECTION, {fld_name: tok[1]})
                    tok = self.lex(file)
                elif tok[0] == self.TOK_NAME:
                    # It's a section
                    section = tok[1]
                    args = []
                    tok = self.lex(file)
                    while tok[0] not in [self.TOK_EOL, self.TOK_ERROR]:
                        args.append(tok[1])
                        tok = self.lex(file)
                    self.reduce(self.STATE_FIELD_OR_SECTION, (section, args))
                    tok = self.lex(file)
                else:
                    tok = (self.TOK_ERROR, 'Expected \':\'-separated field or optional section arguments')

            # print('token {0} {1}'.format(self.TOKEN_NAMES.get(tok[0]), tok[1]))

    def in_state(self, parse_state):
        return self.state_stack[-1] == parse_state


    def current_var(self):
        return self.var_stack[-1]

    def shift(self, parse_state, state_var):
        '''Shift to a new parse state.
        '''
        self.state_stack.append(parse_state)
        self.var_stack.append(state_var)


    def reduce(self, parse_state, state_var):
        '''Actions associated with successfully parsing something
        '''
        if not self.in_state(parse_state):
            pass
        elif parse_state == self.STATE_START:
            self.state_stack[-1] = self.STATE_ELEMENTS
            self.var_stack[-1] = {}
        elif parse_state == self.STATE_ELEMENTS:
            self.state_stack.append(self.STATE_ELEMENT)
        elif parse_state == self.STATE_ELEMENT:
            self.state_stack.pop()
        elif parse_state == self.STATE_FIELD_OR_SECTION:
            self.state_stack.pop()



    # Lexical classes for characters
    CLASS_ALPHA = set([chr(c) for c in range(ord('A'), ord('Z')+1)] + [chr(c) for c in range(ord('a'), ord('z')+1)])
    CLASS_DIGIT = set([chr(c) for c in range(ord('0'), ord('9')+1)])
    CLASS_NAMECORE = CLASS_ALPHA
    CLASS_NAMEEXTRA = CLASS_NAMECORE | CLASS_DIGIT | set(['-', '_', '.', '\''])
    CLASS_SPACETAB = set([' ', '\t'])
    CLASS_NBSPSPACETAB = set([chr(0xa0)]) | CLASS_SPACETAB
    CLASS_OPLIKE = set([',', '.', '=', '<', '>', '+', '*', '-', '&', '|', '!', '$', '%', '^', '@', '#', '?', '/', '\\', '~'])

    def lex(self, file, lexmode=LEX_IN_SECTION):
        # Pull off the token stack, if we have additional tokens to consume
        if self.token_stack:
            retval = self.token_stack[0]
            self.token_stack = self.token_stack[1:]
            return retval

        indent = self.indent_stack[0]
        if self.content == '' or self.content_idx >= len(self.content):
            indent, self.content = self.get_content(file, lexmode)
            self.content_idx = 0
            if indent < 0:
                return (self.TOK_EOF, '')

        retval = ()
        if lexmode == self.LEX_IN_SECTION:
            retval = self.lex_section()
        elif lexmode == self.LEX_IN_FIELD_LAYOUT:
            retval = self.lex_field_arg(file, lexmode)
        else:
            retval = self.lex_error()

        print('lex: ({0}, \'{1}\')'.format(self.TOKEN_NAMES.get(retval[0]), retval[1]))
        return retval

    def lex_section(self):
        '''
        '''
        # skip the whitespace
        i = self.content_idx
        clen = len(self.content)

        while i < clen and self.content[i] in self.CLASS_NBSPSPACETAB:
            i += 1
        curc = self.content[i]
        # print('curc = {0} i = {1}'.format(curc, i))
        if curc in self.CLASS_NAMEEXTRA or curc in self.CLASS_NAMECORE:
            name_begin = i
            while i < clen and self.content[i] in self.CLASS_NAMEEXTRA:
                i += 1
            while i < clen and self.content[i] in self.CLASS_NAMECORE:
                i += 1
            while i < clen and self.content[i] in self.CLASS_NAMEEXTRA:
                i += 1
            retval = (self.TOK_NAME, self.content[name_begin:i])
        elif curc == ':':
            retval = (self.TOK_COLON, ':')
            i += 1
        else:
            retval = self.lex_error()

        self.content_idx = i
        if self.content_idx >= clen:
            self.token_stack.append((self.TOK_EOL, ''))

        return retval

    def lex_field_arg(self, file, lexmode):
        i = self.content_idx
        clen = len(self.content)
        while i < clen and self.content[i] in self.CLASS_NBSPSPACETAB:
            i += 1
        ret_content = []
        if i < clen:
            ret_content.append(self.content[i:])
        indent, self.content = self.get_content(file, lexmode)
        while indent > self.indent_stack[0]:
            ret_content.append(self.content)
            indent, self.content = self.get_content(file, lexmode)

        return (self.TOK_FIELDARG, ret_content)

    def lex_error(self):
        return (self.TOK_ERROR, 'line {0}, column {1}: Unexpected input \'{2}\''.format(self.inp_line, self.content_idx,
                                                                                        self.content[self.content_idx]))
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
