'''Cabal project file reader.
'''

import os.path
import pprint

#import SublimeHaskell.internals.logging as Logging

class CabalFileReader(object):
    '''A cabal file parser that roughly mimics the Haskell Distribution.Parsec code.
    '''
    # Parser tokens:
    TOK_UNDENT = -3
    TOK_ERROR = -2
    TOK_EOF = -1
    TOK_EOL = 1
    TOK_NAME = 2
    TOK_COLON = 3
    TOK_OTHER = 4
    TOK_FIELDARG = 5

    TOKEN_NAMES = {TOK_UNDENT: 'UNDENT',
                   TOK_ERROR: 'ERROR',
                   TOK_EOF: 'EOF',
                   TOK_EOL: 'EOL',
                   TOK_NAME: 'NAME',
                   TOK_COLON: 'COLON',
                   TOK_OTHER: 'OTHER',
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
    LEX_BEGIN_SECTION = 1
    LEX_IN_SECTION = 2
    LEX_IN_FIELD_LAYOUT = 3

    LEX_STATE_NAMES = {LEX_BEGIN_SECTION: 'begin:section',
                       LEX_IN_SECTION: 'in:section',
                       LEX_IN_FIELD_LAYOUT: 'in:field-layout'}

    def __init__(self, project_dir, project_name):
        self.content = ''
        self.content_idx = 0
        self.inp_line = 0
        self.indent_stack = []
        self.state_stack = [self.STATE_START]
        self.var_stack = [{}]
        self.token_stack = []
        self.lexmode = self.LEX_BEGIN_SECTION

        try:
            with open(os.path.join(project_dir, project_name + ".cabal"), "r", encoding='utf-8') as f_cabal:
                self.cabal_info = self.parse_indented_file(project_name, f_cabal)
        except OSError:
            self.cabal_info = {}


    def parse_indented_file(self, project_name, file):
        keep_parsing = True
        while keep_parsing:
            self.diag_stacks()
            if self.in_state(self.STATE_START):
                tok = self.lex(file, lexmode=self.LEX_BEGIN_SECTION)
                self.reduce(self.STATE_START, None)
            elif self.in_state(self.STATE_ELEMENTS):
                if tok[0] == self.TOK_UNDENT:
                    print('state_elements undent')
                    self.reduce(self.STATE_ELEMENTS, self.current_var())
                    self.reduce(self.STATE_FIELD_OR_SECTION, self.current_var())
                    self.reduce(self.STATE_ELEMENT, self.current_var())
                    tok = self.lex(file)
                elif tok[0] == self.TOK_EOF:
                    keep_parsing = False
                else:
                    self.shift(self.STATE_ELEMENT, {})
            elif self.in_state(self.STATE_ELEMENT):
                if tok[0] == self.TOK_NAME:
                    self.shift(self.STATE_FIELD_OR_SECTION, tok[1])
                    tok = self.lex(file)
                elif tok[0] == self.TOK_UNDENT:
                    # End of an indented section
                    print('state_element undent')
                    self.reduce(self.STATE_ELEMENT, self.current_var())
                    self.reduce(self.STATE_ELEMENTS, self.current_var())
                    self.reduce(self.STATE_FIELD_OR_SECTION, self.current_var())
                    tok = self.lex(file)
                else:
                    tok = (self.TOK_ERROR, '{0}:{1}: Expected a section or field name'.format(project_name, self.inp_line))
            elif self.in_state(self.STATE_FIELD_OR_SECTION):
                if tok[0] == self.TOK_COLON:
                    # It's a field. Consume the EOL if it's there (and it'll be on the token stack)
                    if self.token_stack and self.token_stack[0][0] == self.TOK_EOL:
                        self.token_stack = self.token_stack[1:]
                    tok = self.lex(file, lexmode=self.LEX_IN_FIELD_LAYOUT)
                    fld_name = self.current_var()
                    self.reduce(self.STATE_FIELD_OR_SECTION, {fld_name: tok[1]})
                    self.reduce(self.STATE_ELEMENT, self.current_var())
                    tok = self.lex(file)
                elif tok[0] in [self.TOK_EOL, self.TOK_NAME, self.TOK_OTHER]:
                    # It's a section
                    section = self.current_var()
                    args = []
                    # tok = self.lex(file)
                    while tok[0] not in [self.TOK_EOL, self.TOK_ERROR]:
                        args.append(tok[1])
                        tok = self.lex(file)
                    if tok[0] != self.TOK_ERROR:
                        self.update_current_var((section, args))
                        self.shift(self.STATE_ELEMENTS, {})
                        tok = self.lex(file, lexmode=self.LEX_BEGIN_SECTION)
                else:
                    tok = (self.TOK_ERROR,
                           '{0}:{1}: Expected \':\'-separated field or section arguments'.format(project_name, self.inp_line))
            elif self.in_state(self.TOK_UNDENT):
                self.reduce(self.STATE_ELEMENT, self.current_var())

            print('token {0} {1}'.format(self.TOKEN_NAMES.get(tok[0]), tok[1]))
            if tok[0] in [self.TOK_EOF, self.TOK_ERROR]:
                keep_parsing = False
                if tok[0] == self.TOK_ERROR:
                    print(tok[1])

    def in_state(self, parse_state):
        return self.state_stack[-1] == parse_state


    def current_var(self):
        return self.var_stack[-1]

    def update_current_var(self, newval):
        self.var_stack[-1] = newval
        return newval

    def shift(self, parse_state, state_var):
        '''Shift to a new parse state.
        '''
        self.state_stack.append(parse_state)
        self.var_stack.append(state_var)


    def reduce(self, parse_state, state_var):
        '''Actions associated with successfully parsing something
        '''
        if not self.in_state(parse_state):
            print('wrong state in reduce: {0} expected {1}'.format(self.STATE_NAMES.get(parse_state, '???'),
                                                                   self.STATE_NAMES.get(self.state_stack[-1], '???')))
        elif parse_state == self.STATE_START:
            self.state_stack[-1] = self.STATE_ELEMENTS
            self.var_stack[-1] = {}
        elif parse_state == self.STATE_ELEMENTS:
            self.state_stack.pop()
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
    CLASS_PAREN = set(['(', ')', '[', ']'])
    CLASS_OPLIKE = set([',', '.', '=', '<', '>', '+', '*', '-', '&', '|', '!', '$', '%', '^', '@', '#', '?', '/', '\\', '~'])

    def lex(self, file, lexmode=None):
        if self.token_stack:
            # Pull off the token stack, if we have additional tokens to consume
            retval = self.token_stack[0]
            self.token_stack = self.token_stack[1:]
        else:
            if lexmode is not None:
                self.lexmode = lexmode

            retval = ()
            if self.content == '' or self.content_idx >= len(self.content):
                indent, self.content = self.get_content(file, self.lexmode)
                self.content_idx = 0
                if indent < 0:
                    return (self.TOK_EOF, '')
                elif self.indent_stack and indent < self.indent_stack[-1]:
                    self.indent_stack.pop()
                    return (self.TOK_UNDENT, '')
                elif self.lexmode == self.LEX_BEGIN_SECTION:
                    # begin section makes us save the current indent level
                    self.indent_stack.append(indent)
                    self.lexmode = self.LEX_IN_SECTION

            if self.lexmode == self.LEX_IN_SECTION:
                retval = self.lex_section()
            elif self.lexmode == self.LEX_IN_FIELD_LAYOUT:
                retval = self.lex_field_arg(file, self.lexmode)
                self.lexmode = self.LEX_IN_SECTION
            else:
                retval = self.lex_error()

        # print('lex: ({0}, \'{1}\')'.format(self.TOKEN_NAMES.get(retval[0]), retval[1]))
        return retval

    def lex_section(self):
        '''
        '''
        i = self.content_idx
        clen = len(self.content)

        # skip the whitespace
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
        elif curc in self.CLASS_PAREN:
            retval = (self.TOK_OTHER, curc)
            i += 1
        elif curc in self.CLASS_OPLIKE:
            op_begin = i
            i += 1
            while i < clen and self.content[i] in self.CLASS_OPLIKE:
                i += 1
            retval = (self.TOK_OTHER, self.content[op_begin:i])
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
        while indent >= 0 and (self.content == '' or indent > self.indent_stack[-1]):
            ret_content.append(self.content)
            indent, self.content = self.get_content(file, lexmode)

        if indent >= 0:
            while indent < self.indent_stack[-1]:
                # Deal with a pending undent
                self.indent_stack.pop()
                self.token_stack.append((self.TOK_UNDENT, ''))
        else:
            self.token_stack.append((self.TOK_EOF, ''))

        return (self.TOK_FIELDARG, ret_content)

    def lex_error(self):
        return (self.TOK_ERROR, 'line {0}: Unexpected input \'{1}\''.format(self.inp_line, self.content[self.content_idx]))


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

                cmnt = content.find('--')
                if cmnt >= 0:
                    content = content[:cmnt]
                if content != '' or lexmode == self.LEX_IN_FIELD_LAYOUT:
                    got_content = True
            else:
                got_content = True
                indent = -1
                content = ''

        # print('indent: {0} content \'{1}\''.format(indent, content))
        return (indent, content)

    def diag_token_stack(self):
        tok_name = self.TOKEN_NAMES.get(self.token_stack[0], self.token_stack[0]) if self.token_stack else '<empty>'
        print('token_stack[0] {0}'.format(tok_name))

    def diag_stacks(self):
        pretty_states = [self.STATE_NAMES.get(s, '<unknown>') for s in self.state_stack]
        print('state stack:  {0}\nindent stacK: {1}'.format(pretty_states, self.indent_stack))
