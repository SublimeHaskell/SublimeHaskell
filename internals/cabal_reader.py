'''Cabal project file reader.
'''

import os.path
import platform
# import pprint

class IndentedFileReader(object):
    '''An indented file parser that roughly mimics the Haskell Distribution.Parsec code used to parse both
    Cabal project specifications and Cabal's configuration files..
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

    # Lexing modes:
    LEX_BEGIN_SECTION = 1
    LEX_IN_SECTION = 2
    LEX_IN_FIELD_LAYOUT = 3

    LEX_STATE_NAMES = {LEX_BEGIN_SECTION: 'begin:section',
                       LEX_IN_SECTION: 'in:section',
                       LEX_IN_FIELD_LAYOUT: 'in:field-layout'}

    def __init__(self, project_name, file_name):
        self.content = ''
        self.content_idx = 0
        self.inp_line = 0
        self.indent_stack = []
        self.token_stack = []
        self.lexmode = self.LEX_BEGIN_SECTION

        norm_fname = os.path.normcase(os.path.normpath(file_name))
        try:
            with open(norm_fname, 'r', encoding='utf-8') as f_cabal:
                self.cabal_info = self.parse_indented_file(project_name, f_cabal)
        except OSError:
            print('File not found: {0}'.format(norm_fname))
            self.cabal_info = {}


    def parse_indented_file(self, project_name, file):
        tok = self.lex(file, lexmode=self.LEX_BEGIN_SECTION)
        element_dict = {}

        (tok, element_dict) = self.parse_elements(tok, file)
        if tok[0] == self.TOK_ERROR:
            print('parse_indented_file {0}: {1}'.format(project_name, tok[1]))
            return {}

        # pprint.pprint(element_dict)
        return element_dict


    def parse_elements(self, tok, file):
        elts = {}
        while tok[0] not in [self.TOK_UNDENT, self.TOK_ERROR, self.TOK_EOF]:
            (tok, elts) = self.parse_element(tok, file, elts)

        return (tok, elts)


    def parse_element(self, tok, file, element_dict):
        if tok[0] == self.TOK_NAME:
            name = tok[1].lower()
            tok = self.lex(file)
            retval = None

            if tok[0] == self.TOK_COLON:
                self.skip_eol()
                tok = self.lex(file, lexmode=self.LEX_IN_FIELD_LAYOUT)
                # print('tok LEX_IN_FIELD_LAYOUT: {0}'.format(tok))
                field = element_dict.get(name, [])
                if tok[0] not in [self.TOK_EOF, self.TOK_EOL, self.TOK_ERROR]:
                    field += tok[1]
                # print('updating {0} with {1}'.format(name, field))
                element_dict.update({name: field})
                retval = (self.lex(file), element_dict)
            elif tok[0] in [self.TOK_EOL, self.TOK_NAME, self.TOK_OTHER]:
                retval = self.parse_section(tok, file, name, element_dict)
            elif tok[0] == self.TOK_UNDENT:
                retval = (self.lex(file), element_dict)
            else:
                retval = ((self.TOK_ERROR, '\'{0}\' unexpected in input.'.format(tok[1])), element_dict)

            # print('parse_element returning:')
            # pprint.pprint(retval)
            return retval
        else:
            return ((self.TOK_ERROR, 'Expected a name'), element_dict)


    def parse_section(self, tok, file, name, element_dict):
        args = []
        while tok[0] not in [self.TOK_EOF, self.TOK_EOL, self.TOK_ERROR]:
            args.append(tok[1])
            tok = self.lex(file)

        if tok[0] != self.TOK_ERROR:
            (tok, element) = self.parse_elements(self.lex(file, lexmode=self.LEX_BEGIN_SECTION), file)

            section = element_dict.get(name)
            if section is None:
                element_dict[name] = {}
                section = element_dict[name]

            # Ensure that the subsidiary dictionaries exist:
            for arg in args:
                if section.get(arg) is None:
                    section[arg] = {}
                section = section[arg]

            section.update(element)

            if tok[0] == self.TOK_UNDENT:
                tok = self.lex(file)

        return (tok, element_dict)

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
                    retval = (self.TOK_EOF, '')
                else:
                    while self.indent_stack and indent < self.indent_stack[-1]:
                        self.indent_stack.pop()
                        self.token_stack.append((self.TOK_UNDENT, ''))

                    if self.lexmode == self.LEX_BEGIN_SECTION:
                        # Save the current indent level when beginning a new section
                        self.indent_stack.append(indent)
                        self.lexmode = self.LEX_IN_SECTION

                    retval = self.lex(file)
            elif self.lexmode == self.LEX_IN_SECTION:
                retval = self.lex_section()
            elif self.lexmode == self.LEX_IN_FIELD_LAYOUT:
                retval = self.lex_field_arg(file, self.lexmode)
                self.lexmode = self.LEX_IN_SECTION
            else:
                retval = self.lex_error()

        # self.diag_token(retval)
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
        if i >= clen:
            # Mark end of line for next lex()
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
                # Deal with a undents
                self.indent_stack.pop()
                self.token_stack.append((self.TOK_UNDENT, ''))
        else:
            self.token_stack.append((self.TOK_EOF, ''))

        # Trim empty lines from the front and the back...
        while ret_content[0] == '':
            ret_content = ret_content[1:]
        while ret_content[-1] == '':
            ret_content.pop()

        return (self.TOK_FIELDARG, ret_content)


    def skip_eol(self):
        while self.token_stack and self.token_stack[0][0] == self.TOK_EOL:
            self.token_stack = self.token_stack[1:]


    def lex_error(self):
        return (self.TOK_ERROR, 'line {0}: Unexpected input \'{1}\''.format(self.inp_line, self.content[self.content_idx]))


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

    def diag_token(self, token):
        print('token {0} \'{1}\''.format(self.TOKEN_NAMES.get(token[0]), token[1]))


class CabalConfigReader(IndentedFileReader):
    def __init__(self, cabal_config):
        is_windows = platform.system() == "Windows"
        user_prefix = "$HOME/.cabal" if not is_windows else r'%APPDATA%/cabal'
        super().__init__('cabal-config', os.path.join(os.path.expandvars(os.path.expanduser(user_prefix)), cabal_config))


class CabalProjectReader(IndentedFileReader):
    def __init__(self, project_dir, project_name):
        project_cabal = os.path.expandvars(os.path.expanduser(os.path.join(project_dir, project_name + '.cabal')))
        super().__init__(project_name, project_cabal)
