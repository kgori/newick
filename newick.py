#!/usr/bin/env python

import os, sys
if sys.version_info < (3,):
    from cStringIO import StringIO
else:
    from io import StringIO


class TaxNode(object):
    def __init__(self, name):
        self.name = name
        self.up = None
        self.down = list()

    def addChild(self, c):
        if not c in self.down:
            self.down.append(c)

    def addParent(self, p):
        if self.up is not None and self.up != p:
            raise TaxonomyInconsistencyError(
                "Level {} has several parents, at least two: {}, {}"
                .format(self.name, self.up.name, p.name))
        self.up = p

    def iterLeaves(self):
        if len(self.down) == 0:
            yield self
        else:
            for child in self.down:
                for elem in child.iterLeaves():
                    yield elem

class Taxonomy(object):
    def __init__(self):
        raise NotImplementedError("abstract class")

    def iterParents(self, node, stopBefor=None):
        if node == stopBefor:
            return
        tn = self.hierarchy[node]
        while tn.up is not None and tn.up.name != stopBefor:
            tn = tn.up
            yield tn.name

    def _countParentAmongLevelSet(self, levels):
        levelSet = set(levels)
        cnts = dict()
        for lev in levelSet:
            t = set(self.iterParents(lev)).intersection(levelSet)
            cnts[lev] = len(t)
        return cnts

    def mostSpecific(self, levels):
        # count who often each element is a child of any other one.
        # the one with len(levels)-1 is the most specific level
        cnts = self._countParentAmongLevelSet(levels)
        for lev, cnt in cnts.items():
            if cnt==len(levels)-1:
                return lev
        raise Exception("None of the element is subelement of all others")

    def mostGeneralLevel(self, levels):
        # count who often each element is a child of any other one.
        # the one with len(levels)-1 is the most specific level
        cnts = self._countParentAmongLevelSet(levels)
        for lev, cnt in cnts.items():
            if cnt==0:
                return lev
        raise Exception("None of the element is the root of all others")

    def printSubTreeR(self, fd, lev=None, indent=0):
        if lev is None:
            lev = self.root
        fd.write("{}{}\n".format(" "*2*indent, lev))
        for child in self.hierarchy[lev].down:
            self.printSubTreeR(fd, child.name, indent+1)

    def __str__(self):
        fd = io.StringIO()
        self.printSubTreeR(fd)
        res = fd.getvalue()
        fd.close()
        return res

class Item(object):

    itemtypes = {
        0: 'End of file',
        1: 'Tree',
        2: 'LParen', 
        3: 'Label',
        4: 'RParen',
        5: 'Comma',
        6: 'Colon',
        7: 'Branch length',
        8: 'Support value',
        9: 'End',
    }

    def __init__(self, typ, val):
        self.typ = typ
        self.val = val

    def __repr__(self):
        typ = self.itemtypes[self.typ]  
        return '{0}: {1}'.format(typ, self.val)

class LexerStop(Exception):
    pass

class Lexer(object):

    # constants representing item types
    EOF     = 0
    TREE    = 1
    LPAREN  = 2
    LABEL   = 3
    RPAREN  = 4
    COMMA   = 5
    COLON   = 6
    LENGTH  = 7
    SUPPORT = 8
    END     = 9

    def __init__(self, stream):
        if isinstance(stream, str):
            stream = StringIO(stream)
        self.stream = stream
        self.token = None
        self.token_buffer = bytearray()
        self.state = self.lex_tree
        self.pos = 0

    def _next(self):
        char = self.stream.read(1)
        if char == '':
            raise LexerStop
        self.pos += 1
        return char

    def _backup(self):
        self.stream.seek(-1, 1)
        self.pos -= 1

    def _tidyup(self):
        self.token = None
        self.token_buffer
        self.stream.close()

    def _peek(self):
        char = self._next()
        self._backup()
        return char

    def _emit(self, item):
        self.token = item
        self._empty_buffer()

    def _empty_buffer(self):
        self.token_buffer = self.token_buffer[0:0]

    def _stop(self):
        raise LexerStop

    def next(self):
        try:
            while not self.token:
                self.state = self.state()
            token, self.token = self.token, None
            return token
        except LexerStop:
            self._emit(Item(self.EOF, 1))
            self._tidyup()

    def lex_tree(self):
        while not self.stream.closed:
            char = self._next()
            if char == '(':
                self._emit(Item(self.TREE, char))
                return self.lex_node
            if char == '':
                break
        self._emit(Item(self.EOF, None))
        return None

    def lex_before_node(self):
        """
        Enter: after seeing LPAREN
        Expect: LABEL (can be blank) or LPAREN
        Also accept: RPAREN (empty node)
        """
        char = self._next()
        if char == '(':
            self._emit(Item(self.LPAREN, char))
            return self.lex_node

        elif char == ')':
            self._emit(Item(self.RPAREN, char))
            return self.lex_label

        elif char.isspace(): # ignore
            return self.lex_node

        # elif char == ',':  # bug - ignores missing labels eg (,,(,));
        #     return self.lex_node

        elif char == ';':
            self._emit(Item(self.END, char))
            return self.lex_tree

        else:
            self._backup()
            return self.lex_label

    def lex_after_node(self):
        pass


    def lex_label(self):
        """
        Enter: after seeing LPAREN, RPAREN or COMMA
        Expect: LENGTH
        """
        char = self._next()
        if char in ('"', "'"):
            self._match_quoted_label(char)
        else:
            self._backup()
            de_spacer = {' ': ''}
            self._match_run(str.isalnum, accepted_chars='-_|', 
                denied_chars=':,;', replacements=de_spacer)
        self._emit(Item(self.LABEL, str(self.token_buffer)))
        if self._peek() == ',':
            self._next()

        return self.lex_length


    def lex_length(self):
        char = self._next()
        if char == ':':
            self._match_number()
            if len(self.token_buffer) == 0:
                num = 0.0
            else: 
                num = float(self.token_buffer)
        else:
            self._backup()
            num = 0.0
        self._emit(Item(self.LENGTH, num))
        return self.lex_node

    def _match_quoted_label(self, terminator):
        while not self.stream.closed:
            char = self._next()
            if char == terminator:
                return 
            self.token_buffer.append(char)
        raise Exception('Unmatched {0}-quoted string'.format(terminator))

    def _match(self, match_fun, accepted_chars='', denied_chars='',
        replacements=None):
        replacements = (replacements or {})
        char = self._next()
        char = replacements.get(char, char)
        if match_fun(char) or char in accepted_chars:
            self.token_buffer.append(char)
            return 1
        elif char in denied_chars:                
            self._backup()
            return 0
        else:
            self._backup()
            return 0

    def _match_run(self, match_fun, **kwargs):
        nchars = 0
        while not self.stream.closed:
            matched = self._match(match_fun, **kwargs)
            nchars += matched
            if matched == 0:
                return nchars
        raise Exception('Unexpected end of stream')

    def _match_number(self): 
        digits = 0      
        self._match(lambda x: False, '-+')
        digits += self._match_run(str.isdigit)
        if self._match(lambda x: False, '.'):
            digits += self._match_run(str.isdigit)
        
        if digits > 0:
            if self._match(lambda x: False, 'eE'):
                c2=self._match(lambda x: False, '-+')
                ndigits = self._match_run(str.isdigit)
        
        else:
            self._empty_buffer()
            self.token_buffer.append('0')
        
        return









class NewickTaxonomy(Taxonomy):
    def __init__(self, filename):

        if not os.path.exists(filename):
            raise Exception('Newick Taxonomy file not found:', filename)

        self.charstream = self.get_file_iterator(filename)
   
    def get_file_iterator(self, filename):
        with open(filename) as fobj:
            while True:
                next_char = fobj.read(1)
                if next_char == '':
                    raise StopIteration
                yield next_char

    def _match_lparen(self, stream):
        pass

    def parse(self):
        while self.charstream:
            print(next(self.charstream))







def parse(s):
    pass

