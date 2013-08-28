#!/usr/bin/env python

import os
import cStringIO

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
        0: 'Any',
        1: 'LParen', 
        2: 'Label',
        3: 'RParen',
        4: 'Comma',
        5: 'Single quote (open)',
        6: 'Single quote (close)',
        7: 'Double quote (open)',
        8: 'Double quote (close)',
        9: 'Comment',
        10: 'NHX key',
        11: 'NHX value',
        12: 'Branch length',
        13: 'Support value',
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
    ANY     = 0
    LPAREN  = 1
    LABEL   = 2
    RPAREN  = 3
    COMMA   = 4
    LSQUOTE = 5
    RSQUOTE = 6
    LDQUOTE = 7
    RDQUOTE = 8
    COMMENT = 9
    NHXKEY  = 10
    NHKVAL  = 11
    LENGTH  = 12
    SUPPORT = 13

    def __init__(self, stream):
        if isinstance(stream, str):
            stream = cStringIO.StringIO(stream)
        self.stream = stream
        self.token = None
        self.token_buffer = bytearray()
        self.state = self.find_tree

    def _next(self):
        char = self.stream.read(1)
        if char == '':
            self.token = None
            self.token_buffer = []
            l.stream.close()
            raise LexerStop
        return char

    def _backup(self):
        self.stream.seek(-1, 1)

    def _peek(self):
        char = self._next()
        self._backup()
        return char

    def _emit(self, item):
        self.token = item
        self.token_buffer = self.token_buffer[0:0]

    def next(self):
        try:
            self.state = self.state()
            if self.token:
                return self.token
        except LexerStop:
            return None

    def basic_state_function(self):
        char = self._next()
        self.token_buffer.append(char)
        item = Item(ANY, str(self.token_buffer))
        self._emit(item)
        return self.basic_state_function

    def find_tree(self):
        while not self.stream.closed:
            char = self._next()
            if char == '(':
                self._emit(Item(LPAREN, char))
                return self.lex_node
        print 'Could not find a tree'
        return None

    def lex_node(self):
        startpos = self.stream.tell()
        char = self._next()
        if char == '(':
            self._emit(Item(LPAREN, char))
            return self.lex_node
        elif char == "'":
            self._emit(Item(LSQUOTE, char))
            return self.lex_single_quoted_label
        elif char == '"':
            self._emit(Item(LDQUOTE, char))
            return self.lex_double_quoted_label
        elif char.isalpha():
            self._backup()
            return self.lex_label
        elif char == ',':
            self._emit(Item(COMMA, char))
            return self.lex_node
        elif char == ')':
            self._emit(Item(RPAREN, char))
            return self.lex_length_or_label
        else:
            endpos = self.stream.tell()
            self.stream.seek(startpos)
            guilty = self.stream.read(endpos - startpos)
            raise Exception('Error closing node\n{0}'.format(guilty))

    def lex_single_quoted_label(self):
        pass

    def lex_double_quoted_label(self):
        pass

    def lex_label(self):
        pass

    def lex_length_or_label(self):
        pass












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

