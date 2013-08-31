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

class Streamer(object):

    def __init__(self, stream):
        """ _peek always looks ahead 1 position """
        if isinstance(stream, str):
            stream = StringIO(stream)
        self.stream = stream
        self._peek = self.stream.read(1)

    def __iter__(self):
        return self

    def next(self):
        return self.__next__()

    def __next__(self):
        char = self._peek

        self._peek = self.stream.read(1)

        if self.stream.closed:
            raise StopIteration
        
        if char == '':
            self.stream.close()
            raise StopIteration

        return char

    def peek(self):
        return self._peek

    def isclosed(self):
        return self.stream.closed

class Lexer(object):

    """ Breaks newick stream into lexing tokens:
    Works as a state machine, like Rob Pike's Go text template parser """

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

    DEFAULT_BRANCH_LENGTH = 1.0

    def __init__(self, streamer):
        
        self.streamer = streamer
        self.token = None
        self.token_buffer = bytearray()
        self.state = self.lex_tree

    def buffer(self, char):
        """ Adds a streamed character to the token buffer """
        self.token_buffer.append(ord(char))

    def check_stream(self, msg):
        if self.streamer.isclosed():
            raise Exception(msg)

    def eat_spaces(self):
        while self.streamer.peek().isspace():
            next(self.streamer)

    def emit(self, item):
        """ Emits the token buffer's contents as a token; clears the buffer """
        self.token = item
        self.empty_buffer()

    def empty_buffer(self):
        """ Clears the token buffer """
        self.token_buffer = self.token_buffer[0:0]

    def __iter__(self):
        return self

    def next(self):
        """ Hack to enable python 2.x iteration """
        return self.__next__()

    def __next__(self):
        while not self.token:
            self.state = self.state()
        token, self.token = self.token, None
        return token

    def stop(self):
        raise StopIteration

    def lex_tree(self):
        for x in self.streamer:
            if x == '(':
                break
        
        if self.streamer.isclosed():
            self.emit(Item(self.EOF, -1))
            return self.stop

        self.emit(Item(self.LPAREN, '('))
        return self.lex_subtree_start

    def lex_subtree_start(self):
        self.eat_spaces()
        char = self.streamer.peek()

        if char == '(':
            self.emit(Item(self.LPAREN, next(self.streamer)))
            return self.lex_subtree_start

        else:
            return self.lex_label

    def lex_label(self):
        char = self.streamer.peek()
        if char in ('"', "'"):
            next(self.streamer) # throw away opening quote 
            self._match_quoted_label(char)
        else:
            de_spacer = {' ': ''}
            self._match_run(str.isalnum, accepted_chars='-_|', 
                denied_chars=':,;', replacements=de_spacer)
        self.emit(Item(self.LABEL, self.token_buffer.decode()))

        return self.lex_length

    def lex_length(self):
        char = self.streamer.peek()
        if char == ':':
            self.streamer.next() # throw away colon
            self._match_number()
            if len(self.token_buffer) == 0:
                num = self.DEFAULT_BRANCH_LENGTH
            else: 
                num = float(self.token_buffer)
        else:
            num = self.DEFAULT_BRANCH_LENGTH
        self.emit(Item(self.LENGTH, num))
        return self.lex_after_subtree

    def lex_after_subtree(self):
        self.eat_spaces()
        char = self.streamer.peek()

        if char == ';':
            next(self.streamer)
            self.emit(Item(self.END, ';'))
            return self.lex_tree

        elif char == ',':
            next(self.streamer)
            return self.lex_subtree_start

        elif char == ')':
            next(self.streamer)
            self.emit(Item(self.RPAREN, ')'))
            return self.lex_label

        else:
            raise Exception('Don\'t know how to parse this: {0} ({1})'.format(
                char, self.streamer.stream.tell()))

    def _match_quoted_label(self, terminator):
        for char in self.streamer:
            if char == terminator:
                return
            self.buffer(char)

        raise Exception('Unterminated {0}-quoted string'.format(terminator))

    def _match(self, predicate, accepted_chars='', denied_chars='',
        replacements=None):
        """
        Checks next character in stream. If predicate returns True, or char
        is in `accepted_chars`, advances the stream and returns 1. Else, or if
        the char is in `denied_chars`, doesn't advance the stream and returns 0.
        Replacements is an optional dictionary that can be used to replace the
        streamed character with an alternative (e.g. replace spaces with 
        underscores).
        """

        replacements = (replacements or {})
        char = self.streamer.peek()
        char = replacements.get(char, char)
        if predicate(char) or char in accepted_chars:
            self.buffer(next(self.streamer)) # advance stream
            return 1
        elif char in denied_chars:                
            return 0
        else:
            return 0

    def _match_run(self, predicate, **kwargs):
        """
        kwargs are `accepted_chars`, `denied_chars` and `replacements`
        """
        nchars = 0
        try:
            while True:
                matched = self._match(predicate, **kwargs)
                nchars += matched
                if matched == 0:
                    return nchars
        except StopIteration:
            raise Exception('Unexpected end of stream')

    def _match_number(self): 
        digits = 0      
        self._match(lambda x: False, '-+')
        digits += self._match_run(str.isdigit)
        if self._match(lambda x: False, '.'):
            digits += self._match_run(str.isdigit)
        
        if digits > 0:
            if self._match(lambda x: False, 'eE'):
                self._match(lambda x: False, '-+')
                self._match_run(str.isdigit)
        
        else:
            self._empty_buffer()


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
