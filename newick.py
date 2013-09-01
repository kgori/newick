#!/usr/bin/env python

import os, sys
import collections
if sys.version_info < (3,):
    from cStringIO import StringIO
else:
    from io import StringIO

# constants representing item types
EOF     = 0
TREE    = 1
SUBTREE = 2
LEAF    = 3
LABEL   = 4
ENDSUB  = 5
LENGTH  = 6
ENDTREE = 7

DEFAULT_BRANCH_LENGTH = 1.0

class Item(object):

    itemtypes = {
        0: 'End of file',
        1: 'Tree',
        2: 'Subtree', 
        3: 'Leaf',
        4: 'Label',
        5: 'Subtree end',
        6: 'Branch length',
        7: 'End tree',
    }

    def __init__(self, typ, val):
        self.typ = typ
        self.val = val

    def __repr__(self):
        typ = self.itemtypes[self.typ]  
        return '{0}: "{1}"'.format(typ, self.val)

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


    def __init__(self, streamer):
        
        self.streamer = streamer
        self.token = None
        self.token_buffer = bytearray()
        self.state = self.lex_tree

    def buffer(self, char):
        """ Adds a streamed character to the token buffer """
        self.token_buffer.append(ord(char))

    def eat_spaces(self):
        while self.streamer.peek().isspace():
            next(self.streamer)

    def emit(self, item):
        """ Emits the token buffer's contents as a token; clears the buffer """
        self.token = item
        self.empty_buffer()

    def empty_buffer(self):
        """ Clears the token buffer (python 2 has no bytearray.clear()
        method )"""
        self.token_buffer = self.token_buffer[0:0]

    def __iter__(self):
        return self

    def next(self):
        """ Hack to enable python 2.x iteration """
        return self.__next__()

    def __next__(self):
        """ Each iteration returns a token. While a token isn't ready,
        advance the state machine one state. """
        while not self.token:
            self.state = self.state()
        token, self.token = self.token, None
        return token

    def stop(self):
        raise StopIteration

    def lex_tree(self):
        # while self.streamer.peek() != '(':
        #     next(self.streamer)
        for x in self.streamer:
            if x == '(':
                break
        
        if self.streamer.isclosed():
            self.emit(Item(EOF, -1))
            return self.stop

        self.emit(Item(TREE, ''))
        return self.lex_subtree_start

    def lex_subtree_start(self):
        self.eat_spaces()
        char = self.streamer.peek()

        if char == '(':
            self.emit(Item(SUBTREE, next(self.streamer)))
            return self.lex_subtree_start

        else:
            self.emit(Item(LEAF, None))
            return self.lex_label

    def lex_label(self):
        char = self.streamer.peek()
        if char in ('"', "'"):
            next(self.streamer) # throw away opening quote 
            self._match_delimited(char)
        else:
            despacer = {' ': '_'}
            self._match_run(str.isalnum, accepted_chars='-_|.', 
                denied_chars=':,;', replacements=despacer)
        self.emit(Item(LABEL, self.token_buffer.decode()))

        return self.lex_length

    def lex_length(self):
        char = self.streamer.peek()
        if char == ':':
            self.streamer.next() # throw away colon
            self._match_number()
            if len(self.token_buffer) == 0:
                num = DEFAULT_BRANCH_LENGTH
            else: 
                num = float(self.token_buffer)
        else:
            num = DEFAULT_BRANCH_LENGTH
        self.emit(Item(LENGTH, num))
        return self.lex_subtree_end

    def lex_subtree_end(self):
        self.eat_spaces()
        char = self.streamer.peek()

        if char == ';':
            next(self.streamer)
            self.emit(Item(ENDTREE, ';'))
            return self.lex_tree

        elif char == ',':
            next(self.streamer)
            return self.lex_subtree_start

        elif char == ')':
            next(self.streamer)
            self.emit(Item(ENDSUB, ')'))
            return self.lex_label

        else:
            raise LexError('Don\'t know how to lex this: {0} ({1})'.format(
                char, self.streamer.stream.tell()))

    def _match_delimited(self, delimiter):
        for char in self.streamer:
            if char == delimiter:
                return
            self.buffer(char)

        raise LexError('Unterminated {0}-delimited string'.format(delimiter))

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
            if len(char) == 1:
                self.buffer(char) 
            next(self.streamer) # advance stream
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
            raise LexError('Unexpected end of stream')

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
            self.empty_buffer()

class Tree(object):
    """ Container type for nodes in a tree """

    def __init__(self, node):
        self.seed = node

class ParseError(Exception):
    def __init__(self, msg):
        self.msg = msg
    def __str__(self):
        return self.msg

class LexError(Exception):
    def __init__(self, msg):
        self.msg = msg
    def __str__(self):
        return self.msg

class Parser(object):

    def __init__(self, lexer):
        self.lexer = lexer
        self.trees = []
        self.stack = [] # top value is the node to add new siblings of 
            # the current subtree. Below is the same for previous subtrees

    @classmethod
    def parse_from_file(cls, filename):
        f = open(filename)
        s = Streamer(f)
        l = Lexer(s)
        parser = cls(l)
        parser.parse()
        return parser.trees

    @classmethod
    def parse_from_string(cls, s):
        s = Streamer(s)
        l = Lexer(s)
        parser = cls(l)
        parser.parse()
        return parser.trees

    def _get_data(self):
        """
        Get the node data attributes 'label' and 'length'. Assumes these will
        be the next tokens in the stream. Throws ParseError if they are not.
        """
        label = next(self.lexer)
        if label.typ != LABEL:
            raise ParseError('Expected a label')
        
        length = next(self.lexer)
        if length.typ != LENGTH:
            raise ParseError('Expected a length')

        return (label.val, length.val)

    def _add(self):
        if len(self.stack) == 0:
            raise ParseError('No nodes in stack')
        bud = Node(Data(None))
        new = Node(Data(None))
        self.stack[-1].add_next(bud)
        self.stack[-1] = bud
        bud.add_out(new, None)
        return new

    def add_leaf(self, label, length):
        leaf = self._add()
        leaf.data.label = label
        leaf.set_length(length)

    def add_subtree(self):
        subtree = self._add()
        self.stack.append(subtree)

    def close_subtree(self, label, length):
        subtree = self.stack.pop()
        subtree.data.label = label
        subtree.next.set_length(length)

    def parse(self):
        for token in self.lexer:
            if token.typ == EOF:
                return
            
            elif token.typ == TREE:
                seed = Node(Data(None))
                self.trees.append(Tree(seed))
                self.stack.append(seed)

            elif token.typ == SUBTREE:
                self.add_subtree()

            elif token.typ == LEAF:
                label, length = self._get_data()
                self.add_leaf(label, length)

            elif token.typ == ENDSUB:
                label, length = self._get_data()
                self.close_subtree(label, length)

            # labels and lengths should always be dealt with by LEAF and ENDSUB
            # cases, and should be seen here - ParseError is raised
            elif token.typ == LABEL: 
                raise ParseError('Unexpected label token')

            elif token.typ == LENGTH:
                raise ParseError('Unexpected length token')

            elif token.typ == ENDTREE:
                pass

class Node(object):

    def __init__(self, data):
        self.data = data
        self.length = 0
        self.next = None
        self.out = None

    def __str__(self):
        label = self.data.label
        length = self.length
        # return '{0}:{1}'.format(label, length)
        return '{0}: next=\'{1}\'; out=\'{2}\'; length={3}'.format(repr(self),
                repr(self.next), repr(self.out), self.length)

    def __repr__(self):
        if not self.data.label is None:
            return 'Node(Data(\'{0}\'))'.format(self.data.label)
        return 'Node(Data({0}))'.format(self.data.label)

    def add_next(self, node):
        node.data = self.data
        if self.next is None:
            (node.next, self.next) = (self, node)
        else:
            (node.next, self.next) = (self.next, node)

    def add_out(self, node, length):
        (self.out, node.out) = (node, self)
        self.length = node.length = length

    def set_length(self, length):
        self.length = length
        if self.out:
            self.out.length = length

    def isleaf(self):
        return self.next == None

    def preorder_generator(self):

        yield self
        for n in self.loop():
            for val in n.out.preorder_generator():
                yield val

    def postorder_generator(self):

        for n in self.loop():
            for val in n.out.postorder_generator():
                yield val
        yield self

    def levelorder_generator(self):
        q = Queue()
        q.enqueue(self)
        while q:
            node = q.dequeue()
            yield node
            for n in node.loop():
                q.enqueue(n.out)

    def loop(self):
        n = self.next
        if n is None:
            n = self
        while n != self:
            yield n
            n = n.next

class Data(object):

    def __init__(self, label):
        self.label = label
        self.attributes = {}

    def add_attribute(self, key, value):
        self.attributes[key] = value

class Queue(object):

    def __init__(self):
        self.queue = collections.deque()

    def __iter__(self):
        return self

    def __len__(self):
        return len(self.queue)

    def next():
        return self.__next__()

    def __next__(self):
        if self.isempty():
            raise StopIteration
        return self.dequeue()

    def enqueue(self, item):
        self.queue.append(item)

    def dequeue(self):
        if self.isempty():
            raise Exception('empty queue')
        return self.queue.popleft()

    def isempty(self):
        return len(self.queue) == 0









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


class NewickTaxonomy(Taxonomy):
    pass


