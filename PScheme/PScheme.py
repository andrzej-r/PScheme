# -*- coding: utf-8 -*-
# 
# Copyright 2011 PScheme Contributors (see CONTRIBUTORS for details). All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without modification, are
# permitted provided that the following conditions are met:
# 
#    1. Redistributions of source code must retain the above copyright notice, this list of
#       conditions and the following disclaimer.
# 
#    2. Redistributions in binary form must reproduce the above copyright notice, this list
#       of conditions and the following disclaimer in the documentation and/or other materials
#       provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY PSCHEME CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
# FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL PSCHEME CONTRIBUTORS
# BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
# ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# The views and conclusions contained in the software and documentation are those of the
# authors and should not be interpreted as representing official policies, either expressed
# or implied, of PScheme Contributors.

"""
This module implements an interpreter for a subset of Scheme R5RS.

User designs may require some scripting for parametrization or automation.
Although Python interpreter is readily available (the whole program
is written in this language) it cannot be reliably sand-boxed. Executing
user scripts in the context of the application would open a security hole,
which could easily be exploited remotely (by preparing a malicious design file).

Scheme was chosen for its expressiveness to simplicity ratio.
"""

import re
import sys
from copy import copy

# flags for the parser
flags = re.UNICODE

comment = r'(?:;.*$)'
lparen = r'(?:\()'
rparen = r'(?:\))'
number = r'(?:[+\-]?(?:(?:[0-9]+\.?[0-9]*)|(?:[0-9]*\.?[0-9]+))(?:e[+\-]?[0-9]+)?)'
symbol = r'(?:(?:[a-zA-Z!$%&*/:<=>?^_~][a-zA-Z0-9!$%&*/:<=>?^_~\d+-.@]*)|\+|\-|\.\.\.)'
string = r'(?:\"(?:\\"|[^"])*?\")'
brokenstring = r'(?:\"(?:\\"|[^"])*$)'
char = r'(?:#\\\S\w*)|(?:#\\)'
boolean = r'(?:#(?:t|f))'
quote = r'(?:\')'
quasiquote = r'(?:\`)'
unquote_splicing = r'(?:\,@)'
unquote = r'(?:\,)'
other = r'(?:\S+)'

tokens = (comment + '|' + lparen + '|' + rparen + '|' + number + '|' + symbol
          + '|' + string + '|' + brokenstring + '|' + char + '|' + boolean
          + '|' + quote + '|' + quasiquote + '|' + unquote_splicing + '|' + unquote
          + '|' + other)

ptokens = re.compile(tokens, flags)

class SchemeError(Exception):
    """
    Exception class representing Scheme Expression parsing or evaluation error.
    """
    def __init__(self, expr, msg=''):
        "``expr`` can be either SExpression or Token. It must contain ``meta`` attribute."
        self.expr = expr
        self.msg = msg

    def __str__(self):
        if not hasattr(self.expr, 'meta'): # in self.expr.__dict__:
            return 'Error: ' + self.msg
        fileName = self.expr.meta['fileName']
        line = self.expr.meta['line'].rstrip()
        lineNo = str(self.expr.meta['lineNo'])
        start = self.expr.meta['colStart']
        span = self.expr.meta['colEnd'] - start
        return ('Error: ' + self.msg + '\n' + fileName + ':' + lineNo + ', ' + line + '\n'
                + (' ' * (start+len(fileName)+len(lineNo)+2)) + ('-' * span))

class Token(object):
    """
    Class for representing a token of a Scheme source code. In addition to that it also
    stores some metadata (line containing the token, line and column numbers).
    """
    __slots__ = ['text', 'meta']
    def __init__(self, text, meta):
        self.text = text
        self.meta = meta

    def __str__(self):
        return self.text

    def __repr__(self):
        return '<Token ' + str(self) + '>'

class SExpression(object):
    __slots__ = []
    typeName = 'an s-expression'

    pnumber = re.compile(r'^' + number + r'$', flags)
    pstring = re.compile(r'^' + string + r'$', flags)
    pbrokenstring = re.compile(r'^' + brokenstring, flags)
    pchar = re.compile(r'^' + char + r'$', flags)
    pboolean = re.compile(r'^' + boolean + r'$', flags)
    psymbol = re.compile(r'^' + symbol + r'$', flags)

    @classmethod
    def parseTokens(cls, token, tokens, topLevel=False):
        text = token.text
        if text == '(':
            expr = Pair.parseTokens(token, tokens, topLevel=topLevel)
        elif SExpression.pnumber.match(text):
            expr = Number.parseToken(token)
        elif SExpression.pstring.match(text):
            expr = String.parseToken(token)
        elif SExpression.pbrokenstring.match(text):
            raise(SchemeError(token, 'Unterminated string ')) # + text))
        elif SExpression.pchar.match(text):
            expr = Char.parseToken(token)
        elif SExpression.pboolean.match(text):
            expr = Boolean.parseToken(token)
        elif text == '\'':
            try:
                expr = Pair.makeFromList(
                    [Symbol.make('quote'),
                     SExpression.parseTokens(next(tokens), tokens, topLevel=topLevel)])
                expr.meta = token.meta
            except StopIteration:
                raise(SchemeError(token, 'Nothing to quote.'))
        elif text == '`':
            try:
                expr = Pair.makeFromList(
                    [Symbol.make('quasiquote'),
                     SExpression.parseTokens(next(tokens), tokens, topLevel=topLevel)])
                expr.meta = token.meta
            except StopIteration:
                raise(SchemeError(token, 'Nothing to quasiquote.'))
        elif text == ',':
            try:
                expr = Pair.makeFromList(
                    [Symbol.make('unquote'),
                     SExpression.parseTokens(next(tokens), tokens, topLevel=topLevel)])
                expr.meta = token.meta
            except StopIteration:
                raise(SchemeError(token, 'Nothing to unquote.'))
        elif text == ',@':
            try:
                expr = Pair.makeFromList(
                    [Symbol.make('unquote-splicing'),
                     SExpression.parseTokens(next(tokens), tokens, topLevel=topLevel)])
                expr.meta = token.meta
            except StopIteration:
                raise(SchemeError(token, 'Nothing to unquote-splicing.'))
        elif SExpression.psymbol.match(text):
            expr = Symbol.parseToken(token)
        else:
            raise(SchemeError(token, 'Unrecognized token "' + text + '"'))
        return expr
            
    def eval(self, callingForm, frame, cont):
        raise SchemeError(callingForm,
                          'Abstract SExpression should not be evaluated directly')

    def quasiQuote(self, level, callingForm, frame, cont):
        return Trampolined.make(cont, self)

    def __ne__(self, other):
        return not (self == other)

    def __str__(self):
        return '#<unknown-expression 0x%x>' % id(self)

    def __repr__(self):
        return str(self)

    def isSelfEval(self):
        return False

    def isBoolean(self):
        return False

    def isSymbol(self):
        return False

    def isChar(self):
        return False

    def isVector(self):
        return False

    def isNull(self):
        return False

    def isPair(self):
        return False

    def isNumber(self):
        return False

    def isComplex(self):
        return False

    def isReal(self):
        return False

    def isRational(self):
        return False

    def isInteger(self):
        return False

    def isExact(self):
        return False

    def isInexact(self):
        return False

    def isString(self):
        return False

    def isProcedure(self):
        return False

    def isContinuation(self):
        return False

    def isList(self):
        return False

    def isSpecialSyntax(self):
        return False

    def isTrampolined(self):
        return False

class SelfEval(SExpression):
    __slots__ = ['value']
    typeName = 'a self-evaluating expression'

    def eval(self, callingForm, frame, cont):
        return Trampolined.make(cont, self)

    def isSelfEval(self):
        return True

    def __str__(self):
        return str(self.value)

class Nil(SelfEval):
    typeName = 'an undefined value'

    cache = None

    @classmethod
    def make(cls):
        if cls.cache:
            return cls.cache
        self = cls()
        self.value = None
        cls.cache = self
        return self

    def __str__(self):
        return '#<void>'

    def __eq__(self, other):
        return other is self

class Char(SelfEval):
    __slots__ = []
    typeName = 'a char'

    pchar = re.compile(r'^#\\(.)$', flags)
    pcharcode = re.compile(r'^#\\[xX]([0-9a-fA-F]{1,6})$', flags)

    name2char = {
        '#\\': ' ',
        '#\\space': ' ',
        '#\\tab': '\t',
        '#\\newline': '\n',
        '#\\return': '\r',
        '#\\null': '\x00',
        '#\\alarm': '\a',
        '#\\backspace': '\b',
        '#\\escape': '\x1b',
        '#\\delete': '\x7f',
        }

    char2name = dict(zip(name2char.values(), name2char.keys()))
    #print(name2char)
    #print(char2name)
    cache = {}

    @classmethod
    def make(cls, char):
        if char in cls.cache:
            return cls.cache[char]
        self = cls()
        self.value = char
        cls.cache[char] = self
        return self

    @classmethod
    def parseToken(cls, token, tokens=[]):
        char = cls.pchar.match(token.text)
        charCode = cls.pcharcode.match(token.text)
        if token.text in cls.name2char:
            string = cls.name2char[token.text]
        elif char != None:
            string = char.group(1)
        elif charCode != None:
            code = int(charCode.group(1), 16)
            try:
                if sys.version_info[0] == 2:
                    string = unichr(code)
                else:
                    string = chr(code)
            except ValueError:
                raise SchemeError(token, 'Invalid character code.')
        else:
            raise SchemeError(token, 'Unknown character name.')
        char = cls.make(string)
        return char

    def isChar(self):
        return True

    def __repr__(self):
        char = self.value
        if char in self.char2name:
            return self.char2name[char]
        if 32 <= ord(char) < 127:
            return '#\\' + char
        if ord(char) < 256:
            return '#\\x%02x' % ord(char)
        if ord(char) < 256*256:
            return '#\\x%04x' % ord(char)
        else:
            return '#\\x%06x' % ord(char)

    def __str__(self):
        return self.value

    def __unicode__(self):
        return self.value

    def __eq__(self, other):
        return (other is self)
#not needed because of caching  # or (type(other) == type(self) and other.value == self.value)


class String(SelfEval):
    __slots__ = ['mutable']
    typeName = 'a string'

    pstring = re.compile(r'^\"((?:[^"]|\")*)\"$', flags)
    pcharcode = re.compile(r'\\[xX]([0-9a-fA-F]{1,6});', flags)

    name2char = {
        r'\a':     '\a',
        r'\b':     '\b',
        r'\t':     '\t',
        r'\n':     '\n',
        r'\r':     '\r',
        r'\"':     '\"',
        r'\\':    '\\',
        }

    char2name = dict(zip(name2char.values(), name2char.keys()))
    pchar = re.compile(r'(\\a|\\b|\\t|\\n|\\r|\\"|\\\\|(?:\\[xX][0-9a-zA-Z]+;)|(?:.))', flags)

    @classmethod
    def make(cls, string):
        self = cls()
        self.value = string
        self.mutable = True
        return self

    @classmethod
    def parseToken(cls, token, tokens=[]):
        string = ''
        s = cls.pstring.match(token.text).group(1)
        for charMatch in cls.pchar.finditer(s):
            charName = charMatch.group(0)
            charCode = cls.pcharcode.match(charName)
            if charName in cls.name2char:
                string += cls.name2char[charName]
            elif charCode != None:
                code = int(charCode.group(1), 16)
                try:
                    if sys.version_info[0] == 2:
                        string += unichr(code)
                    else:
                        string += chr(code)
                except ValueError:
                    raise SchemeError(token, 'Invalid character code in: %s.' % charName)
            elif len(charName) == 1:
                string += charName
            else:
                raise SchemeError(token, 'Unknown character name: %s.' % charName)

        return cls.make(string)

    def isString(self):
        return True

    def __repr__(self):
        s = self.value
        string = '\"'
        for char in self.value:
            if char in self.char2name:
                string += self.char2name[char]
            elif 32 <= ord(char) < 127:
                string += char
            elif ord(char) < 256:
                string += '\\x%02x;' % ord(char)
            elif ord(char) < 256*256:
                string += '\\x%04x;' % ord(char)
            else:
                string += '\\x%06x;' % ord(char)
        string += '\"'
        return string

    def __str__(self):
        return self.value

    def __unicode__(self):
        return self.value

    def __eq__(self, other):
        return (other is self) or (type(other) == type(self) and other.value == self.value)

class Number(SelfEval):
    __slots__ = []
    typeName = 'a number'

    @classmethod
    def make(cls, value):
        if isinstance(value, int):
            return IntegerNumber.make(value)
        if isinstance(value, float):
            return RealNumber.make(value)

    @classmethod
    def parseToken(cls, token, tokens=[]):
        text = token.text
        if (text.find('.') >= 0 or text.find('e') >= 0):
            return RealNumber.make(float(text))
        else:
            return IntegerNumber.make(int(text))

    def isNumber(self):
        return True

    def __str__(self):
        return str(self.value)

class IntegerNumber(Number):
    __slots__ = []
    cache = [None]*256
    typeName = 'an integer number'

    @classmethod
    def make(cls, value):
        "Construct an ``IntegerNumber`` object. Cache some of them for efficiency."
        if 0 <= value < 256:
            cached = cls.cache[value]
            if cached:
                return cached
            self = cls()
            self.value = value
            cls.cache[value] = self
            return self
        self = cls()
        self.value = value
        return self

    def isInteger(self):
        return True

    def isExact(self):
        return True

    def __eq__(self, other):
        #because caching is partial full comparison is still needed
        return (other is self) or (type(other) == type(self) and other.value == self.value)

class RealNumber(Number):
    __slots__ = []
    typeName = 'a real number'

    @classmethod
    def make(cls, value):
        self = cls()
        self.value = value
        return self

    def isReal(self):
        return True

    def isInexact(self):
        return True

    def __eq__(self, other):
        return (other is self) or (type(other) == type(self) and other.value == self.value)

class Boolean(SelfEval):
    __slots__ = []
    cache = {}
    typeName = 'a boolean value'

    @classmethod
    def make(cls, value):
        if value in cls.cache:
            return cls.cache[value]
        self = cls()
        self.value = value
        cls.cache[value] = self
        return self

    @classmethod
    def parseToken(cls, token, tokens=[]):
        if (token.text == '#f'):
            bool = Boolean.make(False)
        else:
            bool = Boolean.make(True)
        return bool

    def __str__(self):
        if self.value:
            return '#t'
        else:
            return '#f'

    def isBoolean(self):
        return True

    def __eq__(self, other):
        return (other is self) or (type(other) != type(self) and self.value)

class ErrorExpression(SelfEval):
    """
    A 'placeholder' expression serving only as a storage for errors that occurred during at
    the token parsing stage.
    """

    __slots__ = []
    typeName = 'an error'

    @classmethod
    def make(cls, exception):
        self = cls()
        self.value = exception
        return self

class Symbol(SExpression):
    __slots__ = ['name']
    typeName = 'a symbol'

    cache = {}

    @classmethod
    def make(cls, name):
        if name in cls.cache:
            return cls.cache[name]
        self = cls()
        self.name = name
        cls.cache[name] = self
        return self

    @classmethod
    def parseToken(cls, token):
        sym = Symbol.make(token.text)
        return sym

    def eval(self, callingForm, frame, cont):
        return Trampolined.make(cont, frame.resolveSymbol(self, callingForm))

    def isSymbol(self):
        return True

    def __str__(self):
        return self.name

    def __eq__(self, other):
        return (other is self)
# not needed because of chaching # or (type(other) == type(self) and other.name == self.name)

class List(SExpression):
    __slots__ = []
    typeName = 'a list'

class Null(List):
    __slots__ = []
    cache = None
    typeName = 'a null list'

    @classmethod
    def make(cls):
        if cls.cache:
            return cls.cache
        self = cls()
        cls.cache = self
        return self

    def eval(self, callingForm, frame, cont):
        raise SchemeError(callingForm, 'Empty application.')

    def evalElements(self, callingForm, frame, cont, excp=None):
        return Trampolined.make(cont, self)

    def append(self, pair):
        return pair

    def isNull(self):
        return True

    def isList(self):
        return True

    def __len__(self):
        return 0

    def __nonzero__(self):
        return True

    def __bool__(self):
        return True

    def __getitem__(self, index):
        raise IndexError('(null) index out of range')

    def __iter__(self):
        return iter([])

    def __str__(self):
        return '()'

    def __eq__(self, other):
        return (other is self) #or (type(other) == type(self))

class Pair(List):
    __slots__ = ['car', 'cdr', 'meta', 'topLevel']
    typeName = 'a pair'
    
    @classmethod
    def make(cls, car, cdr):
        self = cls()
        self.car = car
        self.cdr = cdr
        return self

    @classmethod
    def makeFromList(cls, lst, proper = True):
        if len(lst) == 0:
            return Null.make()
        self = cls.make(lst[0], Null.make())
        if len(lst) == 1:
            return self
        tail = self
        for e in lst[1:-1]:
            tail.cdr = cls.make(e, Null.make())
            tail = tail.cdr
        if proper:
            tail.cdr = cls.make(lst[-1], Null.make())
        else:
            tail.cdr = lst[-1]
        return self

    @classmethod
    def parseTokens(cls, token, tokens, topLevel=False):
        try:
            #print token.text
            improper = 0
            error = False
            self = cls.make(Null.make(), Null.make())
            tail = self
            for t in tokens:
                #print t.text
                if improper == 0: # proper list (so far)
                    if t.text == '.':
                        if tail == self: #first element
                            error = True
                        improper = True
                    elif t.text == ')':
                        self = self.cdr #discard empty car
                        if self.isPair():
                            self.meta = token.meta
                            self.topLevel = topLevel
                        return self
                    else:
                        #assumes that 'tokens' is a generator, not a list
                        expr = SExpression.parseTokens(t, tokens)
                        tail.cdr = cls.make(expr, Null.make())
                        tail = tail.cdr
                else: # improper
                    if t.text == '.': # more than 1 dot
                        error = True
                    elif t.text == ')':
                        #earlier error or wrong number of expressions after dot
                        if error or improper != 2:
                            raise SchemeError(token, 'Wrong pair format.')
                        self = self.cdr #discard empty car
                        self.meta = token.meta
                        self.topLevel = topLevel
                        return self
                    else:
                        improper += 1
                        expr = SExpression.parseTokens(t, tokens)
                        if not tail.isPair(): # more than 1 token after the dot
                            error = True
                        else:
                            tail.cdr = expr
                            tail = tail.cdr
        except StopIteration:
            raise SchemeError(token, 'Unterminated list.')

    def append(self, pair):
        res = Pair.make(self.car, self.cdr)
        pointer = self
        resPointer = res
        while pointer.cdr.isPair():
            resPointer.cdr = Pair.make(pointer.cdr.car, pointer.cdr.cdr)
            pointer = pointer.cdr
            resPointer = resPointer.cdr
        if not pointer.cdr.isNull():
            resPointer.cdr = Pair.make(pointer.cdr, Null.make())
            resPointer = resPointer.cdr
        resPointer.cdr = pair
        return res

    def toList(self):
        lst = [self.car]
        cdr = self.cdr
        while cdr.isPair():
            lst.append(cdr.car)
            cdr = cdr.cdr
        if cdr.isNull():
            return lst
        return lst + [cdr]
        
    def eval(self, callingForm, frame, cont):
        def step2(operator):
            def step3(operands):
                return operator.checkAndApply(operands, self, cont)
            if not operator.isProcedure():
                raise SchemeError(
                    self, 'procedure application, first operand is not a procedure.')
            return self.cdr.evalElements(
                callingForm, frame, step3,
                SchemeError(self, '"eval": improper operand list'))
        if (self.car.isSymbol() and
            isinstance(frame.resolveSymbol(self.car, callingForm), SpecialSyntax)):
            return frame.resolveSymbol(
                self.car, callingForm).apply(self.cdr, self, frame, cont)
        return self.car.eval(callingForm, frame, step2)

    def evalElements(self, callingForm, frame, cont, excp=None):
        def step2(car):
            def step3(cdr):
                return Trampolined.make(cont, Pair.make(car, cdr))
            if self.cdr.isPair() or self.cdr.isNull():
                return self.cdr.evalElements(callingForm, frame, step3, excp)
            else:
                if excp:
                    raise excp
                else:
                    return self.cdr.eval(callingForm, frame, step3)
        return self.car.eval(callingForm, frame, step2)

    def evalSequence(self, callingForm, frame, cont):
        def step2(car):
            return self.cdr.evalSequence(callingForm, frame, cont)
        if self.cdr.isNull():
            return self.car.eval(callingForm, frame, cont)
        else:
            return self.car.eval(callingForm, frame, step2)

    def quasiQuote(self, level, callingForm, frame, cont):
        def unsplice(lst, cont):
            def step2(cdr):
                return Trampolined.make(cont, Pair.make(lst.car, cdr))
            if lst.isNull():
                return self.cdr.quasiQuote(level, callingForm, frame, cont)
            if not lst.isPair():
                raise SchemeError(self, 'unquote-splicing returned an improper list.')
            return unsplice(lst.cdr, step2)
        def joinLists(listOfLists, cnt2):
            if listOfLists.isNull():
                return Trampolined.make(cnt2, listOfLists)
            car = listOfLists.car
            if not isinstance(car, List):
                raise SchemeError(self, 'unquote-splicing operand is not a list.')
            if listOfLists.cdr.isNull():
                return Trampolined.make(cnt2, car)
            def step2(cdr):
                return Trampolined.make(cnt2, car.append(cdr))
            return joinLists(listOfLists.cdr, step2)
        def step2(car):
            def step3(cdr):
                #print(car, cdr)
                if isinstance(cdr, Values):
                    if len(cdr.values) != 1:
                        raise SchemeError(
                            self,
                            'Multi-operand unquote and unquote-splicing ' +
                            'are not permitted in a scalar context.')
                    cdr = cdr.values.car
                if isinstance(car, Values):
                    return unsplice(car.values, cont)
                return Trampolined.make(cont, Pair.make(car, cdr))
            if car.isSymbol():
                sframe = frame.resolveSymbolLocation(car)
                if sframe:
                    obj = sframe[car.name] 
                    if isinstance(obj, QuasiQuoteForm):
                        return self.cdr.quasiQuote(level+1, callingForm, frame, step3)
                    if isinstance(obj, UnQuoteForm):
                        if level == 1:
                            def step4b(rest):
                                return Trampolined.make(cont, Values.make(rest))
                            return self.cdr.evalElements(callingForm, frame, step4b)
                        else:
                            return self.cdr.quasiQuote(level-1, callingForm, frame, step3)
                    if isinstance(obj, UnQuoteSplicingForm):
                        if level == 1:
                            def step4c(listOfLists):
                                def step5c(rest):
                                    return Trampolined.make(cont, Values.make(rest))
                                return joinLists(listOfLists, step5c)
                            return self.cdr.evalElements(callingForm, frame, step4c)
                        else:
                            return self.cdr.quasiQuote(level-1, callingForm, frame, step3)
            return self.cdr.quasiQuote(level, callingForm, frame, step3)
        return self.car.quasiQuote(level, callingForm, frame, step2)

    def isPair(self):
        return True

    def isList(self):
        cdr = self.cdr
        while cdr.isPair():
            cdr = cdr.cdr
        return cdr.isNull()

    def __len__(self):
        len = 1
        cdr = self.cdr
        while cdr.isPair():
            cdr = cdr.cdr
            len += 1
        if cdr.isNull():
            return len
        else:
            return len + 1

    def __getitem__(self, index):
        length = len(self)
        if isinstance(index, slice):
            indices = index.indices(len(self))
            if indices[1] == length and indices[2] == 1: # reuses the tail
                res = self
                for i in range(0, indices[0]):
                    res = res.cdr
                return res
            else:
                return Pair.makeFromList([self[i] for i in range(*indices)])
            #allocates new list
        if not isinstance(index, int):
            raise TypeError('pair indices must be integers, not '
                            + index.__class__.__name__)
        if index < 0:
            index += length
        if index == 0:
            return self.car
        idx = 1
        cdr = self.cdr
        while cdr.isPair():
            if idx == index:
                return cdr.car
            cdr = cdr.cdr
            idx += 1
        if not cdr.isNull() and idx == index:
            return cdr
        raise IndexError('pair index out of range')

    def __iter__(self):
        yield self.car
        cdr = self.cdr
        while cdr.isPair():
            yield cdr.car
            cdr = cdr.cdr
        if not cdr.isNull():
            yield cdr

    def __repr__(self):
        if self.car.isSymbol() and self.cdr.isPair() and self.cdr.cdr.isNull():
            s = self.car.name
            if s == 'quote':
                return '\'' + repr(self.cdr.car)
            if s == 'quasiquote':
                return '`' + repr(self.cdr.car)
            if s == 'unquote':
                return ',' + repr(self.cdr.car)
            if s == 'unquote-splicing':
                return ',@' + repr(self.cdr.car)
        string = '(' + repr(self.car)
        cdr = self.cdr
        while cdr.isPair():
            string += (' ' + repr(cdr.car))
            cdr = cdr.cdr
        if cdr.isNull():
            return (string + ')')
        else:
            return (string + ' . ' + repr(cdr) + ')')

    def __str__(self):
        return repr(self)

    def __eq__(self, other):
        if other is self:
            return True
        if (type(other) != type(self)) or (other.car != self.car):
            return False
        sCdr = self.cdr
        oCdr = other.cdr
        while True:
            if sCdr == oCdr:
                return True
            if not sCdr.isPair() or not oCdr.isPair():
                return False
            sCdr = sCdr.cdr
            oCdr = oCdr.cdr

class Values(SExpression):
    __slots__ = ['values']
    typeName = 'multiple values'

    @classmethod
    def make(cls, values):
        self = cls()
        self.values = values
        return self

    def quasiQuote(self, level, callingForm, frame, cont):
        return Trampolined.make(cont, self.values)

    def __str__(self):
        return '#<values %s 0x%x>' % (str(self.values), id(self))

class Procedure(SExpression):
    __slots__ = []
    typeName = 'a procedure'

    def apply(self, operands, callingForm, cont):
        pass

    def isProcedure(self):
        return True

    def eval(self, callingForm, frame, cont):
        raise SchemeError(callingForm, 'Procedure should not be evaluated directly')

    def checkAndApply(self, operands, callingForm, cont):
        lenOperands = len(operands)
        minOperands = self.operandsNo[0]
        if len(self.operandsNo) > 1:
            maxOperands = self.operandsNo[1]
            if not minOperands <= lenOperands <= maxOperands:
                if minOperands == maxOperands:
                    raise SchemeError(
                        callingForm,
                        '"%s" requires %d operands, provided %d.'
                        % (self.name, minOperands, lenOperands))
                raise SchemeError(
                    callingForm,
                    '"%s" requires at least %d operands and at most %d operands, provided %d.'
                    % (self.name, minOperands, maxOperands, lenOperands))
        elif not minOperands <= lenOperands:
            raise SchemeError(
                callingForm,
                '"%s" requires at least %d operands, provided %d.'
                % (self.name, minOperands, lenOperands))

        last = len(self.operandTypes) - 1
        for n in range(0,len(operands)):
            if not isinstance(operands[n], self.operandTypes[min(n, last)]):
                raise SchemeError(
                    callingForm,
                    '"%s": operand #%d must be %s, provided %s.'
                    % (self.name, n+1, self.operandTypes[
                        min(n, last)].typeName, operands[n].typeName))

        return self.apply(operands, callingForm, cont)

class Continuation(Procedure):
    __slots__ = ['continuation']
    name = 'continuation'
    typeName = 'a continuation'
    operandsNo = [1,1]
    operandTypes = [SExpression]

    @classmethod
    def make(cls, continuation):
        self = cls()
        self.continuation = continuation
        return self

    def isContinuation(self):
        return True

    def apply(self, operands, callingForm, cont):
        if len(operands) != 1:
            raise SchemeError(
                callingForm,
                'Wrong number of operands to continuation, required 1, provided %d.'
                % len(operands))
        return Trampolined.make(self.continuation, operands.car)

    def __str__(self):
        return '#<continuation 0x%x>' % id(self)

class CompoundProcedure(Procedure):
    __slots__ = ['formals', 'body', 'frame', 'meta', 'operandsNo']
    name = 'procedure'
    typeName = 'a compound procedure'
    operandTypes = [SExpression]

    @classmethod
    def make(cls, formals, body, frame, meta):
        self = cls()
        self.formals = formals
        self.body = body
        self.frame = frame
        self.checkFormals()
        self.meta = meta
        return self

    def checkFormals(self):
        defined = set()
        pointer = self.formals
        operandsNo = 0
        while pointer.isPair():
            if not pointer.car.isSymbol():
                raise SchemeError(self, 'Invalid procedure operand name')
            if pointer.car.name in defined:
                raise SchemeError(self, 'Duplicated operand name')
            defined.add(pointer.car.name)
            pointer = pointer.cdr
            operandsNo += 1
        if not pointer.isSymbol() and not pointer.isNull():
            raise SchemeError(self, 'Invalid procedure operand name')
        if pointer.isSymbol():
            self.operandsNo = [operandsNo]
        else:
            self.operandsNo = [operandsNo,operandsNo]

    def bind(self, formals, operands, callingForm, frame, cont):
        if formals.isNull() and operands.isNull():
            return Trampolined.make(cont, frame)
        elif formals.isSymbol():
            frame.setSymbolValue(formals, operands)
            return Trampolined.make(cont, frame)
        elif formals.isPair() and operands.isPair():
            frame.setSymbolValue(formals.car, operands.car)
            return self.bind(formals.cdr, operands.cdr, callingForm, frame, cont)
        elif formals.isList():
            raise SchemeError(
                callingForm,
                'Wrong number of operands, required %d.' % len(self.formals))
        else:
            raise SchemeError(
                callingForm,
                'Wrong number of operands, required at least %d.'
                % (len(self.formals) - 1))

    def apply(self, operands, callingForm, cont):
        def step2(newFrame):
            return self.body.evalSequence(callingForm, newFrame, cont)
        return self.bind(self.formals, operands, callingForm, Frame.make(self.frame), step2)

    def __str__(self):
        return '#<procedure %s 0x%x>' % (str(self.formals), id(self))

class Trampolined(SExpression):
    __slots__ = ['continuation', 'operand']
    typeName = 'a trampolined value'

    @classmethod
    def make(cls, continuation, operand):
        self = cls()
        self.continuation = continuation
        self.operand = operand
        return self

    def isTrampolined(self):
        return True

    def eval(self, callingForm, frame, cont):
        return self.continuation(self.operand)

    def __str__(self):
        return '#<trampolined (%s) 0x%x>' % (str(self.operand), id(self))

class SpecialSyntax(SExpression):
    typeName = 'a syntax expression'

    object = None

    @classmethod
    def make(cls):
        if not cls.object:
            cls.object = cls()
        return cls.object

    def eval(self, callingForm, frame, cont):
        raise SchemeError(callingForm, 'Special syntax should not be evaluated directly')

    def apply(self, operands, callingForm, frame, cont):
        pass

    def isSpecialSyntax(self):
        return True

    def __str__(self):
        return '#<syntax:%s>' % self.name

class QuoteForm(SpecialSyntax):
    "Implements a :c:macro:`quote` or :c:macro:`'` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isPair():
            raise SchemeError(
                callingForm,
                '"%s" requires 1 operand, given %d.' % (self.name, len(operands)))
        res = operands.car
        return Trampolined.make(cont, res)

class QuasiQuoteForm(SpecialSyntax):
    "Implements a :c:macro:`quasiquote` or :c:macro:`\`` form."
    def apply(self, operands, callingForm, frame, cont):
        def step2(result):
            if isinstance(result, Values):
                if len(result.values) != 1:
                    raise SchemeError(
                        callingForm,
                        '%s: multi-operand unquote ' % self.name +
                        'and unquote-splicing are not permitted in this context.')
                result = result.values.car
            return Trampolined.make(cont, result)
        if operands.isNull() or operands.cdr.isPair():
            raise SchemeError(
                callingForm,
                '"%s" requires 1 operand, given %d.' % (self.name, len(operands)))
        return operands.car.quasiQuote(1, callingForm, frame, step2)

class UnQuoteForm(SpecialSyntax):
    "Implements an :c:macro:`unquote` or :c:macro:`,` form."
    def apply(self, operands, callingForm, frame, cont):
        raise SchemeError(callingForm, '"%s" outside of "quasiquote".' % self.name)

class UnQuoteSplicingForm(SpecialSyntax):
    "Implements an :c:macro:`unquote-splicing` or :c:macro:`,@` form."
    def apply(self, operands, callingForm, frame, cont):
        raise SchemeError(callingForm, '"%s" outside of "quasiquote".' % self.name)

class DefineForm(SpecialSyntax):
    "Implements a :c:macro:`define` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise SchemeError(
                callingForm,
                '"%s" requires at least 2 operands, given %d.'
                % (self.name, len(operands)))
        firstArg = operands.car
        if not firstArg.isSymbol() and not firstArg.isPair():
            raise SchemeError(
                callingForm,
                '"%s": Invalid type of the first operand' % self.name)
        if firstArg.isSymbol():
            def step2(value):
                frame.setSymbolValue(firstArg, value)
                return Trampolined.make(cont, Nil.make())
            return operands.cdr.car.eval(callingForm, frame, step2)
        if firstArg.isPair():
            if not firstArg.car.isSymbol():
                raise SchemeError(callingForm, 'Invalid procedure name in "%s"' % self.name)
            procName = firstArg.car
            formals = firstArg.cdr #.toList()
            body = operands.cdr
            procedure = CompoundProcedure.make(formals, body, frame, callingForm.meta)
            frame.setSymbolValue(procName, procedure)
            return Trampolined.make(cont, Nil.make())

class LambdaForm(SpecialSyntax):
    "Implements a :c:macro:`lambda` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise SchemeError(
                callingForm,
                '"%s" requires at least 2 operands, given %d.' % (self.name, len(operands)))
        formals = operands.car
        if not formals.isSymbol() and not formals.isPair() and not formals.isNull():
            raise SchemeError(
                callingForm, '"%s": Invalid type of the first operand' % self.name)
        body = operands.cdr
        procedure = CompoundProcedure.make(formals, body, frame, callingForm.meta)
        return Trampolined.make(cont, procedure)

class LetForm(SpecialSyntax):
    "Implements a :c:macro:`let` form. Named :c:macro:`let` not yet supported."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise SchemeError(
                callingForm,
                '"%s" requires at least 2 operands, given %d.' % (self.name, len(operands)))
        def step2(newFrame):
            return operands.cdr.evalSequence(callingForm, newFrame, cont)
        return self.processBindings(operands.car, callingForm, frame, Frame.make(frame), step2)

    def processBindings(self, bindings, callingForm, oldFrame, newFrame, cont):
        def step2(bindingValue):
            if not binding.car.isSymbol():
                raise SchemeError(
                    callingForm,
                    '"%s": incorrect binding form (first element is not a symbol).'
                    % self.name)
            if binding.car.name in newFrame.symbols:
                raise SchemeError(
                    callingForm,
                    '"%s": multiple uses of the same variable in the binding form.'
                    % self.name)
            newFrame.setSymbolValue(binding.car, bindingValue)
            return self.processBindings(bindings.cdr, callingForm, oldFrame, newFrame, cont)
        if bindings.isNull():
            return Trampolined.make(cont, newFrame)
        if not bindings.isPair():
            raise SchemeError(
                callingForm,
                '"%s": invalid binding forms (not a list).'
                % self.name)
        binding = bindings.car
        if not binding.isPair():
            raise SchemeError(
                callingForm,
                '"%s": invalid binding form (not a list).' % self.name)
        if not binding.cdr.isPair() or binding.cdr.cdr.isPair():
            raise SchemeError(
                callingForm,
                '"%s": each binding form must consist of 2 elements, given %d.'
                % (self.name, len(binding)))
        return binding.cdr.car.eval(callingForm, oldFrame, step2)

class LetStarForm(SpecialSyntax):
    "Implements a :c:macro:`let*` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise SchemeError(
                callingForm,
                '"%s" requires at least 2 operands, given %d.' % (self.name, len(operands)))
        def step2(newFrame):
            return operands.cdr.evalSequence(callingForm, newFrame, cont)
        return self.processBindings(operands.car, callingForm, frame, step2)

    def processBindings(self, bindings, callingForm, frame, cont):
        def step2(bindingValue):
            if not binding.car.isSymbol():
                raise SchemeError(
                    callingForm,
                    '"%s": incorrect binding form (first element is not a symbol).'
                    % self.name)
            newFrame = Frame.make(frame)
            newFrame.setSymbolValue(binding.car, bindingValue)
            return self.processBindings(bindings.cdr, callingForm, newFrame, cont)
        if bindings.isNull():
            return Trampolined.make(cont, frame)
        if not bindings.isPair():
            raise SchemeError(
                callingForm, '"%s": invalid binding forms (not a list).' % self.name)
        binding = bindings.car
        if not binding.isPair():
            raise SchemeError(
                callingForm, '"%s": invalid binding form (not a list).' % self.name)
        if not binding.cdr.isPair() or binding.cdr.cdr.isPair():
            raise SchemeError(
                callingForm,
                '"%s": each binding form must consist of 2 elements, given %d.'
                % (self.name, len(binding)))
        return binding.cdr.car.eval(callingForm, frame, step2)

class LetrecForm(SpecialSyntax):
    "Implements a :c:macro:`letrec` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise SchemeError(
                callingForm,
                '"%s" requires at least 2 operands, given %d.' % (self.name, len(operands)))
        def step2(newFrame):
            def step3(inits):
                def step4(newFrame2):
                    return operands.cdr.evalSequence(callingForm, newFrame2, cont)
                return self.bindValues(operands.car, inits, callingForm, newFrame, step4)
            return self.evalInits(operands.car, callingForm, newFrame, step3)
        return self.processBindingVariables(
            operands.car, callingForm, frame, Frame.make(frame), step2)

    def processBindingVariables(self, bindings, callingForm, oldFrame, newFrame, cont):
        if bindings.isNull():
            return Trampolined.make(cont, newFrame)
        if not bindings.isPair():
            raise SchemeError(
                callingForm, '"%s": invalid binding forms (not a list).' % self.name)
        binding = bindings.car
        if not binding.isPair():
            raise SchemeError(
                callingForm, '"%s": invalid binding form (not a list).' % self.name)
        if not binding.cdr.isPair() or binding.cdr.cdr.isPair():
            raise SchemeError(
                callingForm,
                '"%s": each binding form must consist of 2 elements, given %d.'
                % (self.name, len(binding)))
        if not binding.car.isSymbol():
            raise SchemeError(
                callingForm,
                '"%s": incorrect binding form (first element is not a symbol).' % self.name)
        if binding.car.name in newFrame.symbols:
            raise SchemeError(
                callingForm,
                '"%s": multiple uses of the same variable in the binding form.' % self.name)
        newFrame.setSymbolValue(binding.car, Nil.make())
        return self.processBindingVariables(
            bindings.cdr, callingForm, oldFrame, newFrame, cont)

    def evalInits(self, bindings, callingForm, frame, cont):
        def step2(initValue):
            def step3(initValues):
                return Trampolined.make(cont, Pair.make(initValue, initValues))
            return self.evalInits(bindings.cdr, callingForm, frame, step3)
        if bindings.isNull():
            return Trampolined.make(cont, Null.make())
        return bindings.car.cdr.car.eval(callingForm, frame, step2)

    def bindValues(self, bindings, inits, callingForm, frame, cont):
        if bindings.isNull():
            return Trampolined.make(cont, frame)
        binding = bindings.car.car
        init = inits.car
        frame.setSymbolValue(binding, init)
        return self.bindValues(bindings.cdr, inits.cdr, callingForm, frame, cont)

class LetrecStarForm(SpecialSyntax):
    "Implements a :c:macro:`letrec*` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise SchemeError(
                callingForm,
                '"%s" requires at least 2 operands, given %d.' % (self.name, len(operands)))
        def step2(newFrame):
            def step3(newFrame2):
                return operands.cdr.evalSequence(callingForm, newFrame2, cont)
            return self.evalInits(operands.car, callingForm, newFrame, step3)
        return self.processBindingVariables(
            operands.car, callingForm, frame, Frame.make(frame), step2)

    def processBindingVariables(self, bindings, callingForm, oldFrame, newFrame, cont):
        if bindings.isNull():
            return Trampolined.make(cont, newFrame)
        if not bindings.isPair():
            raise SchemeError(
                callingForm, '"%s": invalid binding forms (not a list).' % self.name)
        binding = bindings.car
        if not binding.isPair():
            raise SchemeError(
                callingForm, '"%s": invalid binding form (not a list).' % self.name)
        if not binding.cdr.isPair() or binding.cdr.cdr.isPair():
            raise SchemeError(
                callingForm,
                '"%s": each binding form must consist of 2 elements, given %d.'
                % (self.name, len(binding)))
        if not binding.car.isSymbol():
            raise SchemeError(
                callingForm,
                '"%s": incorrect binding form (first element is not a symbol).' % self.name)
        if binding.car.name in newFrame.symbols:
            raise SchemeError(
                callingForm,
                '"%s": multiple uses of the same variable in the binding form.' % self.name)
        newFrame.setSymbolValue(binding.car, Nil.make())
        return self.processBindingVariables(
            bindings.cdr, callingForm, oldFrame, newFrame, cont)

    def evalInits(self, bindings, callingForm, frame, cont):
        def step2(bindingValue):
            frame.setSymbolValue(bindings.car.car, bindingValue)
            return self.evalInits(bindings.cdr, callingForm, frame, cont)
        if bindings.isNull():
            return Trampolined.make(cont, frame)
        return bindings.car.cdr.car.eval(callingForm, frame, step2)


class SetForm(SpecialSyntax):
    "Implements a :c:macro:`set!` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull() or operands.cdr.cdr.isPair():
            raise SchemeError(
                callingForm,
                '"%s" requires 2 operands, given %d.' % (self.name, len(operands)))
        var = operands.car
        if not var.isSymbol():
            raise SchemeError(
                callingForm, '"%s": first operand is not a symbol.' % self.name)
        targetFrame = frame.resolveSymbolLocation(var)
        if targetFrame == None:
            raise SchemeError(
                callingForm, '"%s": undefined symbol %s.' % (self.name, var.name))
        def step2(value):
            targetFrame[var.name] = value
            return Trampolined.make(cont, Nil.make())
        return operands.cdr.car.eval(callingForm, frame, step2)

class IfForm(SpecialSyntax):
    "Implements a :c:macro:`if` form."
    def apply(self, operands, callingForm, frame, cont):
        if (operands.isNull() or operands.cdr.isNull() or
            (operands.cdr.cdr.isPair() and operands.cdr.cdr.cdr.isPair())):
            raise SchemeError(
                callingForm,
                '"%s" requires 2 or 3 operands, given %d.' % (self.name, len(operands)))
        def step2(predicate):
            if not predicate.isBoolean() or predicate.value: #True
                res = operands.cdr.car.eval(callingForm, frame, cont)
            elif operands.cdr.cdr.isPair():
                res = operands.cdr.cdr.car.eval(callingForm, frame, cont)
            else:
                res = Trampolined.make(cont, Nil.make())
            return res
        return operands.car.eval(callingForm, frame, step2)

class CondForm(SpecialSyntax):
    "Implements a :c:macro:`cond` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull():
            raise SchemeError(
                callingForm,
                '"%s" requires at least 1 operand, given %d.' % (self.name, len(operands)))
        return self.processClauses(operands, callingForm, frame, cont)

    def processClauses(self, clauses, callingForm, frame, cont):
        def step2(boolValue):
            if clauses.cdr.isPair() and isinstance(boolValue, ElseIdentifier):
                raise SchemeError(clause, '"%s": "else" not at tail position.' % self.name)
            if (not boolValue.isBoolean() or boolValue.value == True):
                return self.processClause(boolValue, clauses.car, callingForm, frame, cont)
            else:
                return self.processClauses(clauses.cdr, callingForm, frame, cont)
        if clauses.isNull():
            return Trampolined.make(cont, Nil.make())
        clause = clauses.car
        if not clause.isPair():
            raise SchemeError(clause, '"%s": invalid clause syntax.' % self.name)
        return clause.car.eval(callingForm, frame, step2)

    def processClause(self, test, clause, callingForm, frame, cont):
        ccdr = clause.cdr
        if ccdr.isNull():
            if isinstance(test, ElseIdentifier):
                raise SchemeError(
                    clause, '"%s": not enough elements in the "else" clause.' % self.name)
            return Trampolined.make(cont, test)
        if not ccdr.isList():
            raise SchemeError(clause, '"%s": invalid clause syntax.' % self.name)
        def step2(second):
            def step3(third):
                if third.isProcedure():
                    return third.checkAndApply(Pair.make(test, Null.make()), callingForm, cont)
                raise SchemeError(
                    clause,
                    '"%s": last operand in "=>" clause is not a procedure.' % self.name)
            if isinstance(second, EGTIdentifier):
                if (ccdr.cdr.isNull() or ccdr.cdr.cdr.isPair()):
                    raise SchemeError(
                        clause, '"%s": wrong number of elements in "=>" clause.' % self.name)
                if isinstance(test, ElseIdentifier):
                    raise SchemeError(
                        clause, '"%s": "=>" is not allowed in "else" clause.' % self.name)
                return ccdr.cdr.car.eval(callingForm, frame, step3)
            elif ccdr.cdr.isNull():
                return Trampolined.make(cont, second)
            else:
                return ccdr.cdr.evalSequence(callingForm, frame, cont)
        return ccdr.car.eval(callingForm, frame, step2)

class AndForm(SpecialSyntax):
    "Implements a :c:macro:`and` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull():
            return Trampolined.make(cont, Boolean.make(True))
        def step2(result):
            if (result.isBoolean() and result.value == False) or operands.cdr.isNull():
                return Trampolined.make(cont, result)
            else:
                return self.apply(operands.cdr, callingForm, frame, cont)
        return operands.car.eval(callingForm, frame, step2)

class OrForm(SpecialSyntax):
    "Implements a :c:macro:`or` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull():
            return Trampolined.make(cont, Boolean.make(False))
        def step2(result):
            if not result.isBoolean() or result.value == True or operands.cdr.isNull():
                return Trampolined.make(cont, result)
            else:
                return self.apply(operands.cdr, callingForm, frame, cont)
        return operands.car.eval(callingForm, frame, step2)

class ElseIdentifier(SpecialSyntax):
    "Implements an :c:macro:`else` identifier."
    def apply(self, operands, callingForm, frame, cont):
        raise SchemeError(callingForm, '%s: invalid context.' % self.name)

class EGTIdentifier(SpecialSyntax):
    "Implements an :c:macro:`=>` identifier."
    def apply(self, operands, callingForm, frame, cont):
        raise SchemeError(callingForm, '%s: invalid context.' % self.name)
    
class PrimitiveProcedure(Procedure):
    typeName = 'a primitive procedure'

    object = None

    @classmethod
    def make(cls):
        if not cls.object:
            cls.object = cls()
        return cls.object

    def __str__(self):
        return '#<primitive:%s>' % self.name

class IsNullProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`null?` primitive function."
    operandsNo = [1,1]
    operandTypes = [SExpression]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Boolean.make(operands.car.isNull()))

class IsPairProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`pair?` primitive function."
    operandsNo = [1,1]
    operandTypes = [SExpression]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Boolean.make(operands.car.isPair()))

class IsProcedureProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`procedure?` primitive function."
    operandsNo = [1,1]
    operandTypes = [SExpression]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Boolean.make(operands.car.isProcedure()))

class ConsProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`cons` primitive function."
    operandsNo = [2,2]
    operandTypes = [SExpression]
    def apply(self, operands, callingForm, cont):
        expr1 = operands.car
        expr2 = operands.cdr.car
        res = Pair.make(expr1, expr2)
        return Trampolined.make(cont, res)

class CarProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`car` primitive function."
    operandsNo = [1,1]
    operandTypes = [Pair]
    def apply(self, operands, callingForm, cont):
        arg = operands.car
        res = arg.car
        return Trampolined.make(cont, res)

class CdrProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`cdr` primitive function."
    operandsNo = [1,1]
    operandTypes = [Pair]
    def apply(self, operands, callingForm, cont):
        arg = operands.car
        res = arg.cdr
        return Trampolined.make(cont, res)

class Append2Procedure(PrimitiveProcedure):
    "Implements a two-argument :c:macro:`append` primitive function."
    operandsNo = [2,2]
    operandTypes = [List, List]
    def apply(self, operands, callingForm, cont):
        expr1 = operands.car
        expr2 = operands.cdr.car
        res = expr1.append(expr2)
        return Trampolined.make(cont, res)

class NotProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`not` primitive function."
    operandsNo = [1,1]
    operandTypes = [Boolean]
    def apply(self, operands, callingForm, cont):
        arg = operands.car
        value = False
        if arg.isBoolean():
            value = not(arg.value)
        res = Boolean.make(value)
        return Trampolined.make(cont, res)

class EqProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`eq?` primitive function."
    operandsNo = [2]
    operandTypes = [SExpression, SExpression]
    def apply(self, operands, callingForm, cont):
        value = True
        first = operands.car
        cdr = operands.cdr
        while cdr.isPair():
            other = cdr.car
            if first.isNull() or first.isSymbol():
                if first != other:
                    value = False
            elif not cdr.car is first:
                value = False
            cdr = cdr.cdr
        res = Boolean.make(value)
        return Trampolined.make(cont, res)

class EqvProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`eqv?` primitive function."
    operandsNo = [2]
    operandTypes = [SExpression, SExpression]
    def apply(self, operands, callingForm, cont):
        value = True
        first = operands.car
        cdr = operands.cdr
        while cdr.isPair():
            other = cdr.car
            if first.isNumber() and other.isNumber():
                if first.value != other.value:
                    value = False
                if first.isExact() != other.isExact():
                    value = False
            elif first.isNull() or first.isChar() or first.isSymbol():
                if first != other:
                    value = False
            elif not first is other:
                value = False
            cdr = cdr.cdr
        res = Boolean.make(value)
        return Trampolined.make(cont, res)

class EqualProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`equal?` primitive function."
    operandsNo = [2]
    operandTypes = [SExpression, SExpression]
    def apply(self, operands, callingForm, cont):
        value = True
        first = operands.car
        cdr = operands.cdr
        while cdr.isPair():
            if cdr.car != first:
                value = False
            cdr = cdr.cdr
        res = Boolean.make(value)
        return Trampolined.make(cont, res)

class NumEqProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`=` primitive function."
    operandsNo = [2]
    operandTypes = [Number, Number]
    def apply(self, operands, callingForm, cont):
        value = True
        first = operands.car
        cdr = operands.cdr
        while cdr.isPair():
            n = cdr.car
            if n.value != first.value:
                value = False
            cdr = cdr.cdr
        res = Boolean.make(value)
        return Trampolined.make(cont, res)

class NumLTProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`<` primitive function."
    operandsNo = [2]
    operandTypes = [Number, Number]
    def apply(self, operands, callingForm, cont):
        value = True
        previous = operands.car
        cdr = operands.cdr
        while cdr.isPair():
            n = cdr.car
            if previous.value >= n.value:
                value = False
            cdr = cdr.cdr
            previous = n
        res = Boolean.make(value)
        return Trampolined.make(cont, res)

class NumLTEProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`<=` primitive function."
    operandsNo = [2]
    operandTypes = [Number, Number]
    def apply(self, operands, callingForm, cont):
        value = True
        previous = operands.car
        cdr = operands.cdr
        while cdr.isPair():
            n = cdr.car
            if previous.value > n.value:
                value = False
            cdr = cdr.cdr
            previous = n
        res = Boolean.make(value)
        return Trampolined.make(cont, res)

class NumGTProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`>` primitive function."
    operandsNo = [2]
    operandTypes = [Number, Number]
    def apply(self, operands, callingForm, cont):
        value = True
        previous = operands.car
        cdr = operands.cdr
        while cdr.isPair():
            n = cdr.car
            if previous.value <= n.value:
                value = False
            cdr = cdr.cdr
            previous = n
        res = Boolean.make(value)
        return Trampolined.make(cont, res)

class NumGTEProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`>=` primitive function."
    operandsNo = [2]
    operandTypes = [Number, Number]
    def apply(self, operands, callingForm, cont):
        value = True
        previous = operands.car
        cdr = operands.cdr
        while cdr.isPair():
            n = cdr.car
            if previous.value < n.value:
                value = False
            cdr = cdr.cdr
            previous = n
        res = Boolean.make(value)
        return Trampolined.make(cont, res)

class SumProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`+` primitive function."
    operandsNo = [0]
    operandTypes = [Number, Number]
    def apply(self, operands, callingForm, cont):
        value = 0
        for n in operands:
            value += n.value
        res = Number.make(value)
        return Trampolined.make(cont, res)

class SubtractProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`-` primitive function."
    operandsNo = [1]
    operandTypes = [Number, Number]
    def apply(self, operands, callingForm, cont):
        if operands.cdr.isNull():
            res = Number.make(-operands.car.value)
        else:
            value = operands.car.value
            for n in operands.cdr:
                value -= n.value
            res = Number.make(value)
        return Trampolined.make(cont, res)

class MultiplyProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`*` primitive function."
    operandsNo = [0]
    operandTypes = [Number, Number]
    def apply(self, operands, callingForm, cont):
        value = 1
        for n in operands:
            value *= n.value
        res = Number.make(value)
        return Trampolined.make(cont, res)

class DivideProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`/` primitive function."
    operandsNo = [1]
    operandTypes = [Number, Number]
    def apply(self, operands, callingForm, cont):
        if operands.cdr.isNull():
            res = Number.make(1.0/operands[0].value)
        else:
            value = float(operands.car.value)
            for n in operands.cdr:
                if n.value == 0:
                    raise SchemeError(callingForm, '"%s": division by 0.' % self.name)
                value /= n.value
            res = Number.make(value)
        return Trampolined.make(cont, res)

class ModuloProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`modulo` primitive function."
    operandsNo = [2,2]
    operandTypes = [Number, Number]
    def apply(self, operands, callingForm, cont):
        if operands.cdr.car.value == 0:
            raise SchemeError(callingForm, '"%s": division by 0.' % self.name)
        res = Number.make(operands.car.value % operands.cdr.car.value)
        return Trampolined.make(cont, res)

class RemainderProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`remainder` primitive function."
    operandsNo = [2,2]
    operandTypes = [Number, Number]
    def apply(self, operands, callingForm, cont):
        if operands.cdr.car.value == 0:
            raise SchemeError(callingForm, '"%s": division by 0.' % self.name)
        dividend = operands.car.value
        divisor = operands.cdr.car.value
        res = Number.make(dividend - divisor*int(float(dividend)/divisor))
        return Trampolined.make(cont, res)

class QuotientProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`quotient` primitive function."
    operandsNo = [2,2]
    operandTypes = [Number, Number]
    def apply(self, operands, callingForm, cont):
        if operands.cdr.car.value == 0:
            raise SchemeError(callingForm, '"%s": division by 0.' % self.name)
        res = Number.make(operands.car.value // operands.cdr.car.value)
        return Trampolined.make(cont, res)

## Characters

class IsCharProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char?` primitive function."
    operandsNo = [1,1]
    operandTypes = [SExpression]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Boolean.make(operands.car.isChar()))

class IsCharAlphabeticProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char-alphabetic?` primitive function."
    operandsNo = [1,1]
    operandTypes = [Char]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Boolean.make(operands.car.value.isalpha()))

class IsCharNumericProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char-numeric?` primitive function."
    operandsNo = [1,1]
    operandTypes = [Char]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Boolean.make(operands.car.value.isdigit()))

class IsCharWhitespaceProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char-whitespace?` primitive function."
    operandsNo = [1,1]
    operandTypes = [Char]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Boolean.make(operands.car.value.isspace()))

class IsCharUpperCaseProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char-upper-case?` primitive function."
    operandsNo = [1,1]
    operandTypes = [Char]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Boolean.make(operands.car.value.isupper()))

class IsCharLowerCaseProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char-lower-case?` primitive function."
    operandsNo = [1,1]
    operandTypes = [Char]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Boolean.make(operands.car.value.islower()))

class IsCharEqProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char=?` primitive function."
    operandsNo = [2,2]
    operandTypes = [Char, Char]
    def apply(self, operands, callingForm, cont):
        res = Boolean.make(ord(operands.car.value) == ord(operands.cdr.car.value))
        return Trampolined.make(cont, res)

class IsCharLTProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char<?` primitive function."
    operandsNo = [2,2]
    operandTypes = [Char, Char]
    def apply(self, operands, callingForm, cont):
        res = Boolean.make(ord(operands.car.value) < ord(operands.cdr.car.value))
        return Trampolined.make(cont, res)

class IsCharGTProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char>?` primitive function."
    operandsNo = [2,2]
    operandTypes = [Char, Char]
    def apply(self, operands, callingForm, cont):
        res = Boolean.make(ord(operands.car.value) > ord(operands.cdr.car.value))
        return Trampolined.make(cont, res)

class IsCharLTEProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char<=?` primitive function."
    operandsNo = [2,2]
    operandTypes = [Char, Char]
    def apply(self, operands, callingForm, cont):
        res = Boolean.make(ord(operands.car.value) <= ord(operands.cdr.car.value))
        return Trampolined.make(cont, res)

class IsCharGTEProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char>=?` primitive function."
    operandsNo = [2,2]
    operandTypes = [Char, Char]
    def apply(self, operands, callingForm, cont):
        res = Boolean.make(ord(operands.car.value) >= ord(operands.cdr.car.value))
        return Trampolined.make(cont, res)

class CharToIntegerProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char->integer` primitive function."
    operandsNo = [1,1]
    operandTypes = [Char]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Number.make(ord(operands.car.value)))

class IntegerToCharProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`integer->char` primitive function."
    operandsNo = [1,1]
    operandTypes = [IntegerNumber]
    def apply(self, operands, callingForm, cont):
        try:
            if sys.version_info[0] == 2:
                char = unichr(operands.car.value)
            else:
                char = chr(operands.car.value)
        except (ValueError, OverflowError):
            raise SchemeError(callingForm, '"%s": invalid character code.' % self.name)
        return Trampolined.make(cont, Char.make(char))

class CharUpCaseProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char-upcase` primitive function."
    operandsNo = [1,1]
    operandTypes = [Char]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Char.make(operands.car.value.upper()))

class CharDownCaseProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`char-downcase` primitive function."
    operandsNo = [1,1]
    operandTypes = [Char]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Char.make(operands.car.value.lower()))

## String        

class IsStringProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`string?` primitive function."
    operandsNo = [1,1]
    operandTypes = [SExpression]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Boolean.make(operands.car.isString()))

class StringMakeProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`string-make` primitive function."
    operandsNo = [1,2]
    operandTypes = [IntegerNumber, Char]
    def apply(self, operands, callingForm, cont):
        char = chr(0)
        if operands.cdr.isPair():
            char = operands.cdr.car.value
        string = char * operands.car.value
        return Trampolined.make(cont, String.make(string))

class StringProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`string` primitive function."
    operandsNo = [0]
    operandTypes = [Char]
    def apply(self, operands, callingForm, cont):
        string = ""
        for c in operands:
            string += c.value
        return Trampolined.make(cont, String.make(string))

class StringLengthProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`string-length` primitive function."
    operandsNo = [1,1]
    operandTypes = [String]
    def apply(self, operands, callingForm, cont):
        return Trampolined.make(cont, Number.make(len(operands.car.value)))

## IO
class WriteCharProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`write-char` primitive function."
    operandsNo = [1,2]
    operandTypes = [Char, SExpression]
    def apply(self, operands, callingForm, cont):
        if sys.version_info[0] == 2:
            sys.stdout.write(unicode(operands.car).encode('utf-8'))
        else:
            sys.stdout.write(str(operands.car))
        return Trampolined.make(cont, Nil.make())

class DisplayProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`display` primitive function."
    operandsNo = [1,2]
    operandTypes = [SExpression, SExpression]
    def apply(self, operands, callingForm, cont):
        if sys.version_info[0] == 2:
            sys.stdout.write(unicode(operands.car).encode('utf-8'))
        else:
            sys.stdout.write(str(operands.car))
        return Trampolined.make(cont, Nil.make())

class Apply2Procedure(PrimitiveProcedure):
    "Implements a two-argument :c:macro:`apply2` primitive function."
    operandsNo = [2,2]
    operandTypes = [Procedure, List]
    def apply(self, operands, callingForm, cont):
        procedure = operands.car
        operands = operands.cdr.car
        if not operands.isList(): #is it a proper list?
            raise SchemeError(callingForm, '"%s": second operand is not a list.' % self.name)
        return procedure.apply(operands, callingForm, cont)

class CallCCProcedure(PrimitiveProcedure):
    """
    Implements a :c:macro:`call/cc` or :c:macro:`call-with-current-continuation`
    primitive function.
    """
    operandsNo = [1,1]
    operandTypes = [Procedure]
    def apply(self, operands, callingForm, cont):
        continuation = Continuation.make(cont)
        return operands.car.apply(Pair.makeFromList([continuation]), callingForm, cont)


class Frame(SExpression):
    __slots__ = ['parentFrame', 'symbols']
    typeName = 'an environment frame'

    @classmethod
    def make(cls, parentFrame):
        self = cls()
        self.parentFrame = parentFrame
        self.symbols = {}
        return self

    def resolveSymbol(self, symbol, callingForm):
        if symbol.name in self.symbols:
            return self.symbols[symbol.name]
        return self.parentFrame.resolveSymbol(symbol, callingForm)

    def resolveSymbolLocation(self, symbol):
        if symbol.name in self.symbols:
            return self.symbols
        return self.parentFrame.resolveSymbolLocation(symbol)

    def setSymbolValue(self, symbol, value):
        self.symbols[symbol.name] = value

    def evaluateExpressions(self, expressions):
        for expression in expressions:
            #print(expression)
            try:
                def processResult(result):
                    return result
                result = expression.eval(expression, self, processResult)
                while result.isTrampolined():
                    #print(result.operand)
                    #print(result)
                    result = result.eval(expression, self, processResult)
                yield result
            except SchemeError as e:
                yield ErrorExpression.make(e)
            except RuntimeError:
                yield ErrorExpression.make(
                    SchemeError(expression, 'maximum stack depth exceeded'))

    def __str__(self):
        return '#<env-frame 0x%x>' % id(self)

class NullFrame(Frame):
    __slots__ = []
    typeName = 'a null environment frame'

    @classmethod
    def make(cls):
        self = cls()
        self.symbols = {
            #pairs
            'null?':            IsNullProcedure.make(),
            'pair?':            IsPairProcedure.make(),
            'cons':             ConsProcedure.make(),
            'car':              CarProcedure.make(),
            'cdr':              CdrProcedure.make(),
            'append2':          Append2Procedure.make(),
            #boolean
            'eq?':              EqProcedure.make(),
            'eqv?':             EqvProcedure.make(),
            'equal?':           EqualProcedure.make(),
            'not':              NotProcedure.make(),
            #numbers
            '=':                NumEqProcedure.make(),
            '<':                NumLTProcedure.make(),
            '<=':               NumLTEProcedure.make(),
            '>':                NumGTProcedure.make(),
            '>=':               NumGTEProcedure.make(),
            '+':                SumProcedure.make(),
            '-':                SubtractProcedure.make(),
            '*':                MultiplyProcedure.make(),
            '/':                DivideProcedure.make(),
            'quotient':         QuotientProcedure.make(),
            'modulo':           ModuloProcedure.make(),
            'remainder':        RemainderProcedure.make(),
            #characters
            'char?':            IsCharProcedure.make(),
            'char-alphabetic?': IsCharAlphabeticProcedure.make(),
            'char-numeric?':    IsCharNumericProcedure.make(),
            'char-whitespace?': IsCharWhitespaceProcedure.make(),
            'char-upper-case?': IsCharUpperCaseProcedure.make(),
            'char-lower-case?': IsCharLowerCaseProcedure.make(),
            'char=?':           IsCharEqProcedure.make(),
            'char<?':           IsCharLTProcedure.make(),
            'char>?':           IsCharGTProcedure.make(),
            'char<=?':          IsCharLTEProcedure.make(),
            'char>=?':          IsCharGTEProcedure.make(),
            'char->integer':    CharToIntegerProcedure.make(),
            'integer->char':    IntegerToCharProcedure.make(),
            'char-upcase':      CharUpCaseProcedure.make(),
            'char-downcase':    CharDownCaseProcedure.make(),
            #'char-foldcase':    CharFoldCaseProcedure.make(),
            #strings
            'string?':          IsStringProcedure.make(),
            'string':           StringProcedure.make(),
            'make-string':      StringMakeProcedure.make(),
            'string-length':    StringLengthProcedure.make(),
            #io
            'write-char':       WriteCharProcedure.make(),
            'display':          DisplayProcedure.make(),
            #evaluation
            'procedure?':       IsProcedureProcedure.make(),
            'apply2':           Apply2Procedure.make(),
            'call-with-current-continuation': CallCCProcedure.make(),
            'call/cc':          CallCCProcedure.make(),
            #syntax
            'quote':            QuoteForm.make(),
            'quasiquote':       QuasiQuoteForm.make(),
            'unquote':          UnQuoteForm.make(),
            'unquote-splicing': UnQuoteSplicingForm.make(),
            'define':           DefineForm.make(),
            'lambda':           LambdaForm.make(),
            'let':              LetForm.make(),
            'let*':             LetStarForm.make(),
            'letrec':           LetrecForm.make(),
            'letrec*':          LetrecStarForm.make(),
            'set!':             SetForm.make(),
            #'set-car!':         SetCarForm.make(),
            #'set-cdr!':         SetCdrForm.make(),
            'if':               IfForm.make(),
            'cond':             CondForm.make(),
            #'case':             CaseForm.make(),
            'and':              AndForm.make(),
            'or':               OrForm.make(),
            #compile/expand time syntax
            #'define-syntax':    DefineSyntaxForm.make(),
            #special symbols
            'else':             ElseIdentifier.make(),
            '=>':               EGTIdentifier.make(),
            }
        for s in self.symbols:
            #print(s)
            self.symbols[s].name = s
        return self

    def resolveSymbol(self, symbol, callingForm):
        if symbol.name in self.symbols:
            return self.symbols[symbol.name]
        else:
            raise SchemeError(callingForm, 'Undefined symbol "' + symbol.name + '"')

    def resolveSymbolLocation(self, symbol):
        if symbol.name in self.symbols:
            return self.symbols
        else:
            return None

    def __str__(self):
        return '#<null-env-frame>'

class Parser(object):
    def __init__(self, fileName):
        self.lineNo = 0
        self.fileName = fileName

    def tokenizeLine(self, line):
        """
        Tokenizes a single text line.
        @line line of text (string)
        """
        #print line
        line = line.expandtabs()
        self.lineNo += 1
        for token_match in ptokens.finditer(line): #.decode('utf8')):
            if not token_match.group().startswith(';'):
                meta = {}
                meta['fileName'] = self.fileName
                meta['line'] = token_match.string
                meta['lineNo'] = self.lineNo
                meta['colStart'] = token_match.start()+1
                meta['colEnd'] = token_match.end()+1
                token = Token(token_match.group(), meta)
                #print token.text
                yield token

    def tokenizeLines(self, lines):
        """
        Tokenizes a list of lines.
        @lines can be any iterable object, including a generator object
        (a file, interactive console)
        """
        def chain(iterables):
            # chain(['ABC', 'DEF']) --> A B C D E F
            for it in iterables:
                for element in it:
                    yield element
        return chain(self.tokenizeLine(line) for line in lines)

    def tokenizeText(self, txt):
        return self.tokenizeLines(txt.splitlines())

    def read(self, text, tokenizer):
        lines = text.splitlines()
        return self.readLines(lines, tokenizer)

    def readLines(self, lines, tokenizer):
        tokens = tokenizer.tokenizeLines(lines)
        return self.parseTokens(tokens)

    def parseTokens(self, tokens):
        tokens_gen = (t for t in tokens) #make sure tokens is a generator, not a list
        for token in tokens_gen:
            try:
                yield SExpression.parseTokens(token, tokens_gen, topLevel=True)
            except SchemeError as e:
                yield ErrorExpression.make(e)
            

if __name__ == "__main__":
    def t(txt):
        "Tokenize"
        return list(parser.tokenizeText(txt))

    def tp(txt):
        "Tokenize+Parse"
        return list(parser.parseTokens(t(txt)))

    def e(expressions):
        "Evaluate"
        return list(frame.evaluateExpressions(expressions))

    def tpe(txt):
        "Tokenize+Parse+Evaluate"
        return e(tp(txt))
        
    def tpes(txt):
        "Tokenize+Parse+Evaluate+String"
        return s(tpe(txt))

    def s(lst):
        "convert a list of objects into a list of strings"
        return [str(element) for element in lst]

    def flush(generator):
        for element in generator: pass

    import os
    frame = NullFrame.make()
    fname = os.path.join(os.path.dirname(__file__), 'boot.scm')
    parser = Parser(fname)
    with open(fname, 'r') as f:
        tokens = parser.tokenizeLines(f)
        expressions = parser.parseTokens(tokens)
        results = frame.evaluateExpressions(expressions) #result generator
        flush(results) # check results to force delayed execution

    parser = Parser('cmd')

