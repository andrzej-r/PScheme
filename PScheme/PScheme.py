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
char = r'(?:#\\\S\w*)'
boolean = r'(?:#(?:t|f))'
quote = r'(?:\')'
quasiquote = r'(?:\`)'
unquote_splicing = r'(?:\,@)'
unquote = r'(?:\,)'
other = r'(?:\S+)'

tokens = comment + '|' + lparen + '|' + rparen + '|' + number + '|' + symbol + '|' + string + '|' + brokenstring + '|' + char + '|' + boolean + '|' + quote + '|' + quasiquote + '|' + unquote_splicing + '|' + unquote + '|' + other
    
ptokens = re.compile(tokens, flags)
#ptokens = re.compile(ur';.*$|\(|\)|(?:[+\-]?(?:(?:[0-9]+\.?[0-9]*)|(?:[0-9]*\.?[0-9]+))(?:e[+\-]?[0-9]+)?)|(?:(?:[a-zA-Z!$%&*/:<=>?^_~][a-zA-Z0-9!$%&*/:<=>?^_~+\-.@]*)|\+|\-|\.\.\.)|(?:\"(?:\\"|[^"])*?\")|(?:#\\\S\w*)|(?:#(?:t|f))|\'|\".*$', flags)
#ptokens = re.compile(ur'\(|\)|(?:[\w+\-*/<>=!?.]+)|(?:#(?:t|f))|\'', flags)
#ptokens = re.compile(ur'\(|\)', flags)


class SchemeError(Exception):
class Token(object):
    """
    Class for representing a token of a Scheme source code. In addition to that it also
    stores some metadata (line containing the token, line and column numbers).
    """
    def __init__(self, text, meta):
        self.text = text
        self.meta = meta
    
    def __str__(self):
        return self.text
        
    def __repr__(self):
        return '<Token ' + str(self) + '>'
        
class SExpression(object):
    __slots__ = []
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
            raise(ExpressionError(token, 'Unterminated string ')) # + text))
        elif SExpression.pchar.match(text):
            expr = Char.parseToken(token)
        elif SExpression.pboolean.match(text):
            expr = Boolean.parseToken(token)
        elif text == '\'':
            try:
                expr = Pair.makeFromList([Symbol.make('quote'), SExpression.parseTokens(next(tokens), tokens, topLevel=topLevel)])
                expr.meta = token.meta
            except StopIteration:
                raise(ExpressionError(token, 'Nothing to quote.'))
        elif text == '`':
            try:
                expr = Pair.makeFromList([Symbol.make('quasiquote'), SExpression.parseTokens(next(tokens), tokens, topLevel=topLevel)])
                expr.meta = token.meta
            except StopIteration:
                raise(ExpressionError(token, 'Nothing to quasiquote.'))
        elif text == ',':
            try:
                expr = Pair.makeFromList([Symbol.make('unquote'), SExpression.parseTokens(next(tokens), tokens, topLevel=topLevel)])
                expr.meta = token.meta
            except StopIteration:
                raise(ExpressionError(token, 'Nothing to unquote.'))
        elif text == ',@':
            try:
                expr = Pair.makeFromList([Symbol.make('unquote-splicing'), SExpression.parseTokens(next(tokens), tokens, topLevel=topLevel)])
                expr.meta = token.meta
            except StopIteration:
                raise(ExpressionError(token, 'Nothing to unquote-splicing.'))
        elif SExpression.psymbol.match(text):
            expr = Symbol.parseToken(token)
        else:
            raise(ExpressionError(token, 'Unrecognized token "' + text + '"'))
        return expr
            
    def eval(self, frame, cont):
        raise ExpressionError(self, 'Abstract SExpression should not be evaluated directly')
    
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
    def eval(self, frame, cont):
        return Trampolined.make(cont, self)
        #return self
        
    def isSelfEval(self):
        return True
        
    def __str__(self):
        return self.value
        
class Nil(SExpression):
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
    pchar = re.compile(r'^#\\(.+)$', flags)

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
        #match = cls.pchar.match(token.text)
        #char = cls.make(match.group(1))
        char = cls.make(token.text)
        #char.meta = token.meta
        return char

    def isChar(self):
        return True
        
    def __str__(self):
        string = self.pchar.match(self.value).group(1)
        if (string == 'newline'):
            return "\n"
        if (string == 'space'):
            return " "
        return string
        
    def __repr__(self):
        return self.value

    def __eq__(self, other):
        return (other is self) # not needed because of caching  # or (type(other) == type(self) and other.value == self.value)
        
        
class String(SelfEval):
    flags2 = flags #| re.M
    pstring = re.compile(r'^\"((?:[^"]|\")*)\"$', flags)
    p1  = re.compile(r'\\\\', flags2)
    p2  = re.compile(r'\\"', flags2)
    p3  = re.compile(r'\\a', flags2)
    p4  = re.compile(r'\\f', flags2)
    p5  = re.compile(r'\\n', flags2)
    p6  = re.compile(r'\\r', flags2)
    p7  = re.compile(r'\\t', flags2)
    p8  = re.compile(r'\\v', flags2)
    p9  = re.compile(r'\\b', flags2)
    p10 = re.compile(r'\\0', flags2)
    #p11 = re.compile(r'\\\n', flags2)
    p30 = re.compile(r'\\x([0-9a-fA-F]{2})', flags2)
    p31 = re.compile(r'\\u([0-9a-fA-F]{4})', flags2)
    p32 = re.compile(r'\\U([0-9a-fA-F]{6})', flags2)
    
    @classmethod
    def make(cls, string):
        self = cls()
        self.value = string
        return self

    @classmethod
    def parseToken(cls, token, tokens=[]):
        match = cls.pstring.match(token.text)
        string = cls.make(match.group(1))
        string.meta = token.meta
        return string

    def isString(self):
        return True
        
    #def __str__(self):
    #    return unicode(self)
        
    def __str__(self):
        s = self.value
        s = self.p1.sub(r'\\', s)
        s = self.p2.sub('\"', s)
        s = self.p3.sub('\a', s)
        s = self.p4.sub('\f', s)
        s = self.p5.sub('\n', s)
        s = self.p6.sub('\r', s)
        s = self.p7.sub('\t', s)
        s = self.p8.sub('\v', s)
        s = self.p9.sub('\b', s)
        s = self.p10.sub('\0', s)
        s = self.p30.sub(lambda match: unichr(int(match.group(1), 16)), s)
        s = self.p31.sub(lambda match: unichr(int(match.group(1), 16)), s)
        s = self.p32.sub(lambda match: unichr(int(match.group(1), 16)), s)
        #s.encode('cp932')
        return s

    def __repr__(self):
        return '"' + self.value + '"'

    def __eq__(self, other):
        return (other is self) or (type(other) == type(self) and other.value == self.value)
        
class Number(SelfEval):
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
            return RealNumber.make(float(text), token.meta)
        else:
            return IntegerNumber.make(int(text))
            #return IntegerNumber.makeUnCached(long(text), token.meta)

    def isNumber(self):
        return True
        
    def __str__(self):
        return str(self.value)
                
class IntegerNumber(Number):
    cache = [None]*256
    
    @classmethod
    def make(cls, value):
        "Construct an ``IntegerNumber`` object. Cache some of them for efficiency."
        if 0 <= value < 256:
            cached = cls.cache[value]
            if cached:
                #print '*', cached.value
                return cached
            self = cls()
            self.value = value
            cls.cache[value] = self
            #print ' ', self.value
            return self
        self = cls()
        self.value = value
        #print ' ', self.value
        return self

    #@classmethod
    #def makeUnCached(cls, value, meta):
    #    """
    #    Construct an ``IntegerNumber`` object without caching.
    #    Unique objects are needed when ``meta`` is attached.
    #    """
    #    self = cls()
    #    self.value = value
    #    self.meta = meta
    #    return self

    def isInteger(self):
        return True
        
    def isExact(self):
        return True
        
    def __eq__(self, other):
        #because caching is partial full comparison is still needed
        return (other is self) or (type(other) == type(self) and other.value == self.value)

class RealNumber(Number):
    @classmethod
    def make(cls, value, meta = None):
        self = cls()
        self.value = value
        if meta:
            self.meta = meta
        return self

    def isReal(self):
        return True
        
    def isInexact(self):
        return True
        
    def __eq__(self, other):
        return (other is self) or (type(other) == type(self) and other.value == self.value)

class Boolean(SelfEval):
    cache = {}
    
    @classmethod
    def make(cls, value):
        if value in cls.cache:
            return cls.cache[value]
        self = cls()
        self.value = value
        cls.cache[value] = self
        return self

    #@classmethod
    #def makeUnCached(cls, value, meta):
    #    self = cls()
    #    self.value = value
    #    self.meta = meta
    #    return self

    @classmethod
    def parseToken(cls, token, tokens=[]):
        if (token.text == '#f'):
            bool = Boolean.make(False)
            #bool = Boolean.makeUnCached(False, token.meta)
        else:
            bool = Boolean.make(True)
            #bool = Boolean.makeUnCached(True, token.meta)
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
        #return (other is self) or (type(other) == type(self) and other.value == self.value) or (type(other) != type(self) and self.value)
        #return (other is self) or (other.value == self.value) or (not other.isBoolean() and self.value)
        
class ExpressionError(Exception, SelfEval):
    """
    Exception class representing Scheme Expression parsing or evaluation error.
    """
    def __init__(self, expr, msg=''):
        "``expr`` can be either SExpression or Token. It must contain ``meta`` attribute."
        self.expr = expr
        self.msg = msg
        
    #def __str__(self):
    #    return unicode(self)
        
    def __str__(self):
        if not 'meta' in self.expr.__dict__:
            return 'Error: ' + self.msg
        fileName = self.expr.meta['fileName']
        line = self.expr.meta['line'].rstrip()
        lineNo = str(self.expr.meta['lineNo'])
        start = self.expr.meta['colStart']
        span = self.expr.meta['colEnd'] - start
        return 'Error: ' + self.msg + '\n' + fileName + ':' + lineNo + ', ' + line + '\n' + (' ' * (start+len(fileName)+len(lineNo)+2)) + ('-' * span)
        
    def __repr__(self):
        return '<ExpressionError ' + self.msg + '>'

class Symbol(SExpression):
    cache = {}
    
    @classmethod
    def make(cls, name):
        if name in cls.cache:
            return cls.cache[name]
        self = cls()
        self.name = name
        cls.cache[name] = self
        return self
        
    #@classmethod
    #def makeUnCached(cls, name, meta):
    #    self = cls()
    #    self.name = name
    #    self.meta = meta
    #    return self
        
    @classmethod
    def parseToken(cls, token):
        sym = Symbol.make(token.text)
        #sym = Symbol.makeUnCached(token.text, token.meta)
        return sym
        
    def eval(self, frame, cont):
        return Trampolined.make(cont, frame.resolveSymbol(self))

    def isSymbol(self):
        return True
        
    def __str__(self):
        return self.name

    def __eq__(self, other):
        return (other is self) # not needed because of chaching # or (type(other) == type(self) and other.name == self.name)

class Null(SExpression):
    cache = None
        
    @classmethod
    def make(cls):
        if cls.cache:
            return cls.cache
        self = cls()
        cls.cache = self
        return self
    
    #@classmethod
    #def makeUnCached(cls, meta):
    #    self = cls()
    #    self.meta = meta
    #    return self
    
    def eval(self, frame, cont):
        raise ExpressionError(self, 'Empty application.')

    def evalElements(self, frame, cont, excp=None):
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

class Pair(SExpression):
    @classmethod
    def make(cls, car, cdr):
        self = cls()
        self.car = car
        self.cdr = cdr
        self.quasiquoteLevel = 0
        self.unquoteSplice = False
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
                        self.meta = token.meta
                        self.topLevel = topLevel
                        return self
                    else:
                        expr = SExpression.parseTokens(t, tokens)
                        tail.cdr = cls.make(expr, Null.make())
                        tail = tail.cdr
                else: # improper
                    if t.text == '.': # more than 1 dot
                        error = True
                    elif t.text == ')':
                        if error or improper != 2: #earlier error or wrong number of expressions after dot
                            raise ExpressionError(token, 'Wrong pair format.')
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
            raise ExpressionError(token, 'Unterminated list.')
        
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
        
    def eval(self, frame, cont):
        def step2(operator):
            def step3(operands):
                return operator.apply(operands, self, cont)
            if not operator.isProcedure():
                raise ExpressionError(self, 'procedure application, first operand is not a procedure.')
            return self.cdr.evalElements(frame, step3, ExpressionError(self, '"eval": improper operand list'))
        if self.car.isSymbol() and self.car.name in SpecialSyntax.specialForms:
            return SpecialSyntax.specialForms[self.car.name]().apply(self.cdr, self, frame, cont)
        return self.car.eval(frame, step2)

    def evalElements(self, frame, cont, excp=None):
        def step2(car):
            def step3(cdr):
                return Trampolined.make(cont, Pair.make(car, cdr))
            if self.cdr.isPair() or self.cdr.isNull():
                return self.cdr.evalElements(frame, step3, excp)
            else:
                if excp:
                    raise excp
                else:
                    return self.cdr.eval(frame, step3)
        return self.car.eval(frame, step2)

    def evalSequence(self, frame, cont):
        def step2(car):
            return self.cdr.evalSequence(frame, cont)
        if self.cdr.isNull():
            return self.car.eval(frame, cont)
        else:
            return self.car.eval(frame, step2)
        
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
                return Pair.makeFromList([self[i] for i in range(*indices)]) # allocates new list
        if not isinstance(index, int):
            raise TypeError('pair indices must be integers, not ' + index.__class__.__name__)
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

    def __str__(self):
        if self.car.isSymbol() and self.cdr.isPair() and self.cdr.cdr.isNull():
            s = self.car.name
            if s == 'quote':
                return '\'' + str(self.cdr.car)
            if s == 'quasiquote':
                return '`' + str(self.cdr.car)
            if s == 'unquote':
                return ',' + str(self.cdr.car)
            if s == 'unquote-splicing':
                return ',@' + str(self.cdr.car)
        string = '(' + str(self.car)
        cdr = self.cdr
        while cdr.isPair():
            string += (' ' + str(cdr.car))
            cdr = cdr.cdr
        if cdr.isNull():
            return (string + ')')
        else:
            return (string + ' . ' + str(cdr) + ')')
        
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

class Procedure(SExpression):
    def apply(self, operands, callingForm, cont = None):
        pass
        
    def isProcedure(self):
        return True

    def eval(self, frame, cont):
        raise ExpressionError(self, 'Procedure should not be evaluated directly')
    
class Continuation(Procedure):
    @classmethod
    def make(cls, continuation):
        self = cls()
        self.continuation = continuation
        return self
        
    def isContinuation(self):
        return True
        
    def apply(self, operands, callingForm, cont = None):
        if len(operands) != 1:
            raise ExpressionError(callingForm, 'Wrong number of operands, required 1, provided ' + str(len(operands)) + '.')
        return Trampolined(operands.car)
    
    def __str__(self):
        return '#<continuation 0x%x>' % id(self)

class CompoundProcedure(Procedure):
    @classmethod
    def make(cls, formals, body, frame, meta):
        self = cls()
        self.formals = formals
        self.body = body
        #self.body = Body.make(body, meta)
        self.frame = frame
        self.checkFormals()
        self.meta = meta
        return self

    def checkFormals(self):
        defined = set()
        pointer = self.formals
        while pointer.isPair():
            if not pointer.car.isSymbol():
                raise ExpressionError(self, 'Invalid procedure operand name')
            if pointer.car.name in defined:
                raise ExpressionError(self, 'Duplicated operand name')
            defined.add(pointer.car.name)
            pointer = pointer.cdr
        if not pointer.isSymbol() and not pointer.isNull():
            raise ExpressionError(self, 'Invalid procedure operand name')
        
    def bind(self, formals, operands, callingForm, frame, cont):
        if formals.isNull() and operands.isNull():
            return Trampolined.make(cont, frame)
        elif formals.isSymbol():
            frame.addSymbol(formals, operands)
            return Trampolined.make(cont, frame)
        elif formals.isPair() and operands.isPair():
            frame.addSymbol(formals.car, operands.car)
            return self.bind(formals.cdr, operands.cdr, callingForm, frame, cont)
        elif formals.isList():
            raise ExpressionError(callingForm, 'Wrong number of operands, required ' + str(len(self.formals)) + '.')
        else:
            raise ExpressionError(callingForm, 'Wrong number of operands, required at least ' + str(len(self.formals) - 1) + '.')
        
    def bind_(self, formals, operands, callingForm, frame, cont):
        if operands.isNull() and formals.isPair():
            raise ExpressionError(callingForm, 'Wrong number of operands, required ' + str(len(self.formals)) + '.')
        if operands.isNull() and formals.isNull():
            return Trampolined.make(cont, frame)
        if formals.isSymbol():
            frame.addSymbol(formals, operands)
            return Trampolined.make(cont, frame)
        if operands.isPair() and formals.isPair() and formals.car.isSymbol():
            frame.addSymbol(formals.car, operands.car)
            return self.bind(formals.cdr, operands.cdr, callingForm, frame, cont)
        raise ExpressionError(callingForm, 'Wrong format of operands.')
        
    def apply(self, operands, callingForm, cont = None):
        def step2(newFrame):
            return self.body.evalSequence(newFrame, cont)
        return self.bind(self.formals, operands, callingForm, Frame(self.frame), step2)
    
    def __str__(self):
        return '#<procedure %s 0x%x>' % (str(self.formals), id(self))

class Trampolined(SExpression):
    @classmethod
    def make(cls, continuation, operand):
        #if continuation == None:
        #    return operand
        self = cls()
        self.continuation = continuation
        self.operand = operand
        return self

    def isTrampolined(self):
        return True

    def eval(self, frame, cont):
        return self.continuation(self.operand)

    def __str__(self):
        return '#<trampolined (%) 0x%x>' % (str(self.operand), id(self))

class SpecialSyntax(SExpression):
    object = None
    
    specialForms = {
        'quote':            lambda: QuoteForm.make(),
        'quasiquote':       lambda: QuasiQuoteForm.make(),
        'unquote':          lambda: UnQuoteForm.make(),
        'unquote-splicing': lambda: UnQuoteSplicingForm.make(),
        'define':           lambda: DefineForm.make(),
        'lambda':           lambda: LambdaForm.make(),
        'let':              lambda: LetForm.make(),
        'let*':             lambda: LetStarForm.make(),
        'letrec':           lambda: LetrecForm.make(),
        'set!':             lambda: SetForm.make(),
        'set-car!':         lambda: SetCarForm.make(),
        'set-cdr!':         lambda: SetCdrForm.make(),
        'if':               lambda: IfForm.make(),
        'cond':             lambda: CondForm.make(),
        'case':             lambda: CaseForm.make(),
        'and':              lambda: AndForm.make(),
        'or':               lambda: OrForm.make(),
        
    }

    @classmethod
    def make(cls):
        if not cls.object:
            cls.object = cls()
        return cls.object
            
    def eval(self, frame, cont):
        raise ExpressionError(self, 'Special syntax should not be evaluated directly')
    
    def apply(self, operands, callingForm, frame, cont = None):
        pass

    def isSpecialSyntax(self):
        return True
        
    def __str__(self):
        return '#<%s>' % self.__class__.__name__
    
class QuoteForm(SpecialSyntax):
    "Implements a :c:macro:`quote` or :c:macro:`'` form."
    def apply(self, operands, callingForm, frame, cont = None):
        if operands.isNull() or operands.cdr.isPair():
            raise ExpressionError(callingForm, '"quote" requires 1 operand, ' + str(len(operands)) + ' given.')
        res = operands.car
        return Trampolined.make(cont, res)

class QuotedForm(SpecialSyntax):
    "Helper base class for all :class:`QuasiQuoteForm` related classes."
    def processExpression(self, expr, callingForm, frame, cont):
        def unsplice(lst, cont):
            def step2(cdr):
                return Trampolined.make(cont, Pair.make(lst.car, cdr))
            if lst.isNull():
                return self.processExpression(expr.cdr, callingForm, frame, cont)
            return unsplice(lst.cdr, step2)
            
        if not expr.isPair():
            return Trampolined.make(cont, expr)
        else:
            def step2(car):
                def step3(cdr):
                    return Trampolined.make(cont, Pair.make(car, cdr))
                if car.isPair() and car.unquoteSplice:
                    return unsplice(car.car, cont) #Trampolined.make(cont, Nil.make())
                else:
                    return self.processExpression(expr.cdr, callingForm, frame, step3)
            expr.quasiquoteLevel = callingForm.quasiquoteLevel
            if expr.car.isSymbol() and expr.car.name == 'quasiquote':
                return expr.eval(frame, cont)
            if expr.car.isSymbol() and expr.car.name == 'unquote':
                return expr.eval(frame, cont)
            if expr.car.isSymbol() and expr.car.name == 'unquote-splicing':
                return expr.eval(frame, cont)
            return self.processExpression(expr.car, callingForm, frame, step2)

class QuasiQuoteForm(QuotedForm):
    "Implements a :c:macro:`quasiquote` or :c:macro:`\`` form."
    def apply(self, operands, callingForm, frame, cont = None):
        def step2(expr):
            if expr.isPair() and expr.unquoteSplice:
                raise ExpressionError(callingForm, '"unquote-splicing" should not expand directly in "quasiquote".')
            if callingForm.quasiquoteLevel == 1:
                return Trampolined.make(cont, expr)
            else:
                form = Pair.makeFromList([Symbol.make("quasiquote"), expr])
                form.meta = callingForm.meta
                form.quasiquoteLevel = callingForm.quasiquoteLevel
                return Trampolined.make(cont, form)
        if operands.isNull() or operands.cdr.isPair():
            raise ExpressionError(callingForm, '"quasiquote" requires 1 operand, ' + str(len(operands)) + ' given.')
        callingForm.quasiquoteLevel += 1
        return self.processExpression(operands.car, callingForm, frame, step2)
            
class UnQuoteForm(QuotedForm):
    "Implements an :c:macro:`unquote` or :c:macro:`,` form."
    def apply(self, operands, callingForm, frame, cont):
        def step2(expr):
            if expr.isPair() and expr.unquoteSplice:
                raise ExpressionError(callingForm, '"unquote-splicing" should not expand directly in "unquote".')
            form = Pair.makeFromList([Symbol.make("unquote"), expr])
            form.meta = callingForm.meta
            form.quasiquoteLevel = callingForm.quasiquoteLevel
            return Trampolined.make(cont, form)
        callingForm.quasiquoteLevel -= 1
        if operands.isNull() or operands.cdr.isPair():
            raise ExpressionError(callingForm, '"unquote" requires 1 operand, ' + str(len(operands)) + ' given.')
        if callingForm.quasiquoteLevel < 0:
            raise ExpressionError(callingForm, '"unquote" outside of "quasiquote".')
        if callingForm.quasiquoteLevel == 0:
            return operands.car.eval(frame, cont)
        else:
            return self.processExpression(operands.car, callingForm, frame, step2)

class UnQuoteSplicingForm(QuotedForm):
    "Implements an :c:macro:`unquote-splicing` or :c:macro:`,@` form."
    def apply(self, operands, callingForm, frame, cont = None):
        def evaluated(res):
            if not res.isPair() and not res.isNull():
                raise ExpressionError(callingForm, 'result of "unquote-splicing" is not a list.')
            res = Pair.make(res, Null.make()) # wrap it with a pair because we must set 'unquoteSplice' and Null is immutable
            res.unquoteSplice = True
            return Trampolined.make(cont, res)
        def expanded(expr):
            if expr.isPair() and expr.unquoteSplice:
                raise ExpressionError(callingForm, '"unquote-splicing" should not expand directly in "unquote-splicing".')
            form = Pair.makeFromList([Symbol.make("unquote-splicing"), expr])
            form.meta = callingForm.meta
            form.quasiquoteLevel = callingForm.quasiquoteLevel
            return Trampolined.make(cont, form)
        callingForm.quasiquoteLevel -= 1
        if operands.isNull() or operands.cdr.isPair():
            raise ExpressionError(callingForm, '"unquote-splicing" requires 1 operand, ' + str(len(operands)) + ' given.')
        if callingForm.quasiquoteLevel < 0:
            raise ExpressionError(callingForm, '"unquote-splicing" outside of "quasiquote".')
        if callingForm.quasiquoteLevel == 0:
            return operands.car.eval(frame, evaluated)
        else:
            return self.processExpression(operands.car, callingForm, frame, expanded)

class DefineForm(SpecialSyntax):
    "Implements a :c:macro:`define` form."
    def apply(self, operands, callingForm, frame, cont):
        #if not self.topLevel: # and not self.inBody:
        #    raise ExpressionError(self, '"define" only allowed at the top level or in a body of a procedure')
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '"define" requires at least 2 operands, ' + str(len(operands)) + ' given.')
        firstArg = operands.car
        if not firstArg.isSymbol() and not firstArg.isPair():
            raise ExpressionError(callingForm, '"define": Invalid type of the first operand')
        if firstArg.isSymbol():
            def step2(value):
                frame.addSymbol(firstArg, value)
                return Trampolined.make(cont, Nil.make())
            return operands.cdr.car.eval(frame, step2)
        if firstArg.isPair():
            if not firstArg.car.isSymbol():
                raise ExpressionError(callingForm, 'Invalid procedure name in "define"')
            procName = firstArg.car
            formals = firstArg.cdr #.toList()
            body = operands.cdr
            procedure = CompoundProcedure.make(formals, body, frame, callingForm.meta)
            frame.addSymbol(procName, procedure)
            return Trampolined.make(cont, Nil.make())

class LambdaForm(SpecialSyntax):
    "Implements a :c:macro:`lambda` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '"lambda" requires at least 2 operands, ' + str(len(operands)) + ' given.')
        formals = operands.car
        if not formals.isSymbol() and not formals.isPair() and not formals.isNull():
            raise ExpressionError(callingForm, '"lambda": Invalid type of the first operand')
        body = operands.cdr
        procedure = CompoundProcedure.make(formals, body, frame, callingForm.meta)
        return Trampolined.make(cont, procedure)
        
class LetForm(SpecialSyntax):
    "Implements a :c:macro:`let` form. Named :c:macro:`let` not yet supported."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '"let" requires at least 2 operands, ' + str(len(operands)) + ' given.')
        def step2(newFrame):
            return operands.cdr.evalSequence(newFrame, cont)
        return self.processBindings(operands.car, callingForm, frame, Frame(frame), step2)
        
    def processBindings(self, bindings, callingForm, oldFrame, newFrame, cont):
        def step2(bindingValue):
            if not binding.car.isSymbol():
                raise ExpressionError(callingForm, '"let": incorrect binding form (first element is not a symbol).')
            if binding.car.name in newFrame.symbols:
                raise ExpressionError(callingForm, '"let": multiple uses of the same variable in the binding form.')
            newFrame.symbols[binding.car.name] = bindingValue
            return self.processBindings(bindings.cdr, callingForm, oldFrame, newFrame, cont)
        if bindings.isNull():
            return Trampolined.make(cont, newFrame)
        binding = bindings.car
        if not binding.isPair():
            raise ExpressionError(callingForm, '"let": invalid binding form (not a list).')
        if not binding.cdr.isPair() or binding.cdr.cdr.isPair():
            raise ExpressionError(callingForm, '"let": each binding form must consist of 2 elements, ' + str(len(binding)) + ' given.')
        return binding.cdr.car.eval(oldFrame, step2)

class LetStarForm(SpecialSyntax):
    "Implements a :c:macro:`let*` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '"let*" requires at least 2 operands, ' + str(len(operands)) + ' given.')
        def step2(newFrame):
            return operands.cdr.evalSequence(newFrame, cont)
        return self.processBindings(operands.car, callingForm, frame, step2)
        
    def processBindings(self, bindings, callingForm, frame, cont):
        def step2(bindingValue):
            if not binding.car.isSymbol():
                raise ExpressionError(callingForm, '"let*": incorrect binding form (first element is not a symbol).')
            #if binding.car.name in newFrame.symbols:
            #    raise ExpressionError(callingForm, '"let*": multiple uses of the same variable in the binding form.')
            newFrame = Frame(frame)
            newFrame.symbols[binding.car.name] = bindingValue
            return self.processBindings(bindings.cdr, callingForm, newFrame, cont)
        if bindings.isNull():
            return Trampolined.make(cont, frame)
        binding = bindings.car
        if not binding.isPair():
            raise ExpressionError(callingForm, '"let*": invalid binding form (not a list).')
        if not binding.cdr.isPair() or binding.cdr.cdr.isPair():
            raise ExpressionError(callingForm, '"let*": each binding form must consist of 2 elements, ' + str(len(binding)) + ' given.')
        return binding.cdr.car.eval(frame, step2)

class LetrecForm(SpecialSyntax):
    "Implements a :c:macro:`letrec` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '"letrec" requires at least 2 operands, ' + str(len(operands)) + ' given.')
        def step2(newFrame):
            def step3(newFrame2):
                return operands.cdr.evalSequence(newFrame2, cont)
            return self.processBindingValues(operands.car, callingForm, newFrame, step3)
        return self.processBindingVariables(operands.car, callingForm, frame, Frame(frame), step2)
        
    def processBindingVariables(self, bindings, callingForm, oldFrame, newFrame, cont):
        if bindings.isNull():
            return Trampolined.make(cont, newFrame)
        binding = bindings.car
        if not binding.isPair():
            raise ExpressionError(callingForm, '"letrec": invalid binding form (not a list).')
        if not binding.cdr.isPair() or binding.cdr.cdr.isPair():
            raise ExpressionError(callingForm, '"letrec": each binding form must consist of 2 elements, ' + str(len(binding)) + ' given.')
        if not binding.car.isSymbol():
            raise ExpressionError(callingForm, '"letrec": incorrect binding form (first element is not a symbol).')
        if binding.car.name in newFrame.symbols:
            raise ExpressionError(callingForm, '"letrec": multiple uses of the same variable in the binding form.')
        newFrame.symbols[binding.car.name] = Nil.make()
        return self.processBindingVariables(bindings.cdr, callingForm, oldFrame, newFrame, cont)
        #return binding.cdr.car.eval(oldFrame, step2)

    def processBindingValues(self, bindings, callingForm, frame, cont):
        def step2(bindingValue):
            frame.symbols[binding.car.name] = bindingValue
            return self.processBindingValues(bindings.cdr, callingForm, frame, cont)
        if bindings.isNull():
            return Trampolined.make(cont, frame)
        binding = bindings.car
        return binding.cdr.car.eval(frame, step2)

class SetForm(SpecialSyntax):
    "Implements a :c:macro:`set!` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull() or operands.cdr.cdr.isPair():
            raise ExpressionError(callingForm, '"set!" requires 2 operands, ' + str(len(operands)) + ' given.')
        var = operands.car
        if not var.isSymbol():
            raise ExpressionError(callingForm, '"set!" first operand is not a symbol.')
        targetFrame = frame.resolveSymbolLocation(var.name)
        if targetFrame == None:
            raise ExpressionError(callingForm, '"set!" undefined symbol' + var.name + '.')
        def step2(value):
            targetFrame[var.name] = value
            return Trampolined.make(cont, Nil.make())
        return operands.cdr.car.eval(frame, step2)

class IfForm(SpecialSyntax):
    "Implements a :c:macro:`if` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull() or operands.cdr.isNull() or (operands.cdr.cdr.isPair() and operands.cdr.cdr.cdr.isPair()):
            raise ExpressionError(callingForm, '"if" requires 2 or 3 operands, ' + str(len(operands)) + ' given.')
        def step2(predicate):
            if not predicate.isBoolean() or predicate.value: #True
                res = operands.cdr.car.eval(frame, cont)
            elif operands.cdr.cdr.isPair():
                res = operands.cdr.cdr.car.eval(frame, cont)
            else:
                res = Trampolined.make(cont, Nil.make())
            return res
        return operands.car.eval(frame, step2)

class CondForm(SpecialSyntax):
    "Implements a :c:macro:`cond` form."
    def apply(self, operands, callingForm, frame, cont):
        if operands.isNull():
            raise ExpressionError(callingForm, '"cond" requires at least 2 operands, ' + str(len(operands)) + ' given.')
        def step2(clause):
            if clause.isPair():
                return clause.cdr.car.eval(frame, cont)
            else:
                return Trampolined.make(cont, clause)
        return self.processClauses(operands, callingForm, frame, step2)

    def processClauses(self, clauses, callingForm, frame, cont):
        if clauses.isNull():
            return Trampolined.make(cont, Nil.make())
        clause = clauses.car
        if not clause.isPair() or not clause.cdr.isPair() or not clause.cdr.cdr.isNull():
            raise ExpressionError(clause, '"cond": invalid clause format.')
        def step2(boolValue):
            if (not boolValue.isBoolean() or boolValue.value == True):
                return Trampolined.make(cont, clause)
            else:
                return self.processClauses(clauses.cdr, callingForm, frame, cont)
        if clauses.cdr.isNull() and clause.car.isSymbol() and clause.car.name == 'else':
            return Trampolined.make(cont, clause)
        return clause.car.eval(frame, step2)

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
        return operands.car.eval(frame, step2)
    
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
        return operands.car.eval(frame, step2)
    
class PrimitiveProcedure(Procedure):
    object = None

    primitiveFunctions = {
        'null?':        lambda: IsNullProcedure.make(),
        'eq?':          lambda: EqProcedure.make(),
        'eqv?':         lambda: EqvProcedure.make(),
        'equal?':       lambda: EqualProcedure.make(),
        '=':            lambda: NumEqProcedure.make(),
        '<':            lambda: NumLTProcedure.make(),
        '<=':           lambda: NumLTEProcedure.make(),
        '>':            lambda: NumGTProcedure.make(),
        '>=':           lambda: NumGTEProcedure.make(),
        'cons':         lambda: ConsProcedure.make(),
        'car':          lambda: CarProcedure.make(),
        'cdr':          lambda: CdrProcedure.make(),
        'append2':      lambda: Append2Procedure.make(),
        'not':          lambda: NotProcedure.make(),
        '+':            lambda: SumProcedure.make(),
        '-':            lambda: SubtractProcedure.make(),
        '*':            lambda: MultiplyProcedure.make(),
        '/':            lambda: DivideProcedure.make(),
        'quotient':     lambda: QuotientProcedure.make(),
        'modulo':       lambda: ModuloProcedure.make(),
        'remainder':    lambda: RemainderProcedure.make(),
        'write-char':   lambda: WriteCharProcedure.make(),
        'display':      lambda: DisplayProcedure.make(),
        'apply2':       lambda: Apply2Procedure.make(),
        'call-with-current-continuation': lambda: CallCCProcedure.make(),
        'call/cc':      lambda: CallCCProcedure.make(),
    }

    @classmethod
    def make(cls):
        if not cls.object:
            cls.object = cls()
        return cls.object
    
    def __str__(self):
        return '#<%s>' + self.__class__.__name__
    
class IsNullProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`null?` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isPair():
            raise ExpressionError(callingForm, '"null?" requires 1 operand, provided ' + str(len(operands)) + '.')
        return Trampolined.make(cont, Boolean.make(operands.car.isNull()))

class ConsProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`cons` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull() or operands.cdr.cdr.isPair():
            raise ExpressionError(callingForm, '"cons" requires 2 operands, provided ' + str(len(operands)) + '.')
        expr1 = operands.car
        expr2 = operands.cdr.car
        res = Pair.make(expr1, expr2)
        return Trampolined.make(cont, res)

class CarProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`car` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isPair():
            raise ExpressionError(callingForm, '"car" requires 1 operand, provided ' + str(len(operands)) + '.')
        arg = operands.car
        if not arg.isPair():
            raise ExpressionError(callingForm, '"car" operand must be a pair.')
        res = arg.car
        return Trampolined.make(cont, res)

class CdrProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`cdr` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isPair():
            raise ExpressionError(callingForm, '"cdr" requires 1 operand, provided ' + str(len(operands)) + '.')
        arg = operands.car
        if not arg.isPair():
            raise ExpressionError(callingForm, '"cdr" operand must be a pair.')
        res = arg.cdr
        return Trampolined.make(cont, res)
            
class Append2Procedure(PrimitiveProcedure):
    "Implements a two-argument :c:macro:`append` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull() or operands.cdr.cdr.isPair():
            raise ExpressionError(callingForm, '"append2" requires 2 operands, provided ' + str(len(operands)) + '.')
        expr1 = operands.car
        expr2 = operands.cdr.car
        if (not expr1.isNull() and not expr1.isPair()) or (not expr2.isNull() and not expr2.isPair()):
            raise ExpressionError(callingForm, '"append2": operands must be lists.')
        res = expr1.append(expr2)
        return Trampolined.make(cont, res)

class NotProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`not` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isPair():
            raise ExpressionError(callingForm, '"not" requires 1 operand, provided ' + str(len(operands)) + '.')
        arg = operands.car
        value = False
        if arg.isBoolean():
            value = not(arg.value)
        res = Boolean.make(value)
        return Trampolined.make(cont, res)

class EqProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`eq?` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '"eq?" requires at least 2 operands, provided ' + str(len(operands)) + '.')
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
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '"eqv?" requires at least 2 operands, provided ' + str(len(operands)) + '.')
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
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '"equal?" requires at least 2 operands, provided ' + str(len(operands)) + '.')
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
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '"=" requires at least 2 operands, provided ' + str(len(operands)) + '.')
        value = True
        first = operands.car
        if not first.isNumber():
            raise ExpressionError(callingForm, '"=": operand is not a Number')
        cdr = operands.cdr
        while cdr.isPair():
            n = cdr.car
            if not n.isNumber():
                raise ExpressionError(callingForm, '"=": operand is not a Number')
            if n.value != first.value:
                value = False
            cdr = cdr.cdr
        res = Boolean.make(value)
        return Trampolined.make(cont, res)
        
class NumLTProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`<` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '"<" requires at least 2 operands, provided ' + str(len(operands)) + '.')
        value = True
        previous = operands.car
        if not previous.isNumber():
            raise ExpressionError(callingForm, '"<": operand is not a Number')
        cdr = operands.cdr
        while cdr.isPair():
            n = cdr.car
            if not n.isNumber():
                raise ExpressionError(callingForm, '"<": operand is not a Number')
            if previous.value >= n.value:
                value = False
            cdr = cdr.cdr
            previous = n
        res = Boolean.make(value)
        return Trampolined.make(cont, res)
        
class NumLTEProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`<=` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '"<=" requires at least 2 operands, provided ' + str(len(operands)) + '.')
        value = True
        previous = operands.car
        if not previous.isNumber():
            raise ExpressionError(callingForm, '"<=": operand is not a Number')
        cdr = operands.cdr
        while cdr.isPair():
            n = cdr.car
            if not n.isNumber():
                raise ExpressionError(callingForm, '"<=": operand is not a Number')
            if previous.value > n.value:
                value = False
            cdr = cdr.cdr
            previous = n
        res = Boolean.make(value)
        return Trampolined.make(cont, res)
        
class NumGTProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`>` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '">" requires at least 2 operands, provided ' + str(len(operands)) + '.')
        value = True
        previous = operands.car
        if not previous.isNumber():
            raise ExpressionError(callingForm, '">": operand is not a Number')
        cdr = operands.cdr
        while cdr.isPair():
            n = cdr.car
            if not n.isNumber():
                raise ExpressionError(callingForm, '">": operand is not a Number')
            if previous.value <= n.value:
                value = False
            cdr = cdr.cdr
            previous = n
        res = Boolean.make(value)
        return Trampolined.make(cont, res)
        
class NumGTEProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`>=` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull():
            raise ExpressionError(callingForm, '">=" requires at least 2 operands, provided ' + str(len(operands)) + '.')
        value = True
        previous = operands.car
        if not previous.isNumber():
            raise ExpressionError(callingForm, '">=": operand is not a Number')
        cdr = operands.cdr
        while cdr.isPair():
            n = cdr.car
            if not n.isNumber():
                raise ExpressionError(callingForm, '">=": operand is not a Number')
            if previous.value < n.value:
                value = False
            cdr = cdr.cdr
            previous = n
        res = Boolean.make(value)
        return Trampolined.make(cont, res)
        
class SumProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`+` primitive function."
    def apply(self, operands, callingForm, cont):
        value = 0
        for n in operands:
            if not n.isNumber():
                raise ExpressionError(callingForm, '"+": operand is not a Number')
            value += n.value
        res = Number.make(value)
        return Trampolined.make(cont, res)
        
class SubtractProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`-` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull():
            raise ExpressionError(callingForm, '"-" requires at least 1 operand, provided ' + str(len(operands)) + '.')
        if not operands.car.isNumber():
            raise ExpressionError(callingForm, '"-": operand is not a Number')
        if operands.cdr.isNull():
            res = Number.make(-operands.car.value)
        else:
            value = operands.car.value
            for n in operands.cdr:
                if not n.isNumber():
                    raise ExpressionError(callingForm, '"-": operand is not a Number')
                value -= n.value
            res = Number.make(value)
        return Trampolined.make(cont, res)

class MultiplyProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`*` primitive function."
    def apply(self, operands, callingForm, cont):
        value = 1
        for n in operands:
            if not n.isNumber():
                raise ExpressionError(callingForm, '"*": operand is not a Number')
            value *= n.value
        res = Number.make(value)
        return Trampolined.make(cont, res)
        
class DivideProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`/` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull():
            raise ExpressionError(callingForm, '"/" requires at least 1 operand, provided ' + str(len(operands)) + '.')
        if not operands.car.isNumber():
            raise ExpressionError(callingForm, '"/": operand is not a Number')
        if operands.cdr.isNull():
            res = Number.make(1.0/operands[0].value)
        else:
            value = float(operands.car.value)
            for n in operands.cdr:
                if not n.isNumber():
                    raise ExpressionError(callingForm, '"/": operand is not a Number')
                if n.value == 0:
                    raise ExpressionError(callingForm, '"/": division by 0')
                value /= n.value
            res = Number.make(value)
        return Trampolined.make(cont, res)

class ModuloProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`modulo` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull() or operands.cdr.cdr.isPair():
            raise ExpressionError(callingForm, '"modulo" requires 2 operands, provided ' + str(len(operands)) + '.')
        if not operands.car.isNumber():
            raise ExpressionError(callingForm, '"modulo": operand is not a Number')
        if not operands.cdr.car.isNumber():
            raise ExpressionError(callingForm, '"modulo": operand is not a Number')
        if operands.cdr.car.value == 0:
            raise ExpressionError(callingForm, '"modulo": division by 0')
        res = Number.make(operands.car.value % operands.cdr.car.value)
        return Trampolined.make(cont, res)

class RemainderProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`remainder` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull() or operands.cdr.cdr.isPair():
            raise ExpressionError(callingForm, '"remainder" requires 2 operands, provided ' + str(len(operands)) + '.')
        if not operands.car.isNumber():
            raise ExpressionError(callingForm, '"remainder": operand is not a Number')
        if not operands.cdr.car.isNumber():
            raise ExpressionError(callingForm, '"remainder": operand is not a Number')
        if operands.cdr.car.value == 0:
            raise ExpressionError(callingForm, '"remainder": division by 0')
        dividend = operands.car.value
        divisor = operands.cdr.car.value
        res = Number.make(dividend - divisor*int(float(dividend)/divisor))
        return Trampolined.make(cont, res)

class QuotientProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`quotient` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull() or operands.cdr.cdr.isPair():
            raise ExpressionError(callingForm, '"quotient" requires 2 operands, provided ' + str(len(operands)) + '.')
        if not operands.car.isNumber():
            raise ExpressionError(callingForm, '"quotient": operand is not a Number')
        if not operands.cdr.car.isNumber():
            raise ExpressionError(callingForm, '"quotient": operand is not a Number')
        if operands.cdr.car.value == 0:
            raise ExpressionError(callingForm, '"quotient": division by 0')
        res = Number.make(operands.car.value // operands.cdr.car.value)
        return Trampolined.make(cont, res)

class WriteCharProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`write-char` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or (operands.cdr.isPair() and operands.cdr.cdr.isPair()):
            raise ExpressionError(callingForm, '"write-char" requires 1 or 2 operands, provided ' + str(len(operands)) + '.')
        if not operands.car.isChar():
            raise ExpressionError(callingForm, '"write-char": operand is not a Character')
        sys.stdout.write(str(operands.car))
        return Trampolined.make(cont, Nil.make())

class DisplayProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`display` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or (operands.cdr.isPair() and operands.cdr.cdr.isPair()):
            raise ExpressionError(callingForm, '"display" requires 1 or 2 operands, provided ' + str(len(operands)) + '.')
        sys.stdout.write(str(operands.car))
        return Trampolined.make(cont, Nil.make())

class Apply2Procedure(PrimitiveProcedure):
    "Implements a two-argument :c:macro:`apply2` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isNull() or operands.cdr.cdr.isPair():
            raise ExpressionError(callingForm, '"apply2" requires 2 operands, provided ' + str(len(operands)) + '.')
        procedure = operands.car
        if not procedure.isProcedure():
            raise ExpressionError(callingForm, '"apply2": first operand must be a procedure.')
        operands = operands.cdr.car
        #if not operands.isNull() and not operands.isPair():
        if not operands.isList():
            raise ExpressionError(callingForm, '"apply2": second operand must be a list.')
        return procedure.apply(operands, callingForm, cont)

class CallCCProcedure(PrimitiveProcedure):
    "Implements a :c:macro:`call/cc` or :c:macro:`call-with-current-continuation` primitive function."
    def apply(self, operands, callingForm, cont):
        if operands.isNull() or operands.cdr.isPair():
            raise ExpressionError(callingForm, '"call/cc" requires 1 operand, provided ' + str(len(operands)) + '.')
        if not operands.car.isProcedure():
            raise ExpressionError(callingForm, '"call/cc": operand must be a procedure.')
        continuation = Continuation.make(cont)
        return operands.car.apply(Pair.makeFromList([continuation]), callingForm, cont)


class Frame(SExpression):
    __slots__ = ['parentFrame', 'symbols']
    def __init__(self, parentFrame=None):
        self.parentFrame = parentFrame
        self.symbols = {}
                
    def resolveSymbol(self, symbol):
        if symbol.name in self.symbols:
            return self.symbols[symbol.name]
        elif self.parentFrame:
            return self.parentFrame.resolveSymbol(symbol)
        elif symbol.name in PrimitiveProcedure.primitiveFunctions:
            return PrimitiveProcedure.primitiveFunctions[symbol.name]()
            #return symbol
        else:
            raise SchemeError(symbol, 'Undefined symbol "' + symbol.name + '"')

    def resolveSymbolLocation(self, symbol):
        if symbol.name in self.symbols:
            return self.symbols
        elif self.parentFrame:
            return self.parentFrame.resolveSymbolLocation(symbol)
        else:
            return None
        
    def addSymbol(self, symbol, value):
        self.symbols[symbol.name] = value
        
    def evaluateExpressions(self, expressions):
        for expression in expressions:
            #print expression
            try:
                def processResult(result):
                    return result
                result = expression.eval(self, processResult)
                while result.isTrampolined():
                    #print(result.operand)
                    #print(result)
                    result = result.eval(self, processResult)
                yield result
            except SchemeError as e:
                yield ErrorExpression.make(e)
            except RuntimeError:
                yield ErrorExpression.make(SchemeError(expression, 'maximum stack depth exceeded'))

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
        @lines can be any iterable object, including a generator object (a file, interactive console)
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
        for token in tokens:
            try:
                yield SExpression.parseTokens(token, tokens, topLevel=True)
            except SchemeError as e:
                yield ErrorExpression.make(e)
            
