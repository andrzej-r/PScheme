# -*- coding: utf-8 -*-
# 
# Copyright 2011 PScheme Contributors (see CONTIBUTORS for details). All rights reserved.
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
is written in this language) it cannot be reliably sandboxed. Executing
user scripts in the context of the application would open a security hole,
which could easily be exploited remotely (by preparing a malicious design file).

Scheme was chosen for its expressivenes to simplicity ratio.
"""

import re
import sys
from copy import copy

# flags for the parser
flags = re.UNICODE

class ExpressionError(Exception):
    """
    Exception class representing Scheme Expression parsing or evaluation error.
    """
    def __init__(self, expr, msg=''):
        "``expr`` can be either SExpression or Token. It must contain ``meta`` attribute."
        self.expr = expr
        self.msg = msg
        
    def __str__(self):
        if not 'meta' in self.expr.__dict__:
            return 'Error: ' + self.msg
        fileName = self.expr.meta['fileName']
        line = self.expr.meta['line']
        lineNo = str(self.expr.meta['lineNo'])
        start = self.expr.meta['colStart']
        span = self.expr.meta['colEnd'] - start
        return 'Error: ' + self.msg + '\n' + fileName + ':' + lineNo + ', ' + line + '\n' + (' ' * (start+len(fileName)+len(lineNo)+2)) + ('~' * span)
        
    def __repr__(self):
        return self.__str__()

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
        return '<Token ' + str(self) + ', Line :' + self.line + '>'
        
class Tokenizer(object):
    ptokens = re.compile(ur';.*$|\(|\)|(?:[+\-]?(?:(?:[0-9]+\.?[0-9]*)|(?:[0-9]*\.?[0-9]+))(?:e[+\-]?[0-9]+)?)|(?:(?:[a-zA-Z!$%&*/:<=>?^_~][a-zA-Z0-9!$%&*/:<=>?^_~+\-.@]*)|\+|\-|\.\.\.)|(?:\"(?:\\"|[^"])*?\")|(?:#\\\S\w*)|(?:#(?:t|f))|\'|\".*$', flags)
    #ptokens = re.compile(ur'\(|\)|(?:[\w+\-*/<>=!?.]+)|(?:#(?:t|f))|\'', flags)
    #ptokens = re.compile(ur'\(|\)', flags)

    def __init__(self, fileName):
        self.lineNo = 0
        self.fileName = fileName
    
    def tokenizeLine(self, line):
        """
        Tokenizes a single text line.
        @line line of text (string)
        """
        #print line
        self.lineNo += 1
        for token_match in self.ptokens.finditer(line):
            if not token_match.group().startswith(u';'):
                meta = {}
                meta['fileName'] = self.fileName
                meta['line'] = token_match.string
                meta['lineNo'] = self.lineNo
                meta['colStart'] = token_match.start()+1
                meta['colEnd'] = token_match.end()+1
                token = Token(token_match.group(), meta)
                #print token.text
                yield token

    def chain(self, iterables):
        # chain(['ABC', 'DEF']) --> A B C D E F
        for it in iterables:
            for element in it:
                yield element
    
    def tokenizeLines(self, lines):
        """
        Tokenizes a list of lines.
        @lines can be any iterable object, including a generator object (a file, interactive console)
        """
        #print (self.tokenizeLine(line) for line in lines)
        return self.chain(self.tokenizeLine(line) for line in lines)
        #return self.chain([self.tokenizeLine(line) for line in lines])
                
    @classmethod
    def fromText(cls, text):
        self = cls()
        for line in text.splitlines():
            #print line
            self.lineNo += 1
            for token_match in self.ptokens.finditer(line):
                if not token_match.group().startswith(u';'):
                    token = Token()
                    token.text = token_match.group()
                    token.meta = {}
                    token.meta['line'] = token_match.string
                    token.meta['lineNo'] = self.lineNo
                    token.meta['colStart'] = token_match.start()+1
                    token.meta['colEnd'] = token_match.end()+1
                    #print token.text
                    yield token

class Frame(object):
    def __init__(self, parentFrame=None):
        self.parentFrame = parentFrame
        self.symbols = {}
                
    def read(self, text, tokenizer):
        lines = text.splitlines()
        return self.readLines(lines, tokenizer)
    
    def readLines(self, lines, tokenizer):
        tokens = tokenizer.tokenizeLines(lines)
        return self.parseTokens(tokens)
    
    def parseTokens(self, tokens):
        for token in tokens:
            expr = SExpression.parseTokens(token, tokens, topLevel=True)
            yield expr
            
    def resolveSymbol(self, symbol):
        if symbol.name in self.symbols:
            return self.symbols[symbol.name]
        elif self.parentFrame:
            return self.parentFrame.resolveSymbol(symbol)
        elif symbol.name in PrimitiveProcedure.primitiveFunctions:
            return PrimitiveProcedure.primitiveFunctions[symbol.name]()
            #return symbol
        else:
            raise ExpressionError(symbol, 'Undefined symbol "' + symbol.name + '"')

    def addSymbol(self, symbol, value):
        self.symbols[symbol.name] = value
        
class SExpression(object):
    pnumber = re.compile(ur'^[+\-]?(?:(?:[0-9]+\.?[0-9]*)|(?:[0-9]*\.?[0-9]+))(?:e[+\-]?[0-9]+)?$', flags)
    pstring = re.compile(ur'^\"(?:\\"|[^"])*?\"$', flags)
    pbrokenstring = re.compile(ur'^\"(?:\\"|[^"])*?$', flags)
    pchar = re.compile(ur'^#\\\S\w*$', flags)
    pboolean = re.compile(ur'^#(?:t|f)$', flags)
    psymbol = re.compile(ur'^(?:[a-zA-Z!$%&*/:<=>?^_~][a-zA-Z0-9!$%&*/:<=>?^_~\d+-.@]*)|\+|\-|\.\.\.$', flags)
    
    @classmethod
    def parseTokens(cls, token, tokens, topLevel=False):
        text = token.text
        if text == u'(':
            expr = Pair.parseTokens(token, tokens, topLevel=topLevel)
        elif SExpression.pnumber.match(text):
            expr = Number.parseToken(token)
        elif SExpression.pstring.match(text):
            expr = String.parseToken(token)
        elif SExpression.pbrokenstring.match(text):
            raise(ExpressionError(token, 'Unterminated string ' + text))
        elif SExpression.pchar.match(text):
            expr = Char.parseToken(token)
        elif SExpression.pboolean.match(text):
            expr = Boolean.parseToken(token)
        elif text == u'\'':
            try:
                expr = Pair.makeFromList([SpecialSyntax.specialForms[u'quote'](), SExpression.parseTokens(tokens.next(), tokens, topLevel=topLevel)])
                expr.meta = token.meta
            except StopIteration:
                raise(ExpressionError(token, 'Nothing to quote.'))
        elif SExpression.psymbol.match(text):
            if text in SpecialSyntax.specialForms:
                expr = SpecialSyntax.specialForms[text]()
            else:
                expr = Symbol.parseToken(token)
        else:
            raise(ExpressionError(token, 'Unrecognized token "' + text + '"'))
        #expr.meta = token.meta
        return expr
            
    def eval(self, frame):
        raise ExpressionError(self, 'Abstract SExpression should not be evaluated directly')
    
    def __ne__(self, other):
        return not (self == other)
        
    def __repr__(self):
        return str(self)
        
    def equal(self, other):
        return self == other
    
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
        
    def isList(self):
        return False
        
    def isSpecialSyntax(self):
        return False
    
class SelfEval(SExpression):
    def eval(self, frame):
        return self
        
    def __str__(self):
        return self.value
        
class Nil(SelfEval):
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
    pchar = re.compile(ur'^#\\(.+)$', flags)

    cache = {}
    
    @classmethod
    def make(cls, char):
        self = cls()
        self.value = char
        return self
    
    @classmethod
    def parseToken(cls, token, tokens=[]):
        #match = cls.pchar.match(token.text)
        #char = cls.make(match.group(1))
        char = cls.make(token.text)
        char.meta = token.meta
        return char

    def isChar(self):
        return True
        
    def __str__(self):
        string = self.pchar.match(self.value).group(1)
        if (string == u'newline'):
            return "\n"
        if (string == u'space'):
            return " "
        return string
        
    def __repr__(self):
        return self.value

    def __eq__(self, other):
        return (other is self) or (type(other) == type(self) and other.value == self.value)
        
        
class String(SelfEval):
    flags2 = flags #| re.M
    pstring = re.compile(ur'^\"((?:[^"]|\")+)\"$', flags)
    p1  = re.compile(ur'\\\\', flags2)
    p2  = re.compile(ur'\\"', flags2)
    p3  = re.compile(ur'\\a', flags2)
    p4  = re.compile(ur'\\f', flags2)
    p5  = re.compile(ur'\\n', flags2)
    p6  = re.compile(ur'\\r', flags2)
    p7  = re.compile(ur'\\t', flags2)
    p8  = re.compile(ur'\\v', flags2)
    p9  = re.compile(ur'\\b', flags2)
    p10 = re.compile(ur'\\0', flags2)
    #p11 = re.compile(ur'\\\n', flags2)
    p30 = re.compile(ur'\\x([0-9a-fA-F]{2})', flags2)
    p31 = re.compile(ur'\\u([0-9a-fA-F]{4})', flags2)
    p32 = re.compile(ur'\\U([0-9a-fA-F]{6})', flags2)
    
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
        
    def __str__(self):
        return unicode(self)
        
    def __unicode__(self):
        s = self.value
        s = self.p1.sub(ur'\\', s)
        s = self.p2.sub(u'\"', s)
        s = self.p3.sub(u'\a', s)
        s = self.p4.sub(u'\f', s)
        s = self.p5.sub(u'\n', s)
        s = self.p6.sub(u'\r', s)
        s = self.p7.sub(u'\t', s)
        s = self.p8.sub(u'\v', s)
        s = self.p9.sub(u'\b', s)
        s = self.p10.sub(u'\0', s)
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
        if isinstance(value, (long, int)):
            return IntegerNumber.make(value)
        if isinstance(value, float):
            return RealNumber.make(value)

    @classmethod
    def parseToken(cls, token, tokens=[]):
        text = token.text
        if (text.find('.') >= 0 or text.find('e') >= 0):
            return RealNumber.make(float(text), token.meta)
        else:
            return IntegerNumber.makeUnCached(long(text), token.meta)

    def isNumber(self):
        return True
        
    def __str__(self):
        return str(self.value)
                
class IntegerNumber(Number):
    cache = [None]*1024
    
    @classmethod
    def make(cls, value):
        "Construct an ``IntegerNumber`` object. Cache some of them for efficiency."
        if 0 <= value < 1024:
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

    @classmethod
    def makeUnCached(cls, value, meta):
        """
        Construct an ``IntegerNumber`` object without caching.
        Unique objects are needed when ``meta`` is attached.
        """
        self = cls()
        self.value = value
        self.meta = meta
        return self

    def isInteger(self):
        return True
        
    def isExact(self):
        return True
        
    def __eq__(self, other):
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

    @classmethod
    def makeUnCached(cls, value, meta):
        self = cls()
        self.value = value
        self.meta = meta
        return self

    @classmethod
    def parseToken(cls, token, tokens=[]):
        if (token.text == '#f'):
            bool = Boolean.makeUnCached(False, token.meta)
        else:
            bool = Boolean.makeUnCached(True, token.meta)
        return bool

    def __str__(self):
        if self.value:
            return '#t'
        else:
            return '#f'
        
    def isBoolean(self):
        return True
        
    def __eq__(self, other):
        return (other is self) or (type(other) == type(self) and other.value == self.value) or (type(other) != type(self) and self.value)
        #return (other is self) or (other.value == self.value) or (not other.isBoolean() and self.value)
        
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
        
    @classmethod
    def makeUnCached(cls, name, meta):
        self = cls()
        self.name = name
        self.meta = meta
        return self
        
    @classmethod
    def parseToken(cls, token):
        sym = Symbol.makeUnCached(token.text, token.meta)
        return sym
        
    def eval(self, frame):
        return frame.resolveSymbol(self)

    def isSymbol(self):
        return True
        
    def __str__(self):
        return self.name

    def __eq__(self, other):
        return (other is self) or (type(other) == type(self) and other.name == self.name)

class Null(SExpression):
    cache = None
        
    @classmethod
    def make(cls):
        if cls.cache:
            return cls.cache
        self = cls()
        cls.cache = self
        return self
    
    @classmethod
    def makeUnCached(cls, meta):
        self = cls()
        self.meta = meta
        return self
    
    def eval(self, frame):
        raise ExpressionError(self, 'Empty application.')

    def toList(self):
        return []
        
    def isNull(self):
        return True
        
    def isList(self):
        return True
        
    def __len__(self):
        return 0
        
    def __getitem__(self, index):
        raise IndexError('(null) index out of range')

    def __iter__(self):
        return iter([])
        
    def __str__(self):
        return '()'
        
    def __eq__(self, other):
        return (other is self) or (type(other) == type(self))

class Pair(SExpression):
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
        self = None
        tail = None
        elements = []
        for t in tokens:
            if t.text != u')':
                expr = SExpression.parseTokens(t, tokens)
                if not self:
                    self = cls.make(expr, Null.make())
                    tail = self
                else:
                    tail.cdr = cls.make(expr, Null.make())
                    tail = tail.cdr
            else:
                self.meta = token.meta
                self.topLevel = topLevel
                return self
        raise ExpressionError(token, 'Unterminated list.')
        
    def toList(self):
        lst = [self.car]
        cdr = self.cdr
        while cdr.isPair():
            lst.append(cdr.car)
            cdr = cdr.cdr
        if cdr.isNull():
            return lst
        return lst + [cdr]
        
    def eval(self, frame):
        if not self.cdr.isList():
            raise ExpressionError(self, '"eval": improper argument list.')
        if self.car.isSpecialSyntax():
            return self.car.apply(self.cdr.toList(), self, frame)
        args = []
        cdr = self.cdr
        while cdr.isPair():
            args.append(cdr.car.eval(frame))
            cdr = cdr.cdr
        op = self.car.eval(frame)
        #args = Pair.make(self.car, Null.make())
        #cdr = args.cdr
        #for e in self.cdr:
        #    cdr = Pair.make(e.eval(frame), Null.make())
        #    cdr = cdr.cdr
        #args = args.cdr
        if op.isProcedure():
            #return op.apply(self.cdr, self)
            return op.apply(args, self)
        raise ExpressionError(self, 'procedure application, first operand is not a procedure.' + str(operator))

    def isPair(self):
        return True
        
    def isList(self):
        cdr = self.cdr
        while cdr.isPair():
            cdr = cdr.cdr
        return cdr.isNull()
        
    def len(self):
        return len(self)
        
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
                for i in xrange(0, indices[0]):
                    res = res.cdr
                return res
            else:
                return Pair.makeFromList([self[i] for i in range(*indices)]) # allocates new list
        if not isinstance(index, (int, long)):
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

class SpecialSyntax(SExpression):
    name = 'special-syntax'
    object = None
    
    specialForms = {
        u'quote':        lambda: QuoteForm.make(),
        u'define':       lambda: DefineForm.make(),
        u'lambda':       lambda: LambdaForm.make(),
        u'let':          lambda: LetForm.make(),
        u'let*':         lambda: LetStarForm.make(),
        u'letrec':       lambda: LetrecForm.make(),
        u'set!':         lambda: SetForm.make(),
        u'set-car!':     lambda: SetCarForm.make(),
        u'set-cdr!':     lambda: SetCdrForm.make(),
        u'if':           lambda: IfForm.make(),
        u'cond':         lambda: CondForm.make(),
        u'case':         lambda: CaseForm.make(),
        u'and':          lambda: AndForm.make(),
        u'or':           lambda: OrForm.make(),
        
    }

    @classmethod
    def make(cls):
        if not cls.object:
            cls.object = cls()
        return cls.object
            
    def eval(self, frame):
        raise ExpressionError(self, 'Special syntax should not be evaluated directly')
    
    def apply(self, operands, callingForm, frame):
        pass

    def isSpecialSyntax(self):
        return True
        
    def __str__(self):
        return '#<SpecialSyntax ' + self.name + '>'
    
class QuoteForm(SpecialSyntax):
    name = 'quote'
    def apply(self, operands, callingForm, frame):
        if len(operands) != 1:
            raise ExpressionError(self, '"quote" requires 1 argument, ' + str(len(operands)) + ' given.')
        res = copy(operands[0])
        #res.meta = callingForm.meta
        return res


class DefineForm(SpecialSyntax):
    name = 'define'
    def apply(self, operands, callingForm, frame):
        #if not self.topLevel: # and not self.inBody:
        #    raise ExpressionError(self, '"define" only allowed at the top level or in a body of a procedure')
        if len(operands) < 2:
            raise ExpressionError(self, '"define" requires at least 2 arguments, ' + str(len(operands)) + ' given.')
        firstArg = operands[0]
        if not firstArg.isSymbol() and not firstArg.isPair():
            raise ExpressionError(self, '"define": Invalid type of the first argument')
        if firstArg.isSymbol():
            frame.addSymbol(firstArg, operands[1].eval(frame))
            return Nil.make()
        if firstArg.isPair():
            if not firstArg.car.isSymbol():
                raise ExpressionError(firstArg, 'Invalid procedure name in "define"')
            procName = firstArg.car
            formals = firstArg.cdr.toList()
            body = operands[1:]
            procedure = CompoundProcedure.make(formals, body, frame, callingForm.meta)
            frame.addSymbol(procName, procedure)
            return Nil.make()

class CondForm(SpecialSyntax):
    name = 'cond'
    def apply(self, operands, callingForm, frame):
        if len(operands) < 2 or len(operands) > 3:
            raise ExpressionError(self, '"if" requires at least 2 and at most 3 arguments, ' + str(len(operands)) + ' given.')
        firstArg = operands[0].eval(frame)
        if not firstArg.isBoolean() or firstArg.value: #True
            res = operands[1].eval(frame)
        elif len(operands) == 3:
            res = operands[2].eval(frame)
        else:
            res = Nil.make()
        #res.meta = callingForm.meta
        return res

class IfForm(SpecialSyntax):
    name = 'if'
    def apply(self, operands, callingForm, frame):
        if len(operands) < 2 or len(operands) > 3:
            raise ExpressionError(self, '"if" requires at least 2 and at most 3 arguments, ' + str(len(operands)) + ' given.')
        firstArg = operands[0].eval(frame)
        if not firstArg.isBoolean() or firstArg.value: #True
            res = operands[1].eval(frame)
        elif len(operands) == 3:
            res = operands[2].eval(frame)
        else:
            res = Nil.make()
        #res.meta = callingForm.meta
        return res

class AndForm(SpecialSyntax):
    name = 'and'

class OrForm(SpecialSyntax):
    name = 'or'

class Procedure(SExpression):
    def apply(self, operands, callingForm):
        pass
        
    def isProcedure(self):
        return True

    def eval(self, frame):
        raise ExpressionError(self, 'Procedure should not be evaluated directly')
    
class PrimitiveProcedure(Procedure):
    name = 'primitive-procedure'
    object = None

    primitiveFunctions = {
        u'eq?':          lambda: EqProcedure.make(),
        u'eqv?':         lambda: EqvProcedure.make(),
        u'equal?':       lambda: EqualProcedure.make(),
        u'=':            lambda: NumEqProcedure.make(),
        u'<':            lambda: NumLTProcedure.make(),
        u'<=':           lambda: NumLTEProcedure.make(),
        u'>':            lambda: NumGTProcedure.make(),
        u'>=':           lambda: NumGTEProcedure.make(),
        u'cons':         lambda: ConsProcedure.make(),
        u'car':          lambda: CarProcedure.make(),
        u'cdr':          lambda: CdrProcedure.make(),
        u'not':          lambda: NotProcedure.make(),
        u'+':            lambda: SumProcedure.make(),
        u'-':            lambda: SubtractProcedure.make(),
        u'*':            lambda: MultiplyProcedure.make(),
        u'/':            lambda: DivideProcedure.make(),
        u'quotient':     lambda: QuotientProcedure.make(),
        u'modulo':       lambda: ModuloProcedure.make(),
        u'remainder':    lambda: RemainderProcedure.make(),
        u'write-char':   lambda: WriteCharProcedure.make(),
        u'display':      lambda: DisplayProcedure.make(),
    }

    @classmethod
    def make(cls):
        if not cls.object:
            cls.object = cls()
        return cls.object
    
    def __str__(self):
        return '#<PrimitiveProcedure \'' + self.name + '\'>'
    
class ConsProcedure(PrimitiveProcedure):
    name = 'cons'
    def apply(self, operands, callingForm):
        if len(operands) != 2:
            raise ExpressionError(callingForm, '"cons" requires 2 arguments, provided ' + str(len(operands)) + '.')
        expr1 = operands[0]
        expr2 = operands[1]
        res = Pair.make(expr1, expr2)
        #res.meta = callingForm.meta
        return res

class CarProcedure(PrimitiveProcedure):
    name = 'car'
    def apply(self, operands, callingForm):
        if len(operands) != 1:
            raise ExpressionError(callingForm, '"car" requires 1 argument, provided ' + str(len(operands)) + '.')
        arg = operands[0]
        if not arg.isPair():
            raise ExpressionError(callingForm, '"car" argument must be a pair.')
        res = arg.car
        #res.meta = callingForm.meta
        return res

class CdrProcedure(PrimitiveProcedure):
    name = 'cdr'
    def apply(self, operands, callingForm):
        if len(operands) != 1:
            raise ExpressionError(callingForm, '"cdr" requires 1 argument, provided ' + str(len(operands)) + '.')
        arg = operands[0]
        if not arg.isPair():
            raise ExpressionError(callingForm, '"cdr" argument must be a pair.')
        res = arg.cdr
        #res.meta = callingForm.meta
        return res
            
class NotProcedure(PrimitiveProcedure):
    name = 'not'
    def apply(self, operands, callingForm):
        if len(operands) != 1:
            raise ExpressionError(callingForm, '"not" requires 1 argument, provided ' + str(len(operands)) + '.')
        value = False
        if operands[0].isBoolean():
            value = not(operands[0].value)
        res = Boolean.make(value)
        #res.meta = callingForm.meta
        return res

class EqProcedure(PrimitiveProcedure):
    name = 'eq?'
    def apply(self, operands, callingForm):
        if len(operands) < 2:
            raise ExpressionError(callingForm, '"eq?" requires at least 2 arguments, provided ' + str(len(operands)) + '.')
        value = True
        first = operands[0]
        for n in operands[1:]:
            if not n is first:
                value = False
        res = Boolean.make(value)
        #res.meta = callingForm.meta
        return res
        
class EqvProcedure(PrimitiveProcedure):
    name = 'eqv?'
    def apply(self, operands, callingForm):
        if len(operands) < 2:
            raise ExpressionError(callingForm, '"eqv?" requires at least 2 arguments, provided ' + str(len(operands)) + '.')
        value = True
        first = operands[0]
        for n in operands[1:]:
            if n != first:
                value = False
        res = Boolean.make(value)
        #res.meta = callingForm.meta
        return res
        
class EqualProcedure(PrimitiveProcedure):
    name = 'equal?'
    def apply(self, operands, callingForm):
        if len(operands) < 2:
            raise ExpressionError(callingForm, '"equal?" requires at least 2 arguments, provided ' + str(len(operands)) + '.')
        value = True
        first = operands[0]
        for n in operands[1:]:
            if n != first:
                value = False
        res = Boolean.make(value)
        #res.meta = callingForm.meta
        return res
        
class NumEqProcedure(PrimitiveProcedure):
    name = '='
    def apply(self, operands, callingForm):
        if len(operands) < 2:
            raise ExpressionError(callingForm, '"=" requires at least 2 arguments, provided ' + str(len(operands)) + '.')
        value = True
        first = operands[0]
        if not first.isNumber():
            raise ExpressionError(first, '"' + str(first) + '" is not a Number')
        for n in operands[1:]:
            if not n.isNumber():
                raise ExpressionError(n, '"' + str(n) + '" is not a Number')
            if n.value != first.value:
                value = False
        res = Boolean.make(value)
        #res.meta = callingForm.meta
        return res
        
class NumLTProcedure(PrimitiveProcedure):
    name = '<'
    def apply(self, operands, callingForm):
        if len(operands) < 2:
            raise ExpressionError(callingForm, '"<" requires at least 2 arguments, provided ' + str(len(operands)) + '.')
        value = True
        previous = operands[0]
        if not previous.isNumber():
            raise ExpressionError(previous, '"' + str(previous) + '" is not a Number')
        for n in operands[1:]:
            if not n.isNumber():
                raise ExpressionError(n, '"' + str(n) + '" is not a Number')
            if n.value <= previous.value:
                value = False
            previous = n
        res = Boolean.make(value)
        #res.meta = callingForm.meta
        return res
        
class NumLTEProcedure(PrimitiveProcedure):
    name = '<='
    def apply(self, operands, callingForm):
        if len(operands) < 2:
            raise ExpressionError(callingForm, '"<=" requires at least 2 arguments, provided ' + str(len(operands)) + '.')
        value = True
        previous = operands[0]
        if not previous.isNumber():
            raise ExpressionError(previous, '"' + str(previous) + '" is not a Number')
        for n in operands[1:]:
            if not n.isNumber():
                raise ExpressionError(n, '"' + str(n) + '" is not a Number')
            if n.value < previous.value:
                value = False
            previous = n
        res = Boolean.make(value)
        #res.meta = callingForm.meta
        return res
        
class NumGTProcedure(PrimitiveProcedure):
    name = '>'
    def apply(self, operands, callingForm):
        if len(operands) < 2:
            raise ExpressionError(callingForm, '">" requires at least 2 arguments, provided ' + str(len(operands)) + '.')
        value = True
        previous = operands[0]
        if not previous.isNumber():
            raise ExpressionError(previous, '"' + str(previous) + '" is not a Number')
        for n in operands[1:]:
            if not n.isNumber():
                raise ExpressionError(n, '"' + str(n) + '" is not a Number')
            if n.value >= previous.value:
                value = False
            previous = n
        res = Boolean.make(value)
        #res.meta = callingForm.meta
        return res
        
class NumGTEProcedure(PrimitiveProcedure):
    name = '>='
    def apply(self, operands, callingForm):
        if len(operands) < 2:
            raise ExpressionError(callingForm, '">=" requires at least 2 arguments, provided ' + str(len(operands)) + '.')
        value = True
        previous = operands[0]
        if not previous.isNumber():
            raise ExpressionError(previous, '"' + str(previous) + '" is not a Number')
        for n in operands[1:]:
            if not n.isNumber():
                raise ExpressionError(n, '"' + str(n) + '" is not a Number')
            if n.value > previous.value:
                value = False
            previous = n
        res = Boolean.make(value)
        #res.meta = callingForm.meta
        return res
        
class SumProcedure(PrimitiveProcedure):
    name = '+'
    def apply(self, operands, callingForm):
        value = 0
        for n in operands:
            if not n.isNumber():
                raise ExpressionError(n, '"' + str(n) + '" is not a Number')
            value += n.value
        res = Number.make(value)
        #res.meta = callingForm.meta
        return res
        
class SubtractProcedure(PrimitiveProcedure):
    name = '-'
    def apply(self, operands, callingForm):
        if len(operands) < 1:
            raise ExpressionError(callingForm, '"-" requires at least 1 argument, provided ' + str(len(operands)) + '.')
        if not operands[0].isNumber():
            raise ExpressionError(operands[0], '"' + str(operands[0]) + '" is not a Number')
        if len(operands) == 1:
            res = Number.make(-operands[0].value)
        else:
            value = operands[0].value
            for n in operands[1:]:
                if not n.isNumber():
                    raise ExpressionError(n, '"' + str(n) + '" is not a Number')
                value -= n.value
            res = Number.make(value)
        #res.meta = callingForm.meta
        return res

class MultiplyProcedure(PrimitiveProcedure):
    name = '*'
    def apply(self, operands, callingForm):
        value = 1
        for n in operands:
            if not n.isNumber():
                raise ExpressionError(n, '"' + str(n) + '" is not a Number')
            value *= n.value
        res = Number.make(value)
        #res.meta = callingForm.meta
        return res
        
class DivideProcedure(PrimitiveProcedure):
    name = '/'
    def apply(self, operands, callingForm):
        if len(operands) < 1:
            raise ExpressionError(callingForm, '"/" requires at least 1 argument, provided ' + str(len(operands)) + '.')
        if not operands[0].isNumber():
            raise ExpressionError(operands[0], '"' + str(operands[0]) + '" is not a Number')
        if len(operands) == 1:
            res = Number.make(1/operands[0].value)
        else:
            value = operands[0].value
            for n in operands[1:]:
                if not n.isNumber():
                    raise ExpressionError(n, '"' + str(n) + '" is not a Number')
                value /= n.value
            res = Number.make(value)
        #res.meta = callingForm.meta
        return res

class ModuloProcedure(PrimitiveProcedure):
    name = 'modulo'
    def apply(self, operands, callingForm):
        if len(operands) != 2:
            raise ExpressionError(callingForm, '"modulo" requires 2 arguments, provided ' + str(len(operands)) + '.')
        if not operands[0].isNumber():
            raise ExpressionError(operands[0], '"' + str(operands[0]) + '" is not a Number')
        if not operands[1].isNumber():
            raise ExpressionError(operands[1], '"' + str(operands[1]) + '" is not a Number')
        res = Number.make(operands[0].value % operands[1].value)
        #res.meta = callingForm.meta
        return res

class WriteCharProcedure(PrimitiveProcedure):
    name = 'write-char'
    def apply(self, operands, callingForm):
        if not (1 <= len(operands) <= 2):
            raise ExpressionError(callingForm, '"write-char" requires 1 or 2 arguments, provided ' + str(len(operands)) + '.')
        if not operands[0].isChar():
            raise ExpressionError(operands[0], '"' + unicode(operands[0]) + '" is not a Character')
        sys.stdout.write(str(operands[0]))
        return Nil.make()

class DisplayProcedure(PrimitiveProcedure):
    name = 'display'
    def apply(self, operands, callingForm):
        if not (1 <= len(operands) <= 2):
            raise ExpressionError(callingForm, '"display" requires 1 or 2 arguments, provided ' + str(len(operands)) + '.')
        sys.stdout.write(unicode(operands[0]))
        return Nil.make()

class CompoundProcedure(Procedure):
    @classmethod
    def make(cls, formals, body, frame, meta):
        self = cls()
        self.formals = formals
        self.body = Body.make(body, meta)
        self.frame = frame
        self.checkFormals()
        self.meta = meta
        return self

    def checkFormals(self):
        for f in self.formals:
            if not f.isSymbol():
                raise ExpressionError(f, 'Invalid procedure argument name')
        
    def bind(self, arguments, callingForm):
        if len(self.formals) != len(arguments):
            raise ExpressionError(callingForm, 'Wrong number of operands, required ' + str(len(self.formals)) + ', provided ' + str(len(arguments)) + '.')
        newFrame = Frame(self.frame)
        for t in zip(self.formals, arguments):
            newFrame.addSymbol(t[0], t[1])
        return newFrame
        
    def apply(self, operands, callingForm):
        #if len(operands) < 1:
        #    raise ExpressionError(self, '"apply" requires at least 1 operand, ' + str(len(operands)-1) + ' given.')
        #newOperands = operands[0:-1] + operands[-1]
        #if operator.isProcedure():
        newFrame = self.bind(operands, callingForm)
        return self.body.eval(newFrame)
        #else:
        #    raise ExpressionError(self, '"apply": first operand is not a procedure.' + str(operator))
    
    def __str__(self):
        formals = ''
        if len(self.formals) > 0:
            formals = self.formals[0].name
            for f in self.formals[1:]:
                formals.append(' ' + f.name)
        return '#<procedure ('+ formals + ')>'
        
class Body():
    @classmethod
    def make(cls, body, meta):
        self = cls()
        self.body = body
        self.meta = meta
        self.definesAllowed = True
        return self

    def eval(self, frame):
        last = Nil.make()
        for expr in self.body:
            last = expr.eval(frame)
        return last

def readLine(line, frame):
    for expression in expressions:
        #print expression
        try: 
            result = expression.eval(frame)
            if not isinstance(result, Nil):
                yield unicode(result)
        except ExpressionError as e:
            yield unicode(e)
        except RuntimeError:
            yield u'Maximum stack depth exceeded'
        
def eval(expressions, frame):
    for expression in expressions:
        #print expression
        try: 
            result = expression.eval(frame)
            if not isinstance(result, Nil):
                yield unicode(result)
        except ExpressionError as e:
            yield unicode(e)
        except RuntimeError:
            yield u'Maximum stack depth exceeded'
