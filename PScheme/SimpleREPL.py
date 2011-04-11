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

import PScheme
import os
import sys

# Note: readline library is released under a GPL license.
# Any derived work has to either comply with terms of the GPL license or must remove
# the following import statement.
try:
    import readline
except ImportError:
    pass

def readInput():
    try:
        while True:
            if sys.version_info[0] == 2:
                yield str(raw_input('> '))
            else:
                yield str(input('> '))
    except (EOFError, KeyboardInterrupt):
        print('')
        return
        
def flush(generator):
    for element in generator: pass
            
def output(results):
    for result in results:
        if not isinstance(result, PScheme.Nil):
            print(result)
            
def repl():
    frame = PScheme.Frame()
    fname = os.path.join(os.path.dirname(__file__), 'boot.scm')
    tokenizer = PScheme.Tokenizer(fname)
    with open(fname, 'r') as f:
        tokens = tokenizer.tokenizeLines(f)
        expressions = frame.parseTokens(tokens)
        results = PScheme.eval(expressions, frame) #result generator
        flush(results) # check results to force delayed execution
            
        
    tokenizer = PScheme.Tokenizer('console')
    lines = readInput() #line generator
    tokens = tokenizer.tokenizeLines(lines) #token generator
    expressions = frame.parseTokens(tokens) #expression generator
    results = PScheme.eval(expressions, frame) #result generator
    output(results) #print out results
        
if __name__ == "__main__":
    repl()
    
