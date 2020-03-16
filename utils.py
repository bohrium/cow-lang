''' author: samtenka
    change: 2020-03-01
    create: 2019-06-12
    descrp: Helpers for ANSI screen coloration, resource profiling, math, and
            maybe types.
    to use: Import:
                from utils import CC, pre                           # ansi
                from utils import secs_endured, megs_alloced        # profiling
                from utils import reseed, geometric, bernoulli      # math
                from utils import InternalError, internal_assert    # maybe
            Or, run as is to see a pretty rainbow:
                python utils.py
'''

import functools
import time
import sys
import random
import numpy as np

#=============================================================================#
#=====  0. ANSI CONTROL FOR RICH OUTPUT TEXT =================================#
#=============================================================================#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~  0.0 Define Text Modifier  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

class Colorizer(object):
    '''
        Text modifier class, used as in 
            print(CC+'@R i am red @D ')
        where CC is an instance of this class. 
    '''
    def __init__(self):

        #-------------  0.0.0 ANSI command abbreviations  --------------------#

        self.ANSI_by_name = {
            '@^ ': '\033[1A',                 # motion: up

            '@K ': '\033[38;2;000;000;000m',  # color: black
            '@A ': '\033[38;2;128;128;128m',  # color: gray  
            '@W ': '\033[38;2;255;255;255m',  # color: white

            '@R ': '\033[38;2;240;032;032m',  # color: red
            '@O ': '\033[38;2;224;128;000m',  # color: orange 
            '@Y ': '\033[38;2;255;224;000m',  # color: yellow

            '@G ': '\033[38;2;064;224;000m',  # color: green
            '@F ': '\033[38;2;000;224;000m',  # color: forest
            '@C ': '\033[38;2;000;192;192m',  # color: cyan

            '@B ': '\033[38;2;096;064;255m',  # color: blue
            '@P ': '\033[38;2;192;000;192m',  # color: purple  
            '@N ': '\033[38;2;128;032;000m',  # color: brown
        }

        #-------------  0.0.1 default color is cyan  -------------------------#
        self.ANSI_by_name['@D '] = self.ANSI_by_name['@C ']

        self.text = ''

    #-----------------  0.0.2 define application to strings  -----------------#

    def __add__(self, rhs):
        ''' Transition method of type Colorizer -> String -> Colorizer '''
        assert type(rhs) == type(''), 'expected types (Colorizer + string)'
        for name, ansi in self.ANSI_by_name.items():
            rhs = rhs.replace(name, ansi)
        self.text += rhs
        return self

    def __str__(self):
        ''' Emission method of type Colorizer -> String '''
        rtrn = self.text 
        self.text = ''
        return rtrn

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~  0.1 Global Initializations  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

CC = Colorizer()
print(CC+'@D @^ ')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~  0.2 Styles for Special Message Types  ~~~~~~~~~~~~~~~~~~~~~~~~#

def pre(condition, message): 
    ''' assert precondition; if fail, complain in red '''
    assert condition, CC+'@R '+message+'@D '

#=============================================================================#
#=====  1. RESOURCE PROFILING  ===============================================#
#=============================================================================#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~  1.0 Memory Profiling  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#---------------------  1.0.0 check memory profiler  -------------------------#

try:
    import memory_profiler
except ImportError:
    print(CC + '@O failed attempt to import `memory_profiler` @D ')

#---------------------  1.0.1 set memory profiler  ---------------------------#

megs_alloced = None if 'memory_profile' not in sys.modules else lambda: (
    memory_profiler.memory_usage(
        -1, interval=0.001, timeout=0.0011
    )[0]
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~  1.1 Time Profiling  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

start_time = time.time()
secs_endured = lambda: (time.time()-start_time) 

#=============================================================================#
#=====  2. MATH and RANDOMNESS  ==============================================#
#=============================================================================#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~  2.0 Random Seed and Generators  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

def reseed(s):
    random.seed(s)
    np.random.seed(s)

def bernoulli(p):
    return np.random.binomial(1, p)

def uniform(n):
    if type(n) in [int, np.int64]:
        return np.random.randint(n)
    elif type(n)==float:
        pass
    else:
        return random.choice(n)

def geometric(scale):
    ''' Support on the (non-negative) naturals, with mean specified by `scale` 
    '''
    return np.random.geometric(1.0/(1.0 + scale)) - 1

#=============================================================================#
#=====  3. SIMULATE MAYBE TYPES VIA EXCEPTIONS  ==============================#
#=============================================================================#

class InternalError(Exception):
    def __init__(self, msg):
        self.msg = msg

def internal_assert(condition, message):
    if not condition:
        raise InternalError(message)

#=============================================================================#
#=====  4. ILLUSTRATE UTILITIES  =============================================#
#=============================================================================#

if __name__=='__main__':

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~  4.0 Display a Rainbow  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

    print(CC + '@D moo')

    print(CC + '@W moo')
    print(CC + '@A moo')
    print(CC + '@K moo')

    print(CC + '@R moo')
    print(CC + '@O moo')
    print(CC + '@Y moo')
    print(CC + '@G moo')
    print(CC + '@F moo')
    print(CC + '@C moo')
    print(CC + '@B moo')
    print(CC + '@P moo')
    print(CC + '@N moo')
    print(CC + '@R moo')

    print(CC + 'hi @M moo' + 'cow @C \n')
