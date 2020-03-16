''' author: samtenka
    change: 2020-03-07
    create: 2019-02-01
    descrp:
    to use:
'''

from utils import CC
from preprocess import preprocess

class ParseTree:
    ''' 
    '''
    def __init__(self, label='ROOT', kids=[], ignore=False, unroll=False):
        self.label = label
        self.kids = kids
        self.ignore = ignore 
        self.unroll = unroll 

    def get_source(self):
        return ''.join(
            k if (type(k) == str) else k.get_source() for k in self.kids
        )

    def width(self): 
        return len([k for k in self.kids if (type(k) == str) or not k.ignore])

    def relevant_kids(self):
        ''' no literal strings and no ignoreds '''
        return (k for k in self.kids if (type(k) != str) and not k.ignore)

    def display(self, depth=0, delim='|  ', collapse=False):
        '''
            `collapse` indicates whether to collapse unary vines down to
            leafmost node of vine 
        '''
        if self.ignore:
            return
        elif not self.unroll and (not collapse or self.width()!=1):
            source = self.get_source()
            if len(source)>64+3:
                source = '{}@D ...@R {}'.format(source[:32], source[-32:])
            print(CC+'{}@B {}@D [@R {}@D ]'.format(
                delim*depth, self.label, source
            ))
            depth += 1
        for k in self.kids:
            if type(k)==type(''):
                pass
                #print(CC+'{}@R {}@D '.format(delim*depth, k))
            else:
                k.display(depth, delim, collapse)

class Text:
    def __init__(self, string):
        self.string = string
        self.index = 0
    def __get_item__(self, offset):  
        return self.string[self.index + offset] 
    def peek(self):
        return self.string[self.index]
    def match_until(self, delim=' '):
        new_index = self.string.find(delim, self.index)
        if new_index==-1: new_index = len(self.string)
        word = self.string[self.index : new_index]
        self.index = new_index
        return word
    def match(self, word): 
        if self.string[self.index : self.index+len(word)] == word:
            self.index += len(word)
            return True
        return False
    def is_at_end(self):
        return self.index == len(self.string)

