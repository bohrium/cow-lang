''' author: samtenka
    change: 2020-03-13
    create: 2020-03-13
    descrp:
    to use:
'''

from utils import CC, pre

#=============================================================================#
#=====  0. IMPLEMENTATION OF RECORD STRUCTURE FOR CL TYPES  ==================#
#=============================================================================#

def indent(text, delim='  '):
    depth = 0
    indented = []
    for line in text.split('\n'):
        depth -= line.count('}') 
        indented.append(delim*depth + line.strip())
        depth += line.count('{')
    return '\n'.join(indented)

class CLType:

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~  0.0 Constructors  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

    def __init__(self, kind, **kwargs):
        pre(kind in ['base', 'prod', 'enum'],
            'unknown kind `{}`'.format(kind)
        ) 
        self.kind = kind
        if self.kind=='base': self.name = kwargs['name'] 
        elif self.kind=='prod': self.children = kwargs
        elif self.kind=='enum': self.children = kwargs

    def __eq__(self, rhs):
        return repr(self)==repr(rhs)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~  0.1 Rendering  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

    def to_C(self): 
        if self.kind=='base': return self.name
        elif self.kind=='prod':
            return 'struct {{\n{}\n}}'.format(
                '\n'.join(
                    '{} {};'.format(tp.to_C().strip(), nm)
                    for nm, tp in self.children.items()
                ),
            )
        elif self.kind=='enum':
            return 'struct {{\n{}\n{}\n}}'.format(
                'union {{\n{}\n}} data;'.format(
                    '\n'.join(
                        '{} {};'.format(tp.to_C().strip(), nm)
                        for nm, tp in self.children.items()
                    ),
                ),
                'char tag;'
            )

    def __repr__(self):
        if self.kind=='base':
            return self.name
        elif self.kind=='prod':
            return ' '.join(
                ('{}' if tp.kind=='base' else '({})').format(repr(tp))
                for nm, tp in self.children.items()
            )
        elif self.kind=='enum':
            return ' | '.join(
                ('{}' if tp.kind in ('base', 'prod') else '({})').format(repr(tp))
                for nm, tp in self.children.items()
            )

    def __str__(self):   
        return repr(self)

    def __hash__(self):
        return hash(repr(self))

tInt = CLType(kind='base', name='int')
tChar = CLType(kind='base', name='char')

tICp = CLType(kind='prod', hey=tInt, bay=tChar)
tICs = CLType(kind='enum', hey=tInt, bay=tChar)
tICIC = CLType(kind='prod', hey=tICp, bay=tICs)
print(tICIC)
print(indent(tICIC.to_C()))
#print(CLType(kind='enum', hey=tICp, bay=tICs).to_C())
