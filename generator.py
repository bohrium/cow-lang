''' author: samtenka
    change: 2020-03-15
    create: 2019-05-18
    descrp: translate cow-lang to C
    to use:
'''

from parser import ParseTree, Text, ParserGenerator
from preprocess import preprocess
from utils import CC, pre

# TODO: compile with --short-enums 

def indent(text, delim='  '):
    depth = 0
    indented = []
    for line in text.split('\n'):
        diff = line.count('{') - line.count('}')
        depth += min(0, diff)
        indented.append(delim*depth + line.strip())
        depth += diff - min(0, diff)
    return '\n'.join(indented)


with open('templates/main_template.c') as f:
    main_template = f.read()

class CodeGenerator(object):
    def __init__(self, parse_tree):
        self.definitions = {
            'main': {
                'kind': 'func',
                'argtps_by_nm': [],
                'outtype': 'Unit',
                'lines': [],
                'cname': '_main',
            },
        }

        self.analyze_block(parse_tree, ctxt='main')
        print(CC+'@O successful analysis!@D ')

    def render_type_defns(self):
        ccode = '\n\n'.join(
            '\n'.join(data['lines'])
            for ctxt, data in self.definitions.items()
            if data['kind'] == 'type'
        )
        return ccode

    def render_func_decls(self):
        ccode = '\n'.join(
            '{} {}({});'.format(
                data['outtype'],
                data['cname'],
                ', '.join(
                    '{} _{}'.format(typename, ident)
                    for (ident, typename) in data['argtps_by_nm']
                )
            )
            for ctxt, data in self.definitions.items()
            if data['kind'] == 'func'
        )
        return ccode

    def render_func_impls(self):
        ccode = '\n\n'.join(
            '{} {}({}) {{\n{}\n}}'.format(
                data['outtype'],
                data['cname'],
                ', '.join(
                    '{} _{}'.format(typename, ident)
                    for (ident, typename) in data['argtps_by_nm']
                ),
                '\n'.join(data['lines'])
            )
            for ctxt, data in self.definitions.items()
            if data['kind'] == 'func'
        )
        return ccode

    def total_print(self):
        ccode = indent(main_template 
            .replace('/*TYPE_DEFNS*/',          self.render_type_defns())
            .replace('/*FUNCTION_DECLS*/', self.render_func_decls())
            .replace('/*FUNCTION_IMPLS*/', self.render_func_impls())
        )
        return ccode

    def write_code(self, string, ctxt, newline=True):
        for line in string.split('\n'):
            if newline or not self.definitions[ctxt]['lines']:
                self.definitions[ctxt]['lines'].append(line)
            else:
                self.definitions[ctxt]['lines'][-1] += line

    def process_declaration(self, tree):
        pre(tree.label == 'DECLARATION', 'unexpected tree label')
        judgement, = tree.relevant_kids()
        ident, typename = judgement.relevant_kids()
        return (x.get_source() for x in (ident, typename))

    def process_assignment(self, tree):
        pre(tree.label == 'ASSIGNMENT', 'unexpected tree label')
        ident, expr = tree.relevant_kids()
        return ident.get_source(), expr

    def process_guarded_sequence(self, tree):
        body = tree
        bodycontents = list(body.relevant_kids())
        cond_cons_pairs = []
        while True:
            cond_cons_pairs.append(tuple(bodycontents[0].relevant_kids()))
            if 2==len(bodycontents):
                bodycontents = list(bodycontents[1].relevant_kids())
            else:
                break
        return cond_cons_pairs

    def process_function(self, tree):
        pre(tree.label == 'FUNCTION', 'unexpected tree label')
        header, body = tree.relevant_kids()
        ident, functype = header.relevant_kids() 
        arglist, outtype = functype.relevant_kids() 
        argtps_by_nm = []
        arglist = list(arglist.relevant_kids())
        while arglist:
            judgement = arglist[0]
            argident, argtype = judgement.relevant_kids()
            argtps_by_nm.append((argident.get_source(), argtype.get_source()))
            arglist = arglist[1:]
            if arglist:
                arglist = list(arglist[0].relevant_kids())
        return (ident.get_source(), argtps_by_nm, outtype.get_source(), body)

    def process_typebody(self, judgements, ctxt):
        branch_names = []

        js = [judgements]

        while js:
            js = list(js[0].relevant_kids())
            ident, tp = js[0].relevant_kids() 
            branch_names.append(ident.get_source())
            self.process_type(tp, ctxt)
            self.write_code(' {};'.format(ident.get_source()), ctxt, newline=False)
            js = js[1:]

        return branch_names

    def process_type(self, tree, ctxt):
        pre(tree.label == 'TYPE', 'unexpected tree label')

        while tree.label=='TYPE':
            tree, = tree.relevant_kids()

        if tree.label=='BASETYPE':
            self.write_code(tree.get_source(), ctxt)
            return

        kind, judgements = tree.relevant_kids()
        if kind.get_source()=='struct':
            self.write_code('struct {', ctxt)
            self.process_typebody(judgements, ctxt)
            self.write_code('}', ctxt)
        elif kind.get_source()=='enum':
            self.write_code('struct {', ctxt)
            self.write_code('union {', ctxt)
            branch_names = self.process_typebody(judgements, ctxt)
            self.write_code('} data; ', ctxt)
            self.write_code(
                'enum {{ {} }} tag;'.format(', '.join(branch_names)), ctxt
            )
            self.write_code('}', ctxt)
        else:
            pre(False, '')
        
    def process_typedefn(self, tree, ctxt):
        pre(tree.label == 'TYPEDEFN', 'unexpected tree label')
        kind, nm, judgements = tree.relevant_kids()
        ctxt = nm.get_source() 

        pre(ctxt not in self.definitions,
            'type {} already declared!'.format(ctxt)
        )
        self.definitions[ctxt] = {
            'kind': 'type',
            'lines': [],
        }

        if kind.get_source()=='struct':
            self.write_code('struct {} {{'.format(nm.get_source()), ctxt)
            self.process_typebody(judgements, ctxt)
        elif kind.get_source()=='enum':
            self.write_code('struct {} {{'.format(nm.get_source()), ctxt)
            self.write_code('union {', ctxt)
            branch_names = self.process_typebody(judgements, ctxt)
            self.write_code('} data; ', ctxt)
            self.write_code(
                'enum {{ {} }} tag;'.format(', '.join(branch_names)), ctxt
            )
        else:
            pre(False, '')
 
        self.write_code('};', ctxt)




    def translate_expr(self, tree, ctxt, tps_by_nm={}):
        expansions = {
            'or':' || ', 'and':' && ', 'not':'!',
            '+':' + '
        }
        ccode = ''.join(
            (
                (lambda s: (
                    expansions[s] if s in expansions else s
                ))(k.strip())
            )
            if type(k) == str else
            (
                (lambda ident: ( 
                    '_{}'.format(self.definitions[ident]['cname'])
                    if ident in self.definitions else 
                    '_{}'.format(ident)
                    if ident in tps_by_nm else 
                    pre(False, '`{}` not declared! (ctxt {})'.format(ident, ctxt))
                ))(k.get_source())
            )
            if k.label == 'LOWER_IDENTIFIER' else
            (
                self.translate_expr(k, ctxt, tps_by_nm)
            )
            for k in tree.kids
        )
        #if tree.label == 'EQ_EXPR':
        #    ccode = '({})'.format(ccode)
        return ccode

    def analyze_block(self, tree, ctxt, tps_by_nm={}):
        ''' assume no function definitions within
        '''
        # copy:
        tps_by_nm = tps_by_nm.copy()# {k:v for k,v in tps_by_nm.items()}

        node_stack = list(tree.relevant_kids())
        while node_stack:
            k, node_stack = node_stack[0], node_stack[1:]

            if k.label == 'SKIP':
                pass
            elif k.label == 'ABORT':
                self.write_code('ABORT;', ctxt)
            elif k.label == 'DECLARATION':
                ident, typename = self.process_declaration(k)
                pre(ident not in tps_by_nm,
                    'variable {} already declared as {}!'.format(
                        ident, tps_by_nm[ident] if ident in tps_by_nm else None
                    ) 
                )
                tps_by_nm[ident] = typename
                self.write_code('{} _{};'.format(typename, ident), ctxt)
            elif k.label == 'ASSIGNMENT':
                ident, expr = self.process_assignment(k) 
                if ident=='return':
                    self.write_code('return {};'.format(
                        self.translate_expr(expr, ctxt, tps_by_nm)
                    ), ctxt)
                else:
                    pre(ident in tps_by_nm,
                        '{} not declared! (ctxt {})'.format(ident, ctxt)
                    )
                    self.write_code(
                        '_{} = {};'.format(
                            ident, (self.translate_expr(expr, ctxt, tps_by_nm))
                        ), ctxt
                    )
            elif k.label == 'MATCH': 
                print('HIII')
                expr, guardeds = k.relevant_kids()  
                cond_cons_pairs = self.process_guarded_sequence(guardeds)
                self.write_code('switch ({}.tag) {{'.format(
                    self.translate_expr(expr, ctxt, tps_by_nm)
                ), ctxt)
                for i, (cond, cons) in enumerate(cond_cons_pairs): 
                    self.write_code(
                        'case {} :'.format(cond.get_source()), ctxt
                    )
                    self.analyze_block(cons, ctxt, tps_by_nm=tps_by_nm)
                self.write_code('}', ctxt)
            elif k.label == 'IF': 
                guardeds, = k.relevant_kids()
                cond_cons_pairs = self.process_guarded_sequence(guardeds)
                pre(cond_cons_pairs,
                    'alternative constructs must have at least one branch'
                )
                for i, (cond, cons) in enumerate(cond_cons_pairs): 
                    self.write_code(
                        '{} ({}) {{'.format(
                            'if' if i==0 else '} else if',
                            self.translate_expr(cond, ctxt, tps_by_nm).strip()
                        ), ctxt
                    )
                    self.analyze_block(cons, ctxt, tps_by_nm=tps_by_nm)
                self.write_code('} else {', ctxt)
                self.write_code('ABORT;', ctxt)
                self.write_code('}', ctxt)
            elif k.label == 'DO': 
                cond_cons_pairs = self.process_guarded_sequence(k)
                pre(cond_cons_pairs,
                    'repetitive constructs must have at least one branch'
                )
                self.write_code('while (true) {', ctxt)
                for i, (cond, cons) in enumerate(cond_cons_pairs): 
                    self.write_code(
                        '{} ({}) {{'.format(
                            'if' if i==0 else '} else if',
                            self.translate_expr(cond, ctxt, tps_by_nm).strip()
                        ), ctxt
                    )
                    self.analyze_block(cons, ctxt, tps_by_nm=tps_by_nm)
                self.write_code('} else {', ctxt)
                self.write_code('break;', ctxt)
                self.write_code('}', ctxt)
                self.write_code('}', ctxt)
            elif k.label == 'TYPEDEFN':
                self.process_typedefn(k, ctxt)
            elif k.label == 'FUNCTION':
                ident, argtps_by_nm, outtype, body = self.process_function(k)
                new_ctxt = ident
                pre(new_ctxt not in self.definitions,
                    'function {} already declared!'.format(new_ctxt)
                )
                print(CC+'@R create ctxt {}...@D '.format(new_ctxt))
                self.definitions[ident] = {
                    'kind': 'func',
                    'argtps_by_nm': argtps_by_nm,
                    'outtype': outtype,
                    'lines': [],
                    'cname': '_' + ident 
                }
                self.analyze_block(
                    body,
                    tps_by_nm={k:v for k,v in argtps_by_nm},
                    ctxt=new_ctxt
                )
            elif k.label == 'PRINT': 
                ident, = k.relevant_kids()
                ident = ident.get_source()
                pre(ident in tps_by_nm,
                    '{} not declared! (print ctxt {})'.format(ident, ctxt)
                )
                typename = tps_by_nm[ident]
                if typename == 'Float':
                    self.write_code(
                        'printf("{} \\t %f\\n", _{});'.format(
                            ident, ident
                        ),
                    ctxt)
                elif typename == 'Int':
                    self.write_code(
                        'printf("{} \\t %d\\n", _{});'.format(
                            ident, ident
                        ),
                    ctxt)
                elif typename == 'Bool':
                    self.write_code(
                        'printf("{} \\t %s\\n", _{}?"true":"false");'.format(
                            ident, ident
                        ),
                    ctxt)
                else:
                    pre(False, 'unknown typename!')
            else:
                node_stack = list(k.relevant_kids()) + node_stack

   
if __name__ == '__main__':
    import sys
    if len(sys.argv) == 1:
        cow_filenm, c_filenm, verbose = 'test.cow', 'test.c', False 
    elif len(sys.argv) == 3:
        cow_filenm, c_filenm = sys.argv[1:3]
    else:
        assert len(sys.argv) in [1, 3], "expect 0 or 2 command line arguments"

    cow_parser = ParserGenerator('grammars/cow-lang-type.grammar').parsers['MAIN']
    with open(cow_filenm) as f:
        text = f.read()
    text = preprocess(text)
    tree = cow_parser(Text(text))
    tree.display()

    G = CodeGenerator(tree) 
    with open(c_filenm, 'w') as f:
        f.write(G.total_print())
    print(G.total_print())
