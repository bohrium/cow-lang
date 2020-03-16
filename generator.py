''' author: samtenka
    change: 2020-03-16
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
        self.defns = {
            #'main': {
            #    'kind': 'function',
            #    'arg_types_by_nm': {},
            #    'out_type': 'Unit',
            #    'lines': [],
            #    'cname': '_main',
            #},
        }

        self.var_count = 0

        self.ctxt_stack = []
        #self.ctxt_stack = ['main']
        self.analyze_block(parse_tree) 
        print(CC+'@O successful analysis!@D ')

    def fresh_var_nm(self):
        self.var_count += 1
        return 'x{}'.format(self.var_count-1);

    def curr_ctxt(self): return self.ctxt_stack[-1]
    def push_ctxt(self, ctxt): self.ctxt_stack.append(ctxt)
    def pop_ctxt(self): return self.ctxt_stack.pop()

    def make_defn(self, kind, **kwargs):
        ctxt = self.curr_ctxt() 
        pre(ctxt not in self.defns,
            '{} {} already declared!'.format(kind, ctxt)
        )
        val = {
            'root_type': lambda:{
                'kind': 'root_type',
                'alg': None,
                'child_fields': [],
                'child_types': [],
                'new_child_types': [],
                'lines': [],
            },
            'child_type': lambda:{
                'kind': 'child_type',
                'alg': None,
                'child_fields': [],
                'child_types': [],
                'new_child_types': [],
                'lines': [],
            },
            'function': lambda:{ 
                'kind': 'function',
                'arg_types_by_nm': kwargs['arg_types_by_nm'],
                'out_type': kwargs['out_type'],
                'lines': [],
                'cname': '_' + kwargs['ident'], 
            }
        }[kind]()
        self.defns[ctxt] = val
    def curr_defn(self):
        return self.defns[self.curr_ctxt()]

    def render_type_defn(self, type_nm):
        ccode = '{}\n{}'.format(
            '\n'.join(
                self.render_type_defn(child)
                for child in self.defns[type_nm]['new_child_types']
            ),
            '\n'.join(self.defns[type_nm]['lines'])
        )
        return ccode

    def render_type_defns(self):
        ccode = '\n\n'.join(
            self.render_type_defn(type_nm)
            for type_nm, data in self.defns.items()
            if data['kind']=='root_type'
        )
        return ccode

    def render_func_decls(self):
        ccode = '\n'.join(
            '{} {}({});'.format(
                data['out_type'],
                data['cname'],
                ', '.join(
                    '{} _{}'.format(typename, ident)
                    for ident, typename in data['arg_types_by_nm'].items()
                )
            )
            for data in self.defns.values()
            if data['kind'] == 'function'
        )
        return ccode

    def render_func_impls(self):
        ccode = '\n\n'.join(
            (lambda body:
                (
                    '{} {}({}) {{ {} }}'
                    if len(body)<40 and '\n' not in body else
                    '{} {}({})\n{{\n{}\n}}'
                )
                .format(
                    data['out_type'],
                    data['cname'],
                    ', '.join(
                        '{} _{}'.format(typename, ident)
                        for ident, typename in data['arg_types_by_nm'].items()
                    ),
                    body
                )
            )('\n'.join(ln for ln in data['lines']))
            for data in self.defns.values()
            if data['kind'] == 'function'
        )
        return ccode

    def total_print(self):
        pre('main' in self.defns, 'entry point `main` undefined!')
        ccode = indent(main_template 
            .replace('/*TYPE_DEFNS*/',     self.render_type_defns())
            .replace('/*FUNCTION_DECLS*/', self.render_func_decls())
            .replace('/*FUNCTION_IMPLS*/', self.render_func_impls())
        )
        return ccode

    def write_code(self, string, *args, augment_old_line=False):
        ctxt = self.ctxt_stack[-1]
        lines = self.defns[ctxt]['lines']
        string = string.format(*args) 
        for fresh_line in string.split('\n'):
            if augment_old_line and lines: lines[-1] += fresh_line
            else: lines.append(fresh_line)

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
        arglist, out_type = functype.relevant_kids() 
        arg_types_by_nm = {}
        arglist = list(arglist.relevant_kids())
        while arglist:
            judgement = arglist[0]
            argident, argtype = judgement.relevant_kids()
            arg_types_by_nm[argident.get_source()] = argtype.get_source()
            arglist = arglist[1:]
            if arglist:
                arglist = list(arglist[0].relevant_kids())
        return (ident.get_source(), arg_types_by_nm, out_type.get_source(), body)

    def process_type_body(self, alg, type_name, judgements):
        self.curr_defn()['alg'] = alg
        js = [judgements]

        i = 0
        while js:
            js = list(js[0].relevant_kids())
            ident, tp = js[0].relevant_kids() 
            ident = ident.get_source()
            self.curr_defn()['child_fields'].append(ident)
            child_type_name = self.process_type(tp, ident)

            self.push_ctxt(ident)
            if alg=='struct': 
                self.make_defn('function', ident=ident, out_type=child_type_name,
                    arg_types_by_nm={'prod': type_name}
                )
                self.write_code('return _prod._{};', ident)
            elif alg=='enum': 
                self.make_defn('function', ident=ident, out_type=type_name,
                    arg_types_by_nm={'comp': child_type_name}
                )
                self.write_code('{} val = {{ .data={{ ._{}=_comp }}, .tag={} }};', type_name, ident, i)
                self.write_code('return val;', ident, i)
            self.pop_ctxt()

            self.write_code(' _{};', ident, augment_old_line=True)
            js = js[1:]
            i += 1

    def process_composite_type(self, kind, type_name, judgements):
        self.write_code('typedef struct {} {{', type_name)

        if kind.get_source()=='struct':
            self.process_type_body('struct', type_name, judgements)
        elif kind.get_source()=='enum':
            self.write_code('union {{')
            self.process_type_body('enum', type_name, judgements)
            self.write_code('}} data; ')
            self.write_code('char tag;');
        else:
            pre(False, '')

        self.write_code('}} {};', type_name)

    def process_type(self, tree, name):
        pre(tree.label == 'TYPE', 'unexpected tree label')

        while tree.label=='TYPE':
            tree, = tree.relevant_kids()

        if tree.label=='BASETYPE':
            child_type_nm = tree.get_source()
            self.write_code(child_type_nm)
            self.curr_defn()['child_types'].append(child_type_nm)
        else:
            child_type_nm = '{}__{}'.format(self.curr_ctxt(), name) 
            self.curr_defn()['child_types'].append(child_type_nm)
            self.curr_defn()['new_child_types'].append(child_type_nm)
            self.write_code(child_type_nm)

            self.push_ctxt(child_type_nm)
            self.make_defn('child_type')
            kind, judgements = tree.relevant_kids()
            self.process_composite_type(kind, child_type_nm, judgements)
            self.pop_ctxt()

        return child_type_nm
        
    def process_type_defn(self, tree):
        pre(tree.label == 'TYPEDEFN', 'unexpected tree label')

        kind, nm, judgements = tree.relevant_kids()
        nm = nm.get_source()

        self.push_ctxt(nm)
        self.make_defn('root_type')
        self.process_composite_type(kind, nm, judgements)
        self.pop_ctxt()

    def translate_expr(self, tree, types_by_nm={}):
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
                    self.defns[ident]['cname']
                    if ident in self.defns else 
                    '_{}'.format(ident)
                    if ident in types_by_nm else 
                    pre(False, '`{}` not declared! (ctxt {})', ident, self.curr_ctxt())
                ))(k.get_source())
            )
            if k.label == 'LOWER_IDENTIFIER' else
            (
                self.translate_expr(k, types_by_nm)
            )
            for k in tree.kids
        )
        #if tree.label == 'EQ_EXPR':
        #    ccode = '({})'.format(ccode)
        return ccode

    def get_type(self, tree, types_by_nm):
        while True:
            if tree.label=='LOWER_IDENTIFIER':
                return types_by_nm[tree.get_source()]
            kids = list(tree.relevant_kids())
            if len(kids)==1:
                tree = kids[0]
            else:
                return

    def analyze_block(self, tree, types_by_nm={}):
        ''' assume no function defns within
        '''
        # copy:
        types_by_nm = types_by_nm.copy()# {k:v for k,v in types_by_nm.items()}

        node_stack = list(tree.relevant_kids())
        while node_stack:
            k, node_stack = node_stack[0], node_stack[1:]

            if k.label == 'SKIP':
                pass
            elif k.label == 'ABORT':
                self.write_code('ABORT;')
            elif k.label == 'DECLARATION':
                ident, typename = self.process_declaration(k)
                pre(ident not in types_by_nm,
                    'variable {} already declared as {}!'.format(
                        ident, types_by_nm[ident] if ident in types_by_nm else None
                    ) 
                )
                types_by_nm[ident] = typename
                self.write_code('{} _{};', typename, ident)
            elif k.label == 'ASSIGNMENT':
                ident, expr = self.process_assignment(k) 
                if ident=='return':
                    self.write_code('return {};', self.translate_expr(expr, types_by_nm))
                else:
                    pre(ident in types_by_nm, '{} not declared! (ctxt {})'.format(ident, self.curr_ctxt()))
                    self.write_code(
                        '_{} = {};',
                        ident, self.translate_expr(expr, types_by_nm)
                    )
            elif k.label == 'MATCH': 
                expr, guardeds = k.relevant_kids()  
                expr_type = self.get_type(expr, types_by_nm)
                pre(self.defns[expr_type]['alg']=='enum',
                    'can only match on sum types!'
                )
                alternatives = self.defns[expr_type]['child_fields']

                temp_nm = self.fresh_var_nm() 
                cond_cons_pairs = self.process_guarded_sequence(guardeds)
                self.write_code('{} {} = {};',
                    expr_type, temp_nm,
                    self.translate_expr(expr, types_by_nm)
                )
                self.write_code('switch ({}.tag) {{', temp_nm)
                for i, (cond, cons) in enumerate(cond_cons_pairs): 
                    alt, name = cond.relevant_kids()
                    alt = alt.get_source()
                    name = name.get_source()
                    idx = alternatives.index(alt)
                    self.write_code('case {} /*{}*/ : {{',
                        idx, alt
                    )
                    self.write_code('{} _{} = {}.data.{};',
                        self.defns[expr_type]['child_types'][idx],
                        name, temp_nm, alt 
                    )
                    self.analyze_block(cons, types_by_nm)
                    self.write_code('}} break;')
                self.write_code('}}')
            elif k.label == 'IF': 
                guardeds, = k.relevant_kids()
                cond_cons_pairs = self.process_guarded_sequence(guardeds)
                pre(cond_cons_pairs,
                    'alternative constructs must have at least one branch'
                )
                for i, (cond, cons) in enumerate(cond_cons_pairs): 
                    self.write_code(
                        '{} ({}) {{',
                        'if' if i==0 else '} else if',
                        self.translate_expr(cond, types_by_nm).strip()
                    )
                    self.analyze_block(cons, types_by_nm=types_by_nm)
                self.write_code('}} else {{')
                self.write_code('ABORT;')
                self.write_code('}}')
            elif k.label == 'DO': 
                cond_cons_pairs = self.process_guarded_sequence(k)
                pre(cond_cons_pairs,
                    'repetitive constructs must have at least one branch'
                )
                self.write_code('while (true) {')
                for i, (cond, cons) in enumerate(cond_cons_pairs): 
                    self.write_code(
                        '{} ({}) {{',
                        'if' if i==0 else '} else if',
                        self.translate_expr(cond, types_by_nm).strip()
                    )
                    self.analyze_block(cons, types_by_nm=types_by_nm)
                self.write_code('}} else {{')
                self.write_code('break;')
                self.write_code('}}')
                self.write_code('}}')
            elif k.label == 'TYPEDEFN':
                self.process_type_defn(k)
            elif k.label == 'FUNCTION':
                ident, arg_types_by_nm, out_type, body = self.process_function(k)
                print(CC+'@R creating ctxt {}...@D '.format(ident))
                self.push_ctxt(ident)
                self.make_defn('function', ident=ident, out_type=out_type,
                    arg_types_by_nm=arg_types_by_nm
                )
                self.analyze_block(body, types_by_nm=arg_types_by_nm.copy())
                self.pop_ctxt()
            elif k.label == 'PRINT': 
                ident, = k.relevant_kids()
                ident = ident.get_source()
                pre(ident in types_by_nm,
                    '{} not declared! (print ctxt {})'.format(ident, self.curr_ctxt())
                )
                typename = types_by_nm[ident]

                if typename == 'Float':
                    self.write_code('printf("{} \\t %f\\n", _{});', ident, ident)
                elif typename == 'Int':
                    self.write_code('printf("{} \\t %d\\n", _{});', ident, ident)
                elif typename == 'Bool':
                    self.write_code('printf("{} \\t %s\\n", _{}?"true":"false");', ident, ident)
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

    cow_parser = ParserGenerator('grammars/cow-lang-type.grammar').final()
    with open(cow_filenm) as f:
        text = Text(preprocess(f.read()))
    tree = cow_parser(text)
    tree.display(max_depth=2)

    G = CodeGenerator(tree) 
    with open(c_filenm, 'w') as f:
        f.write(G.total_print())
    #print(G.total_print())
