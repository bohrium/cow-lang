''' author: samtenka
    change: 2020-03-07
    create: 2019-02-01
    descrp: Given backus-naur grammar and string, return parse tree.
    to use:
'''

from utils import CC
from preprocess import preprocess
from tree import ParseTree, Text

class ParserGenerator:
    def __init__(self, specs_filename, verbose=False):
        with open(specs_filename) as f:
            specs = f.read()

        self.rules_by_symbol = {}
        self.ignore = set([])
        self.unroll = set([])
        for r in filter(None, specs.split('\n')):
            if r.strip().startswith('//'): continue
            split_index = r.find('=') 
            symbol, rule = r[:split_index].strip(), r[split_index+1:].strip()
            if symbol[-1]=='*':
                symbol = symbol[:-1]
                self.ignore.add(symbol)
            elif symbol[-1]=='!':
                symbol = symbol[:-1]
                self.unroll.add(symbol)

            self.rules_by_symbol[symbol] = rule

        if verbose:
            print(self.rules_by_symbol)

        self.parsers = {s:(lambda text: False) for s in self.rules_by_symbol.keys()} 
        self.get_parser = lambda symbol: ( lambda text: self.parsers[symbol](text) )

        for symbol, rule in self.rules_by_symbol.items():
            self.parsers[symbol] = self.build_labeled(symbol, self.parser_from_disjunction(Text(rule)), symbol in self.ignore, symbol in self.unroll)

    def parser_from_disjunction(self, rule):
        subparsers = []
        while not rule.is_at_end():
            subparsers.append(self.parser_from_sequence(rule))
            while rule.match(' '): pass
            if not rule.match('|'): break
                
        return self.build_disjunction(subparsers)

    def parser_from_sequence(self, rule):
        subparsers = []
        while not rule.is_at_end():
            while rule.match(' '): pass

            if rule.peek() == '(': 
                rule.match('(')
                subparsers.append(self.parser_from_disjunction(rule))
                while rule.match(' '): pass
                assert rule.match(')')
            elif rule.peek() == '"':
                rule.match('"')
                subparsers.append(self.build_literal(rule.match_until('"')))
                assert rule.match('"')
            elif rule.peek().isidentifier():
                symbol = rule.match_until(' ')
                subparsers.append(self.get_parser(symbol))
            else:
                break 

        return self.build_sequence(subparsers) 

    def build_labeled(self, label, subparser, ignore, unroll):
        def labeledp(text):
            kids = subparser(text)
            if not kids: return False
            return ParseTree(label, kids, ignore, unroll) 
        return labeledp

    def build_disjunction(self, subparsers):
        def disjunctionp(text):
            for sp in subparsers:
                kids = sp(text) 
                if kids: return kids
            return False
        return disjunctionp

    def build_sequence(self, subparsers): 
        def sequencep(text):
            kids = []  
            for sp in subparsers:
                new_kids = sp(text) 
                if not new_kids: return False
                if type(new_kids) != type([]): new_kids = [new_kids]
                kids += new_kids
            return kids
        return sequencep

    def build_literal(self, literal): 
        def literalp(text):
            if text.match(literal):
                return [literal]
            return False
        return literalp 


if __name__ == '__main__':
    import sys
    
    if len(sys.argv) == 1:
        grammar_filenm, program_filenm = 'grammars/cow-lang-type.grammar', 'test.cow'
    elif len(sys.argv) == 3:
        grammar_filenm, program_filenm = sys.argv[1:3]
    else:
        assert len(sys.argv) in [1, 3], "expect 0 or 2 command line arguments"

    PG = ParserGenerator(grammar_filenm)
    P = PG.parsers['MAIN']
    with open(program_filenm) as f:
        text = f.read()
    text = preprocess(text)
    print(text)
    PT = P(Text(text))
    PT.display()
