''' author: samtenka
    change: 2020-03-07
    create: 2019-05-20
    descrp: simple processing before feeding into parser
    to use: 
'''

from utils import pre

def preprocess(text):
    pre('\t' not in text, 'cow-lang forbids tab characters') 
    uncommented = [
        line for line in text.split('\n')
        if not line.strip().startswith('//')
    ]
    #return ' '.join(uncommented)
    return ' '.join(' '.join(uncommented).split())
