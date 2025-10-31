import requests
import pickle
from json import load, dump
from functools import reduce
import openie

lang = 'en'
serialized_dir = 'serialized/{}'.format(lang)
triples_dir = 'triples/{}'.format(lang)

serialized = open('{}/res-lv1.pic'.format(serialized_dir), 'rb')
entities = pickle.load(serialized)


def nonsense(x): return not(
    '' in x or ' ' in x or 'he' in x or 'He' in x or 'She' in x or 'They' in x or '+' in x or '=' in x or x[0].islower() or 'member' in x)

def replace_plus(y): return y.replace('+ ', '').replace(' +', '')

cur = 0
for entity in entities:
    if entity['id'] != 1444:
        continue
    texts = reduce(lambda res, a: res + ' ' + a, entity['texts'], '')
    res = requests.post(
        'http://[::]:9000/?properties={"annotators":"tokenize,ssplit,pos,lemma,ner,depparse,natlog,coref,openie", "outputFormat":"json"}', data={'data': texts}).json()
    ts = []
    for sentence in res['sentences']:
        pre = map(lambda x: tuple(map(
            replace_plus, (x['subject'], x['relation'], x['object']))), sentence['openie'])
        triples = list(filter(nonsense, pre))
        ts += triples
    cur += 1
    if len(ts) > 0:
        print(cur, ": ", ts)
        # with open("{}/{}.json".format(triples_dir, entity['name']), "w") as outfile:
        #     dump(list(set(triples)), outfile,
        #          indent=4, separators=(',', ': '))
