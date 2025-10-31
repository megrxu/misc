import re
import csv
from functools import reduce

prog = re.compile('\((.+?)\)')


def valid_filter(triple):
    if triple[0][0].islower() or triple[2][0].islower() or triple[1] == 'is':
        return False
    return True

def text_process(text):
    return text.strip().replace('was', 'is')

def relation_rewrite(triple):
    triple[1] = triple[1].replace('became', 'is')
    triple[1] = triple[1].replace('become', 'is')
    # A is B's
    if '\'s' in triple[2] and triple[1] == 'is':
        triple[1] = 'is ' + triple[2].split('\'s')[1].strip() + ' of'
        triple[2] = triple[2].split('\'s')[0].strip()
    # A's is B => B is of A
    elif '\'s' in triple[0] and triple[1] == 'is':
        subject = triple[2]
        triple[2] = triple[0].split('\'s')[0].strip()
        triple[1] = 'is ' + triple[0].split('\'s')[1].strip() + ' of'
        triple[0] = subject
    # A is of B
    # elif 'of' in triple[2] and triple[1] == 'is':
    #     triple[1] = 'is ' + triple[2].split('of')[0].strip() + ' of'
    #     triple[2] = triple[2].split('of')[1].strip()
    #     print(triple)
    # A of B
    elif 'of' in triple[1] and 'is' not in triple[1] and 'are' not in triple[1]:
        triple[1] = 'is ' + triple[1]
    elif 'are' in triple[1]:
        triple[1] = 'is ' + " ".join(triple[1].split(' ')[1:])
    # adjs
    triple[1] = reduce(lambda res, a: res.replace(a, ''), ['adoptive', 'former', 'supporting', '25th', 'childhood', 'Marine', 'also', 'secondary'], triple[1])
    triple[1] = triple[1].replace('  ', ' ')
    return tuple(triple)


triples = []
for filename in ['Brothers_and_sisters.txt', 'Enemys.txt', 'Huang.txt', 'Other_relatives.txt', 'Upper_and_lower.txt']:
# for filename in ['Brothers_and_sisters.txt', 'Enemys.txt']:
    file = open('input/{}'.format(filename), 'r')
    # extract
    for line in file.readlines():
        ori = prog.search(line).group(1).split(';')
        ori = list(map(text_process, ori))
        ori = relation_rewrite(ori)
        triples.append(tuple(ori))

for filename in ['Couples.csv', 'Friends.csv']:
    file = open('input/{}'.format(filename), 'r')
    reader = csv.reader(file)
    triples += list(map(lambda x: tuple(list(map(text_process, x))), reader))

triples = list(filter(valid_filter, triples))

triples_str = map(lambda x: "{};{};{}\n".format(
    x[0], x[1], x[2]), sorted(list(set(triples))))

outfile = open('rels/res-01.csv', 'w')
outfile.writelines(triples_str)

triples = []
for filename in ['input/zhiye.txt', 'input/zhiye(2).txt']:
    file = open('{}'.format(filename), 'r')
    # extract
    for line in file.readlines():
        ori = prog.search(line).group(1).split(';')
        ori = list(map(text_process, ori))
        triples.append(tuple(ori))

triples_str = map(lambda x: "{};{};{}\n".format(
    x[0], x[1], x[2]), sorted(list(set(triples))))

outfile_z = open('rels/zhiye.csv', 'w')
outfile_z.writelines(triples_str)
