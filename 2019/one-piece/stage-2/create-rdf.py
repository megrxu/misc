import rdflib
from rdflib.extras.external_graph_libs import rdflib_to_networkx_multidigraph
import networkx as nx
import matplotlib.pyplot as plt
from rdflib.namespace import XSD, RDF, RDFS, SKOS, FOAF
from functools import reduce


def get_namelist():
    file = open('namelist', 'r')
    return list(map(lambda x: x.strip(), file.readlines()))


def get_triples():
    file = open('rels/res-01.csv', 'r')
    return list(map(lambda x: tuple(x.split(';')), file.readlines()))


def get_members():
    file = open('rels/zhiye.csv', 'r')
    return list(map(lambda x: tuple(x.split(';')), file.readlines()))


def score(if_a, is_b):
    atokens = if_a.split(' ')
    btokens = is_b.split(' ')
    res = 0
    for token in atokens:
        if token in btokens:
            res += 1
    return res


def get_type(name):
    if name.split(' ')[-1].lower() in ['pirates', 'marines', 'crew', 'village'] or 'CP' in name:
        return 'organization'
    elif name.split(' ')[-1].lower() in ['arc', 'island', 'kingdom', 'country']:
        return 'place'
    else:
        return 'entity'


def build_uri(name):
    return "http://one-piece.local/#{}/{}".format(get_type(name), name.strip().replace(' ', '_'))


def create():
    g = rdflib.Graph()
    chrns = rdflib.Namespace('http://one-piece.local/#character/')
    relns = rdflib.Namespace('http://one-piece.local/#relation/')
    orgns = rdflib.Namespace('http://one-piece.local/#organization/')

    g.namespace_manager.bind('op-rel', relns, override=True, replace=True)
    g.namespace_manager.bind('op-org', orgns, override=True, replace=True)
    g.namespace_manager.bind('op-chr', chrns, override=True, replace=True)
    g.namespace_manager.bind('foaf', FOAF, override=True, replace=True)

    # prepare for the keys
    nl = get_namelist()
    nd = {}
    for name in nl:
        nd[name] = chrns["{}".format(name.replace(' ', '_'))]
        try:
            with open("../stage-1/data/en/{}.txt".format(name), 'r') as file:
                g.add((nd[name], RDFS.comment, rdflib.Literal(
                    reduce(lambda res, a: res + ' ' + a.strip(), file.readlines(), ""))))
        except Exception:
            pass
    preserved = {
        'Straw Hat Pirates': orgns['Straw_Hat_Pirates'],
        'Charlotte Family': orgns['Charlotte_Family'],
        'World Government': orgns['World_Government'],
        'CP9': orgns['CP9']
    }

    for key in preserved.keys():
        nd[key] = preserved[key]

    triples = get_triples()
    member = get_members()
    rels = set(map(lambda x: x[1], triples + member))
    reld = {}
    for rel in rels:
        reld[rel] = relns[rel.replace('is ', '').replace(
            ' of', '').replace(' ', '_')]

    reverse = {}
    reverse = {
        'is husband of': 'is wife of',
        'is wife of': 'is husband of',
        'is mother of': 'is child of',
        'is father of': 'is child of',
        'is grandfather of': 'is grandson of',
        'is grandmother of': 'is grandson of',
        'is grandparent of': 'is grandson of',
        'is parent of': 'is child of',
    }

    for triple in triples:
        sk = max(map(lambda x: [x if score(triple[0].strip(), x) else triple[0].strip(), score(
            triple[0].strip(), x)], nd.keys()), key=lambda x: x[1])
        ok = max(map(lambda x: [x if score(triple[2].strip(), x) else triple[2].strip(), score(
            triple[2].strip(), x)], nd.keys()), key=lambda x: x[1])

        s = nd[sk[0].strip()] if sk[0].strip() in nd.keys(
        ) else rdflib.URIRef(build_uri(sk[0]))
        r = reld[triple[1]]
        o = nd[ok[0].strip()] if ok[0].strip() in nd.keys(
        ) else rdflib.URIRef(build_uri(ok[0]))

        g.add((s, RDFS.label, rdflib.Literal(sk[0], lang="en")))
        g.add((o, RDFS.label, rdflib.Literal(ok[0], lang="en")))

        if sk[0] in nd.keys() or ok[0] in nd.keys():
            # origin relation
            g.add((o, r, s))
            g.add((s, FOAF.name, rdflib.Literal(sk[0], lang="en")))
            g.add((o, FOAF.name, rdflib.Literal(ok[0], lang="en")))
            
            if sk[0] in nl:
                g.add((s, RDF.type, FOAF.Person))
            if ok[0] in nl:
                g.add((o, RDF.type, FOAF.Person))

            # reflexive relation
            if triple[1] in ['is friend of', 'is ally of', 'is enemy of', 'is brother of', 'is sister of']:
                g.add((s, r, o))
            # reverse relation
            elif triple[1] in ['is husband of', 'is wife of', 'is mother of', 'is father of', 'is grandfather of', 'is grandmother of', 'is grandparent of', 'is parent of']:
                g.add((s, reld[reverse[triple[1]]], o))

    # inner properties
    for triple in member:
        s = nd[triple[0].strip()] if triple[0].strip() in nd.keys(
        ) else rdflib.URIRef(build_uri(triple[0].strip()))
        r = reld[triple[1].strip()]
        o = nd[triple[2].strip()] if triple[2].strip() in nd.keys(
        ) else rdflib.URIRef(build_uri(triple[2].strip()))

        g.add((s, RDFS.label, rdflib.Literal(triple[0].strip(), lang="en")))
        g.add((o, RDFS.label, rdflib.Literal(triple[2].strip(), lang="en")))
        g.add((o, r, s))

    g.serialize("graph.rdf")
    # G = rdflib_to_networkx_multidigraph(g)

    # # Plot Networkx instance of RDF Graph
    # pos = nx.spring_layout(G, scale=1)
    # edge_labels = nx.get_edge_attributes(G, 'r')
    # nx.draw_networkx_edge_labels(G, pos, labels=edge_labels)
    # nx.draw(G, with_labels=False)
    # plt.show()


if __name__ == "__main__":
    create()
