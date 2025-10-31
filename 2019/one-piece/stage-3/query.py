import rdflib

g = rdflib.Graph()

g.parse('../stage-2/graph.rdf')

qres = g.query(
    """
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX rel: <http://one-piece.local/#relation/>
PREFIX chr: <http://one-piece.local/#character/>
PREFIX ent: <http://one-piece.local/#entity/>
PREFIX plc: <http://one-piece.local/#place/>
PREFIX org: <http://one-piece.local/#organization/>

SELECT DISTINCT ?x1 WHERE {
  ?x0 rdf:type foaf:Person.
  ?x0 rdfs:label "Sanji"@en.
  ?x0 rdfs:comment ?x1.
}""")

for row in qres:
    print("%s" % row)
