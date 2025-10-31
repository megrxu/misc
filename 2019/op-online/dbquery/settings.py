# coding: utf-8

"""
Settings.
"""

# Generated query language
LANGUAGE = "sparql"

# NLTK config
NLTK_DATA_PATH = ['/usr/share/nltk_data/']  # List of paths with NLTK data

# Encoding config
DEFAULT_ENCODING = "utf-8"

# Sparql config
SPARQL_PREAMBLE = u"""
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX rel: <http://one-piece.local/#relation/>
PREFIX chr: <http://one-piece.local/#character/>
PREFIX ent: <http://one-piece.local/#entity/>
PREFIX plc: <http://one-piece.local/#place/>
PREFIX org: <http://one-piece.local/#organization/>
"""
