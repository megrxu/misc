#!/usr/bin/env bash
# Run in data/en/
cd data/en/
java -Xmx8G -cp "$HOME/stanfordnlp_resources/stanford-corenlp-full-2018-10-05/*" edu.stanford.nlp.naturalli.OpenIE -annotators tokenize,ssplit,pos,lemma,ner,depparse,natlog,coref,openie -resolve_coref true -ignore_affinity true -filelist filelist -format ollie -output ../../triples.json
cd ../../