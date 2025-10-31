
from refo import Group, Plus, Question
from quepy.parsing import Lemma, Pos, QuestionTemplate, Token, Particle, \
    Lemmas
from quepy.dsl import HasKeyword, IsRelatedTo, HasType
from dsl import *

# Openings
LISTOPEN = Lemma("list") | Lemma("name")


class Thing(Particle):
    regex = Plus(Pos("JJ") | Pos("JJ") + Pos("NN") | Pos("NN")
                 | Pos("NNS") | Pos("NNP") | Pos("NNPS"))

    def interpret(self, match):
        name = match.words.tokens
        return HasKeyword(match.words.tokens)


class WhatIs(QuestionTemplate):
    """
    Regex for questions like "What is a blowtorch
    Ex: "What is a car"
        "What is Seinfield?"
    """

    regex = Lemma("what") + Lemma("be") + Question(Pos("DT")) + \
        Thing() + Question(Pos("."))

    def interpret(self, match):
        label = DefinitionOf(match.thing)

        return label, "define"
