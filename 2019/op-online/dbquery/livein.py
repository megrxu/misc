from refo import Group, Plus, Question
from quepy.parsing import Lemma, Pos, QuestionTemplate, Token, Particle, \
    Lemmas
from quepy.dsl import HasKeyword, IsRelatedTo, HasType
from dsl import *
from entity import *
from person import *


class LiveIn(QuestionTemplate):
    regex = Lemma("who") + Lemma("live") + Lemma("in") + \
        Thing() + Question(Pos("."))

    def interpret(self, match):
        label = NameOf(AntagonistOf(match.thing))

        return label, "define"
