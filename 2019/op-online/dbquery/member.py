from refo import Group, Plus, Question
from quepy.parsing import Lemma, Pos, QuestionTemplate, Token, Particle, \
    Lemmas
from quepy.dsl import HasKeyword, IsRelatedTo, HasType
from dsl import *
from entity import *

class WhoMemberOf(QuestionTemplate):
    """
    Regex for questions like "What is a blowtorch
    Ex: "What is a car"
        "What is Seinfield?"
    """

    regex = Lemma("who") + Lemma("be") + Lemma("the") + Lemma("member") + Lemma("of") + Thing() + Question(Pos("."))

    def interpret(self, match):
        label = NameOf(MemberOf(match.thing))

        return label, "define"

class WhoAllyOf(QuestionTemplate):
    regex = Lemma("who") + Lemma("be") + Lemma("the") + (Lemma("ally") | Lemma("friend")) + Lemma("of") + Thing() + Question(Pos("."))

    def interpret(self, match):
        label = NameOf(AllyOf(match.thing))

        return label, "define"