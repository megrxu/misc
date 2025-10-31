from refo import Group, Plus, Question
from quepy.parsing import Lemma, Pos, QuestionTemplate, Token, Particle, \
    Lemmas
from quepy.dsl import HasKeyword, IsRelatedTo, HasType
from person import Person
from dsl import *


class WhoDauOf(QuestionTemplate):
    regex = Lemma("who") + Lemma("be") + (Lemma("son") | Lemma("daughter")) + Lemma("of") + Person() + \
        Question(Pos("."))

    def interpret(self, match):
        definition = NameOf(DauOf(match.person))
        return definition, "define"


class WhoSonOf(QuestionTemplate):
    regex = Lemma("who") + Lemma("be") + (Lemma("son") | Lemma("sons")) + Lemma("of") + Person() + \
        Question(Pos("."))

    def interpret(self, match):
        definition = NameOf(SonOf(match.person))
        return definition, "define"


class WhoWifeOf(QuestionTemplate):
    regex = Lemma("who") + Lemma("be") + (Lemma("the") + Lemma("wife") | Lemma("wife")) + Lemma("of") + Person() + \
        Question(Pos("."))

    def interpret(self, match):
        definition = NameOf(WifeOf(match.person))
        return definition, "define"


class WhoHusbandOf(QuestionTemplate):
    regex = Lemma("who") + Lemma("be") + (Lemma("the") + Lemma("husband") | Lemma("husband")) + Lemma("of") + Person() + \
        Question(Pos("."))

    def interpret(self, match):
        definition = NameOf(HusbandOf(match.person))
        return definition, "define"


class WhoMomOf(QuestionTemplate):
    regex = Lemma("who") + Lemma("be") + (Lemma("the") + Lemma("mother") | Lemma("mother")) + Lemma("of") + Person() + \
        Question(Pos("."))

    def interpret(self, match):
        definition = NameOf(MomOf(match.person))
        return definition, "define"


class WhoPapOf(QuestionTemplate):
    regex = Lemma("who") + Lemma("be") + (Lemma("the") + Lemma("father") | Lemma("father")) + Lemma("of") + Person() + \
        Question(Pos("."))

    def interpret(self, match):
        definition = NameOf(PapOf(match.person))
        return definition, "define"


class WhoGrandPaOf(QuestionTemplate):
    regex = Lemma("who") + Lemma("be") + (Lemma("the") + Lemma("grandfather") | Lemma("grandfather")) + Lemma("of") + Person() + \
        Question(Pos("."))

    def interpret(self, match):
        definition = NameOf(GrandPaOf(match.person))
        return definition, "define"


class WhoGrandMaOf(QuestionTemplate):
    regex = Lemma("who") + Lemma("be") + (Lemma("the") + Lemma("grandmother") | Lemma("grandmother")) + Lemma("of") + Person() + \
        Question(Pos("."))

    def interpret(self, match):
        definition = NameOf(GrandMaOf(match.person))
        return definition, "define"


class WhoBroOf(QuestionTemplate):
    regex = Lemma("who") + Lemma("be") + (Lemma("the") + Lemma("brother") | Lemma("brother")) + Lemma("of") + Person() + \
        Question(Pos("."))

    def interpret(self, match):
        definition = NameOf(BroOf(match.person)) + NameOf(Bro(match.person))
        return definition, "define"


class WhoSisOf(QuestionTemplate):
    regex = Lemma("who") + Lemma("be") + (Lemma("the") + Lemma("brother") | Lemma("brother")) + Lemma("of") + Person() + \
        Question(Pos("."))

    def interpret(self, match):
        definition = NameOf(SisOf(match.person)) + NameOf(Sis(match.person))
        return definition, "define"
