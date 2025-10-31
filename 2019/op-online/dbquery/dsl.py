# coding: utf-8

from quepy.dsl import FixedType, HasKeyword, FixedRelation, FixedDataRelation

# Setup the Keywords for this application
HasKeyword.relation = "rdfs:label"
HasKeyword.language = "en"


class IsPerson(FixedType):
    fixedtype = "foaf:Person"


class SonOf(FixedRelation):
    relation = "rel:son"
    reverse = True


class DauOf(FixedRelation):
    relation = "rel:daughter"
    reverse = True


class HusbandOf(FixedRelation):
    relation = "rel:hunband"
    reverse = True


class WifeOf(FixedRelation):
    relation = "rel:wife"
    reverse = True


class MomOf(FixedRelation):
    relation = "rel:mother"
    reverse = True


class PapOf(FixedRelation):
    relation = "rel:father"
    reverse = True


class GrandPaOf(FixedRelation):
    relation = "rel:grandfather"
    reverse = True


class GrandMaOf(FixedRelation):
    relation = "rel:grandmother"
    reverse = True


class GrandSonOf(FixedRelation):
    relation = "rel:grandson"
    reverse = True


class DefinitionOf(FixedRelation):
    relation = "rdfs:comment"
    reverse = True


class LabelOf(FixedRelation):
    relation = "rdfs:label"
    reverse = True


class PresidentOf(FixedRelation):
    relation = "rel:leader"
    reverse = True


class KingOf(FixedRelation):
    relation = "rel:king"
    reverse = True


class QueenOf(FixedRelation):
    relation = "rel:queen"
    reverse = True


class CapitalOf(FixedRelation):
    relation = "rel:captial"
    reverse = True


class OfficerOf(FixedRelation):
    relation = "rel:officer"
    reverse = True


class IsMemberOf(FixedRelation):
    relation = "rel:member"
    reverse = True


class NameOf(FixedRelation):
    relation = "rdfs:label"
    reverse = True


class AntagonistOf(FixedRelation):
    relation = "rel:antagonist"
    reverse = True


class BroOf(FixedRelation):
    relation = "rel:brother"
    reverse = True


class Bro(FixedRelation):
    relation = "rel:brother"


class SisOf(FixedRelation):
    relation = "rel:sister"
    reverse = True


class Sis(FixedRelation):
    relation = "rel:sister"


class MemberOf(FixedRelation):
    relation = "rel:member"
    reverse = True


class AllyOf(FixedRelation):
    relation = "rel:ally"
    reverse = True


class Ally(FixedRelation):
    relation = "rel:ally"


class FriendOf(FixedRelation):
    relation = "rel:friend"
    reverse = True


class Friend(FixedRelation):
    relation = "rel:friend"


class EnemyOf(FixedRelation):
    relation = "rel:enemy"
    reverse = True
