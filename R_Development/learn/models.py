from typing import Optional

from sqlalchemy import Column, Double, Integer, PrimaryKeyConstraint, String, Table, Text, UniqueConstraint
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column

class Base(DeclarativeBase):
    pass


class BenjaminTest(Base):
    __tablename__ = 'benjamin_test'
    __table_args__ = (
        PrimaryKeyConstraint('id', name='benjamin_test_pkey'),
        UniqueConstraint('hash', name='benjamin_test_hash_key')
    )

    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    hash: Mapped[Optional[str]] = mapped_column(String)
    href: Mapped[Optional[str]] = mapped_column(String)
    title: Mapped[Optional[str]] = mapped_column(String)
    language: Mapped[Optional[str]] = mapped_column(String)
    author_name: Mapped[Optional[str]] = mapped_column(String)
    author_age: Mapped[Optional[int]] = mapped_column(Integer)
    author_sex: Mapped[Optional[str]] = mapped_column(String)
    receiver_name: Mapped[Optional[str]] = mapped_column(String)
    receiver_age: Mapped[Optional[int]] = mapped_column(Integer)
    receiver_sex: Mapped[Optional[str]] = mapped_column(String)
    text_raw: Mapped[Optional[str]] = mapped_column(String)
    text_raw_numtokens: Mapped[Optional[int]] = mapped_column(Integer)
    text: Mapped[Optional[str]] = mapped_column(String)
    text_numtokens: Mapped[Optional[int]] = mapped_column(Integer)
    year: Mapped[Optional[int]] = mapped_column(Integer)
    month: Mapped[Optional[int]] = mapped_column(Integer)
    day: Mapped[Optional[int]] = mapped_column(Integer)
    text_type: Mapped[Optional[str]] = mapped_column(String)
    scrape_state: Mapped[Optional[int]] = mapped_column(Integer)
    scrape_comment: Mapped[Optional[str]] = mapped_column(String)


class Corpus(Base):
    __tablename__ = 'corpus'
    __table_args__ = (
        PrimaryKeyConstraint('hash', name='corpus_pkey'),
    )

    hash: Mapped[str] = mapped_column(String, primary_key=True)
    title: Mapped[Optional[str]] = mapped_column(String)
    language: Mapped[Optional[str]] = mapped_column(String)
    author_name: Mapped[Optional[str]] = mapped_column(String)
    author_age: Mapped[Optional[int]] = mapped_column(Integer)
    author_sex: Mapped[Optional[str]] = mapped_column(String)
    receiver_name: Mapped[Optional[str]] = mapped_column(String)
    receiver_age: Mapped[Optional[int]] = mapped_column(Integer)
    receiver_sex: Mapped[Optional[str]] = mapped_column(String)
    text_raw: Mapped[Optional[str]] = mapped_column(String)
    text_raw_numtokens: Mapped[Optional[int]] = mapped_column(Integer)
    text: Mapped[Optional[str]] = mapped_column(String)
    text_numtokens: Mapped[Optional[int]] = mapped_column(Integer)
    year: Mapped[Optional[int]] = mapped_column(Integer)
    month: Mapped[Optional[int]] = mapped_column(Integer)
    day: Mapped[Optional[int]] = mapped_column(Integer)
    text_type: Mapped[Optional[str]] = mapped_column(String)


t_employees = Table(
    'employees', Base.metadata,
    Column('id', Integer),
    Column('name', Text),
    Column('age', Double(53)),
    Column('salary', Double(53))
)


t_essays = Table(
    'essays', Base.metadata,
    Column('row.names', Text),
    Column('essay_id', Integer),
    Column('author', Text),
    Column('text', Text),
    Column('o_binary', Double(53)),
    Column('c_binary', Double(53)),
    Column('e_binary', Double(53)),
    Column('a_binary', Double(53)),
    Column('n_binary', Double(53))
)


t_essays_test = Table(
    'essays_test', Base.metadata,
    Column('row.names', Text),
    Column('essay_id', Double(53)),
    Column('author', Text),
    Column('text', Text),
    Column('o_bin', Double(53)),
    Column('c_bin', Double(53)),
    Column('e_bin', Double(53)),
    Column('a_bin', Double(53)),
    Column('n_bin', Double(53)),
    Column('Segment', Double(53)),
    Column('WC', Double(53)),
    Column('Analytic', Double(53)),
    Column('Clout', Double(53)),
    Column('Authentic', Double(53)),
    Column('Tone', Double(53)),
    Column('WPS', Double(53)),
    Column('BigWords', Double(53)),
    Column('Dic', Double(53)),
    Column('Linguistic', Double(53)),
    Column('function', Double(53)),
    Column('pronoun', Double(53)),
    Column('ppron', Double(53)),
    Column('i', Double(53)),
    Column('we', Double(53)),
    Column('you', Double(53)),
    Column('shehe', Double(53)),
    Column('they', Double(53)),
    Column('ipron', Double(53)),
    Column('det', Double(53)),
    Column('article', Double(53)),
    Column('number', Double(53)),
    Column('prep', Double(53)),
    Column('auxverb', Double(53)),
    Column('adverb', Double(53)),
    Column('conj', Double(53)),
    Column('negate', Double(53)),
    Column('verb', Double(53)),
    Column('adj', Double(53)),
    Column('quantity', Double(53)),
    Column('Drives', Double(53)),
    Column('affiliation', Double(53)),
    Column('achieve', Double(53)),
    Column('power', Double(53)),
    Column('Cognition', Double(53)),
    Column('allnone', Double(53)),
    Column('cogproc', Double(53)),
    Column('insight', Double(53)),
    Column('cause', Double(53)),
    Column('discrep', Double(53)),
    Column('tentat', Double(53)),
    Column('certitude', Double(53)),
    Column('differ', Double(53)),
    Column('memory', Double(53)),
    Column('Affect', Double(53)),
    Column('tone_pos', Double(53)),
    Column('tone_neg', Double(53)),
    Column('emotion', Double(53)),
    Column('emo_pos', Double(53)),
    Column('emo_neg', Double(53)),
    Column('emo_anx', Double(53)),
    Column('emo_anger', Double(53)),
    Column('emo_sad', Double(53)),
    Column('swear', Double(53)),
    Column('Social', Double(53)),
    Column('socbehav', Double(53)),
    Column('prosocial', Double(53)),
    Column('polite', Double(53)),
    Column('conflict', Double(53)),
    Column('moral', Double(53)),
    Column('comm', Double(53)),
    Column('socrefs', Double(53)),
    Column('family', Double(53)),
    Column('friend', Double(53)),
    Column('female', Double(53)),
    Column('male', Double(53)),
    Column('Culture', Double(53)),
    Column('politic', Double(53)),
    Column('ethnicity', Double(53)),
    Column('tech', Double(53)),
    Column('Lifestyle', Double(53)),
    Column('leisure', Double(53)),
    Column('home', Double(53)),
    Column('work', Double(53)),
    Column('money', Double(53)),
    Column('relig', Double(53)),
    Column('Physical', Double(53)),
    Column('health', Double(53)),
    Column('illness', Double(53)),
    Column('wellness', Double(53)),
    Column('mental', Double(53)),
    Column('substances', Double(53)),
    Column('sexual', Double(53)),
    Column('food', Double(53)),
    Column('death', Double(53)),
    Column('need', Double(53)),
    Column('want', Double(53)),
    Column('acquire', Double(53)),
    Column('lack', Double(53)),
    Column('fulfill', Double(53)),
    Column('fatigue', Double(53)),
    Column('reward', Double(53)),
    Column('risk', Double(53)),
    Column('curiosity', Double(53)),
    Column('allure', Double(53)),
    Column('Perception', Double(53)),
    Column('attention', Double(53)),
    Column('motion', Double(53)),
    Column('space', Double(53)),
    Column('visual', Double(53)),
    Column('auditory', Double(53)),
    Column('feeling', Double(53)),
    Column('time', Double(53)),
    Column('focuspast', Double(53)),
    Column('focuspresent', Double(53)),
    Column('focusfuture', Double(53)),
    Column('Conversation', Double(53)),
    Column('netspeak', Double(53)),
    Column('assent', Double(53)),
    Column('nonflu', Double(53)),
    Column('filler', Double(53)),
    Column('AllPunc', Double(53)),
    Column('Period', Double(53)),
    Column('Comma', Double(53)),
    Column('QMark', Double(53)),
    Column('Exclam', Double(53)),
    Column('Apostro', Double(53)),
    Column('OtherP', Double(53)),
    Column('Emoji', Double(53))
)


class TeslaTest(Base):
    __tablename__ = 'tesla_test'
    __table_args__ = (
        PrimaryKeyConstraint('id', name='tesla_test_pkey'),
        UniqueConstraint('hash', name='tesla_test_hash_key')
    )

    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    hash: Mapped[str] = mapped_column(String, nullable=False)
    href: Mapped[Optional[str]] = mapped_column(String)
    title: Mapped[Optional[str]] = mapped_column(String)
    language: Mapped[Optional[str]] = mapped_column(String)
    author_name: Mapped[Optional[str]] = mapped_column(String)
    author_age: Mapped[Optional[int]] = mapped_column(Integer)
    author_sex: Mapped[Optional[str]] = mapped_column(String)
    receiver_name: Mapped[Optional[str]] = mapped_column(String)
    receiver_age: Mapped[Optional[int]] = mapped_column(Integer)
    receiver_sex: Mapped[Optional[str]] = mapped_column(String)
    text_raw: Mapped[Optional[str]] = mapped_column(String)
    text_raw_numtokens: Mapped[Optional[int]] = mapped_column(Integer)
    text: Mapped[Optional[str]] = mapped_column(String)
    text_numtokens: Mapped[Optional[int]] = mapped_column(Integer)
    year: Mapped[Optional[int]] = mapped_column(Integer)
    month: Mapped[Optional[int]] = mapped_column(Integer)
    day: Mapped[Optional[int]] = mapped_column(Integer)
    text_type: Mapped[Optional[str]] = mapped_column(String)
    scrape_state: Mapped[Optional[int]] = mapped_column(Integer)
    scrape_comment: Mapped[Optional[str]] = mapped_column(String)


class WoolfTest(Base):
    __tablename__ = 'woolf_test'
    __table_args__ = (
        PrimaryKeyConstraint('hash', name='woolf_test_pkey'),
    )

    hash: Mapped[str] = mapped_column(String, primary_key=True)
    title: Mapped[Optional[str]] = mapped_column(String)
    language: Mapped[Optional[str]] = mapped_column(String)
    author_name: Mapped[Optional[str]] = mapped_column(String)
    author_age: Mapped[Optional[int]] = mapped_column(Integer)
    author_sex: Mapped[Optional[str]] = mapped_column(String)
    receiver_name: Mapped[Optional[str]] = mapped_column(String)
    receiver_age: Mapped[Optional[int]] = mapped_column(Integer)
    receiver_sex: Mapped[Optional[str]] = mapped_column(String)
    text_raw: Mapped[Optional[str]] = mapped_column(String)
    text_raw_numtokens: Mapped[Optional[int]] = mapped_column(Integer)
    text: Mapped[Optional[str]] = mapped_column(String)
    text_numtokens: Mapped[Optional[int]] = mapped_column(Integer)
    year: Mapped[Optional[int]] = mapped_column(Integer)
    month: Mapped[Optional[int]] = mapped_column(Integer)
    day: Mapped[Optional[int]] = mapped_column(Integer)
    text_type: Mapped[Optional[str]] = mapped_column(String)
