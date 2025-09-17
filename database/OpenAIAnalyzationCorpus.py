import hashlib
from datetime import datetime
from sqlalchemy import Column, Integer, String, Double, ForeignKey, DateTime
from sqlalchemy.orm import relationship
from database.base import Base


class OpenAIAnalyzationCorpus(Base):
    __tablename__ = 'openai_analyzation_corpus'
    id = Column(Integer, primary_key=True, autoincrement=True)
    hash = Column(String)   # if analyzed this measures a corpus text with unique hash
    entry_id = Column(Integer)  # reference to entry for convenience
    repeat = Column(Integer)     # added repeat count
    # leave columns for compatibility
    of1b = Column(Double)
    of2b = Column(Double)
    of3b = Column(Double)
    cf1b = Column(Double)
    cf2b = Column(Double)
    cf3b = Column(Double)
    ef1b = Column(Double)
    ef2b = Column(Double)
    ef3b = Column(Double)
    af1b = Column(Double)
    af2b = Column(Double)
    af3b = Column(Double)
    nf1b = Column(Double)
    nf2b = Column(Double)
    nf3b = Column(Double)
    of1 = Column(Double)
    of2 = Column(Double)
    of3 = Column(Double)
    of4 = Column(Double)
    of5 = Column(Double)
    of6 = Column(Double)
    cf1 = Column(Double)
    cf2 = Column(Double)
    cf3 = Column(Double)
    cf4 = Column(Double)
    cf5 = Column(Double)
    cf6 = Column(Double)
    ef1 = Column(Double)
    ef2 = Column(Double)
    ef3 = Column(Double)
    ef4 = Column(Double)
    ef5 = Column(Double)
    ef6 = Column(Double)
    af1 = Column(Double)
    af2 = Column(Double)
    af3 = Column(Double)
    af4 = Column(Double)
    af5 = Column(Double)
    af6 = Column(Double)
    nf1 = Column(Double)
    nf2 = Column(Double)
    nf3 = Column(Double)
    nf4 = Column(Double)
    nf5 = Column(Double)
    nf6 = Column(Double)
    model = Column(String)
    state = Column(Integer)
    input_tokens = Column(Double)
    output_tokens= Column(Double)
    error_response = Column(String)
    started_at = Column(DateTime, default=datetime.utcnow)
    finished_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
