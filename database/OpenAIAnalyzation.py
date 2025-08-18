import hashlib
from datetime import datetime
from sqlalchemy import Column, Integer, String, Double, ForeignKey, DateTime
from sqlalchemy.orm import relationship
from database.base import Base


class OpenAIAnalyzation(Base):
    __tablename__ = 'openai_analyzation'
    id = Column(Integer, primary_key=True, autoincrement=True)
    essay_id = Column(Integer, ForeignKey('essays.id'), nullable=False)
    o_openai = Column(Double)
    c_openai = Column(Double)
    e_openai = Column(Double)
    a_openai = Column(Double)
    n_openai = Column(Double)
    of1 = Column(Double)
    of2 = Column(Double)
    of3 = Column(Double)
    cf1 = Column(Double)
    cf2 = Column(Double)
    cf3 = Column(Double)
    ef1 = Column(Double)
    ef2 = Column(Double)
    ef3 = Column(Double)
    af1 = Column(Double)
    af2 = Column(Double)
    af3 = Column(Double)
    nf1 = Column(Double)
    nf2 = Column(Double)
    nf3 = Column(Double)
    model = Column(String)
    temperature = Column(Double)
    input_tokens = Column(Double)
    output_tokens= Column(Double)
    error_response = Column(String)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    essay = relationship("Essay", back_populates="openai_analyzation")
