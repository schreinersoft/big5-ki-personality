import hashlib
from sqlalchemy import Column, Integer, String, Double, ForeignKey
from sqlalchemy.orm import relationship
from base import Base

class LIWCAnalyzationTest(Base):
    __tablename__ = 'liwc_analyzation_test'
    id = Column(Integer, primary_key=True, autoincrement=True, nullable=False)
    essay_id = Column(Integer, ForeignKey('essays.id'), nullable=False, unique=True)
    o_liwc = Column(Double)
    c_liwc = Column(Double)
    e_liwc = Column(Double)
    a_liwc = Column(Double)
    n_liwc = Column(Double)

    essay = relationship("Essay", back_populates="liwc_analyzation")
