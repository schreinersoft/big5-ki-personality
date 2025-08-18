import hashlib
from datetime import datetime
from sqlalchemy import Column, Integer, String, Double, ForeignKey, DateTime
from sqlalchemy.orm import relationship
from database.base import Base


class LIWCAnalyzation(Base):
    __tablename__ = 'liwc_analyzation'
    id = Column(Integer, primary_key=True, autoincrement=True)
    essay_id = Column(Integer, ForeignKey('essays.id'), nullable=False)
    o_liwc = Column(Double)
    c_liwc = Column(Double)
    e_liwc = Column(Double)
    a_liwc = Column(Double)
    n_liwc = Column(Double)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    essay = relationship("Essay", back_populates="liwc_analyzation")
