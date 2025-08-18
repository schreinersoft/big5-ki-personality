import hashlib
from datetime import datetime
from sqlalchemy import Column, Integer, String, Double, ForeignKey, DateTime
from sqlalchemy.orm import relationship
from database.base import Base


class MinejAnalyzation(Base):
    __tablename__ = 'minej_analyzation'
    id = Column(Integer, primary_key=True, autoincrement=True)
    essay_id = Column(Integer, ForeignKey('essays.id'), nullable=False)
    o_minej = Column(Double)
    c_minej = Column(Double)
    e_minej = Column(Double)
    a_minej = Column(Double)
    n_minej = Column(Double)
    classification_type = Column(String)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    essay = relationship("Essay", back_populates="minej_analyzation")
