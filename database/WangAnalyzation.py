import hashlib
from datetime import datetime
from sqlalchemy import Column, Integer, String, Double, ForeignKey, DateTime
from sqlalchemy.orm import relationship
from database.base import Base


class WangAnalyzation(Base):
    __tablename__ = 'wang_analyzation'
    id = Column(Integer, primary_key=True, autoincrement=True)
    essay_id = Column(Integer, ForeignKey('essays.id'), nullable=False)
    o_wang = Column(Double)
    c_wang = Column(Double)
    e_wang = Column(Double)
    a_wang = Column(Double)
    n_wang = Column(Double)
    classification_type = Column(String)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    essay = relationship("Essay", back_populates="wang_analyzation")
