from sqlalchemy import Column, Integer, String
from sqlalchemy.orm import relationship
from database.base import Base

# The complete original Essay data (will be populated by R script)

class Essay(Base):
    __tablename__ = 'essays'
    # Modify 'hash' to be the sole primary key
    id = Column(Integer, primary_key=True)
    author = Column(String)
    text = Column(String)
    o_binary = Column(Integer)
    c_binary = Column(Integer)
    e_binary = Column(Integer)
    a_binary = Column(Integer)
    n_binary = Column(Integer)

    liwc_analyzation = relationship("LIWCAnalyzation", back_populates="essay", cascade="all, delete-orphan")
    wang_analyzation = relationship("WangAnalyzation", back_populates="essay", cascade="all, delete-orphan")