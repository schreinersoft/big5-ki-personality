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
    o_binary = Column(String(1))
    c_binary = Column(String(1))
    e_binary = Column(String(1))
    a_binary = Column(String(1))
    n_binary = Column(String(1))

    liwc_analyzation = relationship("LIWCAnalyzation", back_populates="essay", cascade="all, delete-orphan")
    wang_analyzation = relationship("WangAnalyzation", back_populates="essay", cascade="all, delete-orphan")
    minej_analyzation = relationship("MinejAnalyzation", back_populates="essay", cascade="all, delete-orphan")
    openai_analyzation = relationship("OpenAIAnalyzation", back_populates="essay", cascade="all, delete-orphan")    
    openai_analyzation_v2 = relationship("OpenAIAnalyzationV2", back_populates="essay", cascade="all, delete-orphan")    