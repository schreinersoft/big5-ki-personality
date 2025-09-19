from sqlalchemy import Column, Integer, String
from database.base import Base

class NoiseEntry(Base):
    __tablename__ = 'noise'
    # Modify 'hash' to be the sole primary key
    id = Column(Integer, primary_key=True, autoincrement=True, nullable=False)
    text = Column(String)
    text_numtokens = Column(Integer)
    author_name = Column(String)
    hash = Column(String, unique=True)
    
