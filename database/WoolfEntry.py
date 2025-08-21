import hashlib
from sqlalchemy import Column, Integer, String
from database.base import Base

class WoolfEntry(Base):
    __tablename__ = 'woolf'
    # Modify 'hash' to be the sole primary key
    id = Column(Integer, primary_key=True, autoincrement=True, nullable=False)
    hash = Column(String, unique=True)
    href = Column(String)
    title = Column(String)
    language = Column(String)
    author_name = Column(String)
    author_age = Column(Integer)
    author_sex = Column(String) # Assuming 'f' or 'm' fits in a String
    receiver_name = Column(String)
    receiver_age = Column(Integer)
    receiver_sex = Column(String) # Assuming 'f' or 'm' fits in a String
    text_raw = Column(String)
    text_raw_numtokens = Column(Integer)
    text = Column(String)
    text_numtokens = Column(Integer)
    # Remove the old 'text_hash' field
    # text_hash = Column(String)
    year = Column(Integer)
    month = Column(Integer)
    day = Column(Integer)
    text_type = Column(String) # Assuming 'diary' or 'letter' fits in a String
    scrape_state = Column(Integer) # Assuming 0 for failure, 1 for raw text, 2 for complete text
    scrape_comment = Column(String) # For problem log while parsing


    # Add a method to generate the hash from the 'text' field
    # used for referencing analyzed results
    def generate_hash(self):
        if self.text:
            self.hash = hashlib.sha256(self.text.encode('utf-8')).hexdigest()
        elif self.text_raw:
            self.hash = hashlib.sha256(self.text_raw.encode('utf-8')).hexdigest()
        elif self.href:
            self.hash = hashlib.sha256(self.href.encode('utf-8')).hexdigest()
        # Remove the line below
        # self.hash = None