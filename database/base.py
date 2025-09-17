import os
import hashlib

from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
import tiktoken
tokenizer = tiktoken.encoding_for_model("gpt-5-mini")

from dotenv import load_dotenv
load_dotenv()

# Gemeinsame Base für alle Models
Base = declarative_base()

# Database Engine (optional hier)
def get_engine():
    return create_engine(str(os.environ.get("CONNECTION_STRING", "sqlite:///test.db")))

def get_session():
    engine = get_engine()
    Session = sessionmaker(bind=engine)
    return Session()

def create_tables():
    """Erstellt alle Tabellen"""
    engine = get_engine()
    Base.metadata.create_all(engine)

    
def upsert(entry):
    """ Adds or alters an Entry in the database """

    with get_session() as db:  # Use SessionLocal to create the session
      existing_entry = db.query(entry.__class__).filter(entry.__class__.id == entry.id).first()

      if existing_entry:
          # Update existing entry
          # Use entry.__dict__ to get a dictionary of attributes, excluding SQLAlchemy internal attributes
          for key, value in entry.__dict__.items():
              if key != '_sa_instance_state': # Exclude internal SQLAlchemy state
                  setattr(existing_entry, key, value)
          db.commit()
          db.refresh(existing_entry)
          return existing_entry
      else:
          # Add new entr
          db.add(entry)
          db.commit()
          db.refresh(entry)
          return entry

def upsert_corpus_entry(entry):
    """ Adds or alters a Corpus Entry in the database """
    entry = refresh_entry(entry) # Ensure hash is generated & token counted

    with get_session() as db:  # Use SessionLocal to create the session
      existing_entry = db.query(entry.__class__).filter(entry.__class__.hash == entry.hash).first()

      if existing_entry:
          # Update existing entry
          # Use entry.__dict__ to get a dictionary of attributes, excluding SQLAlchemy internal attributes
          for key, value in entry.__dict__.items():
              if key != '_sa_instance_state': # Exclude internal SQLAlchemy state
                  setattr(existing_entry, key, value)
          db.commit()
          db.refresh(existing_entry)
          return existing_entry
      else:
          # Add new entr
          db.add(entry)
          db.commit()
          db.refresh(entry)
          return entry



# Add a method to generate the hash from the 'text' field
# used for referencing analyzed results
def refresh_entry(entry):
    if entry.text:
        entry.hash = hashlib.sha256(entry.text.encode('utf-8')).hexdigest()
    elif entry.text_raw:
        entry.hash = hashlib.sha256(entry.text_raw.encode('utf-8')).hexdigest()
    
    if entry.text:
        entry.text_numtokens = len(tokenizer.encode(entry.text))
    if entry.text_raw:        
        entry.text_raw_numtokens = len(tokenizer.encode(entry.text_raw))


    return entry

    # elif self.href:
    #     self.hash = hashlib.sha256(self.href.encode('utf-8')).hexdigest()
    # Remove the line below
    # self.hash = None