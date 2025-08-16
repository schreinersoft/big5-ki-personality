import os

from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

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
    entry.generate_hash() # Ensure hash is generated

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

