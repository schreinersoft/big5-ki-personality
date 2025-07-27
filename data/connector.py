import hashlib
from sqlalchemy import create_engine, Column, Integer, String, Date, exists
from sqlalchemy.orm import sessionmaker, Session, declarative_base

from typing import Optional
import requests
from bs4 import BeautifulSoup
from pydantic import BaseModel
from typing import Optional, Literal
import dateparser
import re
import os
from dotenv import load_dotenv

from datamodels.BenjaminEntry import BenjaminEntry, Base

load_dotenv()

# Database init and upsert method
DATABASE_URL = os.environ.get("CONNECTION_STRING")


# Create the table in the database
engine = create_engine(str(os.environ.get("CONNECTION_STRING")))
Base.metadata.create_all(engine)


# Create a session to interact with the database
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()

def upsert_entry(entry):
    """ Adds or alters a Corpus Entry in the database """
    entry.generate_hash() # Ensure hash is generated

    with SessionLocal() as db:  # Use SessionLocal to create the session
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
