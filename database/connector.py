from base import get_session



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

if __name__ == "__main__":
    from LIWCAnalyzation import LIWCAnalyzationTest
    from Essay import Essay
    from BenjaminEntry import BenjaminEntry
    from base import get_session, create_tables, upsert
    

    create_tables()

    with get_session() as db:  # Use SessionLocal to create the session
        # essay = Essay(text="Hello World")
        # db.add(essay)
        # db.commit()

        entry: Essay = db.query(Essay).filter(Essay.id == 1).first()

        liwc: LIWCAnalyzationTest = db.query(LIWCAnalyzationTest).filter(LIWCAnalyzationTest.essay_id == entry.id).first()
        if liwc:
            liwc.o_liwc = 8
            liwc.c_liwc = 9
            liwc.e_liwc = 8
            liwc.a_liwc = 9
            liwc.n_liwc = 8
            db.merge(liwc)
        else:
            liwc = LIWCAnalyzationTest(
                essay_id =entry.id,
                o_liwc = 1,
                c_liwc = 2,
                e_liwc = 1,
                a_liwc = 2,
                n_liwc = 1
            )
            db.add(liwc)
        db.commit() 

        print(liwc.essay.text)

