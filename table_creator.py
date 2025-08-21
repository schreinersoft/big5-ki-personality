if __name__ == "__main__":
    from database import *
    
    create_tables()

    # with get_session() as db:  # Use SessionLocal to create the session
    #     # essay = Essay(text="Hello World")
    #     # db.add(essay)
    #     # db.commit()

    #     entry: Essay = db.query(Essay).filter(Essay.id == 1).first()

    #     liwc: LIWCAnalyzation = db.query(LIWCAnalyzation).filter(LIWCAnalyzation.essay_id == entry.id).first()
    #     if liwc:
    #         liwc.o_liwc = 8
    #         liwc.c_liwc = 9
    #         liwc.e_liwc = 8
    #         liwc.a_liwc = 9
    #         liwc.n_liwc = 8
    #         db.merge(liwc)
    #     else:
    #         liwc = LIWCAnalyzation(
    #             essay_id =entry.id,
    #             o_liwc = 1,
    #             c_liwc = 2,
    #             e_liwc = 1,
    #             a_liwc = 2,
    #             n_liwc = 1
    #         )
    #         db.add(liwc)
    #     db.commit() 

    #     print(liwc.essay.text)

