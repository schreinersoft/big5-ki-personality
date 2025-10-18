from database import *
import classifier_liwc

import json

def process_liwc(batch_size: int, max_num: int):
    i = 0
    with get_session() as db:
        while i < max_num:
            essays = db.query(Essay)\
                    .outerjoin(LIWCAnalyzation)\
                    .filter(LIWCAnalyzation.essay_id.is_(None))\
                    .limit(batch_size)\
                    .all()

            if not essays:
                print("DONE! All Essays processed.")
                return
            for essay in essays:
                print(f"Processing Essay {essay.id}...")
                response, result = classifier_liwc.classify(essay.text)
                new_liwc = LIWCAnalyzation(
                    essay_id = essay.id,
                    o_liwc = result['O'],
                    c_liwc = result['C'],
                    e_liwc = result['E'],
                    a_liwc = result['A'],
                    n_liwc = result['N'],
                    liwc_all = json.dumps(response)
                )
                db.add(new_liwc)
                i+=1
                db.commit()

                
if __name__ == "__main__":
    process_liwc(20000, 1)
