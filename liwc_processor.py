from database.base import get_session
from database.Essay import Essay
from database.WangAnalyzation import WangAnalyzation
from database.LIWCAnalyzation import LIWCAnalyzation
from database.OpenAIAnalyzation import OpenAIAnalyzation
from database.MinejAnalyzation import MinejAnalyzation
import liwc_classifier

import json

def process_liwc(batch_size: int, max_num: int):
    i = 0
    with get_session() as db:
        while i < max_num:
            essays = db.query(Essay)\
                    .outerjoin(LIWCAnalyzation)\
                    .filter(LIWCAnalyzation.essay_id.is_(None))\
                    .filter(Essay.id <= 100)\
                    .limit(batch_size)\
                    .all()

            if not essays:
                print("DONE! All Essays processed.")
                return
            for essay in essays:
                print(f"Processing Essay {essay.id}...")
                response, result = liwc_classifier.classify(essay.text)
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
    process_liwc(10, 1)
