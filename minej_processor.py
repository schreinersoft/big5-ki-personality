from database.base import get_session
from database.Essay import Essay
from database.WangAnalyzation import WangAnalyzation
from database.MinejAnalyzation import MinejAnalyzation
from database.LIWCAnalyzation import LIWCAnalyzation
from database.OpenAIAnalyzation import OpenAIAnalyzation
import minej_classifier

import time

def process_minej(batch_size: int, max_num: int, sleep_mode: bool = False):
    i = 0
    with get_session() as db:
        while i < max_num:
            essays = db.query(Essay)\
                    .outerjoin(MinejAnalyzation)\
                    .filter(MinejAnalyzation.essay_id.is_(None))\
                    .limit(batch_size)\
                    .all()

            if not essays:
                print("DONE! All Essays processed.")
                return
            for essay in essays:
                print(f"Processing Essay {essay.id}...")
                analyzed = minej_classifier.classify_truncated(essay.text)
                new_minej = MinejAnalyzation(
                    essay_id = essay.id,
                    o_minej = analyzed['O'],
                    c_minej = analyzed['C'],
                    e_minej = analyzed['E'],
                    a_minej = analyzed['A'],
                    n_minej = analyzed['N'],
                    classification_type = "truncated"
                )
                db.add(new_minej)

                if sleep_mode:
                    # let CPU cool down when used over night
                    time.sleep(5)


                analyzed = minej_classifier.classify_sliding_windowed(essay.text)
                new_minej = MinejAnalyzation(
                    essay_id = essay.id,
                    o_minej = analyzed['O'],
                    c_minej = analyzed['C'],
                    e_minej = analyzed['E'],
                    a_minej = analyzed['A'],
                    n_minej = analyzed['N'],
                    classification_type = "slidingWindow"
                )
                db.add(new_minej)
                if sleep_mode: 
                    # let CPU cool down when used over night
                    
                    time.sleep(5)

                i+=1
            db.commit()
            if sleep_mode:
                # let CPU cool down when used over night
                time.sleep(10)
                
if __name__ == "__main__":
    process_minej(5, 10000, sleep_mode=True)
