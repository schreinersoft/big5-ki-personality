from database.base import get_session
from database.Essay import Essay
from database.WangAnalyzation import WangAnalyzation
from database.LIWCAnalyzation import LIWCAnalyzation
from database.OpenAIAnalyzation import OpenAIAnalyzation
from database.MinejAnalyzation import MinejAnalyzation



import wang_classifier

def process_wang(batch_size: int, max_num: int):
    i = 0
    with get_session() as db:
        while i < max_num:
            essays = db.query(Essay)\
                    .outerjoin(WangAnalyzation)\
                    .filter(WangAnalyzation.classification_type.is_not("truncated"))\
                    .limit(batch_size)\
                    .all()
#                    .filter(WangAnalyzation.essay_id.is_(None))\



            if not essays:
                print("DONE! All Essays processed.")
                return
            for essay in essays:
                print(f"Processing Essay {essay.id}...")
                # analyzed = wang_classifier.classify_truncated(essay.text)
                # new_wang = WangAnalyzation(
                #     essay_id = essay.id,
                #     o_wang = analyzed['O'],
                #     c_wang = analyzed['C'],
                #     e_wang = analyzed['E'],
                #     a_wang = analyzed['A'],
                #     n_wang = analyzed['N'],
                #     classification_type = "truncated"
                # )
                # db.add(new_wang)

                analyzed = wang_classifier.classify_sliding_windowed(essay.text)
                new_wang = WangAnalyzation(
                    essay_id = essay.id,
                    o_wang = analyzed['O'],
                    c_wang = analyzed['C'],
                    e_wang = analyzed['E'],
                    a_wang = analyzed['A'],
                    n_wang = analyzed['N'],
                    classification_type = "slidingWindow"
                )
                db.add(new_wang)

                i+=1
            db.commit()
                
if __name__ == "__main__":
    process_wang(5, 10000)
