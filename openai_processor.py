from database.base import get_session
from database.Essay import Essay
from database.WangAnalyzation import WangAnalyzation
from database.LIWCAnalyzation import LIWCAnalyzation
from database.OpenAIAnalyzation import OpenAIAnalyzation
from database.MinejAnalyzation import MinejAnalyzation
import openai_classifier




def process_openai(batch_size: int, max_num: int, repeats: int=2, sleep_mode: bool = False, temperature: int = 0.3):
    i = 0
    with get_session() as db:
        while i < (max_num * repeats):
            essays = db.query(Essay)\
                    .outerjoin(OpenAIAnalyzation)\
                    .filter(OpenAIAnalyzation.essay_id.is_(None))\
                    .filter(Essay.id < 10)\
                    .limit(batch_size)\
                    .all()

            if not essays:
                print("DONE! All Essays processed.")
                return
            for essay in essays:
                print(f"Processing Essay {essay.id}...")
                for repeat in range(repeats):
                    print(f"{repeat + 1}. Repeat")
                    try:
                        response, result = openai_classifier.classify(essay.text, temperature=0.0)
                        of1 = result['Intellectual Curiosity']
                        of2 = result['Aesthetic Sensitivity']
                        of3 = result['Creative Imagination']
                        cf1 = result['Organization']
                        cf2 = result['Productiveness']
                        cf3 = result['Responsibility']
                        ef1 = result['Sociability']
                        ef2 = result['Assertiveness']
                        ef3 = result['Energy Level']
                        af1 = result['Compassion']
                        af2 = result['Respectfulness']
                        af3 = result['Trust']
                        nf1 = result['Anxiety']
                        nf2 = result['Depression']
                        nf3 = result['Emotional Volatility']
                        new_openai = OpenAIAnalyzation(
                            essay_id = essay.id,
                            of1 = of1,
                            of2 = of2,
                            of3 = of3,
                            cf1 = cf1,
                            cf2 = cf2,
                            cf3 = cf3,
                            ef1 = ef1,
                            ef2 = ef2,
                            ef3 = ef3,
                            af1 = af1,
                            af2 = af2,
                            af3 = af3,
                            nf1 = nf1,
                            nf2 = nf2,
                            nf3 = nf3,
                            o_openai = (of1 + of2 + of3)/3,
                            c_openai = (cf1 + cf2 + cf3)/3, 
                            e_openai = (ef1 + ef2 + ef3)/3, 
                            a_openai = (af1 + af2 + af3)/3, 
                            n_openai = (nf1 + nf2 + nf3)/3,
                            model = response.model,
                            temperature = 0.0,   # 5-mini only allows 0.0
                            input_tokens = response.usage.completion_tokens,
                            output_tokens = response.usage.prompt_tokens,
                        )
                        db.add(new_openai)
                    except Exception as e:
                        # Store error message
                        new_openai = OpenAIAnalyzation(
                            essay_id = essay.id,
                            error_response = str(e)
                        )
                        db.add(new_openai)
                    finally:
                        i+=1
                db.commit()

                
if __name__ == "__main__":
    process_openai(1, 11, repeats=10)
