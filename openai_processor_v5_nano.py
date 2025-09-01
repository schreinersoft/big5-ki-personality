from database import *
import openai_classifier_nano
from sqlalchemy import or_

def make_system_prompt():
    facets = ['Creative Imagination', 'Fantasy', 'Aesthetics', 'Ideas',
              'Productiveness', 'Responsibility', 'Dutifulness', 'Self-Discipline',
              'Gregariousness', 'Energy Level', 'Activity', 'Excitement-seeking',
              'Compassion', 'Trust', 'Altruism', 'Tender-mindedness',
              'Anxiety', 'Self-consciousness', 'Vulnerability']
    instruction = """You are an impartial text evaluator. You must analyze the given text according to exactly 19 independent features. Each feature must be rated with an integer between 1 and 9, where 1 means the lowest intensity of that feature and 9 means the highest intensity of that feature. In your evaluation, you must concentrate primarily on the psychological traits and inner characteristics of the author of the text, rather than on their social interactions.
The features are:"""
    numbers = ""
    for i in range(len(facets)):
        numbers += f"{i+1}. {facets[i]}\n"
    numbers = numbers[:-1]
    json = "{"
    for i in range(len(facets)):
        json += f'"{facets[i]}": <1-9>,\n'
    json = json[:-2] # remove last "," an "\n"
    json += "}"
    return f"{instruction}\n{numbers}\nYour output must ONLY be valid JSON, with no extra commentary or text, in the following format:\n{json}\nDo not output anything else."

def process_openai_v3(batch_size: int, max_num: int, repeats: int=2, service_tier: str = "flex"):
    system_prompt = make_system_prompt()
    i = 0
    with get_session() as db:
        while i < (max_num * repeats):
            # essays = db.query(Essay)\
            #         .outerjoin(OpenAIAnalyzationV5)\
            #         .filter(OpenAIAnalyzationV5.essay_id.is_(None))\
            #         .filter(OpenAIAnalyzationV5.model != "gpt-5-mini")\
            #         .filter(Essay.id <=1000)\
            #         .limit(batch_size)\
            #         .all()
            excluded_essays = db.query(OpenAIAnalyzationV5.essay_id)\
                   .filter(OpenAIAnalyzationV5.model.startswith("gpt-5-nano"))\
                   .subquery()
            essays = db.query(Essay)\
                    .outerjoin(OpenAIAnalyzationV5)\
                    .filter(Essay.id <=500)\
                    .filter(~Essay.id.in_(excluded_essays))\
                    .limit(batch_size)\
                    .all()
            if not essays:
                print("DONE! All Essays processed.")
                return
            for essay in essays:
                print(f"V5nano: Processing Essay {essay.id}...")
                for repeat in range(repeats):
                    print(f"{repeat + 1}. Repeat")
                    try:
                        response, result = openai_classifier_nano.classify(input_text=essay.text, system_prompt=system_prompt, temperature=0.0, service_tier=service_tier)
                        new_openai = OpenAIAnalyzationV5(
                            essay_id = essay.id,
                            repeat = repeat,
                            of3b = result['Creative Imagination'],
                            of1 = result['Fantasy'],
                            of2 = result['Aesthetics'],
                            of5 = result['Ideas'],
                            cf2b = result['Productiveness'],
                            cf3b = result['Responsibility'],
                            cf3 = result['Dutifulness'],
                            cf5 = result['Self-Discipline'],
                            ef2 = result['Gregariousness'],
                            ef3b = result['Energy Level'],
                            ef4 = result['Activity'],
                            ef5 = result['Excitement-seeking'],
                            af1b = result['Compassion'],
                            af1 = result['Trust'],
                            af3 = result['Altruism'],
                            af6 = result['Tender-mindedness'],
                            nf1 = result['Anxiety'],
                            nf4 = result['Self-consciousness'],
                            nf6 = result['Vulnerability'],
                            model = response.model,
                            input_tokens = response.usage.completion_tokens,
                            output_tokens = response.usage.prompt_tokens,
                        )
                        
                        db.add(new_openai)
                    except KeyError:
                        db.commit()
                        raise
                    except Exception as e:
                        db.commit()
                        raise(e)
                        # Store error message
                        new_openai = OpenAIAnalyzationV5(
                            essay_id = essay.id,
                            error_response = str(e)
                        )
                        db.add(new_openai)
                    finally:
                        i+=1
                db.commit()

                
if __name__ == "__main__":
    process_openai_v3(1, 40, repeats=4) #, service_tier="flex")




