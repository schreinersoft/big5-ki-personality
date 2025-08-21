from database import *
import openai_classifier


def make_system_prompt():
    facets = ['Fantasy', 'Aesthetics', 'Feelings', 'Actions', 'Ideas', 'Values',
                'Competence', 'Order', 'Dutifulness', 'Achievement striving', 'Self-Discipline', 'Deliberation',
                'Warmth','Gregariousness','Assertiveness','Activity','Excitement seeking','Positive emotions',
                'Trust','Straightforwardness','Altruism','Compliance','Modesty','Tender-mindedness',
                'Anxiety','Angry hostility','Depression','Self-consciousness','Impulsiveness','Vulnerability']
    instruction = """You are an impartial text evaluator. You must analyze the given text according to exactly 30 independent features. Each feature must be rated with an integer between 1 and 9, where 1 means the lowest intensity of that feature and 9 means the highest intensity of that feature.
The features are:"""
    #props = {f: {"type": "integer", "minimum": 1, "maximum": 8} for f in features}
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

def process_openai_v3(batch_size: int, max_num: int, repeats: int=2, service_tier: str = "flex", temperature: int = 0.0):
    system_prompt = make_system_prompt()
    i = 0
    with get_session() as db:
        while i < (max_num * repeats):
            essays = db.query(Essay)\
                    .outerjoin(OpenAIAnalyzationV3)\
                    .filter(OpenAIAnalyzationV3.essay_id.is_(None))\
                    .filter(Essay.id <=50)\
                    .limit(batch_size)\
                    .all()

            if not essays:
                print("DONE! All Essays processed.")
                return
            for essay in essays:
                print(f"V2: Processing Essay {essay.id}...")
                for repeat in range(repeats):
                    print(f"{repeat + 1}. Repeat")
                    try:
                        response, result = openai_classifier.classify(input_text=essay.text, system_prompt=system_prompt, temperature=0.0, service_tier=service_tier)
                        new_openai = OpenAIAnalyzationV3(
                            essay_id = essay.id,
                            of1 = result['Fantasy'],
                            of2 = result['Aesthetics'],
                            of3 = result['Feelings'],
                            of4 = result['Actions'],
                            of5 = result['Ideas'],
                            of6 = result['Values'],
                            cf1 = result['Competence'],
                            cf2 = result['Order'],
                            cf3 = result['Dutifulness'],
                            cf4 = result['Achievement striving'],
                            cf5 = result['Self-Discipline'],
                            cf6 = result['Deliberation'],
                            ef1 = result['Warmth'],
                            ef2 = result['Gregariousness'],
                            ef3 = result['Assertiveness'],
                            ef4 = result['Activity'],
                            ef5 = result['Excitement seeking'],
                            ef6 = result['Positive emotions'],
                            af1 = result['Trust'],
                            af2 = result['Straightforwardness'],
                            af3 = result['Altruism'],
                            af4 = result['Compliance'],
                            af5 = result['Modesty'],
                            af6 = result['Tender-mindedness'],
                            nf1 = result['Anxiety'],
                            nf2 = result['Angry hostility'],
                            nf3 = result['Depression'],
                            nf4 = result['Self-consciousness'],
                            nf5 = result['Impulsiveness'],
                            nf6 = result['Vulnerability'],
                            model = response.model,
                            temperature = temperature,
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
                        new_openai = OpenAIAnalyzationV3(
                            essay_id = essay.id,
                            error_response = str(e)
                        )
                        db.add(new_openai)
                    finally:
                        i+=1
                db.commit()

                
if __name__ == "__main__":
    process_openai_v3(5, 50, repeats=10, service_tier="default")

