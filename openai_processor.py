from database import *

import openai_classifier

def process_openai(batch_size: int, max_num: int, repeats: int=2, temperature: int = 0.0, service_tier: str = "flex"):
    with open("prompts\Prompt1_BFI2.txt", "rt") as file:
        system_prompt = file.read()
    i = 0
    with get_session() as db:
        while i < (max_num * repeats):
            essays = db.query(Essay)\
                    .outerjoin(OpenAIAnalyzation)\
                    .filter(OpenAIAnalyzation.essay_id.is_(None))\
                    .filter(Essay.id <=250)\
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
                        response, result = openai_classifier.classify(input_text=essay.text, system_prompt=system_prompt, temperature=0.0, service_tier=service_tier)
                        new_openai = OpenAIAnalyzation(
                            essay_id = essay.id,
                            of1 = result['Intellectual Curiosity'],
                            of2 = result['Aesthetic Sensitivity'],
                            of3 = result['Creative Imagination'],
                            cf1 = result['Organization'],
                            cf2 = result['Productiveness'],
                            cf3 = result['Responsibility'],
                            ef1 = result['Sociability'],
                            ef2 = result['Assertiveness'],
                            ef3 = result['Energy Level'],
                            af1 = result['Compassion'],
                            af2 = result['Respectfulness'],
                            af3 = result['Trust'],
                            nf1 = result['Anxiety'],
                            nf2 = result['Depression'],
                            nf3 = result['Emotional Volatility'],
                            model = response.model,
                            temperature = temperature,
                            input_tokens = response.usage.completion_tokens,
                            output_tokens = response.usage.prompt_tokens,
                        )
                        db.add(new_openai)
                    except KeyError:
                        # technical error on my side: store everything you have before exiting
                        db.commit()
                        raise
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
    process_openai(1, 200, repeats=10) #, service_tier="default")
