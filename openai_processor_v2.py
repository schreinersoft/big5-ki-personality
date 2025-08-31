from database import *
import openai_classifier

# XXX publication: switch v2 and v3 everywhere!!!!

def make_prompt():
    features = ['Fantasy', 'Aesthetics', 'Feelings', 'Actions', 'Ideas', 'Values',
                'Competence', 'Order', 'Dutifulness', 'Achievement striving', 'Self-Discipline', 'Deliberation',
                'Warmth','Gregariousness','Assertiveness','Activity','Excitement seeking','Positive emotions',
                'Trust','Straightforwardness','Altruism','Compliance','Modesty','Tender-mindedness',
                'Anxiety','Angry hostility','Depression','Self-consciousness','Impulsiveness','Vulnerability']
    description = "Analyzes a given text for psychological personality traits. Evaluate the text across exactly 30 independent personality features, each rated on a scale from 1 (lowest intensity or presence) to 8 (highest intensity or presence). The ratings are based solely on the content and style of the provided text, with no external assumptions or moral judgments. Treat each feature as independent; ratings for one feature should not influence another. The output should include all 30 features with their respective scores."
    props = {f: {"type": "integer", "minimum": 1, "maximum": 8} for f in features}
    
    return {
        "name": "score_30_facets",
        "description": description,
        "parameters": {
            "type": "object",
            "properties": props,
            "required": features
        }
    }


def process_openai_v3(batch_size: int, max_num: int, repeats: int=2, service_tier: str = "flex", temperature: int = 0.0):
    
    with open("prompts\Prompt2_NEO_PI_R.txt", "rt") as file:
        system_prompt = file.read()
    i = 0
    with get_session() as db:
        while i < (max_num * repeats):
            essays = db.query(Essay)\
                    .outerjoin(OpenAIAnalyzationV2)\
                    .filter(OpenAIAnalyzationV2.essay_id.is_(None))\
                    .filter(Essay.id <=100)\
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
                        response, result = openai_classifier.classify_by_function(input_text=essay.text, function=make_function_schema(), service_tier=service_tier, temperature=0.0)
                        new_openai = OpenAIAnalyzationV2(
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
                        new_openai = OpenAIAnalyzationV2(
                            essay_id = essay.id,
                            error_response = str(e)
                        )
                        db.add(new_openai)
                    finally:
                        i+=1
                db.commit()

                
if __name__ == "__main__":
    process_openai_v3(5, 50, repeats=5) #, service_tier="default")

