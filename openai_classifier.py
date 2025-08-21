from openai import OpenAI
import json
import time
from dotenv import load_dotenv
load_dotenv()

client = OpenAI()

# | Temperature | Effect                                                                    | Recommendation                             |
# | ----------- | ------------------------------------------------------------------------- | ------------------------------------------ |
# | 0.0 – 0.2   | Almost deterministic; same text gives nearly identical ratings every time | ✅ Best for bulk scoring                    |
# | 0.3 – 0.5   | Slight variation, still stable                                            | Acceptable if some minor variation is okay |
# | 0.6 – 1.0   | More randomness; ratings may vary across runs                             | ❌ Not recommended for numeric scoring      |
# | >1.0        | Highly unpredictable                                                      | ❌ Avoid                                    |

def classify(input_text: str, system_prompt: str, service_tier: str = "flex", temperature: int = 0.0) -> dict:
    # Call API
    attempt = 3
    while attempt >= 0:
        try:
            response = client.chat.completions.create(
                model="gpt-5-mini", # !!!XXX
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": f"TEXT TO EVALUATE: <<<{input_text}>>>"},
                ],
                service_tier=service_tier,
                #temperature=temperature,   # nicht bei gpt-5-mini
                response_format={ "type": "json_object" }  # ensures JSON output
            )
            json_response = response.choices[0].message.content
            return response, json.loads(json_response)
        except Exception as e:
            print(f"Failed on attempt {attempt}, retrying. Error: {e}")
            time.sleep(3)
            attempt -= 1
    raise

def classify_by_function(input_text: str, function: str = "", service_tier: str = "flex", temperature: int = 0.0) -> dict:
    # Call API
    attempt = 1
    while attempt <= 5:
        try:
            response = client.chat.completions.create(
                model="gpt-5-mini", # !!!XXX
                messages=[
                    {"role": "system", "content": "You are an expert text evaluator."},
                    {"role": "user", "content": f"TEXT TO EVALUATE: {input_text}"},
                ],
                functions=[function],
                service_tier=service_tier,
                #temperature=temperature,   # nicht bei gpt-5-mini
                #response_format={ "type": "json_object" },  # ensures JSON output
                function_call={"name":"score_30_facets"}
            )
            fn_call = response.choices[0].message.function_call
            features = fn_call.arguments
            return response, json.loads(features)
        except Exception as e:
            print(f"Failed on attempt {attempt}, retrying. Error: {e}")
            time.sleep(3)
            attempt += 1
    raise


if __name__ == "__main__":
    sample_text = "I really don’t like your idea, it’s ridiculous and a waste of time."
    response, result = classify(sample_text)
    print(json.dumps(result))
    print(json.dumps(response))
