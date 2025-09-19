from openai import OpenAI
import json
import time
from dotenv import load_dotenv
load_dotenv()

client = OpenAI()

from tenacity import (
    retry,
    stop_after_attempt,
    wait_random_exponential,
)  # for exponential backoff

sleep_time = 30

@retry(wait=wait_random_exponential(min=1, max=60), stop=stop_after_attempt(6))
def classify(input_text: str, system_prompt: str, service_tier: str = "flex") -> dict:
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


def classify_old(input_text: str, system_prompt: str, service_tier: str = "flex", temperature: int = 0.0) -> dict:
    # Call API
    attempt = 1
    while attempt <= 10:
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
            print(f"Failed on attempt {attempt}, retrying in {sleep_time} seconds. Error: {e}")
            time.sleep(attempt * sleep_time)
            attempt += 1
    raise Exception(f"Too many retries, aborting.")



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
