import json
import time
from dotenv import load_dotenv
load_dotenv()
from google import genai
from processor_openai_v3 import make_system_prompt

client = genai.Client()

def classify(input_text: str = "", system_prompt: str = "", temperature: float = 0.0, model: str= "gemini-2.5-flash") -> dict:
    # Call API
    attempt = 1
    prompt = f"{system_prompt}\nTEXT TO ANALYZE: {input_text}"
    while attempt <= 10:
        try:
            response = client.models.generate_content(
                model=model,
                contents=prompt,
                config={
                    "response_mime_type": "application/json",
                    "temperature": temperature
                    }
                )
            return response.model_dump(), json.loads(response.text)
        except Exception as e:
            print(f"Failed on attempt {attempt}, retrying. Error: {e}")
            time.sleep(attempt * 5)
            attempt += 1
    raise
if __name__ == "__main__":
    sample_text = "I really don’t like your idea, it’s ridiculous and a waste of time."
    response, result = classify(sample_text, system_prompt=make_system_prompt())
    print(json.dumps(result))
    print(json.dumps(response))
