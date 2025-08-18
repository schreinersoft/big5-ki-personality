from openai import OpenAI
import json
from dotenv import load_dotenv
load_dotenv()

client = OpenAI()

# | Temperature | Effect                                                                    | Recommendation                             |
# | ----------- | ------------------------------------------------------------------------- | ------------------------------------------ |
# | 0.0 – 0.2   | Almost deterministic; same text gives nearly identical ratings every time | ✅ Best for bulk scoring                    |
# | 0.3 – 0.5   | Slight variation, still stable                                            | Acceptable if some minor variation is okay |
# | 0.6 – 1.0   | More randomness; ratings may vary across runs                             | ❌ Not recommended for numeric scoring      |
# | >1.0        | Highly unpredictable                                                      | ❌ Avoid                                    |

def classify(input_text: str, temperature: int = 0.0) -> dict:
    system_prompt = """You are an impartial text evaluator. You must analyze the given text according to exactly 15 independent features. Each feature must be rated with an integer between 1 and 9, where 1 means the lowest intensity of that feature and 9 means the highest intensity of that feature.
The features are:
1. Sociability
2. Assertiveness
3. Energy Level
4. Compassion
5. Respectfulness
6. Trust
7. Organization
8. Productiveness
9. Responsibility
10. Anxiety
11. Depression
12. Emotional Volatility
13. Intellectual Curiosity
14. Aesthetic Sensitivity
15. Creative Imagination
Your output must ONLY be valid JSON, with no extra commentary or text, in the following format:
{"Sociability": <1-9>,
"Assertiveness": <1-9>,
"Energy Level": <1-9>,
"Compassion": <1-9>,
"Respectfulness": <1-9>,
"Trust": <1-9>,
"Organization": <1-9>,
"Productiveness": <1-9>,
"Responsibility": <1-9>,
"Anxiety": <1-9>,
"Depression": <1-9>,
"Emotional Volatility": <1-9>,
"Intellectual Curiosity": <1-9>,
"Aesthetic Sensitivity": <1-9>,
"Creative Imagination": <1-9>}
Do not output anything else."""

    # Call API
    response = client.chat.completions.create(
        model="gpt-5-mini", # !!!XXX
        messages=[
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": f"TEXT TO EVALUATE: <<<{input_text}>>>"},
        ],
        service_tier="flex"
        #temperature=temperature,
        response_format={ "type": "json_object" }  # ensures JSON output
    )

    # Parse JSON safely
    json_response = response.choices[0].message.content
    return response, json.loads(json_response)


if __name__ == "__main__":
    sample_text = "I really don’t like your idea, it’s ridiculous and a waste of time."
    response, result = classify(sample_text)
    print(json.dumps(result))
    print(json.dumps(response))
