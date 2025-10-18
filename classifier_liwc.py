import json
import requests
import os
from dotenv import load_dotenv
load_dotenv()

def classify(input_text: str, test: bool = False) -> dict:
    # Call API
    if test:
        url = 'https://api.receptiviti.com/v2/sandbox/written'
    else:
        url = 'https://api.receptiviti.com/v2/analyze/written'
    api_key = os.environ.get('RECEPTIVITI_API_KEY')
    api_secret = os.environ.get('RECEPTIVITI_SECRET')
    data = [{
        "request_id": "req-1",
        "text": input_text
    }]
    response = requests.post(url=url, data=json.dumps(data), auth=(api_key, api_secret), headers={"Content-Type": "application/json"})
    
    big5 = response.json()['results'][0]['big_5']

    result = {
            "O": big5['openness'],
            "C": big5['conscientiousness'],
            "E": big5['extraversion'],
            "A": big5['agreeableness'],
            "N": big5['neuroticism']
            }

    return response.json(), result


if __name__ == "__main__":
    sample_text = "I really don’t like your idea, it’s ridiculous and a waste of time."
    response, result = classify(sample_text, test=True)
    print(json.dumps(result))
    print(json.dumps(response))
