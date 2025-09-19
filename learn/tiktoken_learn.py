import random
import tiktoken
tokenizer = tiktoken.encoding_for_model("gpt-5-mini")

random.seed(42)

def get_random_token():
    max = tokenizer.max_token_value
    return random.randint(0, max)

for x in range(20):
    random_stream =  [random.randint(0, tokenizer.max_token_value) for x in range(400)]
    print(tokenizer.decode(random_stream))

