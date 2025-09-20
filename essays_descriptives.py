import math
import tiktoken
import numpy as np
tokenizer = tiktoken.encoding_for_model("gpt-5-mini")

from database import *

# REFRESH hashes & tokens
store_to_database = True
bulk = 100
i = 0
if store_to_database:
    with get_session() as db:
        entries = db.query(Essay)\
                .filter(Essay.id <= 250)\
                .all()
        sum_tokens = []
        for entry in entries:
            sum_tokens.append(len(tokenizer.encode(entry.text)))

        sum_array = np.array(sum_tokens)
        count = len(entries)

        print(f"Anzahl: {sum_array.size}, M: {np.mean(sum_array)}, Median: {np.median(sum_array)} ")


                

