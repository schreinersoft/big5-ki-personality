import signal
import sys
import logging
from pathlib import Path
from datetime import datetime

from database import *
import openai_classifier

# Configure logging to write to both console and file
logging.basicConfig(
    level=logging.INFO,
    format='[%(asctime)s]  %(message)s',
    handlers=[
        logging.FileHandler(f'{Path(__file__).with_suffix(".log")}', mode='a'),  # Append to file
        logging.StreamHandler()                                             # Console output
    ]
)

max_exceptions = 30

def signal_handler(sig, frame):
    logging.warning('\nYou pressed Ctrl+C! Cleaning up...')
    if db := frame.f_locals.get("db"):
        if entry := frame.get.f_locals.get("entry"):
            entry.scrape_state = entry.scrape_state - 10   
            db.commit()
    logging.info('Cleanup completed. Goodbye!')
    sys.exit(0)

# Register the signal handler
signal.signal(signal.SIGINT, signal_handler)


def make_system_prompt():
    facets = ['Creative Imagination', 'Fantasy', 'Aesthetics', 'Ideas',
              'Productiveness', 'Responsibility', 'Dutifulness', 'Self-Discipline', 'Order',
              'Gregariousness', 'Energy Level', 'Activity', 'Excitement-seeking',
              'Compassion', 'Trust', 'Altruism', 'Modesty', 'Tender-mindedness',
              'Anxiety', 'Depression', 'Self-consciousness', 'Vulnerability']
    instruction = f"""You are an impartial text evaluator. You must analyze the given text according to exactly {len(facets)} independent features. Each feature must be rated with an integer between 1 and 9, where 1 means the lowest intensity of that feature and 9 means the highest intensity of that feature. In your evaluation, you must concentrate primarily on the psychological traits and inner characteristics of the author of the text, rather than on their social interactions.
The features are:"""
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

def process_openai_corpus(max_entries = 1000, repeats: int=3, service_tier: str = "flex"):
    system_prompt = make_system_prompt()
    i = 0
    exc_counter = 0
    while i < (max_entries * repeats) and (exc_counter < max_exceptions):
        with get_session() as db:
            excluded_hashes_query = db.query(OpenAIAnalyzationCorpus.hash)

            entries = db.query(OrwellEntry)\
                .filter(OrwellEntry.hash.is_not(None))\
                .filter(~OrwellEntry.hash.in_(excluded_hashes_query))\
                .limit(1)\
                .all()

            #                 .filter(OrwellEntry.scrape_state < 10)\

            if not entries:
                logging.info("DONE! All Entries processed.")
                return
            for entry in entries:
                logging.info(f"Corpus: Processing Entry {entry.id}... Exception Counter: {exc_counter}")

                # mark for actual processing for parallel processing possibility
                if not entry.scrape_state:
                    entry.scrape_state = 11
                else:
                    entry.scrape_state = entry.scrape_state + 10   
                db.commit()
                db.refresh(entry)

                try:
                    for repeat in range(repeats):
                        logging.info(f"{repeat + 1}. Repeat")
                        new_openai = OpenAIAnalyzationCorpus(
                                repeat = repeat,
                                entry_id = entry.id,
                                hash = entry.hash)

                        response, result = openai_classifier.classify(input_text=entry.text or entry.text_raw, system_prompt=system_prompt, service_tier=service_tier)
                        new_openai.of3b = result['Creative Imagination']
                        new_openai.of1 = result['Fantasy']
                        new_openai.of2 = result['Aesthetics']
                        new_openai.of5 = result['Ideas']

                        new_openai.cf2b = result['Productiveness']
                        new_openai.cf3b = result['Responsibility']
                        new_openai.cf2 = result['Order']
                        new_openai.cf3 = result['Dutifulness']
                        new_openai.cf5 = result['Self-Discipline']

                        new_openai.ef3b = result['Energy Level']
                        new_openai.ef2 = result['Gregariousness']
                        new_openai.ef4 = result['Activity']
                        new_openai.ef5 = result['Excitement-seeking']

                        new_openai.af1b = result['Compassion']
                        new_openai.af1 = result['Trust']
                        new_openai.af3 = result['Altruism']
                        new_openai.af5 = result['Modesty']
                        new_openai.af6 = result['Tender-mindedness']

                        new_openai.nf1 = result['Anxiety']
                        new_openai.nf3 = result['Depression']

                        new_openai.nf4 = result['Self-consciousness']
                        new_openai.nf6 = result['Vulnerability']

                        new_openai.model = response.model
                        new_openai.input_tokens = response.usage.completion_tokens
                        new_openai.output_tokens = response.usage.prompt_tokens
                        new_openai.finished_at = datetime.now()
                        db.add(new_openai)
                except KeyboardInterrupt:
                    if db and entry:
                        entry.scrape_state = entry.scrape_state - 10   
                        db.commit()
                    sys.exit(0)
                except Exception as e:
                    logging.info(f"Error processing on Entry {entry.id}: {e}")
                    exc_counter += 1
                finally:
                    if entry.scrape_state > 10 or entry.scrape_state < 0:
                        # reset scrape state
                        entry.scrape_state = (entry.scrape_state+1000) % 10   
                    db.commit()
                    logging.info("committed!")                    
                    i += 1
                    if exc_counter > max_exceptions:
                        raise Exception(f"Excepetion Counter exceeded {max_exceptions}!")
                        


                
if __name__ == "__main__":
    process_openai_corpus(max_entries=500, repeats=3)




