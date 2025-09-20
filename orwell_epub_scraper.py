import os
import re
import hashlib
import dateparser
import tiktoken
tokenizer = tiktoken.encoding_for_model("gpt-5-mini")

from bs4 import BeautifulSoup
from database import *
from database.base import upsert_corpus_entry


# Basic data
author_name='George Orwell'
birth_year = 1903
birth_month = 6
author_sex = "m"
language = 'en'
text_type = "letter"

text_classes = ['noindent', 'indent', 'noindentsp', 'indentsp']  

# Add a method to generate the hash from the 'text' field
# used for referencing analyzed results
def generate_hash(object):
    if object.text:
        object.hash = hashlib.sha256(object.text.encode('utf-8')).hexdigest()
    elif object.text_raw:
        object.hash = hashlib.sha256(object.text_raw.encode('utf-8')).hexdigest()
    elif object.href:
        object.hash = hashlib.sha256(object.href.encode('utf-8')).hexdigest()
    return object


def clean_text_content(element):
    """
    Clean text content from an HTML element, handling footnote references properly.
    
    Args:
        element: BeautifulSoup element
        
    Returns:
        str: Cleaned text content
    """
    # Clone the element to avoid modifying the original
    elem_copy = BeautifulSoup(str(element), 'html.parser')
    
    # Remove footnote references (sup tags with links)
    for sup in elem_copy.find_all('sup'):
        sup.decompose()
    
    # Get text and clean up spacing
    text = elem_copy.get_text()
    
    # Clean up extra spaces and punctuation issues
    text = re.sub(r'\s+', ' ', text)  # Multiple spaces to single space
    text = re.sub(r'\s*,\s*,\s*', ', ', text)  # Fix double commas
    text = re.sub(r'\s*:\s*,\s*', ': ', text)  # Fix colon-comma issues
    text = text.strip()
    
    return text

def scrape_multiple_letters(html_content, store_to_database, file_name):
    """
    Scrapes multiple letters from HTML content.
    
    Args:
        html_content (str): HTML content to parse
        
    Returns:
        list: List of dictionaries, each containing a letter's data
    """
    soup = BeautifulSoup(html_content, 'html.parser')
    letters = []
    
    # Find all letter-title elements as starting points
    title_elements = soup.find_all('p', class_='letter-title')
    
    entry = None

    for i, title_elem in enumerate(title_elements):

        entry = OrwellEntry(
            author_name=author_name,
            language = language,
            author_sex = author_sex,
            text_type = text_type,
            scrape_comment = ""
        )
        # start with title = greeting
        first_line = title_elem.get_text(strip=True).replace("*", "")
        if first_line.startswith("A letter"):
            continue
        if first_line.startswith("Extract from"):
            continue
        text = []
        # Find  next elements after title
        current = title_elem.find_next_sibling()
        found_date = False
        
        while current:
            # if not current.name == 'p':
            #     current = current.find_next_sibling()
            #     continue
                
            class_names = current.get('class', [])

            if not class_names:
                continue
            
            class_name = class_names[0]

            if 'letter-date' in class_names:
                try:
                    if not found_date:
                        datetext = current.get_text(strip=True)
                        if datetext.startswith("[X"):
                            break
                        dateandtime = dateparser.parse(datetext)
                        entry.day = dateandtime.day
                        entry.month = dateandtime.month
                        year = dateandtime.year
                        if year > 2000:
                            entry.year = year - 100
                        else:
                            entry.year = year
                        entry.author_age = entry.year - birth_year

                        found_date = True
                    else:
                        # Second letter-date means end of this letter
                        break
                except:
                    entry.scrape_comment += f"{datetext} : Unparsable Date in {file_name}"
                    
                    
            elif class_name in text_classes:
                text_content = clean_text_content(current)

                if text_content:
                    text.append(text_content)
                        
            elif 'letter-title' in class_names:
                # Found next letter title, stop here
                break
            
            current = current.find_next_sibling()
        
        entry.text_raw = ' '.join(text)
        entry.text_raw_numtokens = len(tokenizer.encode(entry.text_raw))
        letters.append(entry)
        if store_to_database:
            try:
                upsert_corpus_entry(entry)
            except Exception as e:
                print(f"Unable to commit {entry.hash}: {e}")

    
    return letters

def printout_letters(letters, out_filename):
    with open(out_filename, "wt", encoding="utf-8") as file:
        for letter in [l for l in letters if l.scrape_comment]:
            file.write(f"{letter.year}-{letter.month}-{letter.day}. Errors: {letter.scrape_comment}\n")
            
        for letter in letters:
            file.write(f"{letter.year}-{letter.month}-{letter.day}. Errors: {letter.scrape_comment}\n")
            file.write(letter.text_raw)
            file.write("\n\n")


if __name__ == "__main__":
    in_dir = "C:/temp/thesis/Orwell - A life in letters\Text"
    out_filename = "C:/temp/thesis/Orwell - A life in letters\scaped.txt"

    all_results = []

    if False:
        for file_name in os.listdir(in_dir):
            file_path = os.path.join(in_dir, file_name)
            with open(file_path, "r", encoding="utf-8", errors="ignore") as file:
                data = file.read()
                result = scrape_multiple_letters(data, store_to_database = True, file_name=file_name)
                all_results.extend(result)

        printout_letters(all_results, out_filename)


    # REFRESH hashes & tokens
    store_to_database = True
    bulk = 100
    i = 0
    if store_to_database:
        with get_session() as db:
            entries = db.query(OrwellEntry)\
                    .filter(OrwellEntry.scrape_state<3)\
                    .all()
        
            for entry in entries:
                #entry.text = basic_clean(entry.text_raw)
                if not entry.month:
                    entry.month = 6
                    entry.scrape_comment += "set to month 6 "
                entry.author_age = int(((entry.year*12)+entry.month - (birth_year*12)+birth_month) / 12)
                #oldhash = str(entry.hash)
                entry = generate_hash(entry) # Ensure hash is generated
                # if oldhash == entry.hash:
                #     # nothing to update
                #     continue
                entry.text_raw_numtokens = len(tokenizer.encode(entry.text_raw))
                if entry.text:
                    entry.text_numtokens = len(tokenizer.encode(entry.text))
                entry.scrape_state = 2
                i += 1
                if i >= bulk:
                    print(f"commiting bulk until entry: {entry.id}")
                    db.commit()
                    i = 0
            db.commit()


                

