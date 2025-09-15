import fitz  # PyMuPDF
import re
import json
import string
import dateparser
import tiktoken
tokenizer = tiktoken.encoding_for_model("gpt-5-mini")

from database import *
from database.base import upsert_corpus_entry


# Walter Benjamin Basic Data
author_name='Virginia Woolf'
birth_year = 1882
author_sex = "f"
language = 'en'
text_type = "diary"

weekdays = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
month = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
month_upper = [m.upper() for m in month]

def clean_page(text):
    """
    Removes page numbers and footnotes from the text of a single letter.
    - Page numbers are assumed to be lines containing only digits.
    - Footnotes are assumed to start with a digit, followed by a space,
      and are typically at the end of the text sections.
    - TOP line can be removed, because it only contains the actual date
    """
    lines = text.strip().split('\n')
    year = "2015"  # default year, will be updated when found

    cleaned_lines = []
    # stop if too few lines
    if len(lines) < 2:
        return cleaned_lines

    for i, line in enumerate(lines):
        line = line.replace("\n", "")
        line = line.strip()

        if not line:
            continue

        # do basic cleaning
        line = basic_clean(line)

        # Stop if footnotes are reached (lines starting with a number)
        if re.match(r'^\d+\.', line):
            break

        # Remove page numbers (lines that are just digits)
        if line.strip().isdigit():
            continue

        if line.split(" ")[0] in month_upper or i == 0:
            # special case, add line with top_date tag, this ist normally the first line
            line_melted = line.replace(" ", "")
            if len(line_melted) > 4 and line_melted[-4].isdigit():
                year = line_melted[-4:]
            else:
                year = line.split(" ")[-1]
            continue

        splits = line.split(" ")
        if splits[0] in weekdays and len(splits) < 5:
            # special case, add line with date in <date> tag
            line = line[line.find(" ")+1:]
            cleaned_lines.append(f"<diarydate>{line} {year}</diarydate>")
            continue

        if line.endswith("-"):
            # remove umbruch
            line = line[:-1]

        cleaned_lines.append(line)
    
    return cleaned_lines


printable_pattern = re.compile("[\W_]")
def basic_clean(text: str):
    # ocr problems
    text = text.replace("·", ".")  
    text = text.replace("®", "")
    text = text.replace(r"\V", "W")
    text = text.replace(r"\v", "W")
    

    # remove footnote references in text
    text = re.sub(r'\.\d+', '.', text)
    text = re.sub(r'\;\d+', ';', text)
    text = re.sub(r'\"\d+', '"', text)
    
    # remove everything that is not char-ish
    text = ''.join([ch for ch in text if ch in string.printable])

    # replace & with and (used very very often in this text)
    text = text.replace("&", "and")


    # replace long distances
    text = text.replace("  ", " ")

    # remove square braces
    text = text.replace("[", "")
    text = text.replace("]", "")

    

    return text 



def parse_letters_from_pdf(in_path, start_page, end_page, store_to_database: bool = True):
    """
    Parses a PDF file to extract individual letters, including their
    receiver, date, and cleaned text content.

    Args:
        pdf_path (str): The file path to the PDF.

    Returns:
        list: A list of dictionaries, where each dictionary represents
              a letter with its metadata and content.
    """
    try:
        doc = fitz.open(in_path)
    except Exception as e:
        print(f"Error opening or reading PDF file: {e}")
        return []

    # Concatenate all text from the PDF into a single string
    full_text = ""
    actual_page = 0

    for page in doc:
        actual_page += 1
        if actual_page < start_page:
            # skip first pages
            continue
        if actual_page > end_page:
            # stop after last page
            break
        page_cleaned_lines = clean_page(page.get_text())
        full_text += " ".join(page_cleaned_lines)

    # Split the full text into individual letters by <diarydate> tags
    # We use a positive lookahead (?=...) to keep the delimiter.
    letter_chunks = re.split(r'(?=<diarydate>.*?</diarydate>)', full_text)

    extracted_letters = []
    

    for chunk in letter_chunks:
        if not chunk.strip():
            # empty content
            continue
        if not chunk.startswith("<diarydate>"):
            # skip content before first letter
            continue

        entry = WoolfEntry(
                    author_name=author_name,
                    language = language,
                    author_sex = author_sex,
                    text_type = text_type,
                    scrape_comment = "",
                    scrape_state = 1
                     )           

        try:       
            date_match = re.match(r'<diarydate>(.*?)</diarydate>', chunk)

            # The full matched date string
            date_info = date_match.groups()[0].strip()
            # ocr mistakes
            date_info = date_info.replace("l ", "1 ")
            date_info = date_info.replace("j ", "3 ")
            date_info = date_info.replace("Apri1", "April")
            date_info = date_info.replace("i5 ", "15 ")
            date_info = date_info.replace("17 ", "ly ")
            date_info = date_info.replace("z5 ", "25 ")

            
        
            # Remove the date line from the letter's main content
            dateandtime = dateparser.parse(date_info)
            entry.day = dateandtime.day
            entry.month = dateandtime.month
            year = dateandtime.year
            if year > 2000:
                entry.year = year - 100
            else:
                entry.year = year
        except:
            entry.scrape_comment += f"'{date_info}': Unparsable date"
            entry.scrape_state = 0

        # The rest of the chunk is the body of the letter
        chunk = chunk[chunk.find("</diarydate>")+13:].strip()
        entry.text_raw = chunk
        entry.text_raw_numtokens = len(tokenizer.encode(chunk))

        if store_to_database:
            upsert_corpus_entry(entry)

        extracted_letters.append(entry)

    return extracted_letters

if __name__ == "__main__":
    # VOLUME ONE
    scrape = False
    store_to_database = False # !!! DONE!
    in_filename = "c:/Temp/thesis/The Diary of Virginia Woolf Volume One 1915-1919.pdf"
    out_filename = "c:/Temp/thesis/The Diary of Virginia Woolf Volume One 1915-1919.txt"
    if scrape:
        letters = parse_letters_from_pdf(in_filename, 
                                        start_page=34, 
                                        end_page=350, 
                                        store_to_database = store_to_database)

        if letters:
            print(f"Successfully extracted {len(letters)} letters.\n")
                
            try:
                with open(out_filename, "wt", encoding="utf-8") as file:
                    for letter in letters:
                        file.write(f"{letter.year}-{letter.month}-{letter.day}\n")
                        file.write(letter.text_raw)
                        file.write("\n\n")
                print(f"Successfully saved extracted data to {out_filename}")
            except Exception as e:
                print(f"Error saving to file: {e}")

    # VOLUME TWO
    scrape = False
    store_to_database = False # !!! DONE
    in_filename = "c:/Temp/thesis/The Diary of Virginia Woolf Volume Two 1920-1924.pdf"
    out_filename = "c:/Temp/thesis/The Diary of Virginia Woolf Volume Two 1920-1924.txt"
    if scrape:
        letters = parse_letters_from_pdf(in_filename, 
                                        start_page=19, 
                                        end_page=344, 
                                        store_to_database = store_to_database)

        if letters:
            print(f"Successfully extracted {len(letters)} letters.\n")
                
            try:
                with open(out_filename, "wt", encoding="utf-8") as file:
                    for letter in letters:
                        file.write(f"{letter.year}-{letter.month}-{letter.day}\n")
                        file.write(letter.text_raw)
                        file.write("\n\n")
                print(f"Successfully saved extracted data to {out_filename}")
            except Exception as e:
                print(f"Error saving to file: {e}")


    # VOLUME THREE
    scrape = False
    store_to_database = False # !!!DONE
    in_filename = "c:/Temp/thesis/The diary of Virginia Woolf Volume Three 1925-1930.pdf"
    out_filename = "c:/Temp/thesis/The diary of Virginia Woolf Volume Three 1925-1930.txt"
    if scrape:
        letters = parse_letters_from_pdf(in_filename, 
                                        start_page=19, 
                                        end_page=344, 
                                        store_to_database = store_to_database)

        if letters:
            print(f"Successfully extracted {len(letters)} letters.\n")
                
            try:
                with open(out_filename, "wt", encoding="utf-8") as file:
                    for letter in letters:
                        file.write(f"{letter.year}-{letter.month}-{letter.day}\n")
                        file.write(letter.text_raw)
                        file.write("\n\n")
                print(f"Successfully saved extracted data to {out_filename}")
            except Exception as e:
                print(f"Error saving to file: {e}")


    # VOLUME FOUR
    scrape = False
    store_to_database = False # DONE!!
    in_filename = "c:/Temp/thesis/The Diary of Virginia Woolf Volume Four 1931-1935.pdf"
    out_filename = "c:/Temp/thesis/The Diary of Virginia Woolf Volume Four 1931-1935.txt"
    if scrape:
        letters = parse_letters_from_pdf(in_filename, 
                                        start_page=18, 
                                        end_page=376, 
                                        store_to_database = store_to_database)

        if letters:
            print(f"Successfully extracted {len(letters)} letters.\n")
                
            try:
                with open(out_filename, "wt", encoding="utf-8") as file:
                    for letter in letters:
                        file.write(f"{letter.year}-{letter.month}-{letter.day}\n")
                        file.write(letter.text_raw)
                        file.write("\n\n")
                print(f"Successfully saved extracted data to {out_filename}")
            except Exception as e:
                print(f"Error saving to file: {e}")





    # VOLUME FIVE
    scrape = False
    store_to_database = False # DONE!!
    in_filename = "c:/Temp/thesis/The Diary of Virginia Woolf Volume Five 1936-1941.pdf"
    out_filename = "c:/Temp/thesis/The Diary of Virginia Woolf Volume Five 1936-1941.txt"
    if scrape:
        letters = parse_letters_from_pdf(in_filename, 
                                        start_page=19, 
                                        end_page=375, 
                                        store_to_database = store_to_database)

        if letters:
            print(f"Successfully extracted {len(letters)} letters.\n")
                
            try:
                with open(out_filename, "wt", encoding="utf-8") as file:
                    for letter in letters:
                        file.write(f"{letter.year}-{letter.month}-{letter.day}\n")
                        file.write(letter.text_raw)
                        file.write("\n\n")
                print(f"Successfully saved extracted data to {out_filename}")
            except Exception as e:
                print(f"Error saving to file: {e}")



    # REFRESH hashes & tokens
    store_to_database = True
    bulk = 100
    i = 0
    with get_session() as db:
        entries = db.query(WoolfEntry)\
                .all()
        
        for entry in entries:
            entry.text = basic_clean(entry.text_raw)

            if store_to_database:
                oldhash = str(entry.hash)
                entry.generate_hash() # Ensure hash is generated
                if oldhash == entry.hash:
                    # nothing to update
                    continue
                entry.text_raw_numtokens = len(tokenizer.encode(entry.text_raw))
                entry.text_numtokens = len(tokenizer.encode(entry.text))
                entry.scrape_state = 2
                i += 1
                if i >= bulk:
                    print(f"commiting bulk until entry: {entry.id}")
                    db.commit()
                    i = 0
        if store_to_database:
            db.commit()


                

