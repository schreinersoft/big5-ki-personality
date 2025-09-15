import fitz  # PyMuPDF
import re
import json
import dateparser

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
    for line in lines:
        line = line.replace("\n", "")
        line = line.strip()

        if not line:
            continue
        # Stop if footnotes are reached (lines starting with a number)
        if re.match(r'^\d+\.', line):
            break

        if line.split(" ")[0] in month_upper:
            # special case, add line with top_date tag
            year = line.split(" ")[-1]
            continue

        if line.split(" ")[0] in weekdays:
            # special case, add line with date in <date> tag
            line = line[line.find(" ")+1:]
            cleaned_lines.append(f"<diarydate>{line} {year}</diarydate>")
            continue

        # Remove page numbers (lines that are just digits)
        if line.strip().isdigit():
            continue

        # replace footnote references in text
        line = re.sub(r'\.\d+', '.', line)

        # replace & with and (used very very often in this text)
        line = line.replace("&", "and")

        line = line.replace("  ", " ")

        if line.endswith("-"):
            # remove umbruch
            line = line[:-1]

        cleaned_lines.append(line)
    
    return cleaned_lines

def parse_letters_from_pdf(pdf_path, start_page, end_page, store_to_database: bool = True):
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
        doc = fitz.open(pdf_path)
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
                    scrape_comment = ""
                     )           

        try:       
            date_match = re.match(r'<diarydate>(.*?)</diarydate>', chunk)

            # The full matched date string
            date_info = date_match.groups()[0].strip()
            # ocr mistakes
            date_info = date_info.replace("l ", "1 ")
            date_info = date_info.replace("j ", "3 ")
        
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
            entry.scrape_comment += f"Unparsable Date"

        chunk = chunk[chunk.find("</diarydate>")+13:].strip()
        # The rest of the chunk is the body of the letter

        entry.scrape_state = 1
        entry.text_raw = chunk

        if store_to_database:
            upsert_corpus_entry(entry)

        extracted_letters.append(entry)

    return extracted_letters

if __name__ == "__main__":
    in_filename = "c:/Temp/thesis/The Diary of Virginia Woolf Volume One 1915-1919.pdf"
    out_filename = "c:/Temp/thesis/The Diary of Virginia Woolf Volume One 1915-1919.json"
    letters = parse_letters_from_pdf(in_filename, 
                                     out_filename, 
                                     start_page=34, 
                                     end_page=350, 
                                     store_to_database = True)

    if letters:
        print(f"Successfully extracted {len(letters)} letters.\n")
            
        try:
            with open(out_filename, "wt", encoding="utf-8") as file:
                for letter in letters:
                    file.write(f"{letter.year}-{letter.month}-{letter.day}")
                    if l
                    file.write(letter.text_raw)
                    file.write("\n\n")
            print(f"Successfully saved extracted data to {out_filename}")
        except Exception as e:
            print(f"Error saving to file: {e}")

