import fitz  # PyMuPDF
import re
import json
import dateparser

from database import *
from database.base import upsert_corpus_entry


# Walter Benjamin Basic Data
author_name='Walter Benjamin'
birth_year = 1892
author_sex = "m"
language = 'de'
text_type = "letter"

in_filename = "c:/temp/thesis/walter-benjamin-gesammelte-briefe-baende_1_bis_2_027-874.pdf"
out_filename = "c:/temp/thesis/walter-benjamin-gesammelte-briefe-baende_1_bis_2_027-874.json"



def clean_text(text):
    """
    Removes page numbers and footnotes from the text of a single letter.
    - Page numbers are assumed to be lines containing only digits.
    - Footnotes are assumed to start with a digit, followed by a space,
      and are typically at the end of the text sections.
    """
    lines = text.strip().split('\n')
    cleaned_lines = []
    for line in lines:
        # Remove page numbers (lines that are just digits)
        line = line.replace("\n", "")
        if line.strip().isdigit():
            continue
        # Remove footnotes (lines starting with a number, but not containing "an", "!" sometimes an ocr problem)
        if re.match(r'^\d+\s.*', line.strip()) or line.startswith("!"):
            if not ("an" in line.lower() or ")" in line.lower()):
                # only footnotes from here, break
                break
        if stripped:= line.strip():
            if (stripped.rfind("-") is (len(stripped)-1)):
                # remove umbruch
                stripped = stripped[:-1]
            cleaned_lines.append(stripped+" ")
    
    return ''.join(cleaned_lines).strip()

def parse_letters_from_pdf(pdf_path, store_to_database: bool = True):
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
    for page in doc:
        full_text += page.get_text("text")
        full_text += "\n" # Add a newline to separate page content

    # Split the full text into individual letters.
    # Each letter starts with a pattern like "1 An Herbert Belmore".
    # We use a positive lookahead (?=...) to keep the delimiter.
    letter_chunks = re.split(r'(?=\n\d+\s+An\s)', full_text)

    extracted_letters = []
    
    # Regex to find the date, which can be in various formats
    # e.g., "[Vaduz], 15. 7. 10" or "Weggis, den 18. 7. 11" or "Wengen, 24. Juli 1911"
    date_pattern = re.compile(
        r"^(\d+)(.*?)(\d+)$",
        re.MULTILINE
    )

    for chunk in letter_chunks:
        entry = BenjaminEntry(
                    author_name=author_name,
                    language = language,
                    author_sex = author_sex,
                    text_type = text_type,
                    scrape_comment = ""
                     )           

        if not chunk.strip():
            continue

        lines = chunk.strip().split('\n')
        
        # The first line should contain the receiver info
        receiver_line = lines.pop(0).strip()
        try:
            receiver = receiver_line[receiver_line.lower().find("an ")+3:].strip()
            entry.receiver_name = receiver
        except:
            entry.scrape_comment += f"Unclear receiver line: {receiver_line}"
        
        # second line should be the date
        date_line = lines.pop(0).strip()
        try:       
            date_match =re.search(r"\d.*\d", date_line)
            
            # The full matched date string
            date_info = date_match.group(0).strip()
            if date_info[-3:] == "101":
                # correction in one case
                date_info = date_info[:-1]
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


        # The rest of the chunk is the body of the letter
        body_text = '\n'.join(lines)

        # Clean the extracted content to remove footnotes and page numbers
        cleaned_content = clean_text(body_text)

        entry.scrape_state = 1
        entry.text_raw = cleaned_content

        if store_to_database:
            upsert_corpus_entry(entry)

        extracted_letters.append({
            "receiver": receiver,
            "date": date_info,
            "text": cleaned_content
        })

    return extracted_letters

if __name__ == "__main__":
    letters = parse_letters_from_pdf(in_filename, store_to_database = False)

    if letters:
        print(f"Successfully extracted {len(letters)} letters.\n")
        # Print the extracted data for each letter
        for i, letter in enumerate(letters, 1):
            print("-" * 40)
            print(f"LETTER {i}")
            print(f"Receiver: {letter['receiver']}")
            print(f"Date: {letter['date']}")
            print("\n--- Text ---\n")
            print(letter['text'])
            print("\n" + "-" * 40 + "\n")
            
        # You can also save this to a JSON file for later use
        try:
            with open(out_filename, "wt", encoding="utf-8") as file:
                json.dump(letters, file, indent=2, ensure_ascii=False)
            print(f"Successfully saved extracted data to {out_filename}")
        except Exception as e:
            print(f"Error saving to JSON file: {e}")

