from textblob import TextBlob
import sys

def read_from_stdin():
    """Liest kompletten Input von stdin"""
    print("Gib Text ein (Ctrl+D/Ctrl+Z zum Beenden):")
    content = sys.stdin.read()
    return content.strip()

while True:
    # Verwendung
    text = read_from_stdin()
    blob = TextBlob(text)
    
    print("Eingegebener Text:")
    print(text)
    print()
    print("Korrigierter Text:")
    print(blob.correct())
    