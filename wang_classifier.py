from transformers import AutoModelForSequenceClassification, AutoTokenizer
import torch

model = AutoModelForSequenceClassification.from_pretrained("KevSun/Personality_LM", ignore_mismatched_sizes=True)
tokenizer = AutoTokenizer.from_pretrained("KevSun/Personality_LM")

def classify(text: str):
    # Encode the text using the same tokenizer used during training
    #encoded_input = tokenizer(new_text, return_tensors='pt', padding=True, truncation=True, max_length=64)
    encoded_input = tokenizer(text, return_tensors='pt', padding=True, truncation=True, max_length=768)   # set length to 768

    model.eval()  # Set the model to evaluation mode

    # Perform the prediction
    with torch.no_grad():
        outputs = model(**encoded_input)

    predictions = torch.nn.functional.softmax(outputs.logits, dim=-1)

    predicted_scores = predictions[0].tolist()

    trait_names = ["agreeableness", "openness", "conscientiousness", "extraversion", "neuroticism"]

    result = {}
    for trait, score in zip(trait_names, predicted_scores):
        result[trait] = score

    return result

if __name__ == "__main__":
    text = "I return to the presidency confident and optimistic that we are at the start of a thrilling new era of national success. A tide of change is sweeping the country, sunlight is pouring over the entire world, and America has the chance to seize this opportunity like never before. But first, we must be honest about the challenges we face. While they are plentiful, they will be annihilated by this great momentum that the world is now witnessing in the United States of America."
    result = classify(text)
    print(result)