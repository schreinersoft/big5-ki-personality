from transformers import AutoModelForSequenceClassification, AutoTokenizer
import torch

model = AutoModelForSequenceClassification.from_pretrained("KevSun/Personality_LM", ignore_mismatched_sizes=True)
tokenizer = AutoTokenizer.from_pretrained("KevSun/Personality_LM")

window_size=512
stride=256


def classify_truncated(text: str):
    # Encode the text using the same tokenizer used during training
    #encoded_input = tokenizer(new_text, return_tensors='pt', padding=True, truncation=True, max_length=64)
    encoded_input = tokenizer(text, return_tensors='pt', padding=True, truncation=True, max_length=window_size)   # set length to 768
    print(f"Text truncated to {window_size} tokens.")
    model.eval()  # Set the model to evaluation mode

    # Perform the prediction
    with torch.no_grad():
        outputs = model(**encoded_input)

    predictions = torch.nn.functional.softmax(outputs.logits, dim=-1)

    predicted_scores = predictions[0].tolist()

    trait_names = ["A", "O", "C", "E", "N"]

    # result = {}
    # for trait, score in zip(trait_names, predicted_scores):
    #     result[trait] = score

    result = {trait_names[i]: predicted_scores[i] for i in range(len(trait_names))}

    return result


def sliding_window_classify(text):
    # Tokenize the full text without truncation
    full_tokens = tokenizer.encode(text, add_special_tokens=True)
    
    # If text is short enough, use original approach
    if len(full_tokens) <= window_size:
        print(f"Text encoded to {len(full_tokens)} tokens.")
        encoded_input = tokenizer(text, return_tensors='pt', padding=True, truncation=True, max_length=window_size)
        model.eval()
        with torch.no_grad():
            outputs = model(**encoded_input)
        predictions = torch.nn.functional.softmax(outputs.logits, dim=-1)
        predicted_scores = predictions[0].tolist()
    else:
        print(f"Text encoded to {len(full_tokens)} tokens. Using sliding window approach.")
        # Sliding window approach
        all_predictions = []
        model.eval()
        
        for i in range(0, len(full_tokens), stride):
            # Extract window
            window_tokens = full_tokens[i:i + window_size]
            
            # Skip if window is too short (less than 50 tokens)
            if len(window_tokens) < 50:
                break
                
            # Ensure we have proper special tokens
            if window_tokens[0] != tokenizer.cls_token_id:
                window_tokens = [tokenizer.cls_token_id] + window_tokens[1:]
            if window_tokens[-1] != tokenizer.sep_token_id:
                window_tokens = window_tokens[:-1] + [tokenizer.sep_token_id]
            
            # Convert to tensor and add batch dimension
            input_ids = torch.tensor([window_tokens])
            attention_mask = torch.ones_like(input_ids)
            
            # Create input dict
            encoded_input = {
                'input_ids': input_ids,
                'attention_mask': attention_mask
            }
            
            # Perform prediction
            with torch.no_grad():
                outputs = model(**encoded_input)
                predictions = torch.nn.functional.softmax(outputs.logits, dim=-1)
                all_predictions.append(predictions[0])
        
        # Aggregate predictions (mean across all windows)
        if all_predictions:
            aggregated_predictions = torch.stack(all_predictions).mean(dim=0)
            predicted_scores = aggregated_predictions.tolist()
        else:
            # Fallback to truncation if no valid windows
            encoded_input = tokenizer(text, return_tensors='pt', padding=True, truncation=True, max_length=window_size)
            with torch.no_grad():
                outputs = model(**encoded_input)
            predictions = torch.nn.functional.softmax(outputs.logits, dim=-1)
            predicted_scores = predictions[0].tolist()
    
    # Map results to trait names
    trait_names = ["A", "O", "C", "E", "N"]
    result = {}
    for trait, score in zip(trait_names, predicted_scores):
        result[trait] = score
    
    return result

if __name__ == "__main__":
    text = "I return to the presidency confident and optimistic that we are at the start of a thrilling new era of national success. A tide of change is sweeping the country, sunlight is pouring over the entire world, and America has the chance to seize this opportunity like never before. But first, we must be honest about the challenges we face. While they are plentiful, they will be annihilated by this great momentum that the world is now witnessing in the United States of America."
    result = classify_truncated(text)
    print(result)