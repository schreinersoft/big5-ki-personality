from transformers import BertTokenizer, BertForSequenceClassification
import torch

tokenizer = BertTokenizer.from_pretrained("Minej/bert-base-personality")
model = BertForSequenceClassification.from_pretrained("Minej/bert-base-personality")

window_size=512
stride = int(window_size / 2)

def classify_truncated(text: str):
    # Encode the text using the same tokenizer used during training
    #encoded_input = tokenizer(new_text, return_tensors='pt', padding=True, truncation=True, max_length=64)
    # encoded_input = tokenizer(text, return_tensors='pt', padding=True, truncation=True, max_length=window_size)   # set length to 768
    
    model.eval()  # Set the model to evaluation mode

    inputs = tokenizer(text, truncation=True, padding=True, return_tensors="pt")
    #print(f"Text truncated to {len(inputs[0])} tokens.")
    with torch.no_grad():
        outputs = model(**inputs)
    predictions = outputs.logits.squeeze().detach().numpy()


    trait_names = ["E", "N", "A", "C", "O"]

    # result = {}
    # for trait, score in zip(trait_names, predicted_scores):
    #     result[trait] = score

    result = {trait_names[i]: float(predictions[i]) for i in range(len(trait_names))}

    return result


def classify_sliding_windowed(text):
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
        result[trait] = float(score)
    
    return result

if __name__ == "__main__":
    text = "I return to the presidency confident and optimistic that we are at the start of a thrilling new era of national success. A tide of change is sweeping the country, sunlight is pouring over the entire world, and America has the chance to seize this opportunity like never before. But first, we must be honest about the challenges we face. While they are plentiful, they will be annihilated by this great momentum that the world is now witnessing in the United States of America."
    #text = """An open keyboard and buttons to push. The thing finally worked and I need not use periods, commas and all those thinks. Double space after a period. We can't help it. I put spaces between my words and I do my happy little assignment of jibber-jabber. Babble babble babble for 20 relaxing minutes and I feel silly and grammatically incorrect. I am linked to an unknown reader. A graduate student with an absurd job. I type. I jabber and I think about dinoflagellates. About sunflower crosses and about the fiberglass that has be added to my lips via clove cigarettes and I think about things that I shouldn't be thinking. I know I shouldn't be thinking. or writing let's say/  So I don't. Thoughts don't solidify. They lodge in the back. behind my tongue maybe. Somewhere at the point of hiding but   dinoflaghelates, protistas and what was that sea weed. I think about the San Luiz valley and I think about the mushrooms in cow shit. I think about the ticos and I think about the chiggers that are living in my legs. I itch. I coat myself with clear nail polish in hopes to suffocate the bugs that are living in my legs and I remember Marco. I remember Ecuador  and I think about my thoughts and what I am not supposed to be doing in this assignment. Thoughts. I wonder if I think in sentences I wonder what affect my slowish typing has on my stream of consciousness and I wonder if there is a way that typing speed can be measured in this study  so that so link some generalization of dorky 301 psyc students. green and the table in my kitchen makes me want to vomit. orange. What an absurd color. wish I wasn't in the united state. My greencard runs out in a few years wonder what I do. I hope Dr. Linder gets back in his lab because I really need to find out if he has funds to pay me. May have to go back to the library. Brainless job of nothingness that would make me wallow in the world of boredom which isn't entirely bad. Need to focus on school organics and such. Period. Two spaces after the period. Mistakes and I want to eat not hungry and I wonder how many people talk about food in there little computer ramblings  Feel open and Happy that I am not having to edit this. Type type I don't know what I am think Hannah Imi and Osdprey house. I remember when I went down to that . she had spiders on hurt wall pain all over the place and we painted clouds on the ceiling and the blue walls were so obnoxious. Carey. Sex sex sex. yeah. This is a strange assignment and Portonoy's complaint is ringing in my head. Eager to finish so that I can start for Whom the Bell Tolls and get on with it. Bio and Carbon atoms bonds and orbitals. Thinking  about the electron configuration that surrounds the last letter in my first name and I think that I must have been granted a full "s" orbital  one up and one down. spinning on opposite directions and I am thinking about Scottish poetry about Mike in his kilt and about my guitar that I am slowly slowly slowly learning to play. I wonder what goes on in this study. I wonder if those happy little bored entertained grad students will scan words and I wonder how I can mess up this study? Random words like . don't know. ;Me me me me me and I wish that some things were easier and I wish that I had been keeping my eye on the clock. Wondering how long I have been typing and wishing that I was finished because I need to find out if I have to / will work in the Botany lab again and all that . ILS Belly and the Flamenco. Bjork and Rozamond Cockrill kickin' it in Saratoga Springs. I hate Molly's cat and wish that it could be exchanged for a worthwhile ferret. Type type type. I have managed to waste over 20 minutes of time I think. Who knows. What If I was to write this out and it took 30 minutes to write and 15 minutes to type. Thinking about nothing and wishing that some financial aid would come my way. Need a job and a sprinkling of time. Time to go and sign outta here. trees"""
    result = classify_truncated(text)
    print(result)
    # result = classify_sliding_windowed(text)
    # print(result)
    