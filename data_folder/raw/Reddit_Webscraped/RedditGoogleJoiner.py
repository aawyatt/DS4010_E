import pandas as pd
import nltk
nltk.download('vader_lexicon')
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import os
import re

# Load datasets
reddit_df = pd.read_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Reddit_Webscraped/Cleaned.csv")
dining_df = pd.read_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Dining_Hall_Webscraped/CleanedDiningHalls.csv")

# Retain only relevant columns
common_columns = ['Rating', 'Body', 'Dining Hall', 'reviewUrl', 'Date']
dining_df = dining_df[common_columns]
reddit_df = reddit_df[['Body', 'Dining Hall', 'Date', 'url']]
reddit_df.rename(columns={'url': 'reviewUrl'}, inplace=True)

# Cleaning function for Dining Hall names
def clean_dining_halls(dining_hall):
    if pd.isna(dining_hall):
        return []
    return [hall.strip() for hall in dining_hall.split(',') if hall.strip()]

# Apply cleaning
reddit_df['Dining Hall'] = reddit_df['Dining Hall'].apply(clean_dining_halls)

# Expand dataset so each row corresponds to a single dining hall
reddit_df = reddit_df.explode('Dining Hall')

# Initialize VADER sentiment analyzer
analyzer = SentimentIntensityAnalyzer()
counter = 0

def extract_contextual_sentiment(body, hall):
    if not isinstance(body, str) or not isinstance(hall, str):
        return 3  # Neutral rating if missing data

    sentences = re.split(r'[.!?]', body)  # Split into sentences
    sentiment_scores = []
    hall_pattern = re.compile(r'\b' + re.escape(hall) + r'\b', re.IGNORECASE)

    for sentence in sentences:
        sentiment = analyzer.polarity_scores(sentence)['compound']
        
        if hall_pattern.search(sentence):
            sentiment_scores.append(sentiment * 1.5)  # Boost direct mentions
        else:
            sentiment_scores.append(sentiment * 0.75)  # Weaker weight for indirect mentions

    if not sentiment_scores:  
        return 3
    
    avg_sentiment = sum(sentiment_scores) / len(sentiment_scores)

    # More reactive thresholds for sentiment → rating conversion
    if avg_sentiment <= -0.2:
        return 1
    elif avg_sentiment <= -0.05:
        return 2
    elif avg_sentiment <= 0.05:
        return 3
    elif avg_sentiment <= 0.2:
        return 4
    else:
        return 5

reddit_df['Rating'] = reddit_df.apply(lambda row: extract_contextual_sentiment(row['Body'], row['Dining Hall']), axis=1)

# Merge with dining dataset
merged_df = pd.concat([dining_df, reddit_df], ignore_index=True)

# Save final dataset
output_path = "C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Reddit_Webscraped/MergedDataset.csv"
os.makedirs(os.path.dirname(output_path), exist_ok=True)
merged_df.to_csv(output_path, index=False)

print("Merging complete. Dataset saved to:", output_path)
