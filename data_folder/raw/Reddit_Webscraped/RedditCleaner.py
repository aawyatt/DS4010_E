import pandas as pd
import datetime

## Plan: 
## 1) since the scraper was run with multiple keywords on the same subreddit, there are likely duplicates so it would be good to first get rid of them 
## 2) when webscraping, for some reason the apostrophy was mapped to â€™ so it would probably be best to remap that for sentiment analysis later
## 3) iterate through all of the data looking for keywords like "dining hall", "convos", "seasons", etc
## 4) Change the time from Unix time to match the dates from the other data (YYYY-MM-DD)
## 5) Of all of that data, keep only those entries and drop the others. Will run a sentiment analysis algo later to get a value for each
## 6) Run sentiment analysis algo
## 7) after the sentiment value is found, it can be put into an equation which weights the sentiment, upvotes, proportion of upvotes to main post if its a comment, time posted(older gets a smaller weight), etc
## 8) standardize this value to be between 1 and 5 and put this into the dataset, this can then be used for further analysis


Reddit = pd.read_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Reddit_Webscraped/Post_Data.csv")
Reddit = Reddit.filter(['Type','Body','Number of Comments','Number of Upvotes','Ratio of Upvotes','Submission ID', 'Parent ID','Date', 'url'])
print(Reddit.columns)
print(Reddit.shape)


#Part 1
Reddit = Reddit.drop_duplicates()
print(len(Reddit))

##Part 2
cols = Reddit.columns.to_list()

for i in range(len(Reddit)):
    if "â€™" in str(Reddit['Body'][i]):
        print(Reddit['Body'][i])
        Reddit['Body'][i] = Reddit['Body'][i].replace("â€™","'")
        print(Reddit['Body'][i])

##Part 3

data = set( )
#"Food","food","Dining","dining","Dining Hall","dining hall",
keywords = ["Friley","friley","frileys","Frileys","union drive community center","UDM","Conversations","conversations", "Convos","convos","UDCC","udcc","Windows","windows","Seasons","seasons"]
badKeywords = ["Desktop", "Laptop","desktop", "Mail","mail","laptop", "Mac", "macbook", "career","Career"]

for i in range(len(Reddit)):
    if any(words in str(Reddit['Body'][i]) for words in keywords) and (Reddit['Type'][i] == "Post"):        ##Finding all posts with the given keywords
        if(any(badWords in str(Reddit['Body'][i]) for badWords in badKeywords)):
            continue
        data.add(tuple(Reddit.iloc[i]))
cleaned = pd.DataFrame(data,columns=['Type','Body','Number of Comments','Number of Upvotes', 'Ratio of Upvotes','Submission ID','Parent ID','Date','url'])
print(cleaned.shape)
cleaned = cleaned.drop_duplicates(subset=['Submission ID'])
print(cleaned.shape)

##Part 4
#for i in range(len(cleaned)):
#    cleaned['Date'][i] = datetime.datetime.fromtimestamp(cleaned['Date'][i]).strftime('%Y-%m-%d')
#    if()

##Part 5
cleaned.to_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Reddit_Webscraped/PreCleaned.csv", index=False)
print(len(cleaned))

##other parts on another script

