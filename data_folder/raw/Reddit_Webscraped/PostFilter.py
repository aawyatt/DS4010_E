import pandas as pd
import datetime

##i personally went through each post and indicated if it was relevant(only about 115 posts)
##now finishing up cleaning and going to pull in related comments to the post

cleaned = pd.read_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Reddit_Webscraped/Cleaned.csv")
allPosts = pd.read_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Reddit_Webscraped/Post_Data.csv")

## function to try and find the topic of the given post
## 1) takes dataframe as an input
## 2) finds all of the posts by looking at the 'type column'
## 3) of those posts looks through the keywords to see which ones apply("convos", "seasons","dining halls")
## 4) puts each post in a category of each dining hall, dining halls in general, or not related.
def catagorizer(df):
    catagories = []
    udcc = ["UDCC", "udcc", "Udcc", "UDM", "Union Drive", "union drive", "Union drive","udc","UDC"]
    seasons = ["seasons","Seasons","Season's", "season's","sesons"]
    convos= ["Conversations","convos","conversations","Convos"]
    windows = ["Windows","windows"]
    Halls = udcc + seasons + convos + windows
    for i in range(len(df)):
        toAppend = ""
        if any(keys in str(df['Body'][i]) for keys in udcc):
            toAppend = toAppend + "Union Drive Community Center,"
        if any(keys in str(df['Body'][i]) for keys in seasons):
            toAppend = toAppend + ("Seasons Marketplace,")
        if any(keys in str(df['Body'][i]) for keys in convos):
            toAppend = toAppend + ("Conversations Dining,")
        if any(keys in str(df['Body'][i]) for keys in windows):
            toAppend = toAppend + ("Friley Windows,")
        if not any(keys in str(df['Body'][i]) for keys in Halls):
            toAppend = toAppend + ("Not relevant")
        catagories.append(toAppend)
    return catagories


desiredIDs = set( )
for i in range(len(cleaned)):
    if cleaned['Relevant'][i] == "x":
        desiredIDs.add(cleaned['Submission ID'][i])             ##getting all of the posts that are considered relevant

desiredComments = set( )
for i in range(len(allPosts)):
    if allPosts['Type'][i] == 'Comment' and allPosts['Submission ID'][i] in desiredIDs:     ## getting all of the comments of the desired posts
        desiredComments.add(tuple(allPosts.iloc[i]))

#cleaned = cleaned.drop(['Relevant'], axis =1)
cleaned = cleaned.filter(['Type', 'Body', 'Number of Comments', 'Number of Upvotes',
       'Ratio of Upvotes', 'Submission ID', 'Parent ID', 'Date', 'url']) ##getting rid of the Relevent identification. Not used anymore

commentDF = pd.DataFrame(desiredComments, columns=cleaned.columns)
cleaned = pd.concat([cleaned,commentDF], ignore_index=True)
cleaned['Dining Hall'] = catagorizer(cleaned)

 ##replacing date data with datetime instead of unix time 
for i in range(len(cleaned)):
     cleaned['Date'][i] = datetime.datetime.fromtimestamp(cleaned['Date'][i]).strftime('%Y-%m-%d')
cleaned = cleaned[cleaned['Dining Hall'] != "Not relevant"]
cleaned.to_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Reddit_Webscraped/Cleaned.csv", index = False)