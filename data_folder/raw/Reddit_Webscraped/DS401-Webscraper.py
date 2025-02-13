from selenium import webdriver
from selenium.webdriver.chrome.service import Service
import pandas as pd
import praw ##python reddit api wrapper
from praw.models import MoreComments

##this driver is for chrome version 133 and up, make sure chrome is at least 133
driver_path = "C:/Users/landa/OneDrive/Documents/DS401 Webscrapper/chromedriver-win64/chromedriver-win64/chromedriver.exe" ## put the path to your chrome driver here
service = Service(driver_path)
driver = webdriver.Chrome(service=service)

##praw user stuff
user_agent = "ISU Dining Scraper v1.0 by /u/Necessary-Two1334"
reddit = praw.Reddit (##setting up the whole reddit instance
client_id="TIIqx-Q-WdqEj8YjpptfEg",  ##sent via email from reddit 
client_secret="0H8yoW389pdU0YUVlqb8DNLf4zHyfw",  ##got from reddit developed applications page
user_agent=user_agent
)


keywords = ["Dining", "Conversations", "Convos", "Seasons", "Dining Hall", "Windows", "Friley Windows", "UDCC", "Dining Hall Food", "union drive community center"]
Data = set ( )     ##empty set for the title of all of the reddit posts relating to dining halls at ISU
#Descriptions = set( )       ## empyt set for all of the main descriptions of the post
#Comments = set( )


Index = 0
for keyword in keywords:        ##iterates through all of the keywords given
    for submission in reddit.subreddit('iastate').search(keyword, sort = "top", limit = None): ##for each keyword, it finds all related posts and stores them    
        submission.comments.replace_more(limit=None) ##includes all sub-comments
        #Posts[Index] = submission.title + str(submission.num_comments) + str(submission.score) + str(submission.upvote_ratio) + submission.url
        #Index +=1
        tup = ["Post",submission.selftext,submission.num_comments,submission.score,submission.upvote_ratio,submission.id,None,submission.url]
        Data.add(tuple(tup))                ##Adding title of main post to list
        print(tup)
        #Descriptions.add(submission.selftext)       ##Adding Description of the main post if it has text
        
        for comment in submission.comments.list():      ##using .list() lists every comment in the comment tree
            Comment_tup = ["Comment",comment.body,len(comment.replies), comment.score,None,submission.id,comment.parent_id,comment.permalink] ##comments aren't given a ratio of upvotes and number of sub comments(from what i saw its to save space on reddits database)
            Data.add(tuple(Comment_tup))
            print(Comment_tup)
            #Comments.add(comment.body)                  ##adds all of the comments in each post
        

# print(len(Descriptions))
# print(len(Comments))
Posts = pd.DataFrame(Data,columns=['Type','Body','Number of Comments','Number of Upvotes', 'Ratio of Upvotes','Submission ID','Parent ID','url'])
Posts.to_csv("C:/Users/landa/OneDrive/Documents/DS401 Webscrapper/Post_Data.csv")

print(Posts)

driver.quit()