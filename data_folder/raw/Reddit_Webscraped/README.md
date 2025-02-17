**Process Description**

**1)** Used the reddit api wrapper "praw" to retrieve all posts and comments in the Iowa state univeristy reddit r/iastate. This was done in the DS401-Webscraper.py

**2)** After the posts and comments were retrieved, the duplicates within the dataset were dropped(no missing important values)

**3)** Fixed a string transformation error which replaces the apostrophy with â€™

**4)** Filtered all of the posts based off of keywords like udcc, windows, seasons (2-4 done in RedditCleaner.py)

**5)** There were only about 115 posts with the keywords so I personally went through the csv to label as relevant or not

**6)** Rows that werent relevant were dropped

**7)** All of the comments for the corresponding posts were brought in by their ID number

**8)** Both comments and posts were put through one last filter called the catagorizer which iterates through the dataset and looks for even more specific keywords and puts the post in a specific(or multiple because some posts talk about more than one hall) category which will be used to bind to the google reviews dataset.

**9)** The last element in order to merge this with the CleanedDiningHalls.csv is to give it a rating. This was done with a sentiment analysis algorithm to give it a rating between 1 and 5
