import pandas as pd
import numpy as np

##Google form data
Form = pd.read_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Reddit_Webscraped/SurveyReviews.csv")
##Merged and cleaned reddit and google reviews data
Full = pd.read_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Reddit_Webscraped/MergedDataset.csv")


##arrays I am going to add to the merged rows(probably a more efficient way to do this)
rate = []
body = []
diningHall = []
reviewUrl = []
date = []

##Iterating though each dining hall to extract their rating and write down corresponding data on arrays
cols = ["Conversations Dining","Friley Windows","Seasons Marketplace","Union Drive Community Center"]
test = set( )
for i in range(len(cols)):
    current = Form[cols[i]]
    for j in range(len(current)-2):
        rate.append(int(current.iloc[j]))
        body.append("")
        diningHall.append(cols[i])
        reviewUrl.append("")
        date.append(Form["Date"].iloc[j])

New = pd.DataFrame({
    "Rating": rate,
    "Body":body,
    "Dining Hall": diningHall,
    "reviewUrl": reviewUrl,
    "Date": date
})

print(len(New))
print(len(Full))
New =pd.concat([Full,New], ignore_index = True,sort =False)
print(len(New))

New.to_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Reddit_Webscraped/FinalDF.csv",index = False)