import pandas as pd

##rearranging google reviews scraped data to simplify and put into 1 file.
##additionally setting up to bind reddit scraped data when cleaned to have a single file of reviews

##loading
Convos = pd.read_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Dining_Hall_Webscraped/Convos.csv", encoding = "ISO-8859-1")
Seasons = pd.read_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Dining_Hall_Webscraped/SeasonsMarketplace.csv", encoding = "ISO-8859-1")
UDCC = pd.read_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Dining_Hall_Webscraped/Udcc.csv", encoding = "ISO-8859-1")
Windows = pd.read_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Dining_Hall_Webscraped/Windows.csv", encoding = "ISO-8859-1")

##removing excess columns and renaming for simplicity
Convos = Convos.filter(['reviewBody','reviewRating','accurateDate','reviewUrl','name'])
Convos = Convos.rename(columns={'reviewBody':'Body','reviewRating':'Rating','accurateDate':'Date','name':'Dining Hall','URL':'reviewUrl'})
Convos = Convos[['Rating','Body','Dining Hall','Date','reviewUrl']]

Seasons = Seasons.filter(['reviewBody','reviewRating','accurateDate','reviewUrl','name'])
Seasons = Seasons.rename(columns={'reviewBody':'Body','reviewRating':'Rating','accurateDate':'Date','name':'Dining Hall'})
Seasons = Seasons[['Rating','Body','Dining Hall','Date','reviewUrl']]

UDCC = UDCC.filter(['reviewBody','reviewRating','accurateDate','reviewUrl','name'])
UDCC = UDCC.rename(columns={'reviewBody':'Body','reviewRating':'Rating','accurateDate':'Date','name':'Dining Hall'})
UDCC = UDCC[['Rating','Body','Dining Hall','Date','reviewUrl']]

Windows = Windows.filter(['reviewBody','reviewRating','accurateDate','reviewUrl','name'])
Windows = Windows.rename(columns={'reviewBody':'Body','reviewRating':'Rating','accurateDate':'Date','name':'Dining Hall'})
Windows = Windows[['Rating','Body','Dining Hall','Date','reviewUrl']]

##Combining
SuperMegaDF = pd.concat([Convos,Seasons,UDCC,Windows])

##Updating
SuperMegaDF.to_csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/data_folder/raw/Dining_Hall_Webscraped/CleanedDiningHalls.csv", index=False)