regents <- read.csv("./data_folder/raw/Regents_Data/Regents.csv")
colnames(regents)

library(dplyr)


isu <- regents %>%
  filter(University.Name == "Iowa State University",
         Year > 2020,
         Student.Classification != "Professional") %>%
  dplyr::select(-Resident.Status) %>%
  group_by(Year, Student.Classification) %>%
  summarize(count = sum(Headcount))

write.csv(isu, "./data_folder/clean/CleanRegents.csv")
