library(httr)
library(jsonlite)

Sys.setenv(GEMINI_API_KEY = "AIzaSyDs3TDrBVzDCwfY0l27cF2NMvwCAQOrEPM")
# Function
gemini <- function(prompt, 
                   temperature=1,
                   max_output_tokens=1024,
                   api_key=Sys.getenv("GEMINI_API_KEY"),
                   model = "gemini-2.0-flash") {
  
  model_query <- paste0(model, ":generateContent")
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(text = prompt)
        )),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_output_tokens
      )
    )
  )
  
  if(response$status_code>200) {
    stop(paste("Error - ", content(response)$error$message))
  }
  
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
  
}

make_prompt <- function(query, relevant_passage) {
  escaped <- gsub("'", "", gsub('"', "", gsub("\n", " ", relevant_passage)))
  prompt <- sprintf("You are a helpful and informative built with the intension of simply explaining a dashboard bot that answers questions using text from the reference text included below. \
  Be sure to respond in a complete sentence, being comprehensive, including all relevant background information. \
  However, you are talking to a non-technical audience, so be sure to break down complicated concepts and \
  strike a friendly and conversational tone. \
  If the query is irrelevant to the answer, you may ignore it.
  QUESTION: '%s'
  PASSAGE: '%s'

    ANSWER:
  ", query, escaped)
  
  return(prompt)
}

#context <- readr::read_file("./data_folder/Context.txt")


#query <- "explain the exploratory analysis tab of the dashboard"
#prompt = make_prompt(query, context)
#cat(gemini(prompt))

