install.packages('RCurl', 'XML')
library(RCurl)
library(RJSONIO)
library(doParallel)

API_KEY <- "WySmXy7wUGuH"

################################ Functions ############################
GetLanguage <- function(text) {
    # Use language detection API to get language from an input text
    
    key = '123ad1302dafd9961c5ddf09bf18fb5b'
    url <- paste0("http://ws.detectlanguage.com/0.2/detect?q=", text, "&key=", key)
    response <- readLines(URLencode(url), warn = F)
    json.response <- fromJSON(response)
    if (length(json.response$data$detections) == 0) {
        result <- c(NA, NA, NA)
    } else {
        js <- json.response$data$detections[[1]]
        result <- c(js$language, js$isReliable, js$confidence)
    }
    
    return(result)
}

tokens <- playlist.6$tokens[1:100]
# Parallelism to speed up process
cl <- makeCluster(2)
registerDoParallel(cl) 
system.time(
    token.langs <- foreach (j=1:length(tokens), 
                            .packages = c('RCurl', 'RJSONIO'), 
                            .combine=rbind) %dopar% {
        print(j) #write out to the listener
        print(tokens[j])
        result <- c(tokens[j], GetLanguage(tokens[j]))
        result.df <- t(as.data.frame(result))
        colnames(result.df) <- c('token', 'language', 'isReliable', 'confidence')
        result.df
    }
)
stopCluster(cl) 


