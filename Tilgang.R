#Herefter anvendes pakkerne
library(httr)
library(jsonlite)
library(tidyverse)

####################################################################
## API-indlæsning via lokal .Renviron fil, og API data udtrækning ##
####################################################################
readRenviron(".Renviron")
api_key <- Sys.getenv("API_KEY")

#URL til dataudtræk (statistik)
URL_stat                <- 'https://api.uddannelsesstatistik.dk/api/v1/statistik'

#Heri indsættes api-nøglen, paste bruges for at sætte Bearer sammen med API-nøglen, som er i en gemt vektor
auth_key                <- paste('Bearer', api_key)


#Her indsættes JSON strengen fra https://api.uddannelsesstatistik.dk/OnlineTool
#OBS! JSON-strengen skal ikke være formateret fra online toolet af hensyn til R's håndtering af linjeskift
# Indenfor trivelses spørgsmålet "Jeg er glad for at gå i skole" der insættes et ydeligere \ før og efter (så det er \\"Jeg er glad for at gå i skole\\""), ellers kommer der en fejlmeldning
query                   <- '{
   "område": "GYM",
   "emne": "OVER",
   "underemne": "OVEREX",
   "nøgletal": [
      "Trivselsvar, spm. 6: ’Jeg er glad for at gå i skole’",
      "Antal elever - Tilgang",
      "Andel elever - Forløbsstatus - Afbrudt uden omvalg",
      "Antal studenter"
   ],
   "detaljering": [
      "[Dato].[månedÅrNavn]",
      "[Dato].[Skoleår]",
      "[Grundskoleresultat].[Lovbundne prøver - karaktergruppe]",
      "[Herkomst].[Herkomstgruppe]",
      "[Institution].[Afdelingsnummer]",
      "[Institution].[Institution]",
      "[Køn].[Køn]",
      "[Uddannelse].[Uddannelsesymbol]"
   ],
   "filtre": {
      "[Institution].[Institution]": [
         "U/NORD"
      ]
   },
   "indlejret": false,
   "tomme_rækker": false,
   "formattering": "json",
   "side": 1
}'

query                   <- '{
   "område": "GYM",
   "emne": "OVER",
   "underemne": "OVEREX",
   "nøgletal": [
      "Antal elever - Tilgang"
   ],
   "detaljering": [
      "[Dato].[Skoleår]",
      "[Uddannelse].[Uddannelsesymbol]"
   ],
   "filtre": {
      "[Institution].[Institution]": [
         "U/NORD"
      ]
   },
   "indlejret": false,
   "tomme_rækker": false,
   "formattering": "json",
   "side": 1
}'

#Her ændres formatet i JSON-strengen vha. escape karakterer, så API'et kan læse JSON koden
format_query            <- gsub('"', '\"', query)

#Her defineres loopet til forespørgsel af multiple pages:
page                    <- 1
api_response_length     <- 1
data                    <- list()

while (api_response_length > 0) {
  change_page           <- gsub('1', page, format_query)
  kald                  <- POST(url = URL_stat, add_headers(Authorization = auth_key), content_type_json(), accept_json(), encode = "json", body = fromJSON(change_page, simplifyVector = FALSE))
  indhold_kald          <- content(kald, as = "text")
  apiresponse           <- fromJSON(indhold_kald)
  data[[page]]          <- apiresponse
  api_response_length   <- length(apiresponse)
  page                  <- page + 1
}

#loopet bindes her til et objekt, som danner en tabel med resultatet af forespørgslen:
Dataudtraek             <- do.call(rbind, data)





#### Data exploration ####
Tilgang_2023 <- Dataudtraek %>% filter(Dataudtraek$`[Dato].[Skoleår].[Skoleår]` == "2024/2025")
Tilgang_2023 <- Tilgang_2023[,c("[Uddannelse].[Uddannelsesymbol].[Uddannelsesymbol]","Antal elever - Tilgang")]
colnames(Tilgang_2023) <- c("Uddannelsessymbol","Antal_Tilgnag")
Tilgang_2023$Antal_Tilgnag <- as.numeric(Tilgang_2023$Antal_Tilgnag)
Tilgang_aggregated <- Tilgang_2023 %>% group_by(Uddannelsessymbol) %>%
                            summarise(sum(Antal_Tilgnag))
# This should be 620 Hhx and 178 Htx
# When using the full query, I get 
#                              Hhx: 4912
#                              Htx: 1360
# When using the test query, I get 
#                              Hhx: 629
#                              Htx: 179

