#For at kunne bruge funktioner til kaldet af API'et skal der installeres følgende pakker
install.packages("httr")
install.packages("jsonlite")

#Herefter anvendes pakkerne
library(httr)
library(jsonlite)

#
readRenviron(".Renviron")
api_key <- Sys.getenv("API_KEY")

#URL til dataudtræk (statistik)
URL_stat                <- 'https://api.uddannelsesstatistik.dk/api/v1/statistik'

#Heri indsættes api-nøglen
auth_key                <- paste('Bearer', api_key)


#Her indsættes JSON strengen fra https://api.uddannelsesstatistik.dk/OnlineTool
#OBS! JSON-strengen skal ikke være formateret fra online toolet af hensyn til R's håndtering af linjeskift
# Indenfor trivelses spørgsmålet "Jeg er glad for at gå i skole" der insættes et ydeligere \ før og efter (så det er \\"Jeg er glad for at gå i skole\\""), ellers kommer der en fejlmeldning
query                   <- '{
   "område": "GYM",
   "emne": "TRIV",
   "underemne": "INDIINST",
   "nøgletal": [
      "Trivselsvar, \\"Jeg er glad for at gå i skole\\"",
      "Indikatorsvar",
      "Indikatorsvar - Landstal"
   ],
   "detaljering": [
      "[TrivselIndikator].[Indikator]",
      "[Uddannelse].[Uddannelsessymbol]",
      "[År].[År]"
   ],
   "filtre": {
      "[Institution].[Afdeling]": [
         "U/NORD",
         "U/NORD Frederikssund Tekniske Gymnasium",
         "U/NORD Helsingør, Rasmus Knudsens Vej",
         "U/NORD Hillerød Handelsgymnasium",
         "U/NORD Hillerød Teknisk Gymnasium",
         "U/NORD Lyngby Gymnasium"
      ],
      "[Uddannelse].[Uddannelsessymbol]": [
         "Stx",
         "Hhx",
         "Htx"
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

#Ændre navne, til noget mere læsbart
colnames(Dataudtraek)[1] = "Indikator"
colnames(Dataudtraek)[2] = "Uddannelse"
colnames(Dataudtraek)[3] = "År"
colnames(Dataudtraek)[4] = "Jeg.er.glad.for.at.gå.i.skole"
colnames(Dataudtraek)[6] = "Indikatorsvar.Landstal"


### Loop for opdeling af Dataudtraek i individuelle dataframes
# Få unikke indikatorer
unikke_indikatorer <- unique(Dataudtraek$Indikator)

# Loop igennem hver indikator
for (indikator in unikke_indikatorer) {
  
  # Filtrér data for hver unik indikator
  indikator_data <- Dataudtraek[Dataudtraek$Indikator == indikator, ]
  
  # Pivot data, så uddannelserne bliver til kolonner
  bred_data <- reshape(indikator_data, 
                       idvar = "År", 
                       timevar = "Uddannelse", 
                       direction = "wide")
  
  # Dynamisk skab en ny dataframe for hver indikator
  assign(paste0("Data_", indikator), bred_data)
}


library(ggplot2)
ggplot
