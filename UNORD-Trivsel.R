#Herefter anvendes pakkerne
library(httr)
library(jsonlite)

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


############################################
############## Data Rensning ##############
############################################
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



############################################################################################################################
##### Dette skal helst ændres, så det ikke er en seperat data.frame, men laves i Data_Mobning
# Kombiner alle kolonnerne for mobning til en enkelt vektor og gentag årstallene
Mobning <- c(Data_Mobning$Indikatorsvar.Stx, 
             Data_Mobning$Indikatorsvar.Hhx, 
             Data_Mobning$Indikatorsvar.Htx)

# Gentag årstallene 3 gange (for hver uddannelse)
År <- rep(Data_Mobning$År, 3)

# Opret en "Uddannelse" kolonne ved hjælp af gsub
Uddannelse <- rep(c("Stx", "Hhx", "Htx"), each = nrow(Data_Mobning))

# Opret den samlede data frame
Data_combined <- data.frame(År, Mobning, Uddannelse)




library(ggplot2)

# Skal finde en måde at overskueliggøre overlapping
ggplot(Data_combined, aes(x = År, y = Mobning, color = Uddannelse, shape = Uddannelse, group = Uddannelse)) +
  geom_point(size = 3, position = position_dodge(width = 0.1)) +
  geom_smooth(se = FALSE) +  
  labs(title = "Udvikling i Mobning indikatorsvar fra U/NORD over Årene",
       x = "År",
       y = "Mobning (%)",
       color = "Uddannelse", shape = "Uddannelse") +
  theme_minimal() +
  theme(legend.position = "bottom")
