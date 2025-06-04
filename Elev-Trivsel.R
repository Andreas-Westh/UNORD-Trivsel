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
   "underemne": "INDIBAG",
   "nøgletal": [
      "Trivselsvar, \\"Jeg er glad for at gå i skole\\"",
      "Indikatorsvar",
      "Indikatorsvar - Landstal"
   ],
   "detaljering": [
      "[Alder].[Alder]",
      "[Herkomst].[Herkomst]",
      "[Institution].[Afdeling]",
      "[Institution].[Afdelingsnummer]",
      "[Institution].[Beliggenhedskommune]",
      "[Institution].[Beliggenhedsregion]",
      "[Institution].[Institution]",
      "[Køn].[Køn]",
      "[TrivselIndikator].[Indikator]",
      "[Uddannelse].[Uddannelsessymbol]",
      "[År].[År]"
   ],
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


<<<<<<< HEAD:UNORD-Trivsel.R
############################################
############## Data Rensning ##############
############################################
=======
df <- Dataudtraek

>>>>>>> b1be374 (new data retrieval):Elev-Trivsel.R
#Ændre navne, til noget mere læsbart
colnames(df)[1] = "Alder"
  colnames(df)[2] = "Herkomst"
  colnames(df)[3] = "Afdeling"
  colnames(df)[4] = "Afdelingsnummer"
  colnames(df)[5] = "Beliggenhedskommune"
  colnames(df)[6] = "Beliggenhedsregion"
  colnames(df)[7] = "Institution"
  colnames(df)[8] = "Køn"
  colnames(df)[9] = "Indikator"
  colnames(df)[10] = "Uddannelsessymbol"
  colnames(df)[11] = "År"
  colnames(df)[12] = "GladForSkole"  
  colnames(df)[14] = "Landstal"  
  
#Datacleaning
df$GladForSkole <- as.numeric(gsub(",",".",df$GladForSkole))
df$Indikatorsvar <- as.numeric(gsub(",",".",df$Indikatorsvar))
df$Landstal <- as.numeric(gsub(",",".",df$Landstal))

library(plotly)
plot_ly(df, 
        x = ~GladForSkole, 
        y = ~Indikatorsvar, 
        z = ~Landstal, 
        color = ~Indikator,
        colors = c("red", "blue", "green"), 
        type = "scatter3d", 
        mode = "markers",
        text = ~paste("Indikator: ", as.character(Indikator), "<br>",
                      "Afdeling: ", as.character(Afdeling), "<br>"),
        hoverinfo = "text") %>%
  layout(title = "title",
         scene = list(
           xaxis = list(title = "Jeg er glad for at gå i skole"),
           yaxis = list(title = "Indikatorsvar"),
           zaxis = list(title = "Landsgennemsnit")
         ))


<<<<<<< HEAD:UNORD-Trivsel.R

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
=======
>>>>>>> b1be374 (new data retrieval):Elev-Trivsel.R
