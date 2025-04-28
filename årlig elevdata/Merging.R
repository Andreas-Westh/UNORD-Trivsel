library(readxl)
library(dplyr)

df <- read_xlsx("årlig elevdata/Data/test.xlsx", sheet = 1)
df1 <- read_xlsx("årlig elevdata/Data/test.xlsx", sheet = 2)


# Isolate year like in df
df1 <- df1 %>% 
  mutate(år = substr(månedÅrNavn, 1, 4))
df1 <- df1 %>%
  filter(Institution == "U/NORD")
df1 <- df1 %>%
  rename(Uddannelse = Uddannelsessymbol)

df <- df %>% 
  rename(år = Bevisår)
df$år <- as.character(df$år)



merged_df <- full_join(df, df1, by = c("år", "Afdelingsnummer", "Køn", "Uddannelse"))

