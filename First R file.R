library(readr)
schibsted <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=SCHA.OSE&csv_format=csv")

summary(schibsted$close)
