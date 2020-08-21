library(tidyverse)

#Load CSVs 


HQH <-read_csv("Documents/trotup code and resources/PH SCRAPES/hqhorsesaugparsehub.csv", col_types = cols(advert_price = col_number()))


HQP <-read_csv("Documents/trotup code and resources/PH SCRAPES/hqponiesaugparsehub.csv", col_types = cols(advert_price = col_number()))

HandH <- read_csv("Documents/trotup code and resources/PH SCRAPES/handhaugparsehub.csv", 
                   col_types = cols(advert_age = col_number(), 
                                    advert_height = col_number(), advert_price = col_number()))

PCUK <- read_csv("Documents/trotup code and resources/PH SCRAPES/pcukaugparsehub.csv", 
            col_types = cols(advert_age = col_number(), 
                             advert_height = col_number(), advert_price = col_number()))

horsemart0_2 <- read_csv("Documents/trotup code and resources/PH SCRAPES/hm0-2augparsehub.csv", 
                         col_types = cols(advert_height = col_number(), 
                                          advert_price = col_number()))

horsemart2_3 <- read_csv("Documents/trotup code and resources/PH SCRAPES/hm2-3augparsehub.csv", 
                         col_types = cols(advert_height = col_number(), 
                                          advert_price = col_number()))

horsemart3_5 <- read_csv("Documents/trotup code and resources/PH SCRAPES/hm3-5augparsehub.csv", 
                         col_types = cols(advert_height = col_number(), 
                                          advert_price = col_number()))

horsemart5_7 <- read_csv("Documents/trotup code and resources/PH SCRAPES/hm5-7augparsehub.csv", 
                         col_types = cols(advert_height = col_number(), 
                                          advert_price = col_number()))

horsemart7_10 <- read_csv("Documents/trotup code and resources/PH SCRAPES/hm7-10augparsehub.csv", 
                         col_types = cols(advert_height = col_number(), 
                                          advert_price = col_number()))


horsemart10_12 <- read_csv("Documents/trotup code and resources/PH SCRAPES/hm10-12augparsehub.csv", 
                         col_types = cols(advert_height = col_number(), 
                                          advert_price = col_number()))

horsemart12_14 <- read_csv("Documents/trotup code and resources/PH SCRAPES/hm12-14augparsehub.csv", 
                         col_types = cols(advert_height = col_number(), 
                                          advert_price = col_number()))

horsemart14_16 <- read_csv("Documents/trotup code and resources/PH SCRAPES/hm14-16augparsehub.csv", 
                         col_types = cols(advert_height = col_number(), 
                                          advert_price = col_number()))

horsemart16plus <- read_csv("Documents/trotup code and resources/PH SCRAPES/hm16augparsehub.csv", 
                         col_types = cols(advert_height = col_number(), 
                                          advert_price = col_number()))


#CLEANING HORSEQUEST HORSE DATA



#ADDING AGE COLUMN FROM AGE EXTRACTED FROM ADVERT DETAILS
hqhwithage<- HQH %>% 
  mutate("age" = str_extract(HQH$advert_details, "[[:digit:]]+"))

hqhwithage$age <- as.numeric(hqhwithage$age)

#EXTRACTING HEIGHT FROM AD DETAILS AND MAKING HEIGHT COL

hqhwithageheight <- hqhwithage %>% 
  mutate("height" = str_extract(hqhwithage$advert_details, "years.*")
  )

hqhwithageheight$height <- gsub("[^0-9.-]", "", hqhwithageheight$height)
hqhwithageheight$height <- as.numeric(hqhwithageheight$height)


#REMOVE NAS

hqhwithageheightnona <- na.omit(hqhwithageheight)


# CONVERT CMS TO HANDS

convertcmstohands <- function(x){
  
  ifelse((x>100) == TRUE, x/10.16, x)
}

hqhwithageheightnona$height <- convertcmstohands(hqhwithageheightnona$height)


#DONE


#CLEANING HQ PONY DATA

#AGE COLUMN
HQP <- HQP %>% 
  mutate("age" = str_extract(HQP$advert_details, "[[:digit:]]+"))
HQPwithage <- HQP

HQPwithage$age <- as.numeric(HQPwithage$age)


#HEIGHT COLUMN  

HQPwithageheight <- HQPwithage %>% 
  mutate("height" = str_extract(HQPwithage$advert_details, "years.*")
  )

HQPwithageheight$height <- gsub("[^0-9.-]", "", HQPwithageheight$height)
HQPwithageheight$height <- as.numeric(HQPwithageheight$height)

#MAKE PRICE NUMERIC
HQPwithageheight$advert_price <- as.numeric(HQPwithageheight$advert_price)

#NO NAS

HQPwithageheightnona <- na.omit(HQPwithageheight)


#CONVERT CMS -> HANDS

HQPwithageheightnona$height <- convertcmstohands(HQPwithageheightnona$height)


#MERGE HQ HORSE AND PONY DFS
mergeCols <- c("advert_name", "advert_price", "height", "age", "advert_details")
HQ <- full_join(hqhwithageheightnona, HQPwithageheightnona, by = mergeCols)


#DROP INFO COL

HQ <- HQ %>% select(advert_name, advert_price, age, height)

#ADD WEBSITE COLUMN

HQ$source <- 'Horsequest'

#RENAME COLUMNS 

names(HQ)[names(HQ)=="advert_price"] <- "price"
names(HQ)[names(HQ)=="advert_name"] <- "title"



#HORSE AND HOUND DATA

#REMOVE NAS

hhnona <- na.omit(HandH)

#ADD SOURCE

hhnona$source <- 'Horse and Hound'

#RENAME COLS

names(hhnona)[names(hhnona)=="advert_price"] <- "price"
names(hhnona)[names(hhnona)=="advert_height"] <- "height"
names(hhnona)[names(hhnona)=="advert_age"] <- "age"
names(hhnona)[names(hhnona)=="advert_name"] <- "title"



#MERGE WITH HQ
mergeCols2 <- c("title", "price", "height", "age", "source" )
HQHH <- full_join(HQ, hhnona, by = mergeCols2)


#PCUK

#RENAME COLS

names(PCUK)[names(PCUK)=="advert_price"] <- "price"
names(PCUK)[names(PCUK)=="advert_height"] <- "height"
names(PCUK)[names(PCUK)=="advert_age"] <- "age"
names(PCUK)[names(PCUK)=="advert_name"] <- "title"

#REMOVE NAS

PCUK <- na.omit(PCUK)

#ADD SOURCE

PCUK$source <- 'Pony Club'

#MERGE WITH REST OF DATA

mergeCols2 <- c("title", "price", "height", "age", "source" )
DATABASE <- full_join(HQHH, PCUK, by = mergeCols2)



#HORSEMART DATA FRAMES

#0-2 YO

#ADD MEAN AGE OF EACH GROUP

#0-2

horsemart0_2$age <- '1'

#2-3

horsemart2_3$age <- '2.5'

#3-5

horsemart3_5$age <- '4'

#5-7

horsemart5_7$age <- '6'

#7-10

horsemart7_10$age <- '8.5'

#10-12

horsemart10_12$age <- '11'

#12-14

horsemart12_14$age <- '13'

#14-16

horsemart14_16$age <- '15'

#16+

horsemart16plus$age <- '17'


#MERGE ALL THE HORSEMARTS

mergeCols3 <- c("advert_name", "advert_price", "advert_height", "age" )
horsemart <- full_join(horsemart0_2, horsemart2_3, by = mergeCols3)

horsemart <- full_join(horsemart, horsemart3_5, by = mergeCols3)

horsemart <- full_join(horsemart, horsemart5_7, by = mergeCols3)

horsemart <- full_join(horsemart, horsemart7_10, by = mergeCols3)

horsemart <- full_join(horsemart, horsemart10_12, by = mergeCols3)

horsemart <- full_join(horsemart, horsemart12_14, by = mergeCols3)

horsemart <- full_join(horsemart, horsemart14_16, by = mergeCols3)

horsemart <- full_join(horsemart, horsemart16plus, by = mergeCols3)


# ADD SOURCE

horsemart$source <- 'Horsemart'

#REMOVE NAS

horsemart <- na.omit(horsemart)

#MAKE AGE NUMERIC

horsemart$age <- as.numeric(horsemart$age)

#CHANGE COLNAMES

names(horsemart)[names(horsemart)=="advert_price"] <- "price"
names(horsemart)[names(horsemart)=="advert_height"] <- "height"
names(horsemart)[names(horsemart)=="advert_name"] <- "title"



#HORSEMART VALUES ARE ALL IN HH. REMOVE LISTINGS THAT ARE 99HH

removefalseheights <- function(x){
  
  ifelse((x>20) == TRUE, 0, x)
}

horsemart$height <- removefalseheights(horsemart$height)


# MERGE DFS

mergeCols2 <- c("title", "price", "height", "age", "source" )
FULLDATABASE <- full_join(DATABASE, horsemart, by = mergeCols2)

#REMOVE PRICE OUTLIERS

FULLDATABASE <- FULLDATABASE %>%
  filter(FULLDATABASE$price < 99000)

#COUNT AND REMOVE DUPLICATES FROM TOTAL DF

duplicated(FULLDATABASE)

duplicates <- duplicated(FULLDATABASE)
duplicates <- duplicates %>% table()

FINALDATABASE <- distinct(FULLDATABASE)


#SWITCH OFF SCIENTIFIC NOTATION

options(scipen = 999)

#EXPORT YOUR CLEAN DF TO CSV

write.csv(FINALDATABASE, "augdata.csv")


