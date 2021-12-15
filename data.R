## 4. Download the data set programatically --------------------------------------------------------------------------------------------------------
library(openxlsx);
library(dplyr);
census2015 <- read.xlsx('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/SDOH_ZCTA_2015.xlsx');
census2016 <- read.xlsx('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/SDOH_ZCTA_2016.xlsx');
census2015_2016 <- rbind(census2015, census2016)
ahrqCodebook2015 <- read.xlsx('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/sdoh_codebook_final.xlsx', sheet ='ZCTA_2015');
ahrqCodebook2016 <- read.xlsx('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/sdoh_codebook_final.xlsx', sheet ='ZCTA_2016');
ahrqCodebook2015_2016 <- rbind(ahrqCodebook2015, ahrqCodebook2016)
sdi <- read.xlsx('https://www.graham-center.org/content/dam/rgc/documents/maps-data-tools/sdi/ACS2015_zctaallvars.xlsx');
library(readr);
outcomes <- read_csv('data/OutcomeData.csv');
outcomes2015_2016 <- outcomes[which(outcomes$Year %in% c("2015", "2016")), ]

## 5. Merge and get into the same format--------------------------------------------------------------------------------------------------------
# check colnames
colnames(census2015_2016);
colnames(ahrqCodebook2015_2016);
colnames(sdi);
colnames(outcomes2015_2016);

# change colnames
colnames(ahrqCodebook2015_2016);
toupper(colnames(ahrqCodebook2015_2016));
colnames(ahrqCodebook2015_2016) <- toupper(colnames(ahrqCodebook2015_2016));
colnames(sdi);
toupper(colnames(sdi));
colnames(sdi) <- toupper(colnames(sdi));
colnames(outcomes2015_2016);
toupper(colnames(outcomes2015_2016));
colnames(outcomes2015_2016) <- toupper(colnames(outcomes2015_2016));
sdi$ZCTA <- as.character(sdi$ZCTA);

# finding the overlapped colnames
intersect(colnames(census2015_2016), colnames(ahrqCodebook2015_2016));
intersect(colnames(census2015_2016), colnames(sdi)); # 1
intersect(colnames(census2015_2016), colnames(outcomes2015_2016)); # 2
intersect(colnames(ahrqCodebook2015_2016), colnames(sdi));
intersect(colnames(ahrqCodebook2015_2016), colnames(outcomes2015_2016));
intersect( colnames(sdi), colnames(outcomes2015_2016)); # 1

# select 2015 and 2016 data, and merge data
# df <- merge(outcomes, census2015, by = intersect(colnames(census2015), colnames(outcomes)), all.x = TRUE) # intersect not good for this step
# dat0 <- merge(df, sdi, by = intersect(colnames(df), colnames(sdi)), all.x = TRUE)# intersect not good for this step
# df <- merge(outcomes, census2015, by = c("ZCTA", "YEAR"), all.x = TRUE)
# dat0 <- merge(df, sdi, by = "ZCTA", all.x = TRUE)
df <- dplyr::left_join(x=outcomes2015_2016, y=census2015_2016, by = c("ZCTA" = "ZCTA",  "YEAR" ="YEAR"))
dat0 <- dplyr::left_join(x=df, y=sdi, by = c("ZCTA" = "ZCTA"))

# create selection dataframe for columns to avoid NA
selection <- sapply(dat0, function(xx) {c("Missing.numbers" = sum(is.na(xx)),
                             "Missing.percentage" = sum(is.na(xx))/nrow(dat0),
                             "Is.numeric" = is.numeric(xx),
                             "Median.values" = ifelse( is.numeric(xx), median(xx, na.rm = TRUE), 999999999) ) }) %>%
  t %>% as.data.frame() %>% add_rownames
hist(selection$Missing.percentage, breaks = 200)
select.names <- subset(selection, Missing.percentage < 0.1 & Is.numeric == 1)$rowname # set 10% as the cutting line to select columns
character.names <- subset(selection, Is.numeric == 0)$rowname # c("ZCTA", "REGION", "STATE")
dat1 <- dat0[, c(character.names, select.names)]
sum(is.na(dat1))
#dat1 <- sapply(dat1, function(xx) {ifelse(is.na(xx), median(xx, na.rm = TRUE), xx)}) %>% as.data.frame() # replace NA with median value
#dat1 <- mutate(dat1,across(where(is.numeric),~coalesce(.x,median(.x,na.rm=T))));
dat1 <- mutate(dat1,across(where(is.numeric),function(xx){coalesce(xx,median(xx,na.rm=T))}));
sum(is.na(dat1))


## 6. Prepare analytical data set--------------------------------------------------------------------------------------------------------
set.seed(10)
table(sample(c("train", "test"), nrow(dat1), rep=T, prob = c(1/2, 1/2)) )/nrow(dat1)
dat1$sample <- sample(c("train", "test", "othere"), nrow(dat1), rep=T, prob = c(1, 1, 8))
dtrain <- subset(dat1, sample == "train")
dtest <- subset(dat1, sample=="test")
nrow(dtrain)
nrow(dtest)

# save data
saveRDS(dat0, file = "data/dat0.Rds")
saveRDS(dtrain, file = "data/dtrain.Rds")
saveRDS(dtest, file = "data/dtest.Rds")


## Run the whole scripts-------------------------------------------------------------------------------
# source("C:/Users/niej/Desktop/2021-08-23 Certificate in Biomedical Data Science/TSCI 5230 Analytical Programming for Biomedical Data Science/FA21TSCI5230_project/data.R", echo=TRUE)
