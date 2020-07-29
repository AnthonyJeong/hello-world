# library we're going to use
library(tidyverse)

# read in that data
farmData <- read_csv("./input/data.csv")

# print the first ten rows of our dataframe
head(farmData, 10)

# get the number of rows and columns in our dataset
dim(farmData)

c(1,2,5,5,10) %>% # concatenate these five numbers and then...
  mean() %>% # get the mean and then...
  prod(5) # multiply by five


# save our vector to a test variable
myVariable <- "some text"

# save a copy of our test variable
myVariable_beforePiping <- myVariable

# do some piped operations
myVariable %>%
  strsplit(split = "") %>% # split into characters
  unlist() %>% # convert list to vector
  sort() # alphabatize

# check to make sure that our original variable hasn't changed
identical(myVariable, myVariable_beforePiping)


# for example the function round(x,n) rounds the values 
# of x to the nearest n decimal places

# this will round the first number to two dicimal places,
# *not* the second number to 3 decimal places
piped <- 3.14152 %>%
  round(1.5690)

piped == round(3.14152, 1.5690) # the same as the above pipeline
piped == round(1.5690, 3.14152) # different than the above pipelines


# get a list of all column names in alphabetic order 
columnsAlphaOrder <- farmData %>% # take farmData and...
  names() %>% # get the column names and...
  sort() # sort alphabetically

# print the first six results
head(columnsAlphaOrder)


farmData %>% # take the farmData dataset and...
  select(gender1) %>% # select the column gender1 and...
  head() # return the first six lines

farmData %>% # take the farmData data frame and...
  select(-gender1) %>% # select every column *except* gender1 and...
  head() # return just the first six lines


farmData %>% # take the farmData data frame and...
  select(starts_with("gender")) %>% # select every column that starts with "gender" and ...
  head() # return just the first six lines

farmData %>% # take the farmData data frame and...
  select(-starts_with("gender")) %>% # select all columns that don't start with "gender"...
  head() # return just the first six lines

farmData %>% # take farmData and...
  filter(vname == "Tikare") # return only the rows where the vname is Tikare

# rows where the number of farm plots is great than or equal to 6
farmData %>% 
  filter(fplots >= 6)

# this cell WILL NOT work
farmData %>% 
  filter(vname == "Tikare" | "Sefula")

# get rows where the village is Tikare or Sefula
farmData %>% 
  filter(vname == "Tikare" | vname == "Sefula")

# get rows where the village is Bonanza and there are less than three farm plots
farmData %>% 
  filter(vname == "Bonanza" & fplots < 3)
# add a variable for whether this household owns land
farmData <- farmData %>%
  mutate(landowner = (tenure1 == 1 | tenure1 == 2)) 

# summerize just our new variable
summary(farmData$landowner)

# get the number of men for each household
countOfMen <- farmData %>% 
  select(starts_with("gender")) %>% # get only the columns with gender information
  magrittr::equals(1) %>% # look at the whole dataframe to see if each cell has "1" in it
  rowSums(na.rm = T) # count the number of times when this is true, by row

# get the nubmber of women for each household
countOfWomen <- farmData %>% 
  select(starts_with("gender")) %>% 
  magrittr::equals(2) %>%
  rowSums(na.rm = T)

# add these columns to our dataframe
farmData <- farmData %>% 
  mutate(men = countOfMen, women = countOfWomen) # add our new variables

# add a new column with the total number of men + women
farmData <- farmData %>%
  mutate(menPlusWomen = men + women)

# check to see how many households have a reported household size different from
# the number of men & women who reported their gender
(farmData$hhsize == farmData$menPlusWomen) %>%
  na.omit() %>%
  table()

#arrange()

farmData %>%
  arrange(menPlusWomen) %>% # sort by the total number of men + women
  head() # just return the first six rows

farmData %>%
  select(menPlusWomen, hhsize) %>% # get just the menPlusWomen and hhsize columns
  arrange(menPlusWomen) %>% # sort by the total number of men + women
  head()

farmData %>% 
  arrange(desc(menPlusWomen)) %>% # sort by the total number of men + women, descending
  head()  # just return the first six rows

farmData %>%
  select(menPlusWomen, hhsize) %>% # get just the menPlusWomen and hhsize columns
  arrange(desc(menPlusWomen)) %>% # sort by the total number of men + women, descending
  head() # just return the first six rows

farmData %>%
  select(menPlusWomen, hhsize) %>% # get just the menPlusWomen and hhsize columns
  arrange(hhsize) %>% # sort by the household size
  head() # just return the first six rows

farmData %>% 
  select(menPlusWomen, hhsize) %>% # get just the menPlusWomen and hhsize columns
  arrange(hhsize) %>% # sort by the household size
  tail() # just return the last six rows


#summarize()

farmData %>%
  summarise(meanMenPlusWomen = mean(menPlusWomen), meanhhsize = mean(hhsize, na.rm = T))

farmData %>%
  summarise(missingMenPlusWomen = sum(menPlusWomen == 0), missinghhsize = sum(is.na(hhsize)))

#group by()

farmData %>%
  group_by(landowner) %>%
  summarise(plots = median(fplots, na.rm = T))

farmData %>%
  group_by(fplots) %>%
  tally()

farmData %>%
  group_by(landowner, fplots) %>%
  tally() %>%
  na.omit()

farmData %>%
  group_by(interviewer) %>%
  tally() %>%
  filter( n > 50) %>%
  arrange(desc(n))

adaptationsCount <- farmData %>%
  select(contains("ad71")) %>%
  rowSums()

adaptationsCount

farmData %>%
  mutate(adaptations = adaptationsCount) %>%
  group_by(adaptations) %>%
  tally()

farmData %>%
  mutate(adaptations = adaptationsCount) %>%
  group_by(landowner) %>%
  summarise(numberOfAdaptations = mean(adaptations, na.rm = T))

