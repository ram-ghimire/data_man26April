library(tidyverse)
gapminder <- read_csv("data/gapminder.csv")

#select function
select(gapminder,country,year,pop,gdpPercap)
select(gapminder, 1,3,5,6)
select(gapminder,-continent,-lifeExp)

#select helper functions
select(gapminder,starts_with("co"))
select(gapminder,contains("e"))
select(gapminder,ends_with("p"))
select(gapminder,contains("p"))
select(gapminder,contains("p",ignore.case = F)) #not showing up results as in the example

select(gapminder,contains("p",ignore.case = F))

#rename(),new_name=old_name

select(gapminder,country_name=country,population=pop) #even with select verb, it selects two column and rename them.

rename(gapminder,country_name=country,population=pop)# with rename verb, it only renames defined two names of original df

rename(gapminder,life_exp=lifeExp,gdp_per_cap=gdpPercap)

#Fulter functions

filter(gapminder, country=="Australia")

filter(gapminder,year>=1997)

filter(gapminder,lifeExp>=80)

filter(gapminder,continent=="Europe")

#multiple conditions, output comes which meets all conditions

filter(gapminder, country=="Australia",year>=1997)

filter(gapminder, lifeExp>=80,gdpPercap>=30000)

#To extract rows with "OR" option

filter(gapminder,country=="Australia"|country=="New Zealand")

filter(gapminder,lifeExp>=80|gdpPercap>=30000)

# Using operator:%in% to select more multiple options

filter(gapminder,country %in% c("Australia","New Zealand"))

filter(gapminder,continent %in% c("Africa","Asia","Europe"))

#Mutate function-create new columns in data frame.

mutate(gapminder, gdp=gdpPercap*pop)

mutate(gapminder,pop_millions=pop/1e6)# this means e raised to the power of 6

mutate(gapminder,log_pop=log(pop))

#To abbreviate country name (any variable name),use str_sub() function

mutate(gapminder, country_abbr=str_sub(country,start = 1,end = 4))

#To find number of letters in a variable use str_length function,within mutate function

mutate(gapminder,num_letters=str_length(country))

#Any data of the same vectors can be used without even referring the data

#There are 1704 rwos in gapminder
index_numbers <- 1:1704
mutate(gapminder,row_num=index_numbers)

#Perform multiple mutations at the same time

mutate(
  gapminder,gdp=gdpPercap*pop,
  log_pop=log(pop)
  )
#Variable careation functions in mutate:lag() & lead(), also called offset functions

mutate(gapminder,life_exp_prv=lag(lifeExp),life_exp_next= lead(lifeExp))

#cumulative computations in mutate:cumsum(),cumprod(),cummean()

mutate(gapminder,cumulative_life_exp=cumsum(lifeExp))

#Challenge 4

diff_lifeExp <- mutate(gapminder,life_exp_change=lifeExp-lag(lifeExp))
filter(diff_lifeExp,life_exp_change==0)

#Summarise and group_by functions

summarise(gapminder,mean_life_exp=mean(lifeExp))

#Multiple summary functions

summarise(
  gapminder,
  mean_life_exp=mean(lifeExp),
  sd_life_exp=sd(lifeExp),
  mean_gdp_per_cap=mean(gdpPercap),
  max_gdp_per_cap=max(gdpPercap)
)

#Challenge 1: mean and median population of gapminder data

summarise(gapminder,mean_pop=mean(pop),median_pop=median(pop))

#Calculate mean life expectancy of all country; the use of group_by function

gapminder_by_country <- group_by(gapminder,country)
summarise(gapminder_by_country,mean_life_exp=mean(lifeExp))

#Challenge 2:Calculate mean and median population of each continent

gapminder_by_continent <- group_by(gapminder,continent)
summarise(gapminder_by_continent,mean_pop=mean(pop),median_pop=median(pop))

#Sorting results; ascending and descending order

arrange(gapminder,gdpPercap)
arrange(gapminder,desc(gdpPercap))

#Challenge 3; calculate average life expetancy per country, which has shortest & which has longest ?

summarised_life_expectancy <- summarise(gapminder_by_country,mean_life_exp=mean(lifeExp))
arrange(summarised_life_expectancy,mean_life_exp)
#shortest life expectancy of Sierra Leone=36.8 years

arrange(summarised_life_expectancy,desc(mean_life_exp))
#longest life expectancy of Iceland=76.5 years.

#Counting with n() function

summarise(gapminder,num_rows=n()) #no. of rows of whole dataset

summarise(gapminder_by_country,num_rows=n()) #no. of rows of partial dataset

summarise(gapminder_by_continent,num_rows=n()) #no. of rows of partial dataset

#n() function is very useful in calculating standard error; i.e standard error=standard deviation/square root of no. of samples

summarise(gapminder_by_country,se_pop=sd(pop)/sqrt(n()))

#Challenge 4: putting it all together, group_by is done for continent already

count_by_continent <- summarise(gapminder_by_continent,num_rows=n())
arrange(count_by_continent,num_rows)
arrange(count_by_continent,desc(num_rows))

#Adding & combining datasets; bind_rows() function is used when both datasets have same no. of columns.
#bind_columns() is used when both datasets have same no. of rows. 

gapminder_2012 <- read_csv("data/gapminder_2012.csv") # A new dataset with same no. of columns as gapminder.

combined_gapminder <- bind_rows(gapminder,gapminder_2012)

tail(combined_gapminder)

#The columns are matched by name. So, we need to make sure that both data frames have same column name.
#if the names donot match, the data frames will still be bound but any missing data will be replaced with NAs.

#let us create a data frame with a one column name different.

renamed_2012 <- rename(gapminder_2012,population=pop)
mismatched_names <- bind_rows(gapminder,renamed_2012)
tail(mismatched_names)

#Merging data: inner_join(),full_join(),left_join

#creating data frames:

df1 <- tibble(sample=c(1,2,3),measure1=c(4.2,5.3,6.1))
df2 <- tibble(sample=c(1,3,4),measure2=c(7.8,6.4,9.0))

inner_join(df1,df2)#joining by "sample"

full_join(df1,df2) #joining by "sample"

left_join(df1,df2)

left_join(df2,df1)#df2 as left data frame

#We can also control which columns are used to merge with the argument

full_join(df1,df2,by=c("sample")) #This is useful if the data frames share a number of column names.

#Let us create a third data frame with a different column name "ID"

df3 <- tibble(ID=c(1,2,4),measure3=c(4.7,3.5,9.3))

#Here we can't do full_join of df1 & df2; we need to specify by=c("left_name"="right_name")
full_join(df1,df3,by=c("sample"="ID"))

#Realistic data



































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































