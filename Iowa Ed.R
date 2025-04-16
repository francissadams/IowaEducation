pacman::p_load(tidyverse, knitr,readxl,janitor,stringr)
library("class")
library("caret")
library("mlbench")

#read in the data
iowa_ed_raw<-read_csv("Iowa_5Year_Ed_Estimates.csv")

#cleaning the data, including removing columns and rows, fixing names, 
#ordering the counties, fixing missing entries, and making the categories an ordered factor
iowa_ed_raw2<-iowa_ed_raw |> 
  clean_names() |> 
  select(-c(1,4,7,13)) |> 
  filter(type=="county") |> 
  select(-1)
write_csv(iowa_ed_raw2,"./Iowa_Ed_Small.csv")

iowa_ed<-iowa_ed_raw |> 
  clean_names() |> 
  select(-c(1,4,7,9:11,13)) |> 
  filter(type=="county",data_collection_period=="2015-2019") |> 
  select(-c(1,4)) |> 
  mutate(name=(str_replace_all(name," County, Iowa",""))) |> 
  arrange(name,educational_category) |> 
  mutate(educational_category=if_else(is.na(educational_category),"Total Over 25",educational_category)) |> 
  mutate(variable_description=factor(variable_description),educational_category=ordered(educational_category,levels=c("Less than High School","High School Graduate","Some College or Associates Degree","Bachelors Degree or Higher","Total Over 25"))) 

summary(iowa_ed)

#We want to have a more manageable number of educational categories, so group them
iowa_ed_grouped<-iowa_ed |> 
  group_by(name,educational_category) |> 
  summarize(total=sum(population_estimate)) |> 
  pivot_wider(names_from=educational_category,values_from=total) |> 
  clean_names() |> 
  mutate(name=str_to_lower(name)) |> 
  mutate(less_than_high_school_pct=less_than_high_school/total_over_25,high_school_graduate_pct=high_school_graduate/total_over_25,
         some_college_or_associates_degree_pct=some_college_or_associates_degree/total_over_25,bachelors_degree_or_higher_pct=bachelors_degree_or_higher/total_over_25)

#upload the income and population data, clean the data, and get just the values in 2019
income_raw<-read_csv("Median_Income.csv")         
population_raw<-read_csv("20102019 Census Pop.csv") 
grid_raw<-read_csv("Iowa County Grid Numbers.csv")

population<-population_raw |>
  clean_names() |>
  select(c(1,13)) |>
  filter(geographic_area!="Iowa") |>
  mutate(geographic_area=str_replace_all(geographic_area," County, Iowa",""),geographic_area=str_remove_all(geographic_area,'\\.')) |>
  rename(population_2019=x2019)|>
  mutate(geographic_area=str_to_lower(geographic_area))

income<-income_raw |> 
  select(c(1,14)) |> 
  clean_names( )|> 
  rename(income_2019=x2019) |> 
  mutate(geographic_name=str_replace_all(geographic_name," County",""),income_2019=str_remove_all(income_2019,'\\$|.00$|\\,'),income_2019=as.numeric(income_2019))|> 
  mutate(geographic_name=str_to_lower(geographic_name))
str(income)

grid_numbers<-grid_raw |> 
  select(1:3) |> 
  clean_names() |>
  mutate(county_name=str_to_lower(county_name),x_location=factor(x_location,levels=1:12),y_location=factor(y_location,levels=c(1:4,4.5,5:9)))

#o'brien has the wrong apostraphe so we fix it to avoid joining issues later
grid_numbers[82,1]<-'o\'brien'

#get the map data, and fix the value for O'Brien County
ia_county <- map_data("county") %>% 
  filter(region == "iowa") |> 
  mutate(subregion=if_else(subregion=="obrien","o'brien",subregion))

#Make joined data sets, first with the map data and again with just the pop and income data added.
iowa_education_map<-ia_county |> 
  left_join(iowa_ed_grouped, by = c("subregion" = "name")) |> 
  left_join(income,by=c("subregion"="geographic_name")) 
# |>
#   left_join(population,by=c("subregion"="geographic_area"))

iowa_education_map 

iowa_education<-iowa_ed_grouped |> 
  left_join(income,by=c("name"="geographic_name"))
# |>
#   left_join(population,by=c("name"="geographic_area"))

iowa_education
summary(iowa_education)

iowa_education_grid<-iowa_education |> 
  left_join(grid_numbers,by=c("name"="county_name"))

#Do a quick check that the data isn't clearly wrong.
#filter(iowa_education,total_over_25>population_2019)
#iowa_education[2]+iowa_education[3]+iowa_education[4]+iowa_education[5]-iowa_education[6]

#Plot education by county. Choose different 
ggplot(iowa_education_map, aes(x = long, y = lat, 
                               fill = high_school_graduate_pct) )+
  geom_polygon(aes(group = group), 
               color = "black") + 
  coord_fixed(1.3)

ggplot(iowa_education_map, aes(x = long, y = lat, 
                               fill = bachelors_degree_or_higher_pct) )+
  geom_polygon(aes(group = group), 
               color = "black") + 
  coord_fixed(1.3)

ggplot(iowa_education_map, aes(x = long, y = lat, 
                               fill = income_2019) )+
  geom_polygon(aes(group = group), 
               color = "black") + 
  coord_fixed(1.3)

ggplot(iowa_education_map, aes(x = long, y = lat, 
                               fill = total_over_25) )+
  geom_polygon(aes(group = group), 
               color = "black") + 
  coord_fixed(1.3)

#In these maps, we can see Polk, Linn, and Johnson are the most educated,
#with the highest population and income. Though Scott has a high population but has less education.

head(arrange(population,desc(population_2019)),20)
#enormous drop in population after top 10, from 93K to 51K

#plot(x=iowa_education$total_over_25,y=iowa_education$population_2019)

summary(lm(data=iowa_education,bachelors_degree_or_higher_pct~(total_over_25+income_2019)^2))

hist(population$population_2019)
boxplot(population$population_2019)
hist(income$income_2019)
boxplot(income$income_2019)
head(arrange(income,desc(income_2019)),20)
#3 counties above 70K with one large outlier

small_counties_education<-iowa_education |> 
  semi_join(filter(population,population_2019<60000),join_by(name==geographic_area))
summary(small_counties_education)

summary(lm(data=small_counties_education,bachelors_degree_or_higher_pct~total_over_25+income_2019))
summary(lm(data=small_counties_education,bachelors_degree_or_higher_pct~total_over_25))
ggplot(data=small_counties_education,mapping=aes(x=total_over_25,y=bachelors_degree_or_higher_pct))+
  geom_point()+
  geom_smooth(method="lm")
ggplot(data=small_counties_education,mapping=aes(x=total_over_25,y=income_2019))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(data=small_counties_education,income_2019~total_over_25))


head(arrange(income,income_2019),20)
#one county below 40K, but not that much of an outlier

#do a test that high school grad percentage is greater than 33%
t.test(iowa_education$high_school_graduate_pct,mu=.34,alternative="g")


#run some chi-squared tests on the data, and then on the hs grad and below categories
O<-iowa_education[,2:5]
chisq.test(O)
E<-chisq.test(O)$expected
T<-(O-E)^2/E

O2<-iowa_education[,2:3]
chisq.test(O2)
E2<-chisq.test(O)$expected
T2<-(O2-E2)^2/E2

#check the predictive value of the grid location
summary(lm(data=iowa_education_grid,bachelors_degree_or_higher_pct~x_location+y_location+income_2019))

#do some clustering of counties
#This clustering uses raw numbers, so it is maybe just refelcting population.
#Try it again with the percentages and income, and maybe include population.
# med<-iowa_education_grid[,c(2:6,11)]
# mgrid<-iowa_education_grid[,c(2:6,11:13)]
# clust_ed<-kmeans(med, centers = 5, nstart = 10)
# clust_ed
# clust_grid<-kmeans(mgrid, centers = 5, nstart = 10)
# clust_grid
# clust_grid$cluster
#The clustering it gives is basically DM, CR and Quad Cities, next level like Ames and IC and IF and Dubuque
#Then smaller cities and then the smallest counties.

#This uses the percentages
med<-iowa_education_grid[,c(7:11)]
mgrid<-iowa_education_grid[,c(7:13)]
clust_ed<-kmeans(med, centers = 5, nstart = 10)
clust_ed
clust_grid<-kmeans(mgrid, centers = 5, nstart = 10)
clust_grid
clust_grid$cluster
#In this clustering, location doesn't do anything.

#Lets take out income.
# med<-iowa_education_grid[,c(7:10)]
# mgrid<-iowa_education_grid[,c(7:10,12,13)]
# clust_ed<-kmeans(med, centers = 5, nstart = 10)
# clust_ed
# clust_grid<-kmeans(mgrid, centers = 5, nstart = 10)
# clust_grid
# clust_grid$cluster

#In this case, including the grid just groups geographically.
#But without it, we get an interesting grouping. 
#We get Ames, IC, and WDM together. 
#Then CR, DM, Dav, Dub, CF, and others. 
#Interestingly north central and southwest are largely grouped together.
#Smaller counties in the eastern half largely together.
#Then a very spacially diverse last group.

#add the clusters to the data and use it color a county map
iowa_education_clustered<-cbind(iowa_education,clust_ed$cluster) |> 
  rename(clusters=...12) |> 
  mutate(clusters=factor(clusters,levels=1:5))

iowa_education_clustered_map<-ia_county |> 
  left_join(iowa_education_clustered, by = c("subregion" = "name"))

str(iowa_education_clustered_map)

ggplot(iowa_education_clustered_map, aes(x = long, y = lat, 
                               fill = clusters) )+
  geom_polygon(aes(group = group), 
               color = "black") + 
  coord_fixed(1.3)+
  scale_fill_brewer(palette = "Dark2")

#Now lets try it again using the data which includes the grid position.
iowa_education_grid_clustered<-cbind(iowa_education,clust_grid$cluster) |> 
  rename(clusters=...12) |> 
  mutate(clusters=factor(clusters,levels=1:5))

iowa_education_grid_clustered_map<-ia_county |> 
  left_join(iowa_education_grid_clustered, by = c("subregion" = "name"))

str(iowa_education_grid_clustered_map)

ggplot(iowa_education_grid_clustered_map, aes(x = long, y = lat, 
                                         fill = clusters) )+
  geom_polygon(aes(group = group), 
               color = "black") + 
  coord_fixed(1.3)+
  scale_fill_brewer(palette = "Dark2")

#Lets go back to the clustered data and analyze the clusters.
iowa_education_clustered |> 
  group_by(clusters) |> 
  summarize(mean_income=mean(income_2019),mean_bachelors_degree_or_higher_pct=mean(bachelors_degree_or_higher_pct),mean_population=mean(total_over_25))
#
