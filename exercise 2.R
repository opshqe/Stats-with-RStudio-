###############################################
#Sheet II: 
###############################################

library(tidyverse)

#1

student1  <- tibble(
  student = c("Adam","Bernd","Christian","Doris"),
  algebra = c(NA, 5, 3, 4),
  analysis = c(2, NA, 1,3),
  diskrete.math = c(3,NA,2,4),
)
student1

student2 <- tibble(
  name = rep(c("Adam", "Bernd", "Christian", "Doris"), each = 2),
  type = rep(c("height", "weight"), 4),
  measure = c(1.83, 81, 1.75, 71, 1.69, 55, 1.57, 62))
student2

student3 <- tibble(
  name = c("Adam", "Bernd", "Christian", "Doris"),
  ratio = c("81/1.83", "71/1.75", "55/1.69", "62/1.57"))

# student1: contains 36 values representing three variables and 12 observations. 
# The variables are: name, exam, grade
# Every combination of name and exam is a single measured observation. 

# student2 and student3: contains 12 values representing three variables and 3 observations. 
# The variables are: name, height, weight
# The 3 single measured observations are the values of height and weight for every name.

# Why are these datasets are not tidy?
# one column should be one variable and thats why its not a tidy dataset

# tidy versions
student1 %>% 
  gather('algebra','analysis','diskrete.math', 
         key = "exam", value = "grade")

student2 %>%
  spread(key = type, value = measure)

student3 %>% 
  separate(col = ratio, into = c("weight","height"), sep = "/")


#2 
#a)
sin(log((5+3)**0.5))
(5+3) %>% sqrt %>% log() %>% sin()


#b)
v <- seq(from = 0.5, to = 5, by = 0.5)


round(sum(log(v**0.5)),2) 
#or 
v %>% sqrt() %>% log() %>% sum() %>% round(2) 


#3
df <- tibble(
  id = 1:10,
  sex = sample(x =c("f","m"), size = 10,
               replace = TRUE),
  age = round(runif(10,20,35)),
  score1 = round(runif(10,0,25))
)
df

#a)
df %>% filter(sex == "m")

#b)
df <- add_row(id = 11, sex = "m", age = 25, score1 = 4)
df

#c)
df <-
  df %>%
  mutate(score2 = round(runif(11,0,25))) %>%
  mutate(score3 = round(runif(11,0,25))) %>%
  mutate(scoresum = score1+score2+score3) %>%
  
#mutate(score3 = round(runif(11,0,25))) %>%
#mutate(scoresum = score1+score2+score3)) %>%
  
  grade = case_when(
    scoresum > 37 
  )

#d)
df %>% 
  arrange(sex) %>%
  select(id,sex,grade) %>%
  filter(grade < 5)

#e)
df %>%
  group_by(sex) %>%
  summarise(mean_scores = mean(scoresum),
            min_scores = min(scoresum),
            max_scores = max(scoresum),
            med_scores = median(scoresum))


#4a)
library(nycflights13)
?flights()

library(tidyverse)

#b)
res.x <-
  flights %>% filter(arr_delay > 120)
#120 because of an hour consists of 60min
res.x


#c)
res.y <-
  flights %>% filter(arr_delay > 120 & dep_delay <= 0)
res.y 

#d)
res.z <-
  #AA-american, DL-delta, UA-united
  flights %>% filter(carrier %in% c("AA", "DL", "UA") & dep_delay <=0)
res.z

#e)
res.e <-
  flights %>% filter(carrier %in% c("UA", "AA", "DL") & month == 5 & arr_delay > 300) %>%
  select(carrier, flights) %>%
  arrange(carrier, flights) %>%
  unique()
res.e 

#f)
new.flights <-
  flights %>%
  mutate(
    dep_time = (dep_time %/% 100)*60 + dep_time %% 100, 
         arr_time = (arr_time %/% 100)*60 + arr_time %% 100)
new.flights


#g)
res.g <-
  mutate(
    speed = distance / air_time * 60)%>%
    select(carrier, flights, speed) %>%
    arrange(desc(speed)) %>%
    top_n(10, speed)
res.g


#h)
res.h <- 
  flights %>%
  filter(!is.na(arr_delay)) %>%
  mutate(bool_del = if_else(arr_delay < 10,1,0)) %>%
  group_by(carrier) %>%
  mutate(nof = n(),
         ndel =sum(bool_del),
         del_ratio = ndel/nof) %>%
  select(carrier,nof,del_ratio)%>%
  unique()
  arrange(desc(del_ratio))
res.h




