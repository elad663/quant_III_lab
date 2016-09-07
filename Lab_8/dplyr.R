#################################################################
# data manipulation with dplyr and data.table
# Lab 8, 27.10.14
# Quant III, Elad Zippory (an extension of a dplyr vignette)
#################################################################

# install.packages(c("dplyr","data.table","nycflights13"))

# Why dplyr or data.table?
# 1) friendlier language, but still a language.
# 2) always faster
# 3) can connect to DB's without you knowing sql.
# 4) very easy grouping, very easy joins.
# 5) consistent and as bug free as it gets.

# dplyr or data.table?
# see discussion here
# http://stackoverflow.com/questions/21435339/data-table-vs-dplyr-can-one-do-something-well-the-other-cant-or-does-poorly
# my take? "pick one" for most things, complement it with the other if you cannot find those features.
# but that should not happen more than once in a project, and that is also pretty a lot.

# "pick one" READ ALL THE DOCUMENTATION ONCE. should take 1 day tops.

# preliminary introduction of dplyr and data.table
# dplyr works in verbs, while data.table works in [] syntex
# dplyr has "filter", "select","rename","distinct" functions to perform these verbs and more
# data.table works in the matrix syntax of R, but once an object a data.table,
# that syntex means something else. DT[i,j,by]
# https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf


rm(list=ls())


library(dplyr)
library(data.table)
library(nycflights13)

# ?flights
dim(flights)
head(flights)
flights <- flights
DT_flights <- data.table(flights)

# select rows by a condition
#------------------------------------------
# dplyr
filter(flights, month == 1, day == 1)
filter(flights, month == 1 | month == 2)

# DT
DT_flights[month==1 | month==2,,]
DT_flights[month %in% 1:2,,]

# select chunk of rows
#------------------------------------------
# dplyr
slice(flights, 10:150)

# DT
DT_flights[10:150,,]

# arrange
#------------------------------------------
# dplyr
arrange(flights,month, desc(arr_delay))

# DT
DT_flights[order(month,-arr_delay),,]

# select columns
#------------------------------------------
#dplyr
select(flights, month, arr_delay)
select(flights, month,dep_time:arr_delay)

#DT
DT_flights[,.(month,arr_delay),]
DT_flights[,c("month","arr_delay"),with=F]


# rename()
#------------------------------------------
#dplyr
rename(flights, tail_num = tailnum)

#DT - this can be vectorized
setnames(DT_flights,old = "tailnum",new = "tail_num")


# Retain only unique/distinct rows
#------------------------------------------
#dplyr
distinct(select(flights, origin, dest))

#DT
setkey(DT_flights,origin, dest)
unique(DT_flights)

# manipulate
#------------------------------------------
# dplyr
flights=mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60))

select(flights,month,gain,gain_per_hour)

#DT
# DT is a bit more tricky at first, but pay attention
# I) .() operator means return only the new created variable
# II) ':=' operator means create new columns in existing DT_flights
# DT[,':='(X=x1+x2,Y=y2+y2),]
# 
# III) ':=' a second time
# c('X', 'Y') := list(x1+x2,X+y2)

# I)
DT_flights[,.(
  gain=arr_delay-dep_delay,
  something= arr_time / (air_time / 60)),]

# II)
# this won't work as gain_per_hour depends on gain
DT_flights[,`:=`(
  gain = arr_delay - dep_delay,
  gain_per_hour = gain / (air_time / 60)),]

# III)
# so let's chain
DT_flights[,gain := arr_delay - dep_delay,][
  ,gain_per_hour := gain / (air_time / 60),]


# DT - could not find an organic implementation

# summarizing
#------------------------------------------
# dplyr
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE))

# DT - and you can already see that DT[i,j,by] is ready to do more in that line
DT_flights[,.(delay = mean(dep_delay, na.rm = TRUE)),]


# with grouping
#------------------------------------------
# dplyr uses group_by()
planes <- group_by(flights, tailnum)
delay <- summarise(planes,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))

# DT
DT_flights[,.(count=.N,
              dist = mean(distance, na.rm = TRUE),
              delay = mean(arr_delay, na.rm = TRUE)),
           by=tail_num]

DT_flights[,`:=`(count=.N,
              dist = mean(distance, na.rm = TRUE),
              delay = mean(arr_delay, na.rm = TRUE)),
           by=tail_num]

#------------------------------------------

# Join
#------------------------------------------
# DT
setkey(DT_1,"x")
setkey(DT_2,"x")

DT_1[DT_2]

# dplyr
DT_1 %>% left_join(DT_2)




















# AND THERE ARE MANY MORE FEATURES IN EACH PACAKGE.

