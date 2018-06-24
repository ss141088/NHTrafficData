getwd()


NH_Cleaned<-read.csv("NH_cleaned.csv")
NH_Cleaned

library("dplyr")
tibble_NH_cleaned <- tbl_df(NH_Cleaned)
tibble_NH_cleaned

library(ggplot2)
library(plotly)

#bayleign code

speed <- tibble_NH_cleaned %>%
  group_by(county_name) %>%
  filter(violation=="Speeding")
data.frame(speed)

s <- ggplot(speed, aes(x = county_name, fill = driver_gender)) + geom_bar(position = "dodge")
s <- s + ggtitle("County Name vs Speeding Violations by Gender")
s <- s + xlab("County Names")+ylab("Frequency of Speeding Violations")
ggplotly(s)


##Phoebes code
NH <- tbl_df(NH_Cleaned)

#creating columns for month, year, and sex
NH$month_nh <- month(ymd(NH$stop_date),label = TRUE)
NH$year_nh <- year(ymd(NH$stop_date))
y <- table(NH$driver_gender)
lut <- c("M" = "Male", "F" = "Female")
NH$sex <- lut[NH$driver_gender]

#boxplot of the months
n <- ggplot(NH, aes(x=month_nh,fill = sex)) + geom_bar(position = "dodge")
n <- n + ggtitle("Stops per Month by Gender")
n <- n + theme_bw()
n <- n + theme(legend.position = "bottom")
#n <- n + geom_text(stat = 'count', aes(label=..count..), position = position_dodge(0.7),vjust=-0.5)
n

##Shatrughan code
#given someone is stoped for stop sign/light what is the average age by gender 

library(dplyr)
library(Hmisc)
library(lubridate)


NH_data <- NH_Cleaned %>%
  group_by(driver_gender)%>%
  filter(violation =="Stop sign/light")%>%
  filter(driver_gender == "M"| driver_gender =="F")%>%
  summarise(mean = mean(driver_age ,na.rm = TRUE))

library(ggplot2)
library(plotly)

stopsign <- ggplot(NH_data, aes(x = NH_data$driver_gender ,y=NH_data$mean, fill = NH_data$mean)) + geom_bar(stat = "identity", position = "dodge")
stopsign <- stopsign + ggtitle("Average Age of Driver Gender")
stopsign <- stopsign + xlab("driver_gender") + ylab(" stop_sign/light violation")
ggplotly(stopsign)
stopsign





##Nicks code

library(Hmisc)
library(dplyr)
library(ggplot2)
library(plotly)
#What gender/race combo had the best/worst odds of getting a ticket/summons?
nh.data <- tbl_df(NH_Cleaned)
nh.data


ticket.or.summons <- c("Ticket", "Summons")

ticket.or.summons.col <- filter(nh.data, nh.data$stop_outcome %in% ticket.or.summons)
ticket.or.summons.col

white.male <- ticket.or.summons.col %>%
  filter(driver_race == "White") %>%
  filter(driver_gender == "M")

tot.white.male.ticket = count(white.male)

# Total white females that got a ticket
white.female <- ticket.or.summons.col %>%
  filter(driver_race == "White") %>%
  filter(driver_gender == "F")

tot.white.female.ticket = count(white.female)


# Total hispanic males that got a ticket
hispanic.male <- ticket.or.summons.col %>%
  filter(driver_race == "Hispanic") %>%
  filter(driver_gender == "M")

tot.hispanic.male.ticket = count(hispanic.male)

# Total hispanic females that got a ticket
hispanic.female <- ticket.or.summons.col %>%
  filter(driver_race == "Hispanic") %>%
  filter(driver_gender == "F")

tot.hispanic.female.ticket = count(hispanic.female)

# Total black males that gota ticket
black.male <- ticket.or.summons.col %>%
  filter(driver_race == "Black") %>%
  filter(driver_gender == "M")

tot.black.male.ticket = count(black.male)


# Total black females that got a ticket
black.female <- ticket.or.summons.col %>%
  filter(driver_race == "Black") %>%
  filter(driver_gender == "F")

tot.black.female.ticket = count(black.female)

# Total asian males that got a ticket
asian.male <- ticket.or.summons.col %>%
  filter(driver_race == "Asian") %>%
  filter(driver_gender == "M")

tot.asian.male.ticket = count(asian.male)


# Total asain females that got a ticket
asian.female <- ticket.or.summons.col %>%
  filter(driver_race == "Asian") %>%
  filter(driver_gender == "F")

tot.asian.female.ticket = count(asian.female)  
tot.asian.female.ticket

# My list of total's from each group
new.nh <- list(col1 = c(tot.asian.female.ticket), col2 = c(tot.asian.male.ticket), col3 = c(tot.black.female.ticket), col4 = c(tot.black.male.ticket), col5 = c(tot.hispanic.female.ticket), col6 = c(tot.hispanic.male.ticket), col7 = c(tot.white.female.ticket), col8 = c(tot.white.male.ticket))
names(new.nh) <- c("Total Asian Female T/S","Total Asian Male T/S","Total Black Female T/S","Total Black Male T/S","Total Hispanic Female T/S","Total Hispanic Male T/S","Total White Female T/S","Total White Male T/S")
new.nh

# Finding overall totals for each gender/age combo
w.m.t <- nh.data %>%
  filter(driver_race == "White")%>%
  filter(driver_gender == "M")
total.w.m.t <- count(w.m.t)


w.f.t <- nh.data %>%
  filter(driver_race == "White")%>%
  filter(driver_gender == "F")
total.w.f.t <- count(w.f.t)


h.m.t <- nh.data %>%
  filter(driver_race == "Hispanic")%>%
  filter(driver_gender == "M")
total.h.m.t <- count(h.m.t)

h.f.t <- nh.data %>%
  filter(driver_race == "Hispanic")%>%
  filter(driver_gender == "F")
total.h.f.t <- count(h.f.t)

b.m.t <- nh.data %>%
  filter(driver_race == "Black")%>%
  filter(driver_gender == "M")
total.b.m.t <- count(b.m.t)

b.f.t <- nh.data %>%
  filter(driver_race == "Black")%>%
  filter(driver_gender == "F")
total.b.f.t <- count(b.f.t)

a.m.t <- nh.data %>%
  filter(driver_race == "Asian")%>%
  filter(driver_gender == "M")
total.a.m.t <- count(a.m.t)

a.f.t <- nh.data %>%
  filter(driver_race == "Asian")%>%
  filter(driver_gender == "F")
total.a.f.t <- count(a.f.t)

# Finding ratio of each combo to get a ticket/summons
percent.w.m <- tot.white.male.ticket/total.w.m.t
percent.w.f <- tot.white.female.ticket/total.w.f.t
percent.h.m <- tot.hispanic.male.ticket/total.h.m.t
percent.h.f <- tot.hispanic.female.ticket/total.h.f.t
percent.b.m <- tot.black.male.ticket/total.b.m.t
percent.b.f <- tot.black.female.ticket/total.b.f.t
percent.a.m <- tot.asian.male.ticket/total.a.m.t
percent.a.f <- tot.asian.female.ticket/total.a.f.t


results <- c(percent.w.m, percent.w.f, percent.h.m, percent.h.f, percent.b.m, percent.b.f, percent.a.m, percent.a.f)


results

results <- list(col1 = c(percent.w.m), col2 = c(percent.w.f), col3 = c(percent.h.m), col4 = c(percent.h.f), col5 = C(percent.b.m), col6 = c(percent.b.f), col7 = c(percent.a.m), col8 = c(percent.a.f))
#names(results) <- c("Percent White Male T/S","Percent White Female T/S","Percent Hispanic MaleT/S","Percent Hispanic Female T/S","Percent Black Male T/S","Percent Black Female T/S","Percent Asian Male T/S","Percent Asian Female T/S")
results

results.df <- data.frame(Reduce(rbind, results))
name.of.rows <- c("White Male","White Female","Hispanic Male","Hispanic Female", "Black Male","Black Female","Asian Male","Asian Female")
results.df$names <- name.of.rows
names(results.df) <- name.of.rows


# Changing Column Names
colnames(results.df) <- c("Percentage", "Categories")

# Bar Plot
g <- ggplot(results.df, aes(x=Categories, y=Percentage, fill=Percentage)) + geom_bar(stat = 'identity')
g <- g + ggtitle("Ticket or Summons % by Race")
g <- g + theme_bw()
ggplotly(g)
g


###################################################################################################################
# was there a higher occurance of Tickets and Warnings for out of state drivers that were pulled over for speeding?
###################################################################################################################

in.or.out.state.speeding <- nh.data %>%
  group_by(violation = "Speeding") %>%
  count(out_of_state, wt = NULL, sort = FALSE)

ticket.or.not.in <- nh.data %>%
  group_by(violation = "Speeding", stop_outcome, out_of_state) %>%
  count(out_of_state, wt = NULL, sort = FALSE) %>%
  filter(out_of_state == "FALSE")

ticket.or.not.out <- nh.data %>%
  group_by(violation = "Speeding", stop_outcome, out_of_state) %>%
  count(out_of_state, wt = NULL, sort = FALSE) %>%
  filter(out_of_state == "TRUE")

ticket.or.not.out
ticket.or.not.in

Percent_out_state_t_w <- (30720+2040)/sum(ticket.or.not.out$n)
Percent_in_state_t_w <- (42370+3397)/sum(ticket.or.not.in$n)

Percent_out_state_t_w
Percent_in_state_t_w

percent_out_in <- list(col1 = c(Percent_out_state_t_w), col2 = c(Percent_in_state_t_w))
in_out.df <- data.frame(Reduce(rbind, percent_out_in))
in_out.df


name.of.rows2 <- c("Out of State", "In State")
in_out.df$names <- name.of.rows2

colnames(in_out.df) <- c("Percent", "In_or_Out_State")

g2 <- ggplot(in_out.df, aes(x=In_or_Out_State, y=Percent, fill=Percent)) + geom_bar(stat = 'identity', position = "dodge")
g2 <- g2 + ggtitle("Given Speeding: % Ticket or Summons by In vs Out State")
g2 <- g2 + theme_bw()
g2 <- g2 + xlab("In State vs Out of State")
ggplotly(g2)
g2




