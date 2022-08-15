#---------------Preparation code------------------------
library(tidyverse)
library(writexl)
library(readxl)
library(lobstr)
library(dplyr)
library(tidyr)
library(stringi)
library(ggplot2)
library(plotly)
rm(list = ls())
dot_colors <- c("purple", "green", "red", "darkorange", "comflowerBlue", "SkyBlue","Turauoise", "Cyan", "SteelBlue",
                "goldenrod", "yellow", "indianred", "Yellowgreen")
#PART 1 data loading 'spxallchart0', 'interestrate'
setwd("C:/Users/INSAGNIFICANT/Downloads/R")
options(digits = 6)
#Get covid data from the internet
#covid_url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
#covid <- read.csv(url(covid_url))
covid <- read.csv('owid-covid-data.csv')
str(covid)
summary(covid)
#Get SPX close data from the internet
spx_url <- "https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=-1325635200&period2=1590969600&interval=1d&events=history"
spx <- read.csv(url(spx_url))
str(spx)
#--USA
covid$iso_code <- as.character(levels(covid$iso_code))[covid$iso_code]
covid$location <- as.character(levels(covid$location))[covid$location]
covid$date <- as.Date(levels(covid$date))[covid$date]
covidusa <- covid[covid$iso_code == "USA",]
str(covidusa)
#prepare SPX data, period since 2019-12-31 and vacations filled
str(spx_short)
spx$Date <- as.Date(levels(spx$Date))[spx$Date]
spx_short <- filter(spx, spx$Date >= '2019-12-31')[,c(1,5)]
    #create tmp table with dates since 2019-12-31
tmp_date <- as.data.frame(seq(from = as.Date('2019-12-31'), by = 1, to = as.Date('2021-08-10')))
#tmp_date
    #name column as in the spx_short table to merge similar 'Date' columns
colnames(tmp_date) <- c("Date")
tmp <- merge(spx_short, tmp_date, all = TRUE)
nrow(tmp)
nrow(avg_data_world)
spx_short <- fill(tmp, Close)
rm(spx, tmp_date, tmp, spx_url, covid_url, t, type, temporary_var)
#------------------TEST regression-------------------------------------
sol <- glm(total_deaths ~ total_cases, data = covid)
summary(sol)
ggplot(NULL) +
  geom_smooth(data = sol, aes(total_cases, total_deaths), method = 'lm')
tmp <- lm(total_deaths ~ date, data = covid)
predict(tmp, data.frame(date = as.Date('2022-05-28')))
#-----------Group_by DATE---create table "covid AVERAGE DATA"-----------------------
temporary_var <- covid[!covid$location == 'World',]
avg_data_world <- temporary_var %>%
group_by(date) %>%
summarise(total_cases = sum(total_cases),
          total_deaths = sum(total_deaths),
          new_cases = sum(new_cases),
          new_deaths = sum(new_deaths),
          aged_65_older = sum(aged_65_older, na.rm = T),
          aged_70_older = sum(aged_70_older, na.rm = T),
          extreme_poverty = mean(extreme_poverty, na.rm = T),
          diabetes_prevalence = sum(diabetes_prevalence, na.rm = T),
          female_smokers = sum(female_smokers, na.rm = T),
          male_smokers = sum(male_smokers, na.rm = T),
          population = sum(population, na.rm = T),
          gdp = sum(gdp_per_capita, na.rm = T))
str(avg_data_world)
part_avg <- avg_data_world[avg_data_world$date >= '2021-08-10',]
rm(temporary_var)
head(avg_data_world)
tail(avg_data_world)
head(part_avg)
#-------------REGRESSION Date--World Data, data group_by DATE----------------------------
#1total_deaths ~
avg_data_world$total_deaths
sol <- glm(total_deaths ~ aged_65_older + aged_70_older + extreme_poverty + diabetes_prevalence + female_smokers + male_smokers +population + gdp, data = avg_data_world)
sol2 <- glm(new_deaths ~ aged_65_older + aged_70_older + extreme_poverty + diabetes_prevalence + female_smokers + male_smokers +population + gdp, data = avg_data_world)
#2
sol <- glm(total_deaths ~ aged_65_older + aged_70_older + extreme_poverty + diabetes_prevalence + female_smokers + male_smokers +population + gdp, data = part_avg)
sol2 <- glm(new_deaths ~ aged_65_older + aged_70_older + extreme_poverty + diabetes_prevalence + female_smokers + male_smokers +population + gdp, data = part_avg)

summary(sol)
summary(sol2)
#-----------Group_by Country---create table "covid AVERAGE DATA"-----------------------
avg_data_location <- covid %>%
  group_by(location) %>%
  summarise(total_cases = max(total_cases),
            total_deaths = max(total_deaths),
            aged_65_older = mean(aged_65_older, na.rm = T),
            aged_70_older = mean(aged_70_older, na.rm = T),
            extreme_poverty = mean(extreme_poverty, na.rm = T),
            cvd_death_rate = mean(cvd_death_rate, na.rm = T),
            diabetes_prevalence = mean(diabetes_prevalence, na.rm = T),
            female_smokers = mean(female_smokers, na.rm = T),
            male_smokers = mean(male_smokers, na.rm = T),
            population = mean(population, na.rm = T),
            gdp = max(gdp_per_capita))
head(avg_data_location)
tail(avg_data_location)
avg_data_location <- avg_data_location[-209,]
which(avg_data_location$location == 'World')
#-------------REGRESSION Location--World Data, data group_by LOCATION----------------------------
sol <- glm(total_deaths ~ aged_65_older + aged_70_older + extreme_poverty + cvd_death_rate + diabetes_prevalence + female_smokers + male_smokers + population + gdp, data = avg_data_location)
#Group by country sum all new deaths and value is equal total_death
sol <- glm(total_deaths ~ aged_65_older + aged_70_older + extreme_poverty + cvd_death_rate + diabetes_prevalence + female_smokers + male_smokers + population + location, data = part_avg)
sol2 <- glm(new_deaths ~ aged_65_older + aged_70_older + extreme_poverty + cvd_death_rate + diabetes_prevalence + female_smokers + male_smokers + population + location, data = part_avg)

summary(sol)
summary(sol2)
avg_data_location[avg_data_location$location == "United States",]$diabetes_prevalence
#--------------------population------------
rownames(avg_data_location) <- avg_data_location$location
ggplot(data = sol, aes(population, total_deaths, label = rownames(data))) +
  geom_smooth(method = 'lm', se = F) +
  geom_point() +
 labs(title = "Group By Date. Population - Total_Deaths.")
 #labs(title = "Group By Countries. Population - Total_Deaths.", x = "Country population")
ggplot(data = sol2) +
  geom_smooth(aes(population, new_deaths), method = 'lm', se = F) +
  geom_point(aes(population, new_deaths))  +
  labs(title = "Group By Date. Age 65 - Total_Deaths.", x = "Country population")
        #----------------without USA, India, China = wuic
which(avg_data_location$location == 'United States')
which(avg_data_location$location == 'China')
which(avg_data_location$location == 'India')
wuic <- avg_data_location[c(-201,-42,-91),]
sol_wuic <- glm(total_deaths ~ aged_65_older + aged_70_older + extreme_poverty + cvd_death_rate + diabetes_prevalence + female_smokers + male_smokers + population + location + gdp, data = wuic)
ggplot(data = sol_wuic, aes(population, total_deaths, label = rownames(data))) +
  geom_smooth(method = 'lm', se = F) +
  geom_point() +
  labs(title = "Relationship b/n population and total_deaths exluding USA, China, India")
ggplot(data = sol_wuic, aes(gdp, total_deaths, label = rownames(data))) +
  geom_smooth(method = 'lm', se = F) +
  geom_point() +
  labs(title = "Relationship b/n GDP and total_deaths exluding USA, China, India")

summary(sol_wuic)
#--------------------aged_65_older------------------------------
         #total_deaths ~ aged_65_older
         #new_deaths ~ aged_65_older
ggplot(data = sol) +
  geom_smooth(aes(aged_65_older, total_deaths), method = 'lm', se = F) +
  geom_point(aes(aged_65_older, total_deaths)) +
  labs(title = "Group By Date. Age 65 - Total_Deaths.")
# labs(title = "Group By Countries. Age 65 - Total_Deaths.")
ggplot(data = sol2) +
  geom_smooth(aes(aged_65_older, new_deaths), method = 'lm', se = F) +
  geom_point(aes(aged_65_older, new_deaths)) +
  labs(title = "Group By Date. Age 65 - New_Deaths.")
# labs(title = "Group By Countries. Age 65 - New_Deaths.")
#--------------------aged_70_older------------------------------
        #total_deaths ~ aged_70_older
        #new_deaths ~ aged_70_older
ggplot(data = sol) +
  geom_smooth(aes(aged_70_older, total_deaths), method = 'lm', se = F) +
  geom_point(aes(aged_70_older, total_deaths))
ggplot(data = sol2) +
  geom_smooth(aes(aged_70_older, new_deaths), method = 'lm', se = F) +
  geom_point(aes(aged_70_older, new_deaths))
#--------------------diabetes_prevalence---------------------------------
         #total_deaths ~ diabetes_prevalence
ggplot(data = sol) +
  geom_smooth(aes(diabetes_prevalence, total_deaths), method = 'lm', se = F) +
  geom_point(aes(diabetes_prevalence, total_deaths)) +
#  labs(title = "Group By Date. Diabetes_prevalence - Total_Deaths.")
 labs(title = "Group By Countries. Diabetes_prevalence - Total_Deaths.")
#--------------------smoking-------------------------------------------
#total_deaths ~ smoking
ggplot(data = sol) +
  geom_smooth(aes(female_smokers, total_deaths), method = 'lm', se = F) +
  geom_point(aes(female_smokers, total_deaths)) +
  labs(title = "Group By Date. female_smokers - Total_Deaths.")
# labs(title = "Group By Countries. female_smokers - Total_Deaths.")
#new_death ~ smoking
ggplot(data = sol2) +
  geom_smooth(aes(female_smokers, new_deaths), method = 'lm', se = F) +
  geom_point(aes(female_smokers, new_deaths)) +
  labs(title = "Group By Date. female_smokers - Total_Deaths.")
# labs(title = "Group By Countries. female_smokers - New_Deaths.")
#total_deaths ~ MALE smoking
ggplot(data = sol) +
  geom_smooth(aes(male_smokers, total_deaths), method = 'lm', se = F) +
  geom_point(aes(male_smokers, total_deaths)) +
  labs(title = "Group By Date. male_smokers - Total_Deaths.")
# labs(title = "Group By Countries. male_smokers - Total_Deaths.")
#new_death ~ MALEsmoking
ggplot(data = sol2) +
  geom_smooth(aes(male_smokers, new_deaths), method = 'lm') +
  geom_point(aes(male_smokers, new_deaths)) +
  labs(title = "Group By Date. male_smokers - Total_Deaths.")
# labs(title = "Group By Countries. male_smokers - New_Deaths.")
#--------------------GDP-----------
ggplot(data = sol) +
  geom_smooth(aes(gdp, total_deaths), method = 'lm', se = F) +
  geom_point(aes(gdp, total_deaths)) +
  labs(title = "Group By Country. GDP per capita - Total_Deaths. Regression")
rownames(avg_data_location) <- avg_data_location$location
ggplot(data = avg_data_location, aes(gdp, total_deaths, 
                                     label = rownames(avg_data_location))) +
  geom_point(size = 1.2) +
  geom_text(check_overlap = TRUE, aes(colour = as.factor(gdp > 40000),
                                      hjust = 0, 
                                      vjust = -0.3)) +
  labs(x = "GDP per person",
       title = "Group By Country. GDP per capita - Total_Deaths.", 
       color = "    GDP\n") +
  theme_bw() +
  theme(legend.position = c(0.8,0.7), 
        plot.title = element_text(size = 12)) +
  scale_linetype_discrete("GDP") +
  scale_color_manual(labels = c(" < 40000", " > 40000", "NA"), values = c("blue", "red", "yellow"))
#--------------------CVD death rate----------------------------------------------------
#The following is a list of the causes of human deaths worldwide for different years arranged 
#by their associated mortality rates
  #Example. There were 57,029,000 deaths tabulated in 2002. Some causes listed include deaths 
  #also included in more specific subordinate causes and some causes are omitted, 
  #so the percentages do not sum to 100
#Age standardized death rate, per 100,000, by cause
ggplot(data = sol) +
  geom_smooth(aes(cvd_death_rate, total_deaths), method = 'lm', se = F) +
  geom_point(aes(cvd_death_rate, total_deaths)) +
#  labs(title = "Group By Date. CVD_Death_Rate - Total_Deaths.")
 labs(title = "Group By Countries. CVD_Death_Rate - Total_Deaths.")
ggplot(data = sol2) +
  geom_smooth(aes(cvd_death_rate, new_deaths), method = 'lm', se = F) +
  geom_point(aes(cvd_death_rate, new_deaths)) +
  labs(title = "Group By Date. CVD_Death_Rate - New_Deaths.",
       subtitle = "")
# labs(title = "Group By Countries. Age 65 - New_Deaths.")
#------------------GGPLOT general charts------------------------------------------------
rownames(avg_data_location) <- c(avg_data_location$location)
    #population - total_deaths
ggplot(data = avg_data_location, mapping = aes(x = population, 
                                               y = total_deaths, 
                                               label = rownames(avg_data_location))) +
  geom_point() +
  geom_text(aes(hjust = 0, vjust = -0.3), check_overlap = TRUE)
rownames(wuic) <- c(wuic$location)
    #without USA, China, India. population - total deaths
rownames(wuic) <- wuic$location
ggplot(data = wuic, mapping = aes(x = gdp, 
                                  y = total_deaths, 
                                  label = rownames(wuic))) +
  geom_point() +
  geom_text(aes(hjust = 0, vjust = -0.3), check_overlap = TRUE) +
  labs(title = "GDP - Total Deaths exluding USA, China, India")
    #aged_65_older - total_deaths
ggplot(data = avg_data_location, mapping = aes(x = aged_65_older, 
                                  y = total_deaths, 
                                  label = rownames(avg_data_location))) +
  geom_point() +
  geom_text(aes(hjust = 0, vjust = -0.3), check_overlap = TRUE) +
  labs(title = "Group By Country. Relationship b/n aged 65 older and Total Deaths")

#-----------------USA stat-----------------
library(ggplot2)
library(ggpubr)
install.packages("ggpubr")
ggplot(data = covidusa) +
  geom_line(aes(x = date, y = total_cases), color = "blue") +
  geom_line(aes(x = date, y = total_deaths), color = "red")
# bars new_cases and new_deaths
ggplot(data = covidusa[covidusa$date > '2020-03-15',], aes(x = date, y = new_cases)) +
  geom_bar(aes(), stat = "identity", color = "white", fill = "darkorange", show.legend = T, alpha = 0.5) +
  geom_bar(aes(x = date, y = new_deaths, fill = new_deaths), stat = "identity", color = "white", show.legend = T)
# bars total_cases and total_deaths
ggplot(data = covidusa[covidusa$date > '2020-03-15',], aes(x = date, y = total_cases)) +
  geom_bar(aes(), stat = "identity", color = "white", fill = "darkorange", show.legend = T, alpha = 0.5) +
  geom_bar(aes(x = date, y = total_deaths, fill = total_deaths), stat = "identity", color = "white", show.legend = T)

#------------------------PLOT--------------------------
#-------total cases and total deaths
par(mar = c(5, 4, 4, 4)+0.3) # move graphic to release space for legend, labels and so on
plot(x = covidusa[covidusa$date > '2020-03-15',]$date, 
     y = covidusa[covidusa$date > '2020-03-15',]$total_cases, 
     type = "l", 
     ann = F,
     main = "UP LABEL",
     col = "SteelBlue") #ann doesn't output x,y main labels
mtext("Total Cases", side = 2, line = 3, col = "SteelBlue")
axis(side = 2, 
     col = "SteelBlue", 
     col.ticks = "SteelBlue", 
     col.axis = "SteelBlue",
     cex = 1)
par(new = TRUE)
plot(x = covidusa[covidusa$date > '2020-03-15',]$date, 
     y = covidusa[covidusa$date > '2020-03-15',]$total_deaths, 
     type = "h", 
     axes = FALSE, 
     xlab = "", 
     ylab = "", 
     col = "Dark Orange")
axis(side = 4, col = "Dark Orange", col.ticks = "Dark Orange", col.axis = "Dark Orange") #side - 4 is a right Y-axes
mtext("Total Deaths", side = 4, line = 3, col = "Dark Orange")
#-------new cases and new deaths
par(mar = c(5, 4, 4, 4)+0.3) # move graphic to release space for legend, labels and so on
plot(x = covidusa[covidusa$date > '2020-03-15',]$date, 
     y = covidusa[covidusa$date > '2020-03-15',]$new_cases, 
     type = "l", 
     ann = F,
     main = "UP LABEL",
     col = "SteelBlue") #ann doesn't output x,y main labels
mtext("New Cases", side = 2, line = 3, col = "SteelBlue")
axis(side = 2, 
     col = "SteelBlue", 
     col.ticks = "SteelBlue", 
     col.axis = "SteelBlue",
     cex = 1)
par(new = TRUE)
plot(x = covidusa[covidusa$date > '2020-03-15',]$date, 
     y = covidusa[covidusa$date > '2020-03-15',]$new_deaths, 
     type = "h", 
     axes = FALSE, 
     xlab = "", 
     ylab = "", 
     col = "Dark Orange")
axis(side = 4, col = "Dark Orange", col.ticks = "Dark Orange", col.axis = "Dark Orange") #side - 4 is a right Y-axes
mtext("New Deaths", side = 4, line = 3, col = "Dark Orange")


#--------------------PLOTLY------------------------
library(plotly)
avg_data_location %>%
  plot_ly(x = ~population,
          y = ~total_deaths,
          type = "scatter",
          text = ~location)

covid[!covid$location == 'World',] %>%
  plot_ly(x = ~population,
          y = ~total_deaths,
          text = covid[!covid$location == 'World',]$location,
          type = "bar") %>%
  group_by(location)
rownames(avg_data_location) <- avg_data_location$location
p <- ggplot(data = avg_data_location, aes(gdp, total_deaths, label = rownames(avg_data_location))) +
  geom_smooth(aes(), method = 'lm', se = F) +
  geom_point(aes()) +
  labs(title = "Group By Country. GDP per capita - Total_Deaths. Regression") +
  geom_text(check_overlap = T)
ggplotly(p)
 #---Date-Cases--Plotly
fig_cases <- avg_data_world %>%
  plot_ly(x = ~date, 
          y = ~total_cases,
          name = "Total Cases")
fig_cases <- fig_cases %>% add_lines()
fig_cases <- fig_cases %>% 
  add_lines(x = ~date, 
            y = ~total_deaths, 
            name = "Total Deaths")
fig_cases <- fig_cases %>%
  layout(
    xaxis = list(
      title = "Date",
      type = 'date',
      #tickformat = "%d %B (%a) <br>%Y"
      tickformat = "%d %B %Y"
    ),
    yaxis = list(
      title = "Total Cases, WorldWide"
    ),
    legend = list(x = 0.1, y = 0.9)
  )
fig_cases
    #--two diff charts Total cases, Total deaths
fig_cases <- avg_data_world %>%
  plot_ly(x = ~date, 
          y = ~total_cases,
          name = "Total Cases")
fig_cases <- fig_cases %>% add_lines()
fig_cases <- fig_cases %>%
  layout(
    xaxis = list(
      type = 'date',
      tickformat = "%d %m %Y"
    ),
    yaxis = list(
      title = "Total Cases"
    )
  )
fig_cases2 <- avg_data_world %>%
  plot_ly(x = ~date,
          y = ~total_deaths,
          name = "Total Deaths")
fig_cases2 <- fig_cases2 %>% add_lines()
fig_cases2 <- fig_cases2 %>%
  layout(
    xaxis = list(
      type = 'date',
      tickformat = "%d %m %Y"
    ),
    yaxis = list(
      title = "Total Deaths"
    )
  )
fig <- subplot(fig_cases, fig_cases2, titleY = T)
fig <- fig %>%
  layout(
    legend = list(x = 0.1, y = 0.9)
  )
fig
# plotly and avg_data_LOCATION
avg_data_location %>%
  plot_ly(x = ~population, 
          y = ~total_deaths,
          text = rownames(avg_data_location),
          hoverinfo = 'text',# in the plot label: remove x and y values, leave only text label
          marker = list(color = 'red')
          )
# Pie by location and total cases
circle <- avg_data_location %>% 
  plot_ly(
    type = 'pie',
    values = ~total_cases,
    labels = ~location,
    textposition = "inside"
  )
circle
# 3D Total cases, total deaths OR SPX, date
avg_data_world$date <- as.Date(levels(avg_data_world$date))[avg_data_world$date]
fig_3d <- avg_data_world[avg_data_world$date <= '2020-03-28',] %>%
  plot_ly()
fig_3d <- fig_3d %>% add_trace(
  x = ~date, 
  y = ~total_cases, 
  z = ~spx_short[spx_short$Date <= '2020-03-28',]$Close, 
  name = "z", 
  type = "scatter3d",
  mode = "lines"
)
fig_3d <- fig_3d %>%
  layout(
    scene = list(
      aspectratio = list(
        x = 1,
        y = 1,
        z = 1
      ),
      dragmode = "turntable",
      zaxis = list(
        title = "SPX"
      )
    )
  )
fig_3d
#advanced hovertemplate
fig_bubbles <- avg_data_location %>%
  plot_ly(
    type = 'scatter',
    mode = 'markers',
    x = ~population,
    y = ~total_deaths,
    marker = list(size = ~total_cases, sizeref = 2000, sizemode = 'area'),
    color = ~gdp,
    text = ~location,
    hovertemplate = paste(
      "<b>%{text}</b><br>",
      "%{xaxis.title.text}: %{x:,.0f}<br>",
      "%{yaxis.title.text}: %{y:,.0f}<br>",
      "Total Cases: %{marker.size:,}<br>",
      "GDP: %{marker.color:,.0f}",
      "<extra></extra>"
    )
  )
# %{x} - short number, 301.2156M
# %{x:,} - full number ',' is sep, 301,215,600
#%{x:$,} - $301,215,600
#%[x:.0f] - 0f' round number
#%{y:,.0%} - 104383 becomes 10438300%
fig_bubbles <- fig_bubbles %>%
  layout(
    legend = list(orientation = 'h'),
    yaxis = list(
      title = "Total Deaths"
    ),
    xaxis = list(
      title = "Population"
    )
  )
fig_bubbles
avg_data_location['United States' %in% avg_data_location$location,]
  #plotly, using color = ~ordered
mutate(year(covid$date))
filter(yea)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(usethis)
library(devtools)
devtools::install_github("plotly/dashR")
remove.packages("Rcpp")
library(lubridate)
data(economics, package = "ggplot2")
econ <- economics %>%
  mutate(yr = year(date), mnth = month(date))
econ %>%
  group_by(yr) %>%
  plot_ly(x = ~mnth, y = ~uempmed) %>%
  add_lines(text = ~yr)
plot_ly(econ, x = ~mnth, y = ~uempmed) %>%
  add_lines(color = ~ordered(yr))

prop.table()

#--------------------PLOTLY two charts simultaneously--------------------
fig <- plot_ly(
  type = 'scatter',
  mode = 'lines'
) %>%
  add_trace(
    x = ~avg_data_world$date,
    y = ~avg_data_world$total_cases,
    name = "Total Cases"
  ) %>%
  add_trace(
    x = ~avg_data_world$date,
    y = ~avg_data_world$total_deaths,
    name = "Total Deaths",
    yaxis = "y2"
  ) %>%
  layout(
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = list(
        text = "Total Deaths",
        standoff = 10
      ),
      mirror = "allticks",
      automargin = TRUE,
      showline = TRUE, linecolor = "grey",
      showgrid = F
    ),
    hovermode = "x unified",
    legend = list(x = 0.1, y = 0.9)
  )
fig
