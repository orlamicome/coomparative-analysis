library(tidyverse)

gdp_data <- read_csv('Statistics.csv')
View(gdp_data)

unique(gdp_data$Country)

latin_America <- c('Argentina', 'Brazil', 'Costa Rica', 'Paraguay', 'Colombia')
Africa <- c('Ghana', 'Niger', 'South Africa', 'Morocco', 'Kenya')

income_class <- c()

for (country in unique(gdp_data$Country)) {
  if (country %in% latin_America){
    income_class <- c(income_class, rep('mi', 10))
  } else {
    income_class <- c(income_class, rep('li', 10))
  }
}

gdp_data$income <- income_class
View(gdp_data)


## Test assumptions of equal variance

bartlett.test(gdp_data$GDP_Growth ~ gdp_data$income)

## p-value > 0.05: means that our variance is homogeneous

## T-test (independent sample t-test)

t.test(gdp_data$GDP_Growth ~ gdp_data$income, var.equal=TRUE)

t.test(gdp_data$External_Debt_Short_term, gdp_data$External_debt_Long_Term, paired=TRUE)


fit<-aov(gdp_data$GDP_Growth ~ gdp_data$income)
summary(fit)


plotdata <- gdp_data %>% 
  group_by(income) %>% 
  summarise(n=n(),
            mean=mean(GDP_Growth),
            sd = sd(GDP_Growth),
            ci = qt(0.975, df = n-1)* sd/sqrt(n))

plotdata
ggplot(plotdata,
       aes(x=income, y=mean, group=1))+
  geom_point(size=3, color='red') +
  geom_line(linetype='dashed', color='darkgrey')+
  geom_errorbar(aes(ymin = mean-ci,
                    ymax=mean+ci),
                width=.1) +
  theme_bw()+
  labs(x='Income Class', y='GDP Growth',
       title = 'Mean Plot with 95% confidence interval')


