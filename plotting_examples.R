library(tidyverse)

# Grammar of graphics

data = mpg # built in R dataset

# Data to geometry

data %>%  
  ggplot(aes(x=displ, y=hwy)) +  
  geom_point()

# Adding geometries

data %>%
  ggplot(aes(x=displ, y=hwy)) +
  geom_point() +
  geom_smooth()

# Adding color as a dimension

data %>%  
  ggplot(aes(x=displ, y=hwy, color=factor(cyl))) + 
  geom_point()

data %>%  
  ggplot(aes(x=displ, y=hwy, color=factor(cyl))) + 
  geom_point() +
  geom_smooth()

# Alternative coordinate systems

data %>%
  ggplot(aes(x=displ, y=hwy, color=factor(cyl))) +
  geom_point() +
  coord_polar()

# Facets

data %>%
  ggplot(aes(x=displ, y=hwy, color=factor(cyl))) +
  geom_point() +
  facet_wrap(~class)

# Naming for clarity

data %>%
  ggplot(aes(x=displ, y=hwy, color=factor(cyl))) +
  geom_point() +
  ggtitle("Highway Gas Mileage by Engine Displacement") +
  xlab("Engine Displacement") +
  ylab("Highway MPG") +
  labs(color="Number of Cylinders")

# Annotations

data %>%
  ggplot(aes(x=displ, y=hwy, color=factor(cyl))) +
  geom_point() +
  ggtitle("Highway Gas Mileage by Engine Displacement") +
  xlab("Engine Displacement") +
  ylab("Highway MPG") +
  labs(color="Number of Cylinders") +
  annotate("text", x = 2.5, y = 44, label = "These are VWs") +
  annotate("rect", xmin = 1.7, xmax = 2.1, ymin = 40, ymax = 45, alpha = .2)


# Themes

data %>%
  ggplot(aes(x=displ, y=hwy, color=factor(cyl))) +
  geom_point(size=4) +
  ggtitle("Highway Gas Mileage by Engine Displacement") +
  xlab("Engine Displacement") +
  ylab("Highway MPG") +
  labs(color="Number of Cylinders") +
  theme_minimal()

data %>%
  ggplot(aes(x=displ, y=hwy, color=factor(cyl))) +
  geom_point(size=4) +
  ggtitle("Highway Gas Mileage by Engine Displacement") +
  xlab("Engine Displacement") +
  ylab("Highway MPG") +
  labs(color="Number of Cylinders") +
  theme_dark()

# Visualizing data to see the pattern that coorelations can not describe

source("http://janhove.github.io/RCode/plot_r.R")
plot_r(r = 0.5, n = 50)
plot_r(r = 0, n = 50)

# Anscombe Quartet

dat <- datasets::anscombe
datLong <- data.frame(
  group  = rep(1:4, each = 11),
  x = unlist(dat[,c(1:4)]),
  y = unlist(dat[,c(5:8)])
)
rownames(datLong) <- NULL
datLong

datLong %>% 
  group_by(group) %>%
  summarize(x_mean = mean(x), y_mean = mean(y))

datLong %>%
  ggplot(aes(x = x, y = y)) +
  geom_smooth(method='lm', formula= y~x, se = FALSE) +
  facet_wrap(~group) +
  ggtitle("Anscombe Quartet Plots by Group Regression Lines")

datLong %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~group) +
  ggtitle("Anscombe Quartet Plots by Group")

datLong %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~group) +
  geom_smooth(method='lm', formula= y~x) +
  ggtitle("Anscombe Quartet Plots by Group with Regression Lines")

# Visualizing Distributions

datLong %>%
  mutate(group = factor(group)) %>%
  ggplot(aes(group = group, y = y)) +
  geom_boxplot() +
  scale_x_continuous(breaks=c(1,2,3,4)) +
  xlab("Groups") +
  ylab(" Y Values") +
  ggtitle("Boxplots for Y Values")

datLong %>%
  mutate(group = factor(group)) %>%
  ggplot(aes(group = group, y = x)) +
  geom_boxplot() +
  scale_x_continuous(breaks=c(1,2,3,4), labels = c(1,2,3,4)) +
  xlab("Groups") +
  ylab("X Values") +
  ggtitle("Boxplots for X Values")

datLong %>%
  mutate(group = factor(group)) %>%
  ggplot(aes(x = x)) +
  geom_density() +
  facet_grid(~group) +
  xlab("Groups") +
  ggtitle("KDE for X Values")

datLong %>%
  ggplot(aes(factor(group),  x, fill=factor(group))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("Groups") +
  ylab("X Values") +
  ggtitle("Violin Plots for X Values") 

# Hidden groups histograms
  
one<-rnorm(10000, mean=5, sd=3)
two<-rnorm(10000, mean=12, sd=3)
df_one <- tibble("data"=one, "group"=rep("one", 10000))
df_two <- tibble("data"=two, "group"=rep("two", 10000))

df_combo <- rbind(df_one, df_two)

hist(c(n1,n2), bins=50)

new<-c(n1,n2)

df<-tibble(new)

df_combo %>%
  ggplot(aes(x=data)) +
  geom_histogram(bins=30) +
  geom_density(aes(y=..density..* 20000), colour="blue") +
  ggtitle("Sample Distribution with Kernel Density Estimation")

df_combo %>%
  ggplot(aes(x=data)) +
  geom_density(colour="blue") +
  ggtitle("Sample Distribution Kernel Density Estimation")



df_combo %>%
  ggplot(aes(x=data)) +
  geom_histogram(aes(fill=factor(group), color=factor(group)), alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  ggtitle("Sample Distribution")

df_combo %>%
  ggplot(aes(x=data)) +
  geom_histogram() +
  facet_wrap(~group) +
  ggtitle("Sample Distribution by Group")

# Comparing distributions

crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
crime <- crime[crime$state != "District of Columbia",]
crime <- crime[crime$state != "United States ",]
crime_df <- gather(crime, rate, key="crime", murder:motor_vehicle_theft)

crime_df %>%
  ggplot(aes(x = rate)) +
  geom_histogram(bins = 60) +
  facet_wrap(~crime, scales = "free_y") +
  ggtitle("Distribution of Crime Types by State Rates")

crime_df %>%
  ggplot(aes(x = rate)) +
  geom_density() +
  facet_wrap(~crime, scales = "free_y") +
  ggtitle("Kernel Density Estimation of Crime Types by State Rates")

# Visualization in regression

d <- mtcars

d %>%
  ggplot(aes(x= wt, y=mpg)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  ggtitle("Weight versus Miles per Gallon")

fit <- lm(mpg ~ wt, data = d) # fit the model
d$predicted <- predict(fit)   # Save the predicted values
d$residuals <- residuals(fit) # Save the residual values

d %>%
ggplot(aes(x = wt, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +       
  geom_segment(aes(xend = wt, yend = predicted), alpha = .2) +       
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  
  scale_color_continuous(low = "green", high = "red") +              
  guides(color = FALSE, size = FALSE) +                             
  geom_point(aes(y = predicted), shape = 3) +
  ggtitle("Residual Plot \n(Red Larger and Green Smaller)")



