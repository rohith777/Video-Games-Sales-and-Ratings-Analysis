#Loading Packages
library("tidyverse")
library("readxl")
library(plotrix)
library(dplyr)
library("ggplot2")
library(cowplot)
library(lattice)
library(readr)
# Loading dataset
dataset <- read_csv(file="/cloud/project/Dataset/final_proj_data.csv")
#Two sample t-test
## Data Cleaning
dataset_filtered_platform_globalsales<-dataset[complete.cases(dataset["Platform"]),]
dataset_filtered_platform_globalsales<-dataset_filtered_platform_globalsales[complete.cases(dataset_filtered_platform_globalsales["Global_Sales"]),]
## Data processing
dataset_filtered_platform_globalsales_grouped<-dataset_filtered_platform_globalsales %>% 
  dplyr::group_by(Platform) %>% 
  dplyr::summarise(mean_sales = mean(Global_Sales))
#Two sample t-test
## Disribution of global sales by platform
ggplot(data=dataset_filtered_platform_globalsales_grouped, aes(x=Platform, y=mean_sales)) +
  geom_bar(stat="identity",position = position_dodge(width=0.5),fill = "#d9b886")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Platform") +
  ylab("Mean global sales")+
  ggtitle("Disribution of global sales by platform")
## Data Filtering
dataset_subset_platform_globalsales <- dataset_filtered_platform_globalsales[dataset_filtered_platform_globalsales$Platform %in% c('PS2', 'Wii'), ]


dataset_grouped_year_platform<-dataset_subset_platform_globalsales[complete.cases(dataset_subset_platform_globalsales["Year_of_Release"]),]

dataset_grouped_year_platform<-dataset_grouped_year_platform %>% 
  dplyr::group_by(Platform,Year_of_Release) %>% 
  dplyr::summarise(mean_sales = mean(Global_Sales))

dataset_grouped_year_platform$Year_of_Release<-as.factor(dataset_grouped_year_platform$Year_of_Release)
##Global sales PS2 and Wii games
ggplot(data=dataset_grouped_year_platform, aes(x=Year_of_Release, y=mean_sales, fill=Platform)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Year Of Release") +
  ylab("Mean global sales")+
  ggtitle("Global sales PS2 and Wii games")
## Data Filtering
dataset_subset_platform_globalsales<-dataset_subset_platform_globalsales[dataset_subset_platform_globalsales$Global_Sales<1, ]
## Frequency distribution of Global sales of PS2 and Wii games
ggplot(dataset_subset_platform_globalsales, aes(x=Global_Sales, fill=Platform)) +
  geom_histogram(color="black",alpha=0.8)+
  xlab("Global Sales") +
  ylab("Frequency")+
  ggtitle("Frequency distribution of Global sales of PS2 and Wii games")
# Box plot to understand the spread of global sales
boxplot(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=='PS2'],
        dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=='Wii'],
        names = c("PS2", "Wii"),
        col = c("blue","red"),
        notch = TRUE,
        ylab="Global Sales",
        main='Box plot for Global sales of PS2 and Wii games')
#One sample t-test
dataset_filtered_critic_score<-dataset[complete.cases(dataset["Critic_Score"]),]
## Frequency distribution Critic Scores
ggplot(dataset_filtered_critic_score, aes(x=Critic_Score)) +
  geom_density(fill="#4065a1",color="#4065a1")+
  xlab("Critic Scores") +
  ylab("Frequency")+
  ggtitle("Frequency distribution Critic Scores")+
  geom_vline(aes(xintercept=mean(Critic_Score)),
             linetype="dashed",color="red")
# Box plot to understand the spread Critic Score
boxplot(dataset_filtered_critic_score$Critic_Score,
        notch = TRUE,
        ylab="Critic Score",
        col = c("blue"),
        horizontal=T,
        main='Box plot for critic scores')
## Two proportion test

dataset_filtered_publisher_genre<-dataset[complete.cases(dataset["Publisher"]),]

dataset_filtered_publisher_genre<-dataset[complete.cases(dataset["Genre"]),]

dataset_filtered_publisher_genre<-dataset_filtered_publisher_genre[dataset_filtered_publisher_genre$Publisher %in% c('Activision', 'Nintendo'), ]

dataset_filtered_publisher_genre$Genre <- ifelse(dataset_filtered_publisher_genre$Genre == 'Racing', dataset_filtered_publisher_genre$Genre, 'Other')

racing_activision <- t(replicate(length(which(dataset_filtered_publisher_genre$Publisher == "Activision" & dataset_filtered_publisher_genre$Genre=="Racing")),
                                 c("Racing", "Activision")))
racing_nintendo <- t(replicate(length(which(dataset_filtered_publisher_genre$Publisher == "Nintendo" & dataset_filtered_publisher_genre$Genre=="Racing")),
                               c("Racing", "Nintendo")))
other_activision <- t(replicate(length(which(dataset_filtered_publisher_genre$Publisher == "Activision" & dataset_filtered_publisher_genre$Genre=="Other")),
                                c("Other", "Activision")))
other_nintendo <- t(replicate(length(which(dataset_filtered_publisher_genre$Publisher == "Nintendo" & dataset_filtered_publisher_genre$Genre=="Other")),
                              c("Other", "Nintendo")))
dataset_2 <- data.frame(rbind(racing_activision,
                              racing_nintendo,
                              other_activision,
                              other_nintendo))
names(dataset_2) <- c("Genre", "Publisher")
ds<-table(dataset_2)
ds<-addmargins(ds)
#One proportion test
dataset_filtered_electronic_arts<-dataset[complete.cases(dataset["Publisher"]),]
dataset_filtered_electronic_arts<-dataset_filtered_electronic_arts$Genre[dataset_filtered_electronic_arts$Publisher=="Electronic Arts"]
## Frequency distribution genres by Electronic Arts
ggplot(data.frame(dataset_filtered_electronic_arts), aes(x=dataset_filtered_electronic_arts)) +
  geom_bar()+
  xlab("genres") +
  ylab("Frequency")+
  ggtitle("Frequency distribution genres by Electronic Arts")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
## Data Filtering
dataset_filtered_electronic_arts <- ifelse(dataset_filtered_electronic_arts == 'Sports', dataset_filtered_electronic_arts, 'Other')
## Frequency distribution genres by Electronic Arts
ggplot(data.frame(dataset_filtered_electronic_arts), aes(x=dataset_filtered_electronic_arts)) +
  geom_bar()+
  xlab("genres") +
  ylab("Frequency")+
  ggtitle("Frequency distribution genres by Electronic Arts")+
  coord_flip()
#chi_squared
## Data Filtering
dataset_filtered_year_genre<-dataset[complete.cases(dataset["Year_of_Release"]),]
dataset_filtered_year_genre<-dataset_filtered_year_genre[complete.cases(dataset_filtered_year_genre["Genre"]),]

dataset_graph <- dataset_filtered_year_genre[dataset_filtered_year_genre$Year_of_Release %in% c('2008'), ]

##Frequency distribution of genres in 2008

ggplot(data.frame(dataset_graph$Genre), aes(x=dataset_graph$Genre)) +
  geom_bar(fill='#99c4a4')+
  xlab("genres") +
  ylab("Frequency")+
  ggtitle("Frequency distribution of genres in 2008")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
## Data Filtering
dataset_filtered_year_genre <- dataset_filtered_year_genre[dataset_filtered_year_genre$Genre %in% c('Platform', 'Fighting', 'Strategy'), ]

dataset_filtered_year_genre<-dataset_filtered_year_genre$Genre[dataset_filtered_year_genre$Year_of_Release=='2008']

dataset_filtered_year_genre_unique<-unique(dataset_filtered_year_genre)
# Checking the normality of the data.
par(mfrow=c(2,2))
#Normality check for Global sales of Wii Games
qqnorm(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="PS2"])
qqline(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="PS2"])
#Normality check for Global sales of Wii Games
qqnorm(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="Wii"])
qqline(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="Wii"])

#Histogram showing the Global sales of PS2 Games
hist(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="PS2"],
     xlab="Global sales in Millions", 
     main="Density plot PS2 games")
#Histogram showing the Global sales of Wii Games
hist(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="Wii"] ,
     xlab="Global sales in Millions",
     main="Density plot Wii Games")
#Histogram to show the distribution of  sample statistic
hist(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="PS2"]-dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="Wii"],
     xlab="Sample Statistic",
     ylab="frequency", 
     main="Sample Statistic Distribution")
# the parts of the test statistic
# sample means
x_bar_p <- mean(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="PS2"])
x_bar_w <- mean(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="Wii"])
# null hypothesized population mean difference between the two groups
mu_0 <- 0
# sample variances
s_p_sq <- sd(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="PS2"])**2
s_w_sq <- sd(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="Wii"])**2
# sample size
n_p <- length(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="PS2"])
n_w <- length(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="Wii"])
# t-test test statistic
t <- (x_bar_p - x_bar_w - mu_0)/sqrt((s_p_sq/n_p) + (s_w_sq/n_w))
#Calculation of P Value
two_sided_diff_t_pval <- pt(q = t, df = min(n_p, n_w)-1, lower.tail = FALSE)*2

#Critical value
t_critcal_1<-qt(0.025,min(n_p,n_w)-1)
##Graphical representation of test statistic
pts<-seq(-5,5,0.1)
plot(pts,dt(pts,df=1200),
     col='red',
     type='l',
     xlab="t-stat",
     ylab="density",
     main="Graphical representation of test statistic")
abline(v=qt(0.025,df=1200), col="black")
abline(v=qt(.975,df=1200), col="black")
text(qt(0.025,df=1200),0.3,"LB")
text(qt(0.975,df=1200),0.3,"UB")
abline(v=t, col="blue")
text(t,0.3,"tstat")
##Building confidence intervals
#lower bound
conf_lower_1<-(x_bar_p-x_bar_w)+(qt(0.025,min(n_p,n_w)-1)*sqrt((s_p_sq/n_p)+(s_w_sq/n_w)))

#higher bound
conf_upper_1<-(x_bar_p-x_bar_w)-(qt(0.025,min(n_p,n_w)-1)*sqrt((s_p_sq/n_p)+(s_w_sq/n_w)))

num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  mean_ps2 <- mean(sample(x = dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="PS2"],
                          size = n_p,
                          replace = TRUE))
  mean_wii <- mean(sample(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="Wii"],
                          size = n_w,
                          replace = TRUE))
  results[i] <- mean_ps2 - mean_wii
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean', xlab = 'Average Difference Global Sales', ylab = 'Density')
## Bootstrap lower confidence interval
lowerbound_bootstrap<-c(quantile(results, .025))
## Bootstrap upper confidence interval
upperbound_bootstrap<-c(quantile(results, .975))
set.seed(0)
num_sims <- 1000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationshipm we should be able to shuffle the groups
  shuffled_global_sales <- transform(dataset_subset_platform_globalsales, Global_Sales=sample(Global_Sales))
  mean_ps2 <- mean(shuffled_global_sales$Global_Sales[shuffled_global_sales$Platform=="PS2"])
  mean_wii <- mean(shuffled_global_sales$Global_Sales[shuffled_global_sales$Platform=="Wii"])
  results_given_H0_true[i] <- mean_ps2 - mean_wii
}
# Finally plot the results
hist(results_given_H0_true, freq = FALSE,
     main='Dist. of the Diff in Sample Means Under Null',
     xlab = 'Average Difference Global Sales under Null',
     ylab = 'Density')
diff_in_sample_means <- mean(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="Wii"]) - mean(dataset_subset_platform_globalsales$Global_Sales[dataset_subset_platform_globalsales$Platform=="PS2"])
abline(v=diff_in_sample_means, col = "blue")
abline(v=abs(diff_in_sample_means), col = "red")
## Bootstrap P-value
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= diff_in_sample_means)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= abs(diff_in_sample_means))
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
# Checking the normality of the data.
#Normality check for the critic score variable
par(mfrow=c(1,2))
qqnorm(dataset_filtered_critic_score$Critic_Score)
qqline(dataset_filtered_critic_score$Critic_Score)

#Histogram showing the frequency distribution of critic score
hist(dataset_filtered_critic_score$Critic_Score,
     xlab="Critic score", 
     main="Density plot of critic score")
#Histogram to show the distribution of  sample statistic
hist(dataset_filtered_critic_score$Critic_Score,
     xlab="Sample Statistic",
     ylab="frequency", 
     main="Sample Statistic Distribution")
# the parts of the test statistic
# sample mean
x_bar <- mean(dataset_filtered_critic_score$Critic_Score)
# null hypothesized population mean
mu_0 <- 70
# sample st. dev
s <- sd(dataset_filtered_critic_score$Critic_Score)
# sample size
n <- length(dataset_filtered_critic_score$Critic_Score)
# t-test test statistic
t <- (x_bar - mu_0)/(s/sqrt(n))
# two-sided p-value so multiply by 2
two_sided_t_pval <- pt(q = t, df = n-1,lower.tail = TRUE)*2
#Critical value
t_critcal_1<-qt(0.025,n-1)
##Graphical representation of test statistic
pts<-seq(-8,8,0.1)
plot(pts,dt(pts,df=8335),
     col='red',
     type='l',
     xlab="t-stat",
     ylab="density",
     main="Graphical representation of test statistic")
abline(v=qt(0.025,df=8335), col="black")
abline(v=qt(.975,df=8335), col="black")
text(qt(0.025,df=8335),0.3,"LB")
text(qt(0.975,df=8335),0.3,"UB")
abline(v=t, col="blue")
text(t,0.3,"tstat")
##Building confidence intervals
#lower bound
conf_lower_1<-(x_bar)+(qt(0.025,n-1)*s/sqrt(n))
#higher bound
conf_upper_1<-(x_bar)-(qt(0.025,n-1)*s/sqrt(n))
num_sims <- 1000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = dataset_filtered_critic_score$Critic_Score,
                            size = n,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean', xlab = 'Average Critic Scores', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
# Shift the sample so that the null hypothesis is true
critic_score_given_H0_true <- dataset_filtered_critic_score$Critic_Score - mean(dataset_filtered_critic_score$Critic_Score) + mu_0

num_sims <- 1000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results_given_H0_true[i] <- mean(sample(x = critic_score_given_H0_true,
                                          size = n,
                                          replace = TRUE))
}
# add line to show values more extreme on lower end
low_end_extreme <-mean(results_given_H0_true)+(mean(results_given_H0_true)-x_bar)
bootstrap_SE_X_bar <- sd(results)
# an estimate is to use the formula statistic +/- 2*SE
bootstrap_lower<-x_bar - 2*bootstrap_SE_X_bar
bootstrap_upper<-x_bar + 2*bootstrap_SE_X_bar
# Shift the sample so that the null hypothesis is true
# Finally plot the results
hist(results_given_H0_true, freq = FALSE,xlim=c(68.8,72.3), main='Sampling Distribution of the Sample Mean, Given Null Hypothesis is True', xlab = 'Average Commute Times', ylab = 'Density')
# add line to show values more extreme on upper end
abline(v=x_bar, col = "red")
# add line to show values more extreme on lower end
low_end_extreme <-mean(results_given_H0_true)+(mean(results_given_H0_true)-x_bar)
abline(v=low_end_extreme, col="red")
# counts of values more extreme than the test statistic in our original sample, given H0is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true >= low_end_extreme)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true <= x_bar)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
# the parts of the test statistic
# sample props

p_hat_a <- length(racing_activision)/(length(racing_activision)+length(other_activision))

p_hat_n <- length(racing_nintendo)/(length(racing_nintendo)+length(other_nintendo))

# null hypothesized population prop difference between the two groups
p_0 <- 0
# sample size
n_a <- (length(racing_activision)+length(other_activision))/2
n_n <- (length(racing_nintendo)+length(other_nintendo))/2
# sample variances
den_p_a <- (p_hat_a*(1-p_hat_a))/n_a
den_p_n <- (p_hat_n*(1-p_hat_n))/n_n
# z-test test statistic
z <- (p_hat_a - p_hat_n - p_0)/sqrt(den_p_a + den_p_n)
# two-sided p-value
two_sided_diff_prop_pval <- pnorm(q = z, lower.tail = FALSE)*2
#Critical value
z_critcal<-qnorm(0.025)
##Graphical representation of test statistic
pts<-seq(-5,5,0.1)
plot(pts,dnorm(pts),
     col='red',
     type='l',
     xlab="t-stat",
     ylab="density",
     main="Graphical representation of test statistic")
abline(v=qnorm(0.025), col="black")
abline(v=qnorm(.975), col="black")
text(qnorm(0.025),0.3,"LB")
text(qnorm(0.975),0.3,"UB")
abline(v=z, col="blue")
text(z,0.2,"test-stat")
##Building confidence intervals
# lower bound
twoprop_lower_bound<-(p_hat_a - p_hat_n)+(qnorm(0.025)*sqrt(den_p_a + den_p_n))
# upper bound
twoprop_upper_bound<-(p_hat_a - p_hat_n)-(qnorm(0.025)*sqrt(den_p_a + den_p_n))

# Make the data
activision <- rep(c(1, 0), c((length(racing_activision)/2), n_a - (length(racing_activision)/2)))
nintendo <- rep(c(1, 0), c((length(racing_nintendo)/2), n_n - (length(racing_nintendo)/2)))
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  prop_act <- mean(sample(activision,
                          size = n_a,
                          replace = TRUE))
  prop_nin <- mean(sample(x = nintendo,
                          size = n_n,
                          replace = TRUE))
  results[i] <- prop_act - prop_nin
}
# Finally plot the results
hist(results, freq = FALSE, main='Dist. of the Diff in Prop', xlab = 'Difference in Prop. of racing games released', ylab = 'Density')
lowerbound_bootstrap<-c(quantile(results, .025))
upperbound_bootstrap<-c(quantile(results, .975))
# Make the data
df_combined <- data.frame("racing_games" = c(activision, nintendo),
                          "publisher" = rep(c("activision", "nintendo"), c(n_a, n_n)))

set.seed(0)
num_sims <- 1000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationshipm we should be able to shuffle the groups
  shuffled_groups <- transform(df_combined, publisher=sample(publisher))
  prop_activision <- mean(shuffled_groups$racing_games[shuffled_groups$publisher=="activision"
                                                       ])
  prop_nintendo <- mean(shuffled_groups$racing_games[shuffled_groups$publisher=="nintendo"])
  results_given_H0_true[i] <- prop_activision - prop_nintendo
}
# Finally plot the results
hist(results_given_H0_true, freq = FALSE,
     main='Dist. of the Diff in Sample Sample Props Under Null',
     xlab = 'Average Difference in Prop. racing games released under Null',
     ylab = 'Density')
diff_in_sample_props <- p_hat_a - p_hat_n
abline(v=diff_in_sample_props, col = "blue")
abline(v=-diff_in_sample_props, col = "red")
# counts of values more extreme than the test statistic in our original sample, given H0, is true
# two sided given the alternate hypothesis

count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= -diff_in_sample_props)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= diff_in_sample_props)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
# the parts of the test statistic

p_hat<-0.4115942
z <- (p_hat - .5)/ sqrt((.05*(1-.05)) / length(dataset_filtered_electronic_arts))

# two-sided p-value
one_sided_diff_prop_pval <- pnorm(z, lower.tail = TRUE)
##Building confidence intervals
# lower bound
oneprop_lower_bound<-0
# upper bound
oneprop_upper_bound<-p_hat - (qnorm(0.05))*sqrt(((p_hat)*(1-p_hat))/length(dataset_filtered_electronic_arts))

Sports <- rep(c(1, 0), c(568, 1380-568))

num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = Sports,
                            size = 1380,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE,
     main='Sampling Distribution of the Sample Proportion',
     xlab = 'Proportion of Sports genre', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(.35, .75, .001),
      dnorm(seq(.35, .75, .001),
            mean = mean(results), sd = sd(results)))
## Bootstrap confidence intervals
bootstrap_lower<-quantile(results,0)
bootstrap_upper<-quantile(results, .95)
Sports <- rep(c(1, 0), c(690, 1380-690))


num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = Sports,
                            size = 1380,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE,xlim=c(0.41,0.55),
     main='Sampling Distribution of the Sample Proportion',
     xlab = 'Proportion of Sports genre', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(.35, .75, .001),
      dnorm(seq(.35, .75, .001),
            mean = mean(results), sd = sd(results)))
abline(v=0.41159420, col="red")
## Bootstrap P-value
count_of_more_extreme_upper_tail <- sum(results <= 0.41159420)
bootstrap_pvalue <- count_of_more_extreme_upper_tail/num_sims
# the parts of the test statistic
equal_prop<-length(dataset_filtered_year_genre)/length(dataset_filtered_year_genre_unique)
chi_square<-sum(((table(dataset_filtered_year_genre) - equal_prop)^2)/equal_prop)
df <- length(dataset_filtered_year_genre_unique)-1
## Calculation of p-value
chisquared_pval <- pchisq(chi_square,df=df , lower.tail = FALSE)
#chi-squared with randamization
solutions_under_H_0<-rep(dataset_filtered_year_genre_unique,equal_prop)
num_sims <- 10000
# A vector to store my results
chisq_stats_under_H0 <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  new_samp <- sample(solutions_under_H_0, length(dataset_filtered_year_genre), replace = T)
  chisq_stats_under_H0[i] <- sum(((table(new_samp) - equal_prop)^2)/equal_prop)
}

## Dist. of the Chi-Square Statistic Under Null
hist(chisq_stats_under_H0, freq = FALSE,
     main='Dist. of the Chi-Square Statistic Under Null',
     xlab = 'Chi-Square Stat under Null',
     ylab = 'Density')
abline(v=sum(((table(dataset_filtered_year_genre) - equal_prop)^2)/equal_prop), col="red")
# Randomized p-value
randamized_p<-sum(chisq_stats_under_H0 >= sum(((table(dataset_filtered_year_genre) - equal_prop)^2)/equal_prop))/num_sims
