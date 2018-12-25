library(tidyverse)
# The objective is to analyse the difficulty of candycrush level 1-15
candydata <- read_csv("/Users/ShuLFO/Downloads/Level difficulty in Candy Crush Saga/datasets/candy_crush.csv")
View(candydata)

count(unique(candydata))

range(candydata$dt)

# calculating the percentage of success for each level
candydata2 <- candydata %>% 
        group_by(level) %>% 
        summarise(total_attempt = sum(num_attempts), total_success = sum(num_success)) %>% 
        mutate(p_win = total_success / total_attempt)

# calculating the standard errors
candydata2 <- candydata2 %>% 
        mutate(SE = sqrt(p_win * (1 - p_win) / total_attempt))

# plot 
ggplot(candydata2, aes(level, p_win)) +
        geom_line() +
        geom_point() +
        scale_x_continuous(breaks = 1:15) +
        scale_y_continuous(labels = scales::percent) +
        geom_errorbar(aes(ymin = p_win - SE, ymax = p_win + SE))

# calculating the likelihood of success from 1-15 in one try
prod(candydata2$p_win)
        