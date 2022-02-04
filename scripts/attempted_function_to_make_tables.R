# Goal: generate table showing distribution of gender, race and age for robots 
# and humans. I can do this by copy and pasting the following six times, but it 
# would be good to write a function to do it instead.

HumanGender <- Characters %>% 
        filter(HumanOrMachine == "Human") %>% 
        count(Gender, sort = TRUE, name = "Count") %>% 
        mutate(Percent = Count/sum(Count)*100)

# So here is my attempt at a function - but it doesn't work.
find_percentages <- function(df, hum_bot, var) {
        filter(!!df, HumanOrMachine == !!hum_bot) %>% 
                count(!!var, sort = TRUE, name = "Count") %>% 
                mutate(Percent = Count/sum(Count)*100)
}

# The !! in front of the variables is based on what it says here 
#  about how to include user input in a dplyr function
#  https://rpubs.com/hadley/dplyr-programming 
#  
#  But seriously, this seems very complicated Perhaps better to write the function in 
#  base R? Or just copy and paste.