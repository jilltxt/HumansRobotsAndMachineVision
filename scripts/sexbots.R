# Trying to do a simple visualisation of the distribution of gender and 
# sexuality as found in this paper:
# 
# Döring, N., Poeschl, S. Love and Sex with Robots: A Content Analysis of 
# Media Representations. Int J of Soc Robotics 11, 665–677 (2019). 
# https://doi.org/10.1007/s12369-019-00517-y
# 
# I manually copied the data for sexbots from [Table 3]
# (https://link.springer.com/article/10.1007/s12369-019-00517-y/tables/3) in 
# the paper, using the values form the column % under Fictional media. It's
# important to realise that the numbers are PERCENTAGES. I actually first put it
# in an Excel spreadsheet, exported as csv, then used dput() to generate the
# structure which allows me to make it a dataframe. I think it's most
# transparent to just have the data in a script like below rather than creating 
# a separate csv file since the data isn't mine.
# 
# Here is a version that only includes gender and sexuality that I used to ask 
# a question about this on StackExchange: 

sexbots <- structure(list(
        Variables = c("Female", "Male", "Heterosexual", "Homosexual", "OtherSexuality"), 
        Human = c(43, 62, 78, 21, 1), 
        Robot = c(46, 55, 78, 21, 1), 
        Type = c("Gender", "Gender", "Sexuality", "Sexuality", "Sexuality")), 
        row.names = c(NA, -5L), 
        class = c("tbl_df", "tbl", "data.frame"))

sexbots %>% 
        filter(Variables == "Female" | Variables == "Male") %>%
        ggplot() +
        geom_col(aes(x = Variables, y = Human, fill = Variables )) +
        geom_col(aes(x = Variables, y = Robot, fill = Variables )) 

# Here is another version of the sexbots dataframe that includes the ages as
# well.

sexbots <- structure(list(
        Variables = c("Female", "Male", "Child", "Adult", "Elderly", 
                      "Heterosexual", "Homosexual", "Other"), 
        Human = c(43, 62, 27, 57, 2, 78, 21, 1), 
        Robot = c(46, 55, 23, 55, NA, 78, 21, 1), 
        Type = c("Gender", "Gender", "Age", "Age", "Age", "Sexuality", "Sexuality", "Sexuality")),
        row.names = c(NA, -8L), 
        class = c("tbl_df", "tbl", "data.frame"))

sexbots %>% 
        filter(Variables == "Female" | Variables == "Male") %>%
        ggplot() +
        geom_col(aes(x = Variables, y = Human, fill = Variables )) 

ggplot(data = sexbots, group = Type) +
        geom_col(mapping=aes(x = Variables,
                             y = Human,
                             fill = Type)) +
        ggtitle("Characteristics of humans who had sexual relationships with robots") +
        ylab("Percentage of sample") +
        xlab("Traits") +
        facet_wrap(~ Type, scale="free_x")
        
ggplot(data = sexbots, group = Type) +
        geom_col(mapping=aes(x = Variables,
                             y = Robot,
                             fill = Type)) +
        ggtitle("Characteristics of robots who had sexual relationships with humans") +
        ylab("Percentage of sample") +
        xlab("Traits") +
        facet_wrap(~ Type, scale="free_x")


# I emailed the authors of the paper (Jan 29, 2022) to ask if I could have their
# full dataset - it would be cool if they have some of the same works in their data!