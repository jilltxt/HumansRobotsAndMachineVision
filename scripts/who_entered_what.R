works_entered_by <- read_csv("data/work_only-simpel-5.csv",
                             col_types = cols(Title = col_skip(),
                                              Creator = col_skip(), 
                                              URL = col_skip(),
                                              Year = col_skip(),
                                              Title = col_skip(),
                                              Genre = col_factor(levels = c("Art", "Game", "Narrative")),
                                              `Authored by` = col_factor(
                                                      levels = c("Jill", "Ragnhild", "Linda", "Marianne", 
                                                                 "Linn Heidi", "Kurdin", "Gabriele", "Ida", 
                                                                 "Ainsley", "Amanda", "Cecilie", "Edward", "Martin", 
                                                                 "Milosz","Milad", "Tijana", "Sunniva", "Annette",
                                                                 "Andrés", "Anne")),
                                              `Science fiction?` = col_integer(),
                                              Country = col_character()
                             )
)


# Bar chart showing how many works each person entered --------------------

works_entered_by %>% 
        select(User = "Authored by", ID, Genre) %>% 
        filter(Genre == "Art" | Genre == "Game" | Genre == "Narrative") %>% 
        distinct() %>% 
        select(User, Genre) %>% 
        pivot_longer(-User) %>% 
        ggplot(aes(x=factor(User), fill=value)) +
        geom_bar() +
        geom_text(stat = "count", 
                  aes(label =..count..), 
                  position = position_stack(vjust = 0.5)) +
        labs(fill="", 
             title ="Creative works entered by each person",
             y = "", 
             x = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust = 1)) 


# Table summarising entries by team member --------------------------------

works_entered_by %>% 
        select(User = "Authored by", ID, Genre) %>% 
        filter(Genre == "Art" | Genre == "Game" | Genre == "Narrative") %>% 
        distinct() %>%  # checked here: 500 rows so correct: 1 row pr work
        select(-ID) %>% 
        group_by(User, Genre) %>% 
        summarise(n)
        

works <- table(works)                           # makes contingency table
works_sum <- addmargins(works, FUN = sum)       # add sums

write.table(works_sum, "data/team_contributions.csv")

# Bar chart showing countries works were from -----------------------------

#install.packages("countrycode")
library(countrycode)
library(RColorBrewer)

works_entered_by$Continent <- countrycode(sourcevar = works_entered_by$Country, origin = "country.name",destination = "region")

works_entered_by %>% 
        select(User = "Authored by", ID, Genre, Continent) %>% 
        filter(Genre == "Art" | Genre == "Game" | Genre == "Narrative") %>% 
        distinct() %>% 
        select(User, Continent) %>% 
        pivot_longer(-User) %>% 
        ggplot(aes(x=factor(User), fill=value)) +
        geom_bar() +
        geom_text(stat = "count", 
                  aes(label =..count..), 
                  position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = c('#EE6677', '#228833', '#4477AA', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')
) +
        labs(fill="", 
             title ="Creative works entered by team member and continent",
             subtitle = "Note that some works have several countries.",
             y = "", 
             x = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust = 1)) 
