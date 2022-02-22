# 19 Feb 2022

# This makes a contingency table of Character_verbs
# Each value (e.g. Female or Human) becomes a column name and the value
# is the count for each verb. 
# I'm still not sure what to use to analyse this further but I think it's a useful
# format for the data.
# Would probably be useful to "normalise" the numbers, at least add column with total 
# verb_freq and check that the sum of each row matches that. 
# 

Character_verbs_contingency <- Character_verbs %>% 
        select(Verb, Gender, Species, RaceOrEthnicity, Age, Sexuality) %>% 
        pivot_longer(cols= -Verb,
                     names_to = "variable", 
                     values_to = "value") %>% 
        drop_na() %>% 
        group_by(Verb, value) %>%
        summarise(n=n()) %>% 
        pivot_wider(names_from = "value", values_from = "n") %>% 
        mutate_all(~replace(., is.na(.), 0)) # convert NA to 0 since it's count

Character_verbs_contingency <- Character_verbs_contingency %>%
        mutate(target = as.double(str_detect(Verb, "ing"))) # add target column

# The correlation matrix works on this contingency table.
# Needed to change all NAs to 0 for cor() to work - makes sense
# because NA means no incidences of this so should be zero.
cormat <- cor(Character_verbs_contingency %>% keep(is.numeric))

cormat %>% 
        as.data.frame %>% 
        mutate(var2=rownames(.)) %>%
        pivot_longer(!var2, values_to = "value") %>%
        ggplot(aes(x=name,y=var2,fill=abs(value),label=round(value,2))) +
        geom_tile() + geom_label() + xlab("") + ylab("") +
        ggtitle("Correlation matrix of our predictors") +
        labs(fill="Correlation\n(absolute):") +
        theme(axis.text.x = element_text(angle=90))


# The next bit does something similar but making percentages.
# 
# Tutorial here: https://bookdown.org/paul/applied-data-visualization/categorical-variables-2.html

# This makes a table of all the verbs with percentages for each value in each 
# character trait. 

temp <- Character_verbs %>% 
        select(Verb, Gender, Species, RaceOrEthnicity, Age, Sexuality) %>% 
        pivot_longer(cols= -Verb,
                names_to = "variable", 
                values_to = "value") %>% 
        drop_na() %>% 
        group_by(Verb) %>%
        
        summarize(pct_female = mean(value == "Female", na.rm=TRUE),
                  pct.male = mean(value == "Male", na.rm=TRUE),
                  pct.trans = mean(value == "Trans Woman", na.rm=TRUE),
                  pct.human = mean(value == "Human", na.rm=TRUE),
                  pct.robot = mean(value == "Machine"|value=="Cyborg", na.rm=TRUE),
                  pct.animal = mean(value == "Animal", na.rm=TRUE),
                  pct.fictional = mean(value == "Fictional", na.rm=TRUE),
                  pct.asian = mean(value == "Asian", na.rm=TRUE),
                  pct.white = mean(value == "White", na.rm=TRUE),
                  pct.black = mean(value == "Black", na.rm=TRUE),
                  pct.PoC.other = mean(value == "Immigrant"|value == "Indigenous"|value == "Complex"|value == "Person of Colour", na.rm=TRUE),
                  pct.heterosexual = mean(value == "Heterosexual", na.rm=TRUE),
                  pct.homosexual = mean(value == "Homosexual", na.rm=TRUE),
                  pct.sexuality.other = mean(value == "Bi-sexual"|value == "Other", na.rm=TRUE),
                  pct.child = mean(value == "Child", na.rm=TRUE),
                  pct.young.adult = mean(value == "Young Adult", na.rm=TRUE),
                  pct.adult = mean(value == "Adult", na.rm=TRUE),
                  pct.elderly = mean(value == "Elderly", na.rm=TRUE)
                  ) 
        
write_csv(temp, "data/Character_verbs_percentages.csv")

# Now add the target (ActiveVerb is TRUE or FALSE)

Character_verbs_pct <- Character_verbs_pct %>%
        mutate(target = (str_detect(Verb, "ing"))) # add target column



# Tried making a correlation matrix, but it's clearly pointless, it just says stuff
# like "-0.76 correlation between male and female" which yeah, obviously they're
# negatively correlated and it's not -1 because there are other gender options. 
# Sigh. 
# 
# How do I even use this table? 

# cormat <- cor(Character_verbs_pct %>% keep(is.numeric))

# cormat %>% 
#        as.data.frame %>% 
#        mutate(var2=rownames(.)) %>%
#  pivot_longer(!var2, values_to = "value") %>%
#  ggplot(aes(x=name,y=var2,fill=abs(value),label=round(value,2))) +
#  geom_tile() + geom_label() + xlab("") + ylab("") +
#  ggtitle("Correlation matrix of our predictors") +
#  labs(fill="Correlation\n(absolute):") 
#  




cormat <- cor(Character_verbs_contingency %>% keep(is.numeric))

cormat %>% 
        as.data.frame %>% 
        mutate(var2=rownames(.)) %>%
  pivot_longer(!var2, values_to = "value") %>%
  ggplot(aes(x=name,y=var2,fill=abs(value),label=round(value,2))) +
  geom_tile() + geom_label() + xlab("") + ylab("") +
  ggtitle("Correlation matrix of our predictors") +
  labs(fill="Correlation\n(absolute):") +
        theme(axis.text.x = element_text(angle=90))

write_csv(temp, "data/Character_verbs_percentages.csv")

# Now add the target (ActiveVerb is TRUE or FALSE)

Character_verbs_pct <- Character_verbs_pct %>%
        mutate(target = (str_detect(Verb, "ing"))) # add target column


