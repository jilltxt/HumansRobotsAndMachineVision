# Import situations_long.csv, remove unnecessary columns and rows with subgenres.
# Then save as situations.csv.
# 

#import the original export (misspellings and all)

situations <- read_csv("data/sitiuations_long_v3.csv")

situations[!rowSums(is.na(situations[-1:4, 8])),]
# filter so it ONLY includes art, game, narrative and not duplicate lines for all the 
# subgenres.
# Also filter out the few rows which list a verb with no agent - I went through
# the database looking for these and can't see why they're listed that way. So
# will just delete them.
# 
# WHY WON'T THIS WORK!!!!

situations %>%  
        filter(Genre == "Art" | Genre == "Narrative" | Genre == "Game") %>% 
        filter(!is.na(Technology) & !is.na(Entity))
               
               %>% 
        select(SituationID, Situation = SituationTitle, Genre, Character, 
               CharacterID, Entity, Technology, Verb)

write_csv(situations, "data/situations.csv")

rownames(situations, is.na(Technology) && is.na(Entity) && is.na(Character))
