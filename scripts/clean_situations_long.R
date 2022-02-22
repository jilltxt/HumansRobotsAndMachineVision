# Import situations_long.csv, remove unnecessary columns and rows with subgenres.
# Then save as situations.csv.
# 



Situations_long <- read_csv("data/situations_long.csv",
        col_types = cols(
                SituationID = col_integer(),
                SituationTitle = col_skip(),
                Genre = col_character(),
                GenreID = col_skip(), 
                Character = col_character(),
                Entity = col_character(),
                Technology = col_character(),
                Verb = col_character()
        )
)

situations_long <- situations_long %>%  
        filter(Genre == "Art" | Genre == "Narrative" | Genre == "Game")

write_csv(situations_long, "data/situations.csv")
