

worksURL <- read_csv("data/creativeworks_simple.csv") %>% 
        select(WorkID = ID, URL, Country, Creator, Genre, 
               IsSciFi = `Science fiction?`) %>% 
        filter(Genre == "Art"|Genre == "Game"| Genre =="Narrative") %>% 
        pivot_longer(-WorkID, names_to = "Variable", values_to = "Value") %>% 
        distinct()

worksWiki <- read_csv("data/wikidata_works_creators.csv") %>% 
        select(WorkID, WorkTitle, Work_WikidataID) %>% 
        pivot_longer(-WorkID, names_to = "Variable", values_to = "Value") %>% 
        distinct()

worksYear <- creativeworks %>% 
        select(WorkID, Year) %>% 
        pivot_longer(-WorkID, names_to = "Variable", values_to = "Value") %>% 
        distinct()

worksinfo <- rbind(worksURL, worksWiki, worksYear)

# Add levels to Variables so they sort nicely.

worksinfo$Variable <- factor(
        worksinfo$Variable,
        c("WorkTitle", "Year", "Genre", "Creator", "URL", "Country", 
          "Work_WikidataID", "IsSciFi"),
        ordered = TRUE)

worksinfo <- worksinfo %>% 
        arrange(WorkID, Variable)

write_csv(worksinfo, "data/worksinfo.csv")

# reimport if returning to this later
# 
worksinfo_full <- read_csv("data/worksinfo.csv")

# Pivot wider to make a human-readable version

worksinfo <- worksinfo_full %>% 
        arrange(WorkID, Variable) %>% 
        group_by(WorkID, Variable) %>% 
        mutate(dupe = n()>1,
               dup_id = row_number(),
               Variable = as.character(Variable),
               Variable = case_when(
                       dup_id == 1 & Variable == "Creator" ~ "Creator1",
                       dup_id == 2 & Variable == "Creator" ~ "Creator2",
                       dup_id == 3 & Variable == "Creator" ~ "Creator3",
                       dup_id == 4 & Variable == "Creator" ~ "Creator4",
                       dup_id == 5 & Variable == "Creator" ~ "Creator5",
                       dup_id == 6 & Variable == "Creator" ~ "Creator6",
                       dup_id == 7 & Variable == "Creator" ~ "Creator7",
                       dup_id == 8 & Variable == "Creator" ~ "Creator8",
                       dup_id == 1 & Variable == "Country" ~ "Country1",
                       dup_id == 2 & Variable == "Country" ~ "Country2",
                       dup_id == 3 & Variable == "Country" ~ "Country3",
                       dup_id == 4 & Variable == "Country" ~ "Country4",
                       dup_id == 5 & Variable == "Country" ~ "Country5",
                       dup_id == 1 & Variable == "URL" ~ "URL1",
                       dup_id == 2 & Variable == "URL" ~ "URL2",
                       dup_id == 3 & Variable == "URL" ~ "URL3",
                       TRUE ~ Variable)) %>% 
        select(-c(dupe, dup_id)) %>% 
        pivot_wider(names_from = Variable, 
                    values_from = Value) %>% 
        select(WorkID, Work_WikidataID, WorkTitle, Genre, Year, Creator1, 
               Country1, URL1, URL2, URL3, Country2, Country3, Country4, 
               Country5, Creator2, Creator3, Creator4, Creator5, Creator6, 
               Creator7, Creator8) %>% 
        ungroup()
        

# Export to a very simple text file giving a quick overview of the dataset.

write.table(creativeworks_overview, "data/creativeworks_overview.txt", 
            sep = ". ", 
            quote = FALSE,
            row.names = FALSE,
            na = "Country unknown")

write_csv(worksinfo, "data/worksinfo.csv")

worksinfo_simple <- worksinfo %>% 
        select(WorkID, Work_WikidataID, WorkTitle, Genre, Year, Creator = Creator1, URL = URL1)

creativeworks_overview <- worksinfo %>% 
        select(Genre, Year, Creator1, WorkTitle, Country1) %>% 
        arrange(Genre, Year, Creator1)

write_tsv(creativeworks_overview, "data/creativeworks_overview.txt", 
            na = "Country unknown")
               

# add creator1 wikiIDs

creators <- wikidata_works_creators %>% 
        select(Creator, Creator_WikidataID) %>% 
        distinct()

worksinfo_temp <-merge(worksinfo_simple, creators, by = "Creator")

# Put columns in right order, name Creator back to Creator1 so people
# are aware there may be more creators, and remove any rows where there were
# multiple entries for a creator and one had a wikiID and one not.

worksinfo <- worksinfo_temp %>% 
        select(WorkID, Work_WikidataID, WorkTitle, Genre, Year, 
               Creator1 = Creator, Creator_WikidataID, URL) %>% 
        filter(!(duplicated(WorkID) & is.na(Creator_WikidataID)))

# worksinfo now has 500 rows, one for each work. Perfect.
# 
write_csv(worksinfo, "data/worksinfo.csv")
               