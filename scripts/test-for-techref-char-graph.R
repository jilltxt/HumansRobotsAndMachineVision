# Create a column with the count of men for each tech so we can sort the chart
# according to how many male characters are in a work using that tech.

MaleTechCount <- Character_background %>% 
        select(TechRef, Character, Genre, Gender) %>% 
        distinct() %>%
        group_by(TechRef, Gender) %>% 
        summarise(Men = n()) %>% 
        filter(Gender == "Male") %>% 
        arrange(Men) %>% 
        select(TechRef, Men)

# Connect the "Men" column to the big Character_background dataframe.
TechGender <- merge(Character_background, MaleTechCount, by = "TechRef")

# Select just a few columns and create the barchart. 
# I feel like this should sort the fill, but it doesn't...
TechGender %>% 
        select(TechRef, Character, Genre, Gender, Men) %>% 
        distinct() %>%
        group_by(TechRef, Men) %>% 
        arrange(desc(Men), TechRef) %>% 
        ggplot(aes(x = TechRef, fill = Gender)) +
        geom_bar(position = "fill") +
        geom_text(aes(label = Men),
                  position = position_fill(vjust = 0.5),
                  color = "white") +
        labs (y = "Gender of characters in works that reference these technologies") +
        coord_flip()

# Argh. Maybe I need the count for each gender after all? But just to somehow
# sort by one? I dunno, very frustrating. 
# 
# 
# 
# Maybe thsi? https://stackoverflow.com/questions/58628562/reorder-geom-bar-based-on-sort-of-one-of-the-fill-variables
# @camille Thanks. I ended up arrange()ing the dataframe by group, desc(prop) then mutate(rsk = rsk %>% factor(levels = unique(rsk))) to get the appropriate order of rsk along the x-axis. I need to explore fct_reorder2() more. I've used fct_reorder() quite bit but never it's cousin.

# Check out fct_reorder2() -> do the help. 

df <- tibble::tribble(
        ~color,     ~a, ~b,
        "blue",      1,  2,
        "green",     6,  2,
        "purple",    3,  3,
        "red",       2,  3,
        "yellow",    5,  1
)
df$color <- factor(df$color)
fct_reorder(df$color, df$a, min)
fct_reorder2(df$color, df$a, df$b)
                
