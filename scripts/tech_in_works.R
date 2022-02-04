

# This code works but I'd like to make a version that combines TechRef and TechUsed
# so it's just Tech. This would make artworks more comparable with narr and games.
 
CreativeWorksFull %>% 
        select(WorkTitle, Year, Genre, Country, TechRef, TechUsed, Sentiment) %>% 
        distinct() %>%
        ggplot(aes(x = TechRef, fill = Genre)) +
        geom_bar()  +
        labs (x = "Technologies referenced in the work",
              y = "Count of works each tech is referenced in",
              title ="How often each technology is referenced in a work") +
        theme(axis.text=element_text(size=8),
              title=element_text(size=10,face="bold")) +
        coord_flip() +
        facet_wrap(~Genre)

# To do this with just Technology instead of TechRef vs TechUsed we could either
# - make one df x with just TechRef and another y with just TechUsed, rename 
# both columns Tech then vbind and 
# remove duplicates using distinct().
# - or use the technologies in situations instead and merge with creativeworks to 
# connect them to the WorkTitle.

CreativeWorks_TechRef <- CreativeWorksFull %>% 
        select(WorkTitle, Year, Genre, Country, TechRef) %>% 
        distinct() %>%
        
        CreativeWorks_TechUsed <-
                
 