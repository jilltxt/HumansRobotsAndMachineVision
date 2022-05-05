# Importing each file with factors etc

CreativeWorks <- read_csv(
        "https://github.com/jilltxt/HumansRobotsAndMachineVision/raw/main/data/creativeworks.csv",
        col_types = cols(
                WorkID = col_integer(),
                WorkTitle = col_character(),
                Sentiment = col_factor(levels = c(
                        "Exciting", "Flawed", "Helpful", "Neutral", "Wondrous",
                        "Hostile","Oppressive", "Alien", "Creepy", "Subversive", 
                        "Dangerous",  "Intrusive", "Empowering", "Protective", 
                        "Intimate", "Misleading", "Fun", "Overwhelming", 
                        "Prosocial", "Disgusting")),
                Topic = col_factor(levels = c(
                        "Nudity", "Social Media", "Romantic relationship", "Climate Change", 
                          "Dystopian", "Horror", "Robots/androids", "Surveillance", "Automation", 
                          "City", "Labour", "War", "Identity", "AI", "Animals", "Consciousness", 
                          "Nature", "Companionship", "Competition", "Playful", "Family", 
                          "Free will", "Physical violence", "Crime", "Hacking", "Conflict", 
                          "Empathy", "Utopian", "Race", "Sex", "Cyborgs", "Inequality", 
                          "Economy", "Grief", "Autonomous vehicles", "Gender")),
                TechRef= col_factor(levels = c(
                        "Holograms", "Augmented reality", "Ocular implant", 
                        "Emotion recognition", "Surveillance cameras", "AI", 
                        "Virtual reality", "Motion tracking", "Body scans", 
                        "Drones", "MicroscopeOrTelescope", "Biometrics", 
                        "Image generation", "Facial recognition", 
                        "Object recognition",  "3D scans", "Machine learning", 
                        "Filtering", "Deepfake", "Camera",  "Cameraphone", 
                        "Interactive panoramas", "Non-Visible Spectrum", "UGV",
                        "Webcams", "Satellite images")),
                TechUsed= col_factor(levels = c(
                        "Holograms", "Augmented reality", "Ocular implant", 
                        "Emotion recognition", "Surveillance cameras", "AI", 
                        "Virtual reality", "Motion tracking", "Body scans", 
                        "Drones", "MicroscopeOrTelescope", "Biometrics", 
                        "Image generation", "Facial recognition", 
                        "Object recognition",  "3D scans", "Machine learning", 
                        "Filtering", "Deepfake", "Camera",  "Cameraphone", 
                        "Interactive panoramas", "Non-Visible Spectrum", "UGV",
                        "Webcams", "Satellite images")
                )
        )
)

Characters <- read_csv(
        "https://github.com/jilltxt/HumansRobotsAndMachineVision/raw/main/data/characters.csv",
        col_types = cols(
                CharacterID = col_integer(),
                Character = col_character(),
                Species = col_factor(levels = c(
                        "Animal", "Cyborg", "Fictional", 
                        "Human", "Machine", "Unknown")),
                Gender = col_factor(levels = c(
                        "Female","Male","Non-binary or Other", "Trans Woman",
                        "Unknown")),
                RaceOrEthnicity = col_factor(levels = c(
                        "Asian", "Black", "Person of Colour", "White", "Immigrant", "Indigenous",
                        "Complex", "Unknown")),
                Age = col_factor(levels = c(
                        "Child", "Young Adult", "Adult", "Elderly", 
                        "Unknown")),
                Sexuality = col_factor(levels = c(
                        "Homosexual", "Heterosexual", "Bi-sexual", "Other",
                        "Unknown")),
                IsGroup = col_logical(),
                IsCustomizable = col_logical()
        )
)

#Make narrativegenres.csv
#
creativeworks_simple %>% 
        select(NarrativeGenre = Genre, WorkID = ID, WorkTitle = Title) %>% 
        filter(NarrativeGenre != "Game" 
               & NarrativeGenre != "Art"
               & NarrativeGenre != "Installation art"
               & NarrativeGenre != "Online art"
               & NarrativeGenre != "Video art"
               & NarrativeGenre != "Performance art"
               & NarrativeGenre != "Photograph/print/painting"
               & NarrativeGenre != "Narrative") %>% 
        arrange(NarrativeGenre, WorkTitle) %>% 
        distinct() %>% 
        write_csv("data/narrativegenres.csv")
        
        