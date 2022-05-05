# Classifying / classified
# Based on flawed.R
# 


library(tidyverse)

# Import situations.
Situations <- read_csv("data/situations.csv",
                       col_types = cols(
                               SituationID = col_integer(),
                               Situation = col_character(), 
                               Genre = col_character(),
                               Character = col_character(),
                               Entity = col_character(),
                               Technology = col_character(),
                               Verb = col_character()
                       )
)

Situations %>% 
        filter(Verb == "Classifying" | Verb == "Classified")

# find synonyms for flawed using the syn library - which basically just
# produces lists of synonyms (or antonyms using the ant() function)

library(syn)

# Make a vector (list of words) consisting of synonyms to verbs I know are in our
# dataset that are similar to flawed. Add the actual words from the dataset so they're
# also included.
# 
classify <- c("Classifying", "Classified", "Organised", "Sorted",
              "Sorting", "Organising",
           syn("classifying"), syn("classified"), syn("organising"), 
           syn("sorting"))

# get synonyms for all THOSE synonyms. 
# just doing syn(flaws) won't work, because you have to do syn on ecah
# individual word (each value in the vector)
# lapply lets you feed a vector to a function and have the function run on
# each value consecutively. lapply produces a list, so we need to unlist it 
# to get a vector again.

more_classify <- unlist(lapply(classify, syn))
# But this is a BAD IDEA for classify because it gives almost 10000 words and 
# is WAY too extensive. So we'll stick to just classify.

# convert to title case (first letter capitalised) to match the capitalisations
# in Situations$Verb. Subset only the synonyms that end in -ed or -ing. I 
# suppose that last thing isn't really necessary, but...

classify <- str_to_title(
        classify[str_detect(classify, "ing")|str_detect(classify, "ed")])

# Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
Situations %>% 
             mutate(Classify = case_when(Verb %in% classify ~ "Classify",
                                       TRUE ~ "Not classifying"),
                    Agent = case_when(!is.na(Entity) ~ "Entity",
                                      !is.na(Technology) ~ "Technology",
                                      !is.na(Character) ~ "Character")) %>% 
             filter(Classify == "Classify") %>% 
             select(Verb) %>% 
             add_count(Verb) %>%
             distinct()

# This gives 12 different verbs. Disposted, Concealed, Reporting seem wrong, and we have used Scanning differently. 

#  [34] "hierarchic"                 "hush-hush"                  "indexed"                   
[37] "latent"                     "marshaled"                  "methodized"                
[40] "mysterious"                 "normalized"                 "occult"                    
[43] "on file"                    "ordered"                    "orderly"                   
[46] "organized"                  "pigeonholed"                "placed"                    
[49] "pyramidal"                  "ranged"                     "ranked"                    
[52] "rated"                      "regularized"                "regulated"                 
[55] "restricted"                 "routinized"                 "secret"                    
[58] "smothered"                  "sorted"                     "standardized"              
[61] "stifled"                    "stratified"                 "suppressed"                
[64] "synchronized"               "systematized"               "tabular"                   
[67] "top secret"                 "ulterior"                   "unbreatheable"             
[70] "under security"             "under wraps"                "undisclosable"             
[73] "undisclosed"                "undivulgable"               "undivulged"                
[76] "unrevealable"               "unrevealed"                 "unspoken"                  
[79] "untellable"                 "untold"                     "unutterable"               
[82] "unuttered"                  "unwhisperable"              "analysis"                  
[85] "analyzing"                  "appraisal"                  "arrangement"               
[88] "assessment"                 "assortment"                 "cataloging"                
[91] "categorization"             "classification"             "classifying"               
[94] "codification"               "collating"                  "computer technology"       
[97] "computer typesetting"       "computing"                  "culling"                   
[100] "data processing"            "data retrieval"             "division"                  
[103] "EDP"                        "electronic data processing" "evaluation"                
[106] "factoring"                  "filing"                     "gauging"                   
[109] "gradation"                  "grading"                    "grouping"                  
[112] "high-speed data handling"   "identification"             "indexing"                  
[115] "machine computation"        "pigeonholing"               "placement"                 
[118] "ranging"                    "ranking"                    "rating"                    
[121] "reporting"                  "scanning"                   "screening"                 
[124] "selection"                  "sifting"                    "sifting out"               
[127] "sorting"                    "sorting out"                "stratification"            
[130] "subdivision"                "subordination"              "tabulation"                
[133] "taxonomy"                   "triage"                     "typology"                  
[136] "weighing"                   "winnowing"                 
> more_classify <- unlist(lapply(classify, syn))
> more_flaws <- str_to_title(
        +         more_flaws[str_detect(more_flaws, "ing")|str_detect(more_flaws, "ed")])
> more_classify <- str_to_title(
        +         more_flaws[str_detect(more_classify, "ing")|str_detect(more_classify, "ed")])
> more_classify
[1] "Blemished"                   "Cicatrized"                  "Cracked"                    
[4] "Crazed"                      "Defaced"                     "Deformed"                   
[7] "Disfigured"                  "Distorted"                   "Flawed"                     
[10] "Harmed"                      "Impaired"                    "Kinked"                     
[13] "Outlawed"                    "Scabbed"                     "Scarified"                  
[16] "Scarred"                     "Self-Annulling"              "Self-Refuting"              
[19] "Tarnished"                   "Unallowed"                   "Unauthorized"               
[22] "Unproved"                    "Weakened"                    "Blinded"                    
[25] "Blindfolded"                 "Darkened"                    "Dazzled"                    
[28] "Dim-Sighted"                 "Hoodwinked"                  "Nearsighted"                
[31] "Nonunderstanding"            "Obscured"                    "Shortsighted"               
[34] "Unapprehending"              "Uncomprehending"             "Undiscerning"               
[37] "Adulterated"                 "Aggravated"                  "Blemished"                  
[40] "Burned"                      "Busted"                      "Exacerbated"                
[43] "Found Wanting"               "Impaired"                    "Mangled"                    
[46] "Mediocre"                    "Mixed"                       "Mutilated"                  
[49] "Ruptured"                    "Scalded"                     "Scorched"                   
[52] "Shattered"                   "Slashed"                     "Smashed"                    
[55] "Spoiled"                     "Undeveloped"                 "Unfinished"                 
[58] "Unperfected"                 "Wanting"                     "Weakened"                   
[61] "Departing"                   "Deviating"                   "Distorted"                  
[64] "Erring"                      "Hardly The Thing"            "Not The Thing"              
[67] "Perverted"                   "Rambling"                    "Shifting"                   
[70] "Straying"                    "Swerving"                    "Turning"                    
[73] "Twisting"                    "Undirected"                  "Unfitting"                  
[76] "Unproved"                    "Veering"                     "Wandering"                  
[79] "Wicked"                      "Bewildered"                  "Dismayed"                   
[82] "Distracted"                  "Embarrassed"                 "Flawed"                     
[85] "Guessing"                    "Mazed"                       "Perturbed"                  
[88] "Unproved"                    "Flawed"                      "Lawmaking"                  
[91] "Outlawed"                    "Unallowed"                   "Unauthorized"               
[94] "Unwarranted"                 "Alternating"                 "Bothered"                   
[97] "Discomposed"                 "Disconcerted"                "Dismayed"                   
[100] "Disoriented"                 "Distorted"                   "Distracted"                 
[103] "Embarrassed"                 "Erring"                      "Flawed"                     
[106] "Flickering"                  "Rambling"                    "Roving"                     
[109] "Straying"                    "Turned Around"               "Uncontrolled"               
[112] "Undisciplined"               "Flawed"                      "Perturbed"                  
[115] "Shuffled"                    "Bollixed Up"                 "Ravening"                   
[118] "Rip-Roaring"                 "Screwed Up"                  "Undefined"                  
[121] "Flawed"                      "Unauthorized"                "Unwarranted"                
[124] "Agee-Jawed"                  "Bowed"                       "Contorted"                  
[127] "Convulsed"                   "Crookedly"                   "Crumpled"                   
[130] "Crunched"                    "Deranged"                    "Disarranged"                
[133] "Discomfited"                 "Discomposed"                 "Disconcerted"               
[136] "Dislocated"                  "Disordered"                  "Disorganized"               
[139] "Distorted"                   "Disturbed"                   "Erring"                     
[142] "Flawed"                      "Lopsided"                    "One-Sided"                  
[145] "Perturbed"                   "Skew-Jawed"                  "Straying"                   
[148] "Unproved"                    "Wamper-Jawed"                "Disconcerted"               
[151] "Disoriented"                 "Distorted"                   "Distracted"                 
[154] "Disturbed"                   "Embarrassed"                 "Erring"                     
[157] "Flawed"                      "Guessing"                    "Mazed"                      
[160] "Perverted"                   "Straying"                    "Unproved"                   
[163] "Distorted"                   "Erring"                      "Flawed"                     
[166] "Inculpated"                  "Involved"                    "Perverted"                  
[169] "Straying"                    "Unproved"                    "Cockeyed"                   
[172] "Disarranged"                 "Discomposed"                 "Distorted"                  
[175] "Disturbed"                   "Erring"                      "Flawed"                     
[178] "Misplaced"                   "Perturbed"                   "Perverted"                  
[181] "Shuffled"                    "Skew-Jawed"                  "Skewed"                     
[184] "Squinting"                   "Straying"                    "Unproved"                   
[187] "Unsettled"                   "Wamper-Jawed"                "Blemished"                  
[190] "Bloated"                     "Blotted"                     "Bowlegged"                  
[193] "Checked"                     "Cicatrized"                  "Club-Footed"                
[196] "Cracked"                     "Crazed"                      "Defaced"                    
[199] "Deformed"                    "Disfigured"                  "Distorted"                  
[202] "Dwarfed"                     "Dysphemized"                 "Flatfooted"                 
[205] "Flawed"                      "Found Wanting"               "Ill-Proportioned"           
[208] "Ill-Shaped"                  "Impaired"                    "Kinked"                     
[211] "Knock-Kneed"                 "Lacking"                     "Malformed"                  
[214] "Mediocre"                    "Misproportioned"             "Mixed"                      
[217] "Mutilated"                   "Pigeon-Toed"                 "Pimpled"                    
[220] "Pug-Nosed"                   "Scarified"                   "Scarred"                    
[223] "Snub-Nosed"                  "Spoiled"                     "Swaybacked"                 
[226] "Talipedic"                   "Truncated"                   "Twisted"                    
[229] "Uglified"                    "Undeveloped"                 "Unfinished"                 
[232] "Unpleasing"                  "Wanting"                     "Warped"                     
[235] "Flawed"                      "Outlawed"                    "Unallowed"                  
[238] "Unauthorized"                "Unwarranted"                 "White Lightning"            
[241] "Banned"                      "Barred"                      "Cigarette Smuggling"        
[244] "Disapproved"                 "Dope Smuggling"              "Excluded"                   
[247] "Flawed"                      "Forbidding"                  "Narcotics Smuggling"        
[250] "Not Permitted"               "Outlawed"                    "Proscribed"                 
[253] "Ruled Out"                   "Smuggled Goods"              "Tabooed"                    
[256] "Unallowed"                   "Unauthorized"                "Unlicensed"                 
[259] "Unsanctioned"                "Zoning"                      "Zoning Laws"                
[262] "Clashing"                    "Confronting"                 "Confuting"                  
[265] "Contrasted"                  "Countervailing"              "Denying"                    
[268] "Disagreeing"                 "Disallowing"                 "Disavowing"                 
[271] "Disclaiming"                 "Disowning"                   "Flawed"                     
[274] "Grating"                     "Refuting"                    "Unconnected"                
[277] "Contaminated"                "Corrupted"                   "Crooked"                    
[280] "Debased"                     "Decomposed"                  "Distorted"                  
[283] "Festering"                   "Flawed"                      "Gangrened"                  
[286] "Miseducate"                  "Necrosed"                    "Putrefied"                  
[289] "Rotting"                     "Sphacelated"                 "Spoiled"                    
[292] "Steeped In Iniquity"         "Straying"                    "Subverted"                  
[295] "Tainted"                     "Unconscienced"               "Underhanded"                
[298] "Unprincipled"                "Unproved"                    "Vitiated"                   
[301] "Warped"                      "Blithering"                  "Burbling"                   
[304] "Busted"                      "Crackbrained"                "Cracked"                    
[307] "Croaking"                    "Damaged"                     "Dithering"                  
[310] "Drooling"                    "Embittered"                  "Exacerbated"                
[313] "Hallucinated"                "Harsh-Sounding"              "In Shreds"                  
[316] "Maddened"                    "Marred"                      "Mazed"                      
[319] "Mentally Handicapped"        "Mentally Retarded"           "Pimpled"                    
[322] "Quartered"                   "Ragged"                      "Retarded"                   
[325] "Scabbed"                     "Scalded"                     "Scarified"                  
[328] "Scarred"                     "Shattered"                   "Slashed"                    
[331] "Slobbering"                  "Smashed"                     "Splintered"                 
[334] "Squawking"                   "Touched"                     "Unbalanced"                 
[337] "Unhinged"                    "Untuned"                     "Weakened"                   
[340] "Corrupted"                   "Crooked"                     "Depraved"                   
[343] "Flawed"                      "Hardly The Thing"            "Implicated"                 
[346] "Inculpated"                  "Involved"                    "Not The Thing"              
[349] "Outlawed"                    "Unallowed"                   "Unauthorized"               
[352] "Unconscienced"               "Underhanded"                 "Unfitting"                  
[355] "Unprincipled"                "Unwarranted"                 "Wicked"                     
[358] "Chipped"                     "Crazed"                      "Deteriorated"               
[361] "Harmed"                      "Irritated"                   "Marred"                     
[364] "Mixed"                       "Unfounded"                   "Blemished"                  
[367] "Disfigured"                  "Disturbed"                   "Flawed"                     
[370] "The Crippled"                "Unsatisfying"                "Blemished"                  
[373] "Bowlegged"                   "Cracked"                     "Crazed"                     
[376] "Kinked"                      "Knock-Kneed"                 "Malformed"                  
[379] "Scabbed"                     "Twisted"                     "Deceiving"                  
[382] "Erring"                      "Blemished"                   "Blotted"                    
[385] "Bowlegged"                   "Checked"                     "Club-Footed"                
[388] "Cracked"                     "Damaged"                     "Defaced"                    
[391] "Deformed"                    "Kinked"                      "Malformed"                  
[394] "Ruined"                      "Scarred"                     "Snub-Nosed"                 
[397] "Truncated"                   "Affected"                    "Bowed"                      
[400] "Checked"                     "Cockeyed"                    "Counterfeited"              
[403] "Crunched"                    "Dispersed"                   "Flawed"                     
[406] "Garbled"                     "Kinked"                      "Marred"                     
[409] "Misapprehended"              "Pimpled"                     "Twisted"                    
[412] "Unproved"                    "Erring"                      "Flitting"                   
[415] "Roaming"                     "Shifting"                    "Wandering"                  
[418] "Mediocre"                    "Unproved"                    "Perverted"                  
[421] "Unconnected"                 "Backsliding"                 "Chiseling"                  
[424] "Cunning"                     "False-Principled"            "Lying"                      
[427] "Perverted"                   "Simulated"                   "Unproved"                   
[430] "Crazed"                      "Kinked"                      "Malfunctioning"             
[433] "Mixed"                       "Twisted"                     "Unperfected"                
[436] "Warped"                      "Unallowed"                   "Crazed"                     
[439] "Erring"                      "Flawed"                      "Weakened"                   
[442] "Aggravated"                  "Damaged"                     "Exacerbated"                
[445] "Harmed"                      "Smashed"                     "Disbelieving"               
[448] "Unconverted"                 "Flawed"                      "Unproved"                   
[451] "Undeserved"                  "Unlicensed"                  "Dressed Up"                 
[454] "Falsified"                   "Flawed"                      "Miscreated"                 
[457] "Perverted"                   "Warped"                      "Barred"                     
[460] "Prohibited"                  "Unlicensed"                  "Unsanctioned"               
[463] "Distorted"                   "Unconnected"                 "Enchanting"                 
[466] "Rarefied"                    "Checked"                     "Embittered"                 
[469] "Flawed"                      "Lacerated"                   "Mediocre"                   
[472] "Scalded"                     "Scorched"                    "Shattered"                  
[475] "Chipped"                     "Embittered"                  "Exacerbated"                
[478] "Failing"                     "Lacerated"                   "Mixed"                      
[481] "Mutilated"                   "Unfinished"                  "Not Proved"                 
[484] "Uncorroborated"              "Unestablished"               "Unsustained"                
[487] "Untested"                    "Untried"                     "Amusing"                    
[490] "Assorted"                    "Differentiated"              "Ill-Suited"                 
[493] "Screaming"                   "Unbefitting"                 "Unsuited"                   
[496] "Variegated"                  "Flawed"                      "Self-Refuting"              
[499] "Balancing"                   "Contrasted"                  "Contrasting"                
[502] "Counterbalancing"            "Disagreeing"                 "Disjoined"                  
[505] "Flawed"                      "Not Following"               "Squared Off"                
[508] "Uncontrolled"                "Unfixed"                     "Unjoined"                   
[511] "Wandering"                   "Wavering"                    "Cloistered Monk"            
[514] "Etiolated"                   "Exhausted"                   "Flawed"                     
[517] "Reduced In Health"           "Self-Refuting"               "Crackbrained"               
[520] "Ill-Advised"                 "Inexpedient"                 "Pin-Brained"                
[523] "Tetched"                     "Touched"                     "Unadvised"                  
[526] "Unbalanced"                  "Unconsidered"                "Undiscerning"               
[529] "Unforeseeing"                "Ungifted"                    "Unhinged"                   
[532] "Unreasoning"                 "Unreflecting"                "Unseeing"                   
[535] "Dispersed"                   "Easygoing"                   "Flawed"                     
[538] "Fluctuating"                 "Granulated"                  "Guttering"                  
[541] "Halting"                     "Ill-Matched"                 "Mismatched"                 
[544] "Offhanded"                   "Overbalanced"                "Pitted"                     
[547] "Potholed"                    "Ragged"                      "Rambling"                   
[550] "Relaxed"                     "Roving"                      "Unaffected"                 
[553] "Unassuming"                  "Unordered"                   "Unstudied"                  
[556] "Wimpled"                     "Wobbling"                    "Blemished"                  
[559] "Cicatrized"                  "Cracked"                     "Crazed"                     
[562] "Defaced"                     "Deformed"                    "Disfigured"                 
[565] "Distorted"                   "Kinked"                      "Pimpled"                    
[568] "Scabbed"                     "Scarified"                   "Disobedient"                
[571] "Flawed"                      "Indisciplined"               "Unallowed"                  
[574] "Unbridled"                   "Unchecked"                   "Uncomplying"                
[577] "Uncontrolled"                "Uncurbed"                    "Ungoverned"                 
[580] "Unrestrained"                "Wicked"                      "Abandoned"                  
[583] "Afoot And Lighthearted"      "Chambering"                  "Confused"                   
[586] "Dangling"                    "Debauched"                   "Detached"                   
[589] "Dilapidated"                 "Disengaged"                  "Disorganized"               
[592] "Draggletailed"               "Escaped"                     "Expedite"                   
[595] "Floating"                    "Freed"                       "Ill-Defined"                
[598] "Liberated"                   "Lop-Eared"                   "Maundering"                 
[601] "Neglecting"                  "Noninterfering"              "Not Following"              
[604] "Offhanded"                   "Perverted"                   "Procrastinating"            
[607] "Ragged"                      "Raggedy"                     "Rambling"                   
[610] "Relaxed"                     "Roving"                      "Sagging"                    
[613] "Sagging In Folds"            "Seedy"                       "Self-Annulling"             
[616] "Self-Refuting"               "Slighting"                   "Speed"                      
[619] "Streaming"                   "Sweeping"                    "Tattered"                   
[622] "Unaffected"                  "Unattached"                  "Unbridled"                  
[625] "Unchained"                   "Unchecked"                   "Uncoerced"                  
[628] "Uncommitted"                 "Uncompelled"                 "Unconfined"                 
[631] "Unconnected"                 "Uncontrolled"                "Undefined"                  
[634] "Undestined"                  "Undetermined"                "Unengaged"                  
[637] "Unfastened"                  "Unfettered"                  "Unfixed"                    
[640] "Uninvolved"                  "Unmuzzled"                   "Unrepressed"                
[643] "Unreserved"                  "Unrestrained"                "Unshackled"                 
[646] "Unspecified"                 "Unstring"                    "Unstudied"                  
[649] "Veiled"                      "Wandering"                   "Blemished"                  
[652] "Cracked"                     "Dwarfed"                     "Flatfooted"                 
[655] "Kinked"                      "Knock-Kneed"                 "Misproportioned"            
[658] "Mutilated"                   "Pimpled"                     "Pug-Nosed"                  
[661] "Ruined"                      "Scabbed"                     "Crazed"                     
[664] "Demented"                    "Deranged"                    "Disoriented"                
[667] "Erring"                      "Flawed"                      "Maddened"                   
[670] "Unbalanced"                  "Unhinged"                    "Unsettled"                  
[673] "Distorted"                   "Unproved"                    "Adulterated"                
[676] "Barring"                     "Blemished"                   "Discounting"                
[679] "Flawed"                      "Found Wanting"               "Lacking"                    
[682] "Leisured"                    "Maddened"                    "Mixed"                      
[685] "Out Of Soundings"            "Perverted"                   "Rambling"                   
[688] "Right-Wing"                  "Right-Wingish"               "Soured"                     
[691] "Straying"                    "Tainted"                     "Unmatched"                  
[694] "Unperfected"                 "Unresembling"                "Wandering"                  
[697] "Abandoned"                   "Dated"                       "Discontinued"               
[700] "Outmoded"                    "Outstanding"                 "Outward-Facing"             
[703] "Resigned"                    "Retired"                     "Stand Revealed"             
[706] "Superannuated"               "Unjointed"                   "Displaced Person"           
[709] "Outlawed"                    "Unallowed"                   "Unauthorized"               
[712] "Unwarranted"                 "Weed Out"                    "Outlawed"                   
[715] "Prohibited"                  "Ruled Out"                   "Tabooed"                    
[718] "Unlicensed"                  "Unwarranted"                 "Festering"                  
[721] "Flawed"                      "Gangrened"                   "Inculpated"                 
[724] "Poisoned"                    "Sphacelated"                 "Cussed"                     
[727] "Opposed"                     "Affected"                    "Bisexed"                    
[730] "Debauched"                   "Misused"                     "Morally Polluted"           
[733] "Outraged"                    "Pretended"                   "Simulated"                  
[736] "Slanted"                     "Checked"                     "Rough-Grained"              
[739] "Unpolished"                  "Unrefined"                   "Crazed"                     
[742] "Ill-Judged"                  "Misadvised"                  "Not Following"              
[745] "Pea-Brained"                 "Pin-Brained"                 "Self-Annulling"             
[748] "Unadvised"                   "Unforeseeing"                "Unhinged"                   
[751] "Wandering"                   "Disfigured"                  "Distorted"                  
[754] "Twisted"                     "Warped"                      "Disfigured"                 
[757] "Not Following"               "Perverted"                   "Benumbed"                   
[760] "Deprived Of Reason"          "Deranged"                    "Ill-Advised"                
[763] "Ill-Considered"              "Misadvised"                  "Nitwitted"                  
[766] "Pea-Brained"                 "Pin-Brained"                 "Self-Annulling"             
[769] "Shortsighted"                "Slackminded"                 "Tetched"                    
[772] "Touched"                     "Unarranged"                  "Unclassified"               
[775] "Undirected"                  "Undiscerning"                "Ungraded"                   
[778] "Unmeaning"                   "Untalented"                  "Confined"                   
[781] "Deprived Of Reason"          "Disoriented"                 "Dispirited"                 
[784] "Disturbed"                   "Feeling Awful"               "Feeling Faint"              
[787] "Feeling Something Terrible"  "Flawed"                      "Good And Tired"             
[790] "Hallucinated"                "Irritated"                   "Jaded"                      
[793] "Maddened"                    "Repelled"                    "Repulsed"                   
[796] "Revolted"                    "Sickened"                    "Stark-Staring Mad"          
[799] "Tetched"                     "Tired Of Living"             "Tired To Death"             
[802] "Unhinged"                    "Unsettled"                   "Wandering"                  
[805] "Wearied"                     "Wretched"                    "Aggravated"                 
[808] "Bifurcated"                  "Bisected"                    "Borrowed Stock"             
[811] "Branched"                    "Busted"                      "Checked"                    
[814] "Cicatrized"                  "Convertible Preferred Stock" "Cracked"                    
[817] "Cumulative Preferred Stock"  "Damaged"                     "Defaced"                    
[820] "Deformed"                    "Deteriorated"                "Distorted"                  
[823] "Divided"                     "Dividedness"                 "Embittered"                 
[826] "Exacerbated"                 "Fractured"                   "Gaping"                     
[829] "Guaranteed Stock"            "Halved"                      "Have A Falling-Out"         
[832] "Hypothecated Stock"          "In Shreds"                   "Injured"                    
[835] "Irritated"                   "Issued Capital Stock"        "Laying Open"                
[838] "Loaned Stock"                "Mangled"                     "Marred"                     
[841] "Mutilated"                   "Nearly Die Laughing"         "Ragged"                     
[844] "Scorched"                    "Slashed"                     "Spring Open"                
[847] "Stock Ledger"                "Swing Open"                  "Unissued Capital Stock"     
[850] "Warped"                      "Weakened"                    "Worsened"                   
[853] "Blighted"                    "Botched"                     "Butchered"                  
[856] "Cloying"                     "Defaced"                     "Desolated"                  
[859] "Dysphemized"                 "Festering"                   "Gangrened"                  
[862] "Ill-Considered"              "Ill-Devised"                 "Ill-Executed"               
[865] "Ill-Managed"                 "Impaired"                    "Irremediable"               
[868] "Misdirected"                 "Mismanaged"                  "Nauseating"                 
[871] "Necrosed"                    "Pampered"                    "Ravaged"                    
[874] "Ruined"                      "Spoiled"                     "Stinking"                   
[877] "Tainted"                     "Dirtied"                     "Drabbled"                   
[880] "Dyed"                        "Fouled"                      "Soiled"                     
[883] "Spotted"                     "Sullied"                     "Tinged"                     
[886] "Floating"                    "Meandering"                  "Shifting"                   
[889] "Strolling"                   "Swerving"                    "Swinging"                   
[892] "Besmirched"                  "Cankered"                    "Darkened"                   
[895] "Dirtied"                     "Discolored"                  "Smudged"                    
[898] "Steeped In Iniquity"         "Stigmatized"                 "Tarnished"                  
[901] "Turned"                      "Ulcerated"                   "Befouled"                   
[904] "Blackened"                   "Fouled"                      "Affected"                   
[907] "Agonized"                    "Balled Up"                   "Bowed"                      
[910] "Colored"                     "Convoluted"                  "Counterfeited"              
[913] "Cracked"                     "Crazed"                      "Crucified"                  
[916] "Crumpled"                    "Crunched"                    "Defaced"                    
[919] "Deformed"                    "Disfigured"                  "Distorted"                  
[922] "Distressed"                  "Doctored"                    "Embellished"                
[925] "Embrangled"                  "Entangled"                   "Falsified"                  
[928] "Feigned"                     "Flawed"                      "Influenced"                 
[931] "Interested"                  "Involuted"                   "Involved"                   
[934] "Kinked"                      "Knotted"                     "Know-Nothing"               
[937] "Lacerated"                   "Lopsided"                    "Loused Up"                  
[940] "Many-Faceted"                "Martyred"                    "Martyrized"                 
[943] "Meandering"                  "Misquoted"                   "Misrepresented"             
[946] "Mixed Up"                    "Mucked Up"                   "Opinionated"                
[949] "Perverted"                   "Pimpled"                     "Prejudiced"                 
[952] "Prepossessed"                "Pretended"                   "Racked"                     
[955] "Ramified"                    "Scabbed"                     "Screwed Up"                 
[958] "Self-Styled"                 "Simulated"                   "So-Called"                  
[961] "Strained"                    "Tangled"                     "Tormented"                  
[964] "Twisted"                     "Undetached"                  "Wounded"                    
[967] "Barred"                      "Not Permitted"               "Outlawed"                   
[970] "Unallowed"                   "Unauthorized"                "Vetoed"                     
[973] "Affected"                    "Colored"                     "Counterfeited"              
[976] "Dressed Up"                  "Embellished"                 "Embroidered"                
[979] "Feigned"                     "Miscited"                    "Misreported"                
[982] "Misstated"                   "Not Following"               "Perverted"                  
[985] "Self-Annulling"              "Self-Refuting"               "Self-Styled"                
[988] "Titivated"                   "Twisted"                     "Unaccepted"                 
[991] "Unapproved"                  "Unattested"                  "Uncertified"                
[994] "Unconfirmed"                 "Uncorroborated"              "Unfounded"                  
[997] "Banned"                      "Barred"                      "Flawed"                     
[1000] "Not Permitted"              
[ reached getOption("max.print") -- omitted 6059 entries ]
> # Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
        > view(Situations %>% 
                       +              mutate(Classify = case_when(Verb %in% more_classify ~ "Classify",
                                                                  +                                        TRUE ~ "Not classifying"),
                                             +                     Agent = case_when(!is.na(Entity) ~ "Entity",
                                                                                     +                                       !is.na(Technology) ~ "Technology",
                                                                                     +                                       !is.na(Character) ~ "Character")) %>% 
                       +              filter(Classify == 1) %>% 
                       +              select(Verb) %>% 
                       +              add_count(Verb) %>%
                       +              distinct())
> # Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
        > Situations %>% 
        +              mutate(Classify = case_when(Verb %in% more_classify ~ "Classify",
                                                   +                                        TRUE ~ "Not classifying"),
                              +                     Agent = case_when(!is.na(Entity) ~ "Entity",
                                                                      +                                       !is.na(Technology) ~ "Technology",
                                                                      +                                       !is.na(Character) ~ "Character")) %>% 
        +              filter(Classify == 1) %>% 
        +              select(Verb) %>% 
        +              add_count(Verb) %>%
        +              distinct())
Error: unexpected ')' in:
        "             add_count(Verb) %>%
             distinct())"
> Situations %>% 
        +              mutate(Classify = case_when(Verb %in% more_classify ~ "Classify",
                                                   +                                        TRUE ~ "Not classifying"),
                              +                     Agent = case_when(!is.na(Entity) ~ "Entity",
                                                                      +                                       !is.na(Technology) ~ "Technology",
                                                                      +                                       !is.na(Character) ~ "Character"))
# A tibble: 9,593 × 10
SituationID Situation                         Genre Character CharacterID Entity Technology Verb  Classify Agent
<int> <chr>                             <chr> <chr>           <dbl> <chr>  <chr>      <chr> <chr>    <chr>
        1        3269 Star Wars: Episode VII - The For… Narr… C-3PO            2164 NA     NA         Disc… Not cla… Char…
2        3269 Star Wars: Episode VII - The For… Narr… NA                 NA NA     Holograms  Visu… Not cla… Tech…
3        3269 Star Wars: Episode VII - The For… Narr… Leia Org…        2159 NA     NA         Lear… Not cla… Char…
4        3269 Star Wars: Episode VII - The For… Narr… NA                 NA NA     Holograms  Mapp… Not cla… Tech…
5        3269 Star Wars: Episode VII - The For… Narr… Leia Org…        2159 NA     NA         Disc… Not cla… Char…
6        3269 Star Wars: Episode VII - The For… Narr… C-3PO            2164 NA     NA         Info… Not cla… Char…
7        3269 Star Wars: Episode VII - The For… Narr… Han Solo         2157 NA     NA         Lear… Not cla… Char…
8        3269 Star Wars: Episode VII - The For… Narr… C-3PO            2164 NA     NA         Plan… Not cla… Char…
9        3269 Star Wars: Episode VII - The For… Narr… Han Solo         2157 NA     NA         Disc… Not cla… Char…
10        3269 Star Wars: Episode VII - The For… Narr… Leia Org…        2159 NA     NA         Info… Not cla… Char…
# … with 9,583 more rows
> # Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
        > view(Situations %>% 
                       +              mutate(Classify = case_when(Verb %in% more_classify ~ "Classify",
                                                                  +                                        TRUE ~ "Not classifying"),
                                             +                     Agent = case_when(!is.na(Entity) ~ "Entity",
                                                                                     +                                       !is.na(Technology) ~ "Technology",
                                                                                     +                                       !is.na(Character) ~ "Character"))) %>% 
        +              filter(Classify == 1) %>% 
        +              select(Verb) %>% 
        +              add_count(Verb) %>%
        +              distinct())
Error: unexpected ')' in:
        "             add_count(Verb) %>%
             distinct())"
> view(Situations %>% 
               +              mutate(Classify = case_when(Verb %in% more_classify ~ "Classify",
                                                          +                                        TRUE ~ "Not classifying"),
                                     +                     Agent = case_when(!is.na(Entity) ~ "Entity",
                                                                             +                                       !is.na(Technology) ~ "Technology",
                                                                             +                                       !is.na(Character) ~ "Character")))
> # Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
        > Situations %>% 
        +              mutate(Classify = case_when(Verb %in% more_classify ~ "Classify",
                                                   +                                        TRUE ~ "Not classifying"),
                              +                     Agent = case_when(!is.na(Entity) ~ "Entity",
                                                                      +                                       !is.na(Technology) ~ "Technology",
                                                                      +                                       !is.na(Character) ~ "Character")) %>% 
        +              filter(Classify == "Classify") %>% 
        +              select(Verb) %>% 
        +              add_count(Verb) %>%
        +              distinct())
Error: unexpected ')' in:
        "             add_count(Verb) %>%
             distinct())"
> # Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
        > Situations %>% 
        +              mutate(Classify = case_when(Verb %in% more_classify ~ "Classify",
                                                   +                                        TRUE ~ "Not classifying"),
                              +                     Agent = case_when(!is.na(Entity) ~ "Entity",
                                                                      +                                       !is.na(Technology) ~ "Technology",
                                                                      +                                       !is.na(Character) ~ "Character")) %>% 
        +              filter(Classify == "Classify") %>% 
        +              select(Verb) %>% 
        +              add_count(Verb) %>%
        +              distinct()
# A tibble: 151 × 2
Verb            n
<chr>       <int>
        1 Classified     95
2 Running         9
3 Stopped         2
4 Ignored         6
5 Ignoring       11
6 Flawed         16
7 Disoriented     3
8 Disabled       14
9 Following      16
10 Ordered         1
# … with 141 more rows
> more_classify <- str_to_title(
        +         more_classify[str_detect(more_classify, "ing")|str_detect(more_classify, "ed")])
> # Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
        > Situations %>% 
        +              mutate(Classify = case_when(Verb %in% more_classify ~ "Classify",
                                                   +                                        TRUE ~ "Not classifying"),
                              +                     Agent = case_when(!is.na(Entity) ~ "Entity",
                                                                      +                                       !is.na(Technology) ~ "Technology",
                                                                      +                                       !is.na(Character) ~ "Character")) %>% 
        +              filter(Classify == "Classify") %>% 
        +              select(Verb) %>% 
        +              add_count(Verb) %>%
        +              distinct()
# A tibble: 151 × 2
Verb            n
<chr>       <int>
        1 Classified     95
2 Running         9
3 Stopped         2
4 Ignored         6
5 Ignoring       11
6 Flawed         16
7 Disoriented     3
8 Disabled       14
9 Following      16
10 Ordered         1
# … with 141 more rows
> rm(flaws)
> rm(more_flaws)
> rm(more_classify)
> # Make a vector (list of words) consisting of synonyms to verbs I know are in our
        > # dataset that are similar to flawed. Add the actual words from the dataset so they're
        > # also included.
        > # 
        > classify <- c("Classifying", "Classified", "Organised", "Sorted",
                        +               "Sorting", "Organising",
                        +            syn("classifying"), syn("classified"), syn("organising"), 
                        +            syn("sorting"))
> more_classify <- unlist(lapply(classify, syn))
> more_classify <- str_to_title(
        +         more_classify[str_detect(more_classify, "ing")|str_detect(more_classify, "ed")])
> # Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
        > Situations %>% 
        +              mutate(Classify = case_when(Verb %in% more_classify ~ "Classify",
                                                   +                                        TRUE ~ "Not classifying"),
                              +                     Agent = case_when(!is.na(Entity) ~ "Entity",
                                                                      +                                       !is.na(Technology) ~ "Technology",
                                                                      +                                       !is.na(Character) ~ "Character")) %>% 
        +              filter(Classify == "Classify") %>% 
        +              select(Verb) %>% 
        +              add_count(Verb) %>%
        +              distinct()
# A tibble: 126 × 2
Verb              n
<chr>         <int>
        1 Mapping          33
2 Planning         22
3 Classifying     155
4 Overwhelmed       8
5 Classified       95
6 Controlling      92
7 Stopped           2
8 Understanding    10
9 Ignored           6
10 Projected        20
# … with 116 more rows
> # Make a vector (list of words) consisting of synonyms to verbs I know are in our
        > # dataset that are similar to flawed. Add the actual words from the dataset so they're
        > # also included.
        > # 
        > classify <- c("Classifying", "Classified", "Organised", "Sorted",
                        +               "Sorting", "Organising",
                        +            syn("classifying"), syn("classified"), syn("organising"), 
                        +            syn("sorting"))
> classify
[1] "Classifying"                "Classified"                 "Organised"                 
[4] "Sorted"                     "Sorting"                    "Organising"                
[7] "aligned"                    "arcane"                     "arranged"                  
[10] "arrayed"                    "assorted"                   "cabalistic"                
[13] "cataloged"                  "categorized"                "censored"                  
[16] "classified"                 "close"                      "closed"                    
[19] "composed"                   "concealed"                  "constituted"               
[22] "cryptic"                    "dark"                       "disposed"                  
[25] "enigmatic"                  "esoteric"                   "filed"                     
[28] "fixed"                      "graded"                     "grouped"                   
[31] "harmonized"                 "hermetic"                   "hidden"                    
[34] "hierarchic"                 "hush-hush"                  "indexed"                   
[37] "latent"                     "marshaled"                  "methodized"                
[40] "mysterious"                 "normalized"                 "occult"                    
[43] "on file"                    "ordered"                    "orderly"                   
[46] "organized"                  "pigeonholed"                "placed"                    
[49] "pyramidal"                  "ranged"                     "ranked"                    
[52] "rated"                      "regularized"                "regulated"                 
[55] "restricted"                 "routinized"                 "secret"                    
[58] "smothered"                  "sorted"                     "standardized"              
[61] "stifled"                    "stratified"                 "suppressed"                
[64] "synchronized"               "systematized"               "tabular"                   
[67] "top secret"                 "ulterior"                   "unbreatheable"             
[70] "under security"             "under wraps"                "undisclosable"             
[73] "undisclosed"                "undivulgable"               "undivulged"                
[76] "unrevealable"               "unrevealed"                 "unspoken"                  
[79] "untellable"                 "untold"                     "unutterable"               
[82] "unuttered"                  "unwhisperable"              "analysis"                  
[85] "analyzing"                  "appraisal"                  "arrangement"               
[88] "assessment"                 "assortment"                 "cataloging"                
[91] "categorization"             "classification"             "classifying"               
[94] "codification"               "collating"                  "computer technology"       
[97] "computer typesetting"       "computing"                  "culling"                   
[100] "data processing"            "data retrieval"             "division"                  
[103] "EDP"                        "electronic data processing" "evaluation"                
[106] "factoring"                  "filing"                     "gauging"                   
[109] "gradation"                  "grading"                    "grouping"                  
[112] "high-speed data handling"   "identification"             "indexing"                  
[115] "machine computation"        "pigeonholing"               "placement"                 
[118] "ranging"                    "ranking"                    "rating"                    
[121] "reporting"                  "scanning"                   "screening"                 
[124] "selection"                  "sifting"                    "sifting out"               
[127] "sorting"                    "sorting out"                "stratification"            
[130] "subdivision"                "subordination"              "tabulation"                
[133] "taxonomy"                   "triage"                     "typology"                  
[136] "weighing"                   "winnowing"                 
> more_classify <- unlist(lapply(classify, syn))
> more_classify
[1] "aligned"                    "arcane"                     "arranged"                  
[4] "arrayed"                    "assorted"                   "cabalistic"                
[7] "cataloged"                  "categorized"                "censored"                  
[10] "classified"                 "close"                      "closed"                    
[13] "composed"                   "concealed"                  "constituted"               
[16] "cryptic"                    "dark"                       "disposed"                  
[19] "enigmatic"                  "esoteric"                   "filed"                     
[22] "fixed"                      "graded"                     "grouped"                   
[25] "harmonized"                 "hermetic"                   "hidden"                    
[28] "hierarchic"                 "hush-hush"                  "indexed"                   
[31] "latent"                     "marshaled"                  "methodized"                
[34] "mysterious"                 "normalized"                 "occult"                    
[37] "on file"                    "ordered"                    "orderly"                   
[40] "organized"                  "pigeonholed"                "placed"                    
[43] "pyramidal"                  "ranged"                     "ranked"                    
[46] "rated"                      "regularized"                "regulated"                 
[49] "restricted"                 "routinized"                 "secret"                    
[52] "smothered"                  "sorted"                     "standardized"              
[55] "stifled"                    "stratified"                 "suppressed"                
[58] "synchronized"               "systematized"               "tabular"                   
[61] "top secret"                 "ulterior"                   "unbreatheable"             
[64] "under security"             "under wraps"                "undisclosable"             
[67] "undisclosed"                "undivulgable"               "undivulged"                
[70] "unrevealable"               "unrevealed"                 "unspoken"                  
[73] "untellable"                 "untold"                     "unutterable"               
[76] "unuttered"                  "unwhisperable"              "aligned"                   
[79] "arranged"                   "arrayed"                    "assorted"                  
[82] "cataloged"                  "categorized"                "classified"                
[85] "composed"                   "constituted"                "disposed"                  
[88] "filed"                      "fixed"                      "graded"                    
[91] "grouped"                    "harmonized"                 "hierarchic"                
[94] "indexed"                    "marshaled"                  "methodized"                
[97] "normalized"                 "on file"                    "ordered"                   
[100] "orderly"                    "organized"                  "pigeonholed"               
[103] "placed"                     "pyramidal"                  "ranged"                    
[106] "ranked"                     "rated"                      "regularized"               
[109] "regulated"                  "routinized"                 "sorted"                    
[112] "standardized"               "stratified"                 "synchronized"              
[115] "systematized"               "tabular"                    "analysis"                  
[118] "analyzing"                  "appraisal"                  "arrangement"               
[121] "assessment"                 "assortment"                 "cataloging"                
[124] "categorization"             "classification"             "classifying"               
[127] "codification"               "collating"                  "computer technology"       
[130] "computer typesetting"       "computing"                  "culling"                   
[133] "data processing"            "data retrieval"             "division"                  
[136] "EDP"                        "electronic data processing" "evaluation"                
[139] "factoring"                  "filing"                     "gauging"                   
[142] "gradation"                  "grading"                    "grouping"                  
[145] "high-speed data handling"   "identification"             "indexing"                  
[148] "machine computation"        "pigeonholing"               "placement"                 
[151] "ranging"                    "ranking"                    "rating"                    
[154] "reporting"                  "scanning"                   "screening"                 
[157] "selection"                  "sifting"                    "sifting out"               
[160] "sorting"                    "sorting out"                "stratification"            
[163] "subdivision"                "subordination"              "tabulation"                
[166] "taxonomy"                   "triage"                     "typology"                  
[169] "weighing"                   "winnowing"                  "abstract"                  
[172] "abstruse"                   "arcane"                     "cabalistic"                
[175] "censored"                   "classified"                 "close"                     
[178] "closed"                     "concealed"                  "cryptic"                   
[181] "dark"                       "deep"                       "eerie"                     
[184] "enigmatic"                  "esoteric"                   "extramundane"              
[187] "extraterrestrial"           "fey"                        "hermetic"                  
[190] "hidden"                     "hush-hush"                  "hypernormal"               
[193] "hyperphysical"              "impenetrable"               "inscrutable"               
[196] "latent"                     "mysterious"                 "mystic"                    
[199] "mystical"                   "numinous"                   "occult"                    
[202] "otherworldly"               "preterhuman"                "preternatural"             
[205] "preternormal"               "pretersensual"              "profound"                  
[208] "psychic"                    "recondite"                  "restricted"                
[211] "secret"                     "smothered"                  "spiritual"                 
[214] "stifled"                    "superhuman"                 "supernatural"              
[217] "supernormal"                "superphysical"              "supersensible"             
[220] "supersensual"               "suppressed"                 "supramundane"              
[223] "supranatural"               "top secret"                 "transcendental"            
[226] "transmundane"               "ulterior"                   "unaccountable"             
[229] "unbreatheable"              "uncanny"                    "under security"            
[232] "under wraps"                "undisclosable"              "undisclosed"               
[235] "undivulgable"               "undivulged"                 "unearthly"                 
[238] "unguessed"                  "unhuman"                    "unknowable"                
[241] "unrevealable"               "unrevealed"                 "unspoken"                  
[244] "untellable"                 "untold"                     "unutterable"               
[247] "unuttered"                  "unwhisperable"              "unworldly"                 
[250] "weird"                      "agreed"                     "aligned"                   
[253] "arranged"                   "arrayed"                    "assorted"                  
[256] "blueprinted"                "businesslike"               "calculated"                
[259] "categorized"                "charted"                    "classified"                
[262] "compacted"                  "composed"                   "constituted"               
[265] "contracted"                 "contrived"                  "covenanted"                
[268] "designed"                   "devised"                    "disposed"                  
[271] "engaged"                    "figured"                    "fixed"                     
[274] "formal"                     "graded"                     "grouped"                   
[277] "habitual"                   "harmonious"                 "harmonized"                
[280] "in hand"                    "in the works"               "marshaled"                 
[283] "methodical"                 "methodized"                 "normal"                    
[286] "normalized"                 "on the agenda"              "on the anvil"              
[289] "on the calendar"            "on the carpet"              "on the docket"             
[292] "on the tapis"               "ordered"                    "orderly"                   
[295] "organized"                  "placed"                     "planned"                   
[298] "plotted"                    "projected"                  "promised"                  
[301] "ranged"                     "ranked"                     "rationalized"              
[304] "regular"                    "regularized"                "regulated"                 
[307] "routine"                    "routinized"                 "scheduled"                 
[310] "schematized"                "sealed"                     "set"                       
[313] "settled"                    "shaped"                     "signed"                    
[316] "sorted"                     "standardized"               "steady"                    
[319] "stipulated"                 "strategetic"                "strategic"                 
[322] "sur le tapis"               "symmetrical"                "synchronized"              
[325] "systematic"                 "systematized"               "tactical"                  
[328] "undertaken"                 "uniform"                    "usual"                     
[331] "well-ordered"               "well-regulated"             "worked out"                
[334] "aligned"                    "appareled"                  "armed"                     
[337] "arranged"                   "arrayed"                    "assorted"                  
[340] "attired"                    "battled"                    "bedecked"                  
[343] "breeched"                   "capped"                     "categorized"               
[346] "chausse"                    "clad"                       "classified"                
[349] "cloaked"                    "clothed"                    "coifed"                    
[352] "composed"                   "constituted"                "costumed"                  
[355] "decked"                     "deployed"                   "dight"                     
[358] "disguised"                  "disposed"                   "dressed"                   
[361] "embattled"                  "endued"                     "engaged"                   
[364] "fixed"                      "garbed"                     "garmented"                 
[367] "gowned"                     "graded"                     "grouped"                   
[370] "habilimented"               "habited"                    "harmonized"                
[373] "hooded"                     "in battle array"            "invested"                  
[376] "liveried"                   "mantled"                    "marshaled"                 
[379] "methodized"                 "normalized"                 "ordered"                   
[382] "orderly"                    "organized"                  "pantalooned"               
[385] "placed"                     "raimented"                  "ranged"                    
[388] "ranked"                     "regularized"                "regulated"                 
[391] "rigged out"                 "robed"                      "routinized"                
[394] "shod"                       "shoed"                      "sorted"                    
[397] "standardized"               "synchronized"               "systematized"              
[400] "tired"                      "togged"                     "tricked out"               
[403] "trousered"                  "vested"                     "vestmented"                
[406] "adapted"                    "aligned"                    "arranged"                  
[409] "arrayed"                    "associated"                 "assorted"                  
[412] "at odds"                    "at variance"                "bracketed"                 
[415] "cataloged"                  "categorized"                "chosen"                    
[418] "classified"                 "composed"                   "conformable"               
[421] "conglomerate"               "constituted"                "contrary"                  
[424] "contrasted"                 "contrasting"                "coupled"                   
[427] "departing"                  "deviating"                  "deviative"                 
[430] "different"                  "differentiated"             "differing"                 
[433] "disaccordant"               "disagreeing"                "discordant"                
[436] "discrepant"                 "discrete"                   "discriminated"             
[439] "disjoined"                  "disparate"                  "disposed"                  
[442] "dissimilar"                 "dissonant"                  "distinct"                  
[445] "distinguished"              "divergent"                  "diverging"                 
[448] "divers"                     "diverse"                    "diversified"               
[451] "filed"                      "fitted"                     "fixed"                     
[454] "graded"                     "grouped"                    "harmonized"                
[457] "heterogeneous"              "hierarchic"                 "in disagreement"           
[460] "inaccordant"                "incompatible"               "incongruous"               
[463] "inconsistent"               "inconsonant"                "indexed"                   
[466] "indiscriminate"             "inharmonious"               "irreconcilable"            
[469] "linked"                     "many"                       "many and various"          
[472] "marshaled"                  "matched"                    "methodized"                
[475] "mixed"                      "motley"                     "multifarious"              
[478] "normalized"                 "of all sorts"               "on file"                   
[481] "ordered"                    "orderly"                    "organized"                 
[484] "picked"                     "pigeonholed"                "placed"                    
[487] "poles apart"                "poles asunder"              "preferred"                 
[490] "promiscuous"                "pyramidal"                  "ranged"                    
[493] "ranked"                     "rated"                      "regularized"               
[496] "regulated"                  "routinized"                 "selected"                  
[499] "separate"                   "separated"                  "several"                   
[502] "sorted"                     "standardized"               "stratified"                
[505] "suited"                     "sundry"                     "synchronized"              
[508] "systematized"               "tabular"                    "unconformable"             
[511] "unequal"                    "unlike"                     "variant"                   
[514] "varied"                     "variegated"                 "various"                   
[517] "varying"                    "widely apart"               "worlds apart"              
[520] "arcane"                     "cabalistic"                 "censored"                  
[523] "classified"                 "close"                      "closed"                    
[526] "concealed"                  "cryptic"                    "dark"                      
[529] "enigmatic"                  "esoteric"                   "hermetic"                  
[532] "hidden"                     "hush-hush"                  "latent"                    
[535] "mysterious"                 "occult"                     "restricted"                
[538] "secret"                     "smothered"                  "stifled"                   
[541] "suppressed"                 "top secret"                 "ulterior"                  
[544] "unbreatheable"              "under security"             "under wraps"               
[547] "undisclosable"              "undisclosed"                "undivulgable"              
[550] "undivulged"                 "unrevealable"               "unrevealed"                
[553] "unspoken"                   "untellable"                 "untold"                    
[556] "unutterable"                "unuttered"                  "unwhisperable"             
[559] "aligned"                    "arcane"                     "arranged"                  
[562] "arrayed"                    "assorted"                   "cabalistic"                
[565] "cataloged"                  "categorized"                "censored"                  
[568] "classified"                 "close"                      "closed"                    
[571] "composed"                   "concealed"                  "constituted"               
[574] "cryptic"                    "dark"                       "disposed"                  
[577] "enigmatic"                  "esoteric"                   "filed"                     
[580] "fixed"                      "graded"                     "grouped"                   
[583] "harmonized"                 "hermetic"                   "hidden"                    
[586] "hierarchic"                 "hush-hush"                  "indexed"                   
[589] "latent"                     "marshaled"                  "methodized"                
[592] "mysterious"                 "normalized"                 "occult"                    
[595] "on file"                    "ordered"                    "orderly"                   
[598] "organized"                  "pigeonholed"                "placed"                    
[601] "pyramidal"                  "ranged"                     "ranked"                    
[604] "rated"                      "regularized"                "regulated"                 
[607] "restricted"                 "routinized"                 "secret"                    
[610] "smothered"                  "sorted"                     "standardized"              
[613] "stifled"                    "stratified"                 "suppressed"                
[616] "synchronized"               "systematized"               "tabular"                   
[619] "top secret"                 "ulterior"                   "unbreatheable"             
[622] "under security"             "under wraps"                "undisclosable"             
[625] "undisclosed"                "undivulgable"               "undivulged"                
[628] "unrevealable"               "unrevealed"                 "unspoken"                  
[631] "untellable"                 "untold"                     "unutterable"               
[634] "unuttered"                  "unwhisperable"              "abandonment"               
[637] "abbreviated"                "abort"                      "about"                     
[640] "about to be"                "abridged"                   "abstruse"                  
[643] "accented"                   "accommodate"                "accost"                    
[646] "accurate"                   "adjacent"                   "adjust"                    
[649] "advance"                    "agree"                      "airless"                   
[652] "airtight"                   "alert"                      "all but"                   
[655] "alley"                      "alleyway"                   "almost"                    
[658] "along toward"               "alongside"                  "already in sight"          
[661] "alveolar"                   "angustifoliate"             "angustirostrate"           
[664] "angustisellate"             "angustiseptal"              "apical"                    
[667] "apico-alveolar"             "apico-dental"               "aposiopestic"              
[670] "approach"                   "approaching"                "appropinquate"             
[673] "approximate"                "approximately"              "approximating"             
[676] "approximative"              "arcane"                     "arena"                     
[679] "arm-in-arm"                 "around"                     "arrange"                   
[682] "arterial"                   "arterial highway"           "arterial street"           
[685] "artery"                     "articulated"                "as good as"                
[688] "assiduous"                  "assimilated"                "at close quarters"         
[691] "at hand"                    "attached"                   "attentive"                 
[694] "autistic"                   "Autobahn"                   "autoroute"                 
[697] "autostrada"                 "avaricious"                 "avenue"                    
[700] "back"                       "bang"                       "bar"                       
[703] "barricade"                  "barytone"                   "bashful"                   
[706] "batten"                     "batten down"                "battle"                    
[709] "bear down on"               "bear down upon"             "bear up"                   
[712] "beclouded"                  "belt highway"               "bilabial"                  
[715] "blind"                      "blind alley"                "block"                     
[718] "block up"                   "blockade"                   "bolt"                      
[721] "bonded"                     "boulevard"                  "box"                       
[724] "brawl"                      "breakoff"                   "breathless"                
[727] "breezeless"                 "brewing"                    "brief"                     
[730] "bristling"                  "broad"                      "broil"                     
[733] "brusque"                    "buddy-buddy"                "buried"                    
[736] "burning"                    "button"                     "button up"                 
[739] "bypass"                     "byway"                      "cabalistic"                
[742] "cacuminal"                  "camino real"                "careful"                   
[745] "carriageway"                "causeway"                   "causey"                    
[748] "cease"                      "ceasing"                    "cemented"                  
[751] "censored"                   "center"                     "central"                   
[754] "centralize"                 "cerebral"                   "cessation"                 
[757] "chaussee"                   "cheap"                      "checked"                   
[760] "cheek-by-jowl"              "chock"                      "choke"                     
[763] "choke off"                  "cincture"                   "circumferential"           
[766] "circumscribed"              "clap"                       "clash"                     
[769] "classified"                 "climax"                     "clinch"                    
[772] "clipped"                    "clos"                       "close"                     
[775] "close about"                "close at hand"              "close by"                  
[778] "close down"                 "close in"                   "close off"                 
[781] "close tight"                "close up"                   "close with"                
[784] "close-fitting"              "close-knit"                 "close-lipped"              
[787] "close-textured"             "close-tongued"              "close-woven"               
[790] "closed"                     "closefisted"                "closely"                   
[793] "closemouthed"               "closing"                    "clouded"                   
[796] "collide"                    "combat"                     "come"                      
[799] "come closer"                "come forward"               "come near"                 
[802] "come on"                    "come to blows"              "come together"             
[805] "come up"                    "coming"                     "compact"                   
[808] "compacted"                  "compactly"                  "comparable"                
[811] "compass"                    "compass about"              "compendious"               
[814] "complete"                   "completing"                 "completion"                
[817] "compose"                    "compressed"                 "concealed"                 
[820] "concenter"                  "concentralize"              "concentrate"               
[823] "concentrated"               "concise"                    "conclude"                  
[826] "conclusion"                 "concrete"                   "condensed"                 
[829] "confidential"               "confine"                    "confined"                  
[832] "confining"                  "confront"                   "congested"                 
[835] "connect"                    "conscientious"              "consolidated"              
[838] "consonant"                  "consonantal"                "constant"                  
[841] "constrict"                  "constricted"                "contain"                   
[844] "container"                  "contend"                    "contest"                   
[847] "continuant"                 "contract"                   "contracted"                
[850] "converge"                   "coop"                       "corduroy road"             
[853] "correct"                    "county road"                "court"                     
[856] "courtyard"                  "cover"                      "covered"                   
[859] "covert"                     "crammed"                    "crammed full"              
[862] "cramp"                      "cramped"                    "crawling"                  
[865] "crescent"                   "crisp"                      "critical"                  
[868] "croft"                      "crowd"                      "crowded"                   
[871] "cryptic"                    "cul-de-sac"                 "culmination"               
[874] "curt"                       "curtilage"                  "cut"                       
[877] "cut and thrust"             "dark"                       "dead-end street"           
[880] "debar"                      "decline"                    "delicate"                  
[883] "delimited field"            "demanding"                  "dense"                     
[886] "densely"                    "dental"                     "desinence"                 
[889] "desistance"                 "detailed"                   "determine"                 
[892] "devoted"                    "dike"                       "direct"                    
[895] "dirt road"                  "discontinuance"             "discontinuation"           
[898] "discontinue"                "discreet"                   "disregard"                 
[901] "dissimilated"               "dissociable"                "docked"                    
[904] "dog"                        "dorsal"                     "draw near"                 
[907] "draw nigh"                  "drive"                      "driveway"                  
[910] "duel"                       "dumb"                       "dustproof"                 
[913] "dusttight"                  "eclipsed"                   "economical of words"       
[916] "elliptic"                   "embay"                      "embosom"                   
[919] "embrace"                    "enclasp"                    "enclave"                   
[922] "enclose"                    "enclosure"                  "encompass"                 
[925] "encounter"                  "end"                        "ending"                    
[928] "enfold"                     "enigmatic"                  "envelop"                   
[931] "environ"                    "enwrap"                     "epigrammatic"              
[934] "esoteric"                   "establish"                  "evasive"                   
[937] "even"                       "exact"                      "exacting"                  
[940] "exchange blows"             "exigent"                    "express"                   
[943] "expressway"                 "exquisite"                  "faithful"                  
[946] "fall in with"               "familiar"                   "fast"                      
[949] "fast by"                    "fasten"                     "fastened"                  
[952] "fence"                      "feud"                       "field"                     
[955] "fight"                      "fight a duel"               "fine"                      
[958] "finical"                    "finicking"                  "finicky"                   
[961] "finish"                     "finish up"                  "finishing"                 
[964] "firm"                       "firmly"                     "fix"                       
[967] "fixed"                      "flat"                       "fold"                      
[970] "fold up"                    "forthcoming"                "forty"                     
[973] "freeway"                    "front"                      "full"                      
[976] "full development"           "funnel"                     "fussy"                     
[979] "fusty"                      "future"                     "gain upon"                 
[982] "gasproof"                   "gastight"                   "gathering"                 
[985] "give and take"              "give satisfaction"          "glide"                     
[988] "glossal"                    "glottal"                    "glued"                     
[991] "gluey"                      "gnomic"                     "go around"                 
[994] "go round"                   "going to happen"            "grapple"                   
[997] "grapple with"               "gravel road"                "ground"                    
[1000] "guarded"                   
[ reached getOption("max.print") -- omitted 8933 entries ]
> classify <- str_to_title(
        +         classify[str_detect(classify, "ing")|str_detect(classify, "ed")])
> # Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
        > Situations %>% 
        +              mutate(Classify = case_when(Verb %in% more_classify ~ "Classify",
                                                   +                                        TRUE ~ "Not classifying"),
                              +                     Agent = case_when(!is.na(Entity) ~ "Entity",
                                                                      +                                       !is.na(Technology) ~ "Technology",
                                                                      +                                       !is.na(Character) ~ "Character")) %>% 
        +              filter(Classify == "Classify") %>% 
        +              select(Verb) %>% 
        +              add_count(Verb) %>%
        +              distinct()
# A tibble: 3 × 2
Verb           n
<chr>      <int>
        1 consenting     2
2 calling        1
3 hypnotized     2
> # Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
        > Situations %>% 
        +              mutate(Classify = case_when(Verb %in% mclassify ~ "Classify",
                                                   +                                        TRUE ~ "Not classifying"),
                              +                     Agent = case_when(!is.na(Entity) ~ "Entity",
                                                                      +                                       !is.na(Technology) ~ "Technology",
                                                                      +                                       !is.na(Character) ~ "Character")) %>% 
        +              filter(Classify == "Classify") %>% 
        +              select(Verb) %>% 
        +              add_count(Verb) %>%
        +              distinct()
Error in `mutate_cols()`:
        ! Problem with `mutate()` column `Classify`.
ℹ `Classify = case_when(Verb %in% mclassify ~ "Classify", TRUE ~ "Not classifying")`.
x object 'mclassify' not found
Caused by error in `Verb %in% mclassify`:
        ! object 'mclassify' not found
Run `rlang::last_error()` to see where the error occurred.
> # Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
        > Situations %>% 
        +              mutate(Classify = case_when(Verb %in% classify ~ "Classify",
                                                   +                                        TRUE ~ "Not classifying"),
                              +                     Agent = case_when(!is.na(Entity) ~ "Entity",
                                                                      +                                       !is.na(Technology) ~ "Technology",
                                                                      +                                       !is.na(Character) ~ "Character")) %>% 
        +              filter(Classify == "Classify") %>% 
        +              select(Verb) %>% 
        +              add_count(Verb) %>%
        +              distinct()
# A tibble: 12 × 2
# Verb            n
# <chr>       <int>
#        1 Classifying   155
# 2 Classified     95
# 3 Scanning      217
# 4 Ordered         1
# 5 Sorting         9
# 6 Sorted          1
# 7 Ranking         2
# 8 Concealed       2
# 9 Reporting      18
# 10 Disposed        1
# 11 Rating          2
# 12 Censored        1

# Add a column in Situations that is 1 if the Verb is in the classify vector. 
Situations %>% 
        mutate(Classify = case_when(Verb %in% classify ~ "Classify",
                                  TRUE ~ "Not classify"),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        filter(Classify == "Classify") %>% 
        select(Genre, Verb, Entity, Technology, Agent) %>% 
        pivot_longer(!Verb, values_to = "value") %>%
        drop_na() %>% 
        ggplot(aes(x=value, fill=Verb)) +
        geom_bar(aes(y = ..count..))+
        theme(axis.line = element_line(colour = "darkblue", 
                                       size = 1, 
                                       linetype = "solid")) +
        theme_minimal() +
        labs(
                title ="Which agents fail in interactions with machine vision?",
                subtitle="") +
        theme(axis.text.x = element_blank()) +
        coord_flip() +
        facet_wrap(~name, scales="free_y") 

# But this is absolute count and it'd be more useful to see the proportions. 

Situations %>% 
        mutate(Classify = case_when(Verb %in% classify ~ "Classify",
                                    TRUE ~ "Not classify"),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        filter(Classify == "Classify") %>%
        filter(Verb != "Disposed" 
               & Verb !="Concealed"
               & Verb != "Reporting"
               & Verb != "Scanning") %>%  # manually removed cos seems wrong
        select(Verb, Entity, Technology, Agent, Genre) %>% 
        group_by(Agent, Verb) %>% 
        add_count(Verb, name = "Freq", sort = TRUE) %>% 
        add_count(Verb, name = "Freq", sort = TRUE) %>% 
        ggplot(aes(x = Verb, fill = Agent)) +
        geom_bar(position="stack") +
        labs(title = "Classified and Classifying and similar actions - actions shown by agent") +
        coord_flip() +
        facet_wrap(~Genre) 

