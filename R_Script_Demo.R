print("Started Validating CSV...")
sony <- read.csv("/home/webonise/R_Data/Sony_Seg_2017_US_Jan_19.csv", header=TRUE)
demoNames <- read.csv("/home/webonise/R_Data/demo_names.csv", header = FALSE)
demoRecords <- read.csv("/home/webonise/R_Data/demo_names_record.csv", header = FALSE)

demoNameRows <- demoNames[['V1']]
demoRecordRows <- demoRecords[['V1']]

newBNnames <- data.frame()
missinNPnames <- data.frame()
missinRnames <- data.frame()
wrongValues <- data.frame()

new_list <- list()

for(j in levels(demoNameRows))
{
  #&& !(j %in% c('Gender','State','WEIGHT'))  
  if(!(j %in% names(sony)) && !(j %in% c('Gender_Gender','State_State')))
  {
    missinNP <- data.frame(MissingNP_Names = j)
    missinNPnames <- rbind(missinNPnames,missinNP)
  }
}
for(k in levels(demoRecordRows))
{
  #&& !(j %in% c('Gender','State','WEIGHT'))  
  if(!(k %in% names(sony)) && !(k %in% c('Gender_Gender','State_State')))
  {
    missinR <- data.frame(MissingR_Names = k)
    missinRnames <- rbind(missinRnames,missinR)
  }
}


for(i in names(sony))
{  
      sonyFactor <- sony[[i]]
      bundle <- substring(i, 0, regexpr("_",i)-1)
      name <- substring(i, regexpr("_",i)+1)
      if(!(i %in% demonames[['V1']]) && !(i %in% c('Gender','State','WEIGHT')))
      {
        newBN <- data.frame(New_Names = i)
        newBNnames <- rbind(newBNnames,newBN)
      }
      else if(bundle %in% c('PrimetimeTV','Fitness','Social','Soda','Music','Mobile','Toy','Finance','Speakers','SpanishTV','Like','Car','TV','MusicWeb',
                            'MiscSubscription','Beer','Newspapers','FreeStreaming','Electronics','Luxury','Magazines','Clothing','Food','PersonalCare','Airline',
                            'ActiveWear','MusicTV','MusicDiscovery','PremiumTV','PaidStreaming','CableTV','Restaurant','MobileApps','ThemeParks','MobileMusic',
                            'Web','Retailer','DaytimeTV','Beverage','Provider','LateTV','Speakers/Headphones','MusicPubs','Blogs','Online','BigFan','Hotel','Cruise',
                            'Alcohol','Concerts','Sports','Awards','Live','Genre','KidsHousehold','ArtistMerch','Purchased','Consumption','Mood','Hobbies',
                            'Bonus','Devices','SocialContent','SocialActivity','SocialAccount','PurchaseInfluence','MusicSitutation','Movies','Movie','Misc',
                            'FavArtist','ArtistsMerch','Values'))
      {
            boolFactor <- c("True","False"," ","TRUE","FALSE")
            if(!all(levels(sony[[i]]) %in% boolFactor))
            {
              new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],boolFactor))])
            }  
      }
      else if(i == "Gender")
      {
         genderFactor <- sony[[i]]
         checkGenderFactor <- c("Male","Female")
         if(!all(levels(genderFactor) %in% checkGenderFactor))
         {
            new_list[[paste0("",i,sep="")]] = as.character(genderFactor[is.na(match(genderFactor,checkGenderFactor))])
            #new_list[[paste0("",i,sep="")]] = genderFactor[is.na(match(genderFactor,checkGenderFactor))]
         }
      }
      else if(i == "State")
      {
        stateFactor <- sony[[i]]
        checkStateFactor <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida",
                              "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
                              "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
                              "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
                              "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas",
                              "Utah", "Vermont", "Virginia", "Washington", "Washington, D.C", "West Virginia", "Wisconsin", "Wyoming")
        if(!all(levels(stateFactor) %in% checkStateFactor))
        {
          new_list[[paste0("",i,sep="")]] = as.character(stateFactor[is.na(match(stateFactor,checkStateFactor))])
          #new_list[[paste0("",i,sep="")]] = stateFactor[is.na(match(stateFactor,checkStateFactor))]
        }
      }
      else if(i == "Single_Age")
      {
        if(class(sony[[i]]) != "integer")
        {
          new_list[[paste0("",i,sep="")]] = i
        }
      }
      
      else if(bundle == "Household")
      {
            if(name == "Income")
            {
              incomeFactor <- sony[[i]]
              checkIncomeFactor <- c("Don't know","Between $75,000 and $99,000","Prefer not to say","Between $100,000 and $250,000",
                                     "Less than $25,000","Between $25,000 and $44,000","Between $45,000 and $74,000","More than $250,000")
              if(!all(levels(incomeFactor) %in% checkIncomeFactor))
              {
                new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkIncomeFactor))])
              }
            }
        else if(name == "Makeup")
        {
            makeupFactor <- sony[[i]]
            checkMakeupFactor <- c("I live with friend(s) / roommate(s)", "I live with my partner, but our kid(s) have left home", 
                                   "I live with my parent(s)", "I live in a multi-generational household", "Other", 
                                   "I live with my partner, but we don't have any kids", "I live with my partner and our kid(s)", 
                                   "I am a single parent, living with my kid(s)", "I live alone")
            if(!all(levels(makeupFactor) %in% checkMakeupFactor))
            {
              new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkMakeupFactor))])
            }
        }
        else
        {
          otherFactor <- sony[[i]]
          boolFactor <- c('True','False')
          if(!all(levels(otherFactor) %in% checkOtherFactor))
          {
              new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],boolFactor))])
          }
        }
      }
      else if(i == 'WEIGHT')
      {
        if(class(sony[[i]]) != "numeric")
        {
            new_list[[paste0("",i,sep="")]] = i
        }
      }
      #FECI Starts
      else if(bundle == 'FECI')
      {
        if(name == "Score")
        {
          if(class(sony[[i]]) != "integer")
          {
            new_list[[paste0("",i,sep="")]] = i
          }
        }
        else if(name == "GROUP" )
        {
          groupFactor <- sony[[i]]
          checkGenderFactor <- c('Casual', 'Enthusiast', 'Fanatic', 'Indifferent')
          if(!all(levels(groupFactor) %in% checkGenderFactor))
          {
            new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkGenderFactor))])
          }
        }
        else if(name == "GROUP_AGE")
        {
          groupageFactor <- sony[[i]]
          checkGroupAgeFactor <- c("Casuals 13-17","Casuals 18-25","Casuals 26-34","Casuals 35-49","Casuals 50+",
                                   "Enthusiasts 13-17","Enthusiasts 18-21","Enthusiasts 22-29","Enthusiasts 30-39",
                                   "Enthusiasts 40-49","Enthusiasts 50+","Fanatics 13-17","Fanatics 18-25","Fanatics 26-34",
                                   "Fanatics 35-44","Fanatics 45-54","Fanatics 55+","Indifferents 13-25","Indifferents 26-39",
                                   "Indifferents 40-49","Indifferents 50+")
          if(!all(levels(groupageFactor) %in% checkGroupAgeFactor))
          {
            new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkGroupAgeFactor))])
          }
        }
        else if(name == "GROUP_FINAL")
        {
          groupfinalFactor <- sony[[i]]
          checkGroupFinalFactor <- c("0","13 to 21 Savage","Age Against the Machine","Beat Ballers","Beer Bands & BBQ",
                                     "Broing Up","Classic Reels & All The Feels","Cosmo Cuties","Curator Raider",
                                     "Earth, Wind & Retired","Electric Youth","Executive Rockers","Frat Pack",
                                     "Golden Ears","Grammy Grannies","Happy Hour Hustlers","Highschool Hype Beast",
                                     "Kids Before Kicks","Martha Stewart Moms","Maternal Madonnas","Notorious M.O.M.",
                                     "Par 4 Fathers","Passion Pitters","Poetic Pop Princesses","Punk Posse","Rambling Man",
                                     "Rebelution","Rhythm & Babies","Rock & Role Model","Set in the (Family) Stone",
                                     "Silver Lining's Playlist","Still Foo Fighting","Stressed for Success","Stressed Songstresses",
                                     "Tastemakers Mark","Trendy & Trending","Work then Twerk")
          if(!all(levels(groupfinalFactor) %in% checkGroupFinalFactor))
          {
            new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkGroupFinalFactor))])
          }
        }
        else if(name == "COUNTRY_FINAL")
        {
          countryfinalFactor <- sony[[i]]
          checkCountryFianlFactor <- c("0","Apps & Naps","Ballet, Cabernet & Lady A","Broadway Bros","Bro-Mantics","Country Segment 1",
                                       "Country Segment 11","Country Segment 12","Country Segment 13","Country Segment 19","Country Segment 2",
                                       "Country Segment 25","Country Segment 3","Country Segment 4","Country Segment 5","Country Segment 6",
                                       "Cowboys with Calculators","Front Porch Rockers","George Strait Shooters","Golden Girls","Grey Goose, Gadgets & Guitars",
                                       "Guitars, Bars & Car Seats","Hell-Raising Homemakers","If It Ain't Broke","Lil Show Bros","Modern Day Marthas",
                                       "Mommy Maxinistas","Music & Moonshine","Now That's What I Call Dad","Pinning & Pampers","Red Tech Crazy",
                                       "Tailgate Princesses","Vinyl & Viagra")
          if(!all(levels(countryfinalFactor) %in% checkCountryFianlFactor))
          {
            new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkCountryFianlFactor))])
          }
        }
        else if(name == "HISPANIC_FINAL")
        {
          hispanicfinalFactor <- sony[[i]]
          checkHispanicFianlFactor <- c("0","A Band of B*tches","Aventurera","Bachata Margarita","Bambinos y Beisbol","Blazers & Mimosas","Boombox Boomers",
                                        "Born to Tango","Carpe PM","Cerveza, Soccer & Santana","Fiesta Fashionista","Golfing, Gaming & Gastando",
                                        "Grand Theft Audio","Hard Rocking Dads","Hispanic Segment 1","Hispanic Segment 10","Hispanic Segment 2",
                                        "Hispanic Segment 3","Hispanic Segment 4","Hispanic Segment 5","Hispanic Segment 6","!Homework & Twerk!",
                                        "Joysticks & Chicks","Moms en Casa","Mujeres Comsmos","Oye Como Van","Pacifiers + Pinterest","Psalms & Slots",
                                        "Quiero Mi MTV","Retired Frat Stars","Rock & Roll Riders","Smartinas","Super Mama")
          if(!all(levels(hispanicfinalFactor) %in% checkHispanicFianlFactor))
          {
            new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkHispanicFianlFactor))])
          }
        }
      } #FECI ends
      else if(i == 'Sexual_Orientation')
      {
        sexualFactor <- sony[[i]]
        checkSexualFactor <- c("Gay/Lesbian", "None of the above", "Transsexual/Intersex", "Transgender", "Bisexual", "Heterosexual/Straight", "Prefer not to answer")
        if(!all(levels(sexualFactor) %in% checkSexualFactor))
        {
          new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkSexualFactor))])
        }
      }
      else if(i == 'Latino_Filter')
      {
        latinoFactor <- factor(sony[[i]])
        checkLatinoFactor <- c(0,1)
        if(!all(levels(latinoFactor) %in% checkLatinoFactor))
        {
          new_list[[paste0("",i,sep="")]] = as.character(latinoFactor[is.na(match(latinoFactor,checkLatinoFactor))])
        }
      }
      else if(i == 'Country_Filter')
      {
        countryFactor <- sony[[i]]
        checkCountryFactor <- c("Selected","Not Selected")
        if(!all(levels(countryFactor) %in% checkCountryFactor))
        {
             new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkCountryFactor))])
        }
      }
      else if(bundle == 'Latin')
      {
        if(name == 'Ethnicity')
        {
          if(class(sony[[i]]) != "integer")
          {
            new_list[[paste0("",i,sep="")]] = i
          }
        }
        else
        {
          latinFactor <- sony[[i]]
          boolFactor <- c("True","False"," ","TRUE","FALSE")
          if(!all(levels(latinFactor) %in% boolFactor))
          {
            new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],boolFactor))])
          }
        }
      }
      #ends Latin
      else if(bundle == "Race")
      {
        raceFactor <- sony[[i]]
        if(name ==  "Detailed")
        {
          checkDetailFactor <- c("Black Hispanic","Asian Hispanic","Other Non-Hispanic","Other Hispanic","White Non-Hispanic",
                                 "Multi Racial Non-Hispanic","White Hispanic","Asian Non-Hispanic","Black Non-Hispanic","Hispanic Only")
          if(!all(levels(raceFactor) %in% checkDetailFactor))
          {
            new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkDetailFactor))])
          }
          
        }
        else if(name == "Simple")
        {
          checkSimpleFactor <- c("Black Non Hispanic","Asian Non Hispanic","Hispanic","White Non Hispanic","Other Non Hispanic","Multi Racial Non Hispanic")
          if(!all(levels(raceFactor) %in% checkSimpleFactor))
          {
             new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkSimpleFactor))])
          }
        }
        else
        {
          boolFactor <- c("True","False"," ","TRUE","FALSE")
          if(!all(levels(raceFactor) %in% boolFactor))
          {
            new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],boolFactor))])
          }
        }
      }
      #end Race
      else if(i == 'Occupation_Status')
      {
        sonyFacotr <- sony[[i]]
        checkOccupFactor <- c("Currently looking for employment","Retired","Working part time","Part time student","Full time student",
                              "Other","Prefer not to answer","Working full time")
        if(!all(levels(sonyFacotr) %in% checkOccupFactor))
        {
          new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkOccupFactor))])
        }
      }
      else if(bundle == 'Age')
      {
        sonyFacotr <- sony[[i]]
        checkCohorts <- c("60+", "55-59", "26-29", "22-25", "45-49", "40-44", "50-54", "35-39", "18-21", "13-17", "30-34")
        if(name == 'Cohorts')
        {
          if(!all(levels(sonyFacotr) %in% checkCohorts))
          {
            new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkCohorts))])
          }
        }
        else if(name == 'by_Gender')
        {
          checkByGenderFactor <- c("Females 18-21","Males 30-39","Females 60+","Females 40-49","Females 22-29","Females 50-59","Females 30-39",
                                   "Males 18-21","Males 40-49","Males 60+","Males 13-17","Males 22-29","Females 13-17","Males 50-59")
          if(!all(levels(sonyFacotr) %in% checkByGenderFactor))
          {
            new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkByGenderFactor))])
          }
        }
      }
      #ends Age
      else if(i == 'Political_Affiliation')
      {
        sonyFacotr <- sony[[i]]
        checkPolitcalFactor <- c("None of the above", "Libertarian", "Constitution", "Other", "Republican", "Democratic", "Independent", 
                                 "Prefer not to answer", "Green", "Socialist")
        if(!all(levels(sonyFacotr) %in% checkPolitcalFactor))
        {
          new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkPolitcalFactor))])
        }
      }
      else if(bundle == 'Tactic')
      {
        sonyFactor <- sony[[i]]
        checkTacticFactor <- c('Yes, I like it','Neither like nor dislike','No, I dislike it')
        if(!all(levels(sonyFactor) %in% checkTacticFactor))
        {
          new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkTacticFactor))])
        }
      }
      else if(bundle == 'Attitude')
      {
        sonyFactor <- sony[[i]]
        checkAttitudeFactor <- c("Strongly Disagree","Tend to Agree","Tend to Disagree","Neither Agree Nor Disagree","Strongly Agree")
        if(!all(levels(sonyFactor) %in% checkAttitudeFactor))
        {
          new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkAttitudeFactor))])
        }
      }
      else if(i == 'Living_In_US')
      {
        sonyFactor <- sony[[i]]
        checkLivingFactor <- c(" ","Born here","Less than 1 year","10+ years","1 to 5 years","5 to 10 years","Prefer not to answer")
        if(!all(levels(sonyFactor) %in% checkLivingFactor))
        {
          new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkLivingFactor))])
        }
      }
      else if(bundle == 'Primary')
      {
        sonyFactor <- sony[[i]]
        boolFactor <- c("True","False"," ","TRUE","FALSE")
        if(name == 'Language')
        {
          checkPrimaryFactor <- c('English', 'Spanish', 'Other')
          if(!all(levels(sony[[i]]) %in% checkPrimaryFactor))
          {
            new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],checkPrimaryFactor))])
          }
        }
        else
        {
                if(!all(levels(sony[[i]]) %in% boolFactor))
                {
                  new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],boolFactor))])
                }
        } 
        
      }#Ends Primary
      else if(bundle == 'Hours')
      {
          checkHoursFactor <- c('NEVER DO THIS')
          if(!(class(sony[[i]]) == 'numeric' | class(sony[[i]]) == 'integer') | !(all(levels(sony[[i]]) %in% checkHoursFactor)))
          {
            new_list[[paste0("",i,sep="")]] = i
          }
      }
      else if(bundle == 'VideoContent')
      {
        boolFactor <- c("True","False"," ","TRUE","FALSE")
        if(!all(levels(sony[[i]]) %in% boolFactor))
        {
          new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],boolFactor))])
        }
      }
      else if(bundle == 'StreamingDiscovery')
      {
        boolFactor <- c("True","False"," ","TRUE","FALSE")
        if(!all(levels(sony[[i]]) %in% boolFactor))
        {
          new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],boolFactor))])
        }  
      }
      else if(bundle == 'Stream')
      {
           streamFactor <- c("NONE OF THE ABOVE","I ONLY BUY/DOWNLOAD music (e.g., CDs/MP3s on ITunes) and NEVER STREAM",
                          "I ONLY STREAM music (e.g., Spotify, Apple Music, YouTube) and NEVER BUY","I MAINLY BUY/DOWNLOAD music, but sometimes stream",
                          "I MAINLY STREAM music, but sometimes buy","I regularly BOTH STREAM AND BUY music")
            if(!all(levels(sony[[i]]) %in% streamFactor))
            {
              new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],streamFactor))])
            }
      }
      else if(bundle == 'Number')
      {
        numberFactor <- c("3-5 movies","More than 15 movies","I haven't watched any movies in a theatre this past year",
                          "6-8 movies","9-12 movies","Less than 3","13-15 movies")
        if(!all(levels(sony[[i]]) %in% numberFactor))
        {
          new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],numberFactor))])
        }
      }
      else if(bundle == 'Spend')
      {
        spendFactor <- c("$1 to $10", "$1001+", "$0", "$51 to $100", "$11 to $20", "$21 to $50", "$251 to $500", "$501 to $1000", "Don't Know", "$101 to $250")
        if(!all(levels(sony[[i]]) %in% spendFactor))
        {
          new_list[[paste0("",i,sep="")]] = as.character(sony[[i]][is.na(match(sony[[i]],spendFactor))])
        }
      }
}
new_list

max.length <- max(sapply(new_list, length))
new_list <- lapply(new_list, function(v) { c(v, rep("", max.length-length(v)))})
## Rbind
#newdate <- do.call(rbind, new_list)
newdata1 <- rbind(wrongValues,new_list)


write.csv(newdata1, file = "/home/webonise/R_Data/R2/wrongValues1.csv")  # csv for wrong value in the column
# New Bundles CSV is being generated...
write.csv(newBNnames, file = "/home/webonise/R_Data/R2/newBNnames1.csv")   # csv for New Bundles and Names
# First Missing Bundles CSV is being generated...
write.csv(missinNPnames, file = "/home/webonise/R_Data/R2/missinNPnames1.csv")  # csv for Missing bundle with reference of NameProperty Table
# Second Missing Bundles CSV is being generated...
write.csv(missinRnames, file = "/home/webonise/R_Data/R2/missinRnames1.csv")   # csv for Missing bundle with reference of current record Table

# newBNnames <- data.frame()
# missinNPnames <- data.frame()
# missinRnames <- data.frame()
# wrongValues <- data.frame()


























