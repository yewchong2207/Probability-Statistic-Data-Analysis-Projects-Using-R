
# PSDA Project1 

# Please run from above to bottom as there might have codes thatsome codes need is on above


# Gender specification ( Bar plot )


gender <- table(Dataset[,2])

barplot(gender,ylim = c(0,50),col =c("pink","light blue"),main="Gender Specification")




# Races (Pie chart )


race <- table(Dataset[,3])

print(race)

races <- c("Bumiputera sabah","Chinese","Indian","Malay")

number_of_particular_races <- c(1,46,8,14)

library(plotrix)

pie3D(number_of_particular_races,labels = races,main = "RACES",explode=0.1)




# Height Preference by Male ( Frequency Distribution )

library(dplyr)

male_data <- filter(Dataset,Gender=="Male")  # filter dataset to have only Male

stem(male_data$`Height preference of your ideal partner(In CM).`) # Height preference by Male

a1<-male_data$`Height preference of your ideal partner(In CM).`

mean(a1) # calculate the mean of the height prefered by Male ( used in report )




# Height Preference by Female ( Frequency Distribution )


female_data <- filter(Dataset,Gender=="Female") #filter dataser to have only Female 

stem(female_data$`Height preference of your ideal partner(In CM).`)

b1<-female_data$`Height preference of your ideal partner(In CM).` # Height preference by Female

mean(b1)#calcululatet the mean of the height prefered by Female( used in report )









# Average time (in hours) that you would spend with partner in a week. ( Box plot )

boxplot(Dataset$`Average time(in hours) that you would spend with your partner in a week.`,
        main = "Average time (in hours) that you would spend With  partner In A Week ",
        xlab = "Time Spent",col = "gold",border = "black",horizontal = TRUE, notch = TRUE )




# Age difference prefered by Female with their ideal partner ( Dot plot )


library(ggplot2)

female_plot<-ggplot(female_data, aes( x =`Age difference between yourself and ideal partner.`,
                                      fill = `Age preference of your ideal partner.` ))+
                    scale_y_continuous(NULL, breaks = NULL)+
                    geom_dotplot(binaxis = "x", stackgroups = TRUE, binwidth = 0.5,method = "histodot")+
                    theme(plot.background = element_rect(fill = "pink"))

print(female_plot + ggtitle("Age difference prefered by female with their ideal partner"))




# Age difference prefered by Male with their ideal partner ( Dot plot )


male_plot<-ggplot(male_data, aes( x =`Age difference between yourself and ideal partner.`,
                                  fill = `Age preference of your ideal partner.` ))+

scale_y_continuous(NULL, breaks = NULL)+

geom_dotplot(binaxis = "x", stackgroups = TRUE, binwidth = 0.5, method = "histodot") +

theme(plot.background = element_rect(fill = "lightblue"))

print(male_plot + ggtitle("Age difference prefered by male with their ideal partner"))




# minimum monthly salary accepted  by Male ( Histogram )


male1 <- table(male_data[,17])

print(male1)

v<- c(1100,1100,1200,1200,1500,1500,1700,2000,2300,2300,2500,
      2500,2500,2500,2800,2900,3000,3000,3200,3200,3500,3500,
      3500,3600,3600,3600,3900,4500,4500,4500,4800,5900,8000,
      9500,11500,12000)

print(v)

hist(v,main= "Histogram of Minimumum accepted monthly salary(RM) by Male",
     xlab = "Minimum accepted momthly salary(RM)",col = "yellow",
     border = "orange", xlim = c(0,12000), ylim = c(0,20),breaks = 5)




# minimum monthly salary accepted  by Female ( Histogram )


female1 <- table(female_data[,17])

print(female1)

b <-c(1800,2100,2100,2400,2500,2500,2500,2500,2800,2900,3100,3200,
     3500,3750,3800,4500,4500,4500,4500,4500,4500,4600,4850,4900,
     6500,6500,6700,7500,7500,8900,9500,10000,12000)

print(b)

hist(b,main= "Histogram of Minimumum accepted monthly salary(RM) by Female" ,
     xlab = "Minimum accepted momthly salary(RM)",col = "pink",
     border = "red", xlim = c(0,12000), ylim = c(0,20),breaks = 5)



# showing data of mimimum monthly accepted by Female in presentation


test1 <- data.frame(Minimum_monthly_salary = c( "0 - 2000","2000 - 4000",
                                                "4000 - 6000","6000 - 8000",
                                                "8000 - 10000" ,
                                                "10000 - 12000" ),
                    Frequency = c(1,14,9,5,3,1))


print(test1)  




# showing data of mimimum monthly accepted by Female in presentation


test2 <- data.frame(Minimum_monthly_salary = c( "0 - 2000","2000 - 4000",
                                                "4000 - 6000", "6000 - 8000",
                                                "8000 - 10000" ,
                                                "10000 - 12000" ),
                    Frequency = c(8,19,5,1,1,2))


print(test2)




# Previous relationship vs Average time spent willigly spent with partner(Scatter plot)


relationship <- Dataset$`Number of previous relationship.`

ave_time <- Dataset$`Average time(in hours) that you would spend with your partner in a week.`


plot(relationship, ave_time,
     main = "Previous relationship vs Average time willingly to spent(hours)",
     xlab = "The number of previous relationship",
     ylab = "Average time willingly spent(hours) with future partner in a week ", 
     las =1, pch = 19 , col = "purple", frame = FALSE)




# Belief in Love ( Male ) ( Pie chart )


male_belief_data <- table(male_data[,22])


print(male_belief_data)

labelling <- c("Maybe", "Yes","No")

male_belief_value <- c(10.00,22.00,4.00)

male_belief_value1<- male_belief_value

per <- round(male_belief_value/sum(male_belief_value)*100)

male_belief_value1 <- paste(male_belief_value1," (", per)

male_belief_value1<- paste(male_belief_value1,"%",sep=""," )")


pie(male_belief_value,male_belief_value1,main="Belief in love ( Male )",
    col=rainbow(length(male_belief_value)))

legend("topleft",legend<- labelling,fill = rainbow(length(male_belief_value)))




# Belief in Love ( Female ) ( Pie chart )


female_belief_data <- table(female_data[,22])

print(female_belief_data)

female_belief_value <-c (13.00,17.00,3.00)

female_belief_value1<- female_belief_value

per <- round(female_belief_value/sum(female_belief_value)*100)

female_belief_value1 <- paste(female_belief_value1," (",per)

female_belief_value1<- paste(female_belief_value1,"%",sep=""," )")

pie(female_belief_value,female_belief_value1,main="Belief in love ( Female )",
    col=rainbow(length(female_belief_value)))

legend("topleft",legend<- labelling,fill = rainbow(length(female_belief_value)))




#Does appearance matter ?  ( Male ) ( Pie chart )


male2 <- table(male_data[,11])

print(male2)

male2_value <- c(20.00,11.00,5.00)


male2_value1 <- male2_value

per <- round(male2_value/sum(male2_value)*100)

male2_value1 <- paste(male2_value1," (", per)

male2_value1<- paste(male2_value1,"%",sep=""," )")


pie(male2_value,male2_value1,main="Does appearance matter (Male) ",
    col=rainbow(length(male2_value)))

legend("topleft",legend<- labelling,fill = rainbow(length(male2_value)))




#Does appearance matter ?  ( Female ) ( Pie chart )


female2 <- table(female_data[,11])

print(female2)

female2_value <- c(17.00,10.00,6.00)

female2_value1 <- female2_value

per <- round(female2_value/sum(female2_value)*100)

female2_value1 <- paste(female2_value1," (", per)

female2_value1<- paste(female2_value1,"%",sep=""," )")


pie(female2_value,female2_value1,main="Does appearance matter (Female) ",
    col=rainbow(length(female2_value)))

legend("topleft",legend<- labelling,fill = rainbow(length(female2_value)))




# intelligence ( Frequency distribution )


library(expss)

intell = apply_labels(Dataset,
                      `The intelligence of your ideal partner.` = "Intelligence of ideal partner",
                       Gender = "Gender")

cro(intell$Gender, intell$`The intelligence of your ideal partner.`)


# Race Preferences ( Frequency Distribution )

intelli = apply_labels( Dataset, 
                        `Preference race of your ideal partner.` = "Preference race of ideal partner",
                        Gender = "Gender")

cro(intelli$Gender, intelli$`Preference race of your ideal partner.`)




#Rating Characterististic ( Male ) ( Frequency Distribution )


a1<-table(male_data[,12])
a2<-table(male_data[,13])
a3<-table(male_data[,14])
a4<-table(male_data[,15])
a5<-table(male_data[,16])

print(a1)
print(a2)
print(a3)
print(a4)
print(a5)

char1 <- data.frame(Characteristics = c("Humor","Intelligence","Caring","Romance","Supportive"),
                    Strongly_Disagree = c(0,0,0,2,1),Disagree=c(0,0,1,1,0), 
                    Neutral=c(14,13,4,11,4), Agree=c(11,16,9,13,9),Strongly_Agree= c(11,7,22,9,22))

print(char1)




#Rating Characterististic ( Female ) ( Frequency Distribution )

b1<-table(female_data[,12])
b2<-table(female_data[,13])
b3<-table(female_data[,14])
b4<-table(female_data[,15])
b5<-table(female_data[,16])

print(b1)
print(b2)
print(b3)
print(b4)
print(b5)

char2 <- data.frame(Characteristics = c("Humor","Intelligence","Caring","Romance","Supportive"),
                    Strongly_Disagree = c(1,2,2,1,2),Disagree=c(0,0,0,0,0), Neutral=c(5,5,0,14,1)
                    , Agree=c(12,9,5,6,5),Strongly_Agree= c(15,17,26,12,25))

print(char2)




# Physical Appearances prefered by Male and Female (Barplot)


g1<-table(male_data[,10])

print(g1)

y1<-table(female_data[,10])

print(y1)

golden <- data.frame(Gender=rep(c("Male", "Female"), each=5),
                     Physical_preference =rep(c("Athelectic", "Chubby", "Moderate","Muscular","Slim"),2),
                     Frequency=c(2,2,24,1,7,7,0,25,1,0))


ggbarplot(golden, "Physical_preference", "Frequency",
          fill = "Gender", color = "Gender", palette = "Paired",
          label = TRUE, title="Physical Preference of Male and Female"
          ,position = position_dodge(0.9))

