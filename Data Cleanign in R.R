library(ggplot2)
library(plotly)
library(readr)

schooldata <- read_csv("completelyfabricateddata.csv")

summary(schooldata)

table(schooldata$school_type)

schooldata$school_type <- tolower(schooldata$school_type)

schooldata$school_type <- ifelse(schooldata$school_type =="primary",
                                 "elementary", schooldata$school_type)

schooldata$school_type <- ifelse(schooldata$school_type == "graduate school",
                                 "college gr", schooldata$school_type)
schooldata$school_type <- ifelse(schooldata$school_type == "post bach",
                                 "college gr", schooldata$school_type)
schooldata$school_type <- ifelse(schooldata$school_type == "junior high",
                                 "middle school", schooldata$school_type)
table(schooldata$school_type)

ggplotly(ggplot(data=schooldata) +
           geom_boxplot(mapping=aes(y=schooldata$student_absences,
                                    x=schooldata$school_type) ))

absence_outliers <- boxplot.stats(schooldata$student_absences)$out         

schooldata <- schooldata[-which(schooldata$student_absences %in%
                                  c(absence_outliers)),]
# Reconstructed Boxplots, with the outliers removed
ggplotly(ggplot(data=schooldata) +
           geom_boxplot(mapping=aes(y=schooldata$student_absences,
                                    x=schooldata$school_type) ))

cor(schooldata[3:6], use="complete.obs")

hs <- schooldata[schooldata$school_type=="high school",]
cor(hs[3:6],use="complete.obs")

#Non Color-coded
ggplotly( ggplot(data=schooldata) +
            geom_point(mapping=aes(x=schooldata$student_absences,
                                   y=schooldata$student_gpa)))
#Color Coded
ggplotly(ggplot(data=schooldata) +
           geom_point(mapping=aes(x=schooldata$student_absences,
                                  y=schooldata$student_gpa,
                                  color=schooldata$school_type)))

schooldata[which(is.na(schooldata$student_gpa)),]

hs <- schooldata[schooldata$school_type=="high school",]
hsmodel <- lm(data=hs,student_gpa~student_absences)
summary(hsmodel)

hs$predicted_gpa <- predict(hsmodel,hs)
hs

hs[which(is.na(hs$student_gpa)),]
