library(readxl)
rm(list = ls())

###simplified data for those participants who participated in the game play and the survey ###

knowledge_s5 <- read_excel("Datafest/knowledge_s5.xlsx")
Refusal_s5 <- read_excel("Datafest/Refusal_s5.xlsx")

View(knowledge_s5)
View(Refusal_s5)



knowledge_s5_score = knowledge_s5$`Avg. S5 mean`
len = length(knowledge_s5_score)

knowledge_top_half_s5_score = knowledge_s5_score[1:(len/2)]
knowledge_bottom_half_s5_score = knowledge_s5_score[(len/2):len]  



# t-test to compare the avg score of the participants who spent the most and the least 
# amount of time on knowledge stack
knowledge_res = t.test(knowledge_top_half_s5_score, knowledge_bottom_half_s5_score)
knowledge_res

# p-value = = 0.8789



# Refusal data analysis
refusal_s5_score = Refusal_s5$`Avg. S5 mean`
len = length(refusal_s5_score)

refusal_top_half_s5_score = refusal_s5_score[1:(len/2)]
refusal_bottom_half_s5_score = refusal_s5_score[(len/2):len]  



# t-test to compare the avg score of the participants who spent the most and the least 
# amount of time on refusal stack
refusal_res = t.test(refusal_top_half_s5_score, refusal_bottom_half_s5_score)
refusal_res

# p-value = 0.1839 Non-signifcant

# Anhs's code
library(readxl)
library(dplyr)
rm(list = ls())

###############################
###simplified player 11 data###
###############################
simplified_knowledge_11 <- read_excel("C:/Users/Anh Nguyet Nguyen/Dropbox/na/DataFest/Data/simplified player 11.xlsx", 
                                      sheet = "knowledge 11")
View(simplified_knowledge_11)
filtered_knowledge_11 = as.data.frame(simplified_knowledge_11 %>% group_by(session) %>% filter(event_id %in% c(411, 413:419)))
View(filtered_knowledge_11)

simplified_refuse_11 <- read_excel("C:/Users/Anh Nguyet Nguyen/Dropbox/na/DataFest/Data/simplified player 11.xlsx", 
                                   sheet = "refuse 11")
View(simplified_refuse_11)
filtered_refuse_11 = as.data.frame(simplified_refuse_11 %>% group_by(session) %>% filter(event_id %in% c(502, 515)))
View(filtered_refuse_11)

###Average time to learn a knowledge each session###
# session_means = numeric(length(unique(filtered_data$session)))
knowledge_session_mean_11 = c()
for (session in unique(filtered_knowledge_11$session)){
  knowledge_session_mean_11 = append(knowledge_session_mean_11,sum(filtered_knowledge_11[filtered_knowledge_11$session == session,]$`time lapse`))
}
mean(knowledge_session_mean_11)

###Average time to learn to reject a temptation###
refuse_session_mean_11 = c()
for (session in unique(filtered_refuse_11$session)){
  refuse_session_mean_11 = append(refuse_session_mean_11,sum(filtered_refuse_11[filtered_refuse_11$session == session,]$`time lapse`))
}
mean(refuse_session_mean_11)

###############################
###simplified player 29 data###
###############################
simplified_knowledge_29 <- read_excel("C:/Users/Anh Nguyet Nguyen/Dropbox/na/DataFest/Data/simplified player 29.xlsx", 
                                      sheet = "knowledge 29")
View(simplified_knowledge_29)
filtered_knowledge_29 = as.data.frame(simplified_knowledge_29 %>% group_by(session) %>% filter(event_id %in% c(411, 413:419)))
View(filtered_knowledge_29)

simplified_refuse_29 <- read_excel("C:/Users/Anh Nguyet Nguyen/Dropbox/na/DataFest/Data/simplified player 29.xlsx", 
                                   sheet = "refuse 29")
View(simplified_refuse_29)
filtered_refuse_29 = as.data.frame(simplified_refuse_29 %>% group_by(session) %>% filter(event_id %in% c(502, 515)))
View(filtered_refuse_29)

###Average time to learn a knowledge each session###
# session_means = numeric(length(unique(filtered_data$session)))
knowledge_session_mean_29 = c()
for (session in unique(filtered_knowledge_29$session)){
  knowledge_session_mean_29 = append(knowledge_session_mean_29,sum(filtered_knowledge_29[filtered_knowledge_29$session == session,]$`time lapse`))
}
mean(knowledge_session_mean_29)

###Average time to learn to reject a temptation###
refuse_session_mean_29 = c()
for (session in unique(filtered_refuse_29$session)){
  refuse_session_mean_29 = append(refuse_session_mean_29,sum(filtered_refuse_29[filtered_refuse_29$session == session,]$`time lapse`))
}
mean(refuse_session_mean_29)

###############################
###simplified player 31 data###
###############################
simplified_knowledge_31 <- read_excel("C:/Users/Anh Nguyet Nguyen/Dropbox/na/DataFest/Data/simplified player 31.xlsx", 
                                      sheet = "knowledge 31")
View(simplified_knowledge_31)
filtered_knowledge_31 = as.data.frame(simplified_knowledge_31 %>% group_by(session) %>% filter(event_id %in% c(411, 413:419)))
View(filtered_knowledge_31)

simplified_refuse_31 <- read_excel("C:/Users/Anh Nguyet Nguyen/Dropbox/na/DataFest/Data/simplified player 31.xlsx", 
                                   sheet = "refuse 31")
View(simplified_refuse_31)
filtered_refuse_31 = as.data.frame(simplified_refuse_31 %>% group_by(session) %>% filter(event_id %in% c(502, 515)))
View(filtered_refuse_31)

###Average time to learn a knowledge each session###
# session_means = numeric(length(unique(filtered_data$session)))
knowledge_session_mean_31 = c()
for (session in unique(filtered_knowledge_31$session)){
  knowledge_session_mean_31 = append(knowledge_session_mean_31,sum(filtered_knowledge_31[filtered_knowledge_31$session == session,]$`time lapse`))
}
mean(knowledge_session_mean_31)

###Average time to learn to reject a temptation###
refuse_session_mean_31 = c()
for (session in unique(filtered_refuse_31$session)){
  refuse_session_mean_31 = append(refuse_session_mean_31,sum(filtered_refuse_31[filtered_refuse_31$session == session,]$`time lapse`))
}
mean(refuse_session_mean_31)

####################
###comparing data###
####################
knowledge_time = c(mean(knowledge_session_mean_11), mean(knowledge_session_mean_29), mean(knowledge_session_mean_31))
refuse_time = c(mean(refuse_session_mean_11), mean(refuse_session_mean_29), mean(refuse_session_mean_31))

barplot(knowledge_time, main="mean knowledge acquiring time", xlab="player", 
        names=c("6607011", "6486029", "6427031"), col=c("darkblue","red","yellow"))
barplot(refuse_time, main="mean refusal practicing time", xlab="player", 
        names=c("6607011", "6486029", "6427031"), col=c("darkblue","red","yellow"))

