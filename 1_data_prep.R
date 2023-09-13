# setup
library(tidyverse)
library(haven)

df <- read_dta("~/bwSyncShare/Multilabel open q/Dataset_Happy.dta")

df$lhappro_character <- haven::as_factor(df$lhappro_character)
df$lhap_ec <- haven::as_factor(df$lhap_ec)
df$lhap <- haven::as_factor(df$lhap)

df$lhappro_nonresponse <- haven::as_factor(df$lhappro_nonresponse)
# remove hard nonresponses
df <- df %>% 
  filter(lhappro_nonresponse!="hard nonresponse")
table(df$lhappro_nonresponse)

df$lhappro_themes <- haven::as_factor(df$lhappro_themes)
table(df$lhappro_themes)

df$lhappro_character <- haven::as_factor(df$lhappro_character)
table(df$lhappro_character)

# df_temp <- df %>% 
#   select(lseh1_1 , lseh1_2 , lseh1_3,lseh1_4 , lseh1_5 , lseh1_6 , lseh1_7,
#               lseh1_8 , lseh1_9 , lseh2_11 , lseh2_12, Iseh2_13 , Iseh2_14 , 
#               lseh2_21 , lseh2_22, lseh2_31 , lseh2_32 , lseh3_11 , lseh3_12, 
#               lseh3_21 , Iseh3_22 , Iseh3_23 , Iseh3_24, lseh3_31 , lseh3_32 , 
#               lseh3_41 , lseh3_42, lseh3_51 , lseh3_52 , lseh4_11 , lseh4_12, 
#               lseh4_21 , lseh4_22 , lseh4_31 , lseh4_32, Iseh4_33 , lseh4_41 , 
#               lseh4_42 , lseh4_51, lseh4_61 , lseh4_71 , lseh4_81 , lseh4_91,
#               lseh4_101, lseh4_102) %>%
#   mutate_all(~haven::as_factor(.))
# 
# df <- df %>% 
#   select(-c(lseh1_1 , lseh1_2 , lseh1_3,lseh1_4 , lseh1_5 , lseh1_6 , lseh1_7,
#             lseh1_8 , lseh1_9 , lseh2_11 , lseh2_12, Iseh2_13 , Iseh2_14 , 
#             lseh2_21 , lseh2_22, lseh2_31 , lseh2_32 , lseh3_11 , lseh3_12, 
#             lseh3_21 , Iseh3_22 , Iseh3_23 , Iseh3_24, lseh3_31 , lseh3_32 , 
#             lseh3_41 , lseh3_42, lseh3_51 , lseh3_52 , lseh4_11 , lseh4_12, 
#             lseh4_21 , lseh4_22 , lseh4_31 , lseh4_32, Iseh4_33 , lseh4_41 , 
#             lseh4_42 , lseh4_51, lseh4_61 , lseh4_71 , lseh4_81 , lseh4_91,
#             lseh4_101, lseh4_102))
# 
# df <- cbind(df, df_temp)
# rm(df_temp)
df_onebox <- df %>% 
  filter(lhap_ec=="one_box")
df_threebox <- df %>% 
  filter(lhap_ec=="three_boxes")
df_fivebox <- df %>% 
  filter(lhap_ec=="five_boxes")
df_tenbox <- df %>% 
  filter(lhap_ec=="ten_boxes")



### one box
df_onebox$text <- df_onebox$lhappro_c1_neu
df_onebox$label_1 <- df_onebox$lseh1_1
df_onebox$label_2 <- df_onebox$lseh1_2
df_onebox$label_3 <- df_onebox$lseh1_3
df_onebox$label_4 <- df_onebox$lseh1_4
df_onebox$label_5 <- df_onebox$lseh1_5
df_onebox$label_6 <- df_onebox$lseh1_6
df_onebox$label_7 <- df_onebox$lseh1_7
df_onebox$label_8 <- df_onebox$lseh1_8
df_onebox$label_9 <- df_onebox$lseh1_9

names(df_onebox)
df_onebox <- df_onebox %>% 
  select(lfdn, lhap_ec, text, starts_with("label_"),lhappro_nonresponse)


## three boxes
df_threebox <- df_threebox %>% 
  select(lfdn, lhap_ec, lhappro1_c2:lhappro3_c2, lseh2_11:lseh2_32,lhappro_nonresponse)

df_threebox <- gather(df_threebox, answerbox, text, lhappro1_c2:lhappro3_c2, factor_key=TRUE)
df_threebox <- df_threebox %>% 
  mutate(answerbox = case_when(
    answerbox=="lhappro1_c2" ~ "one",
    answerbox=="lhappro2_c2" ~ "two",
    answerbox=="lhappro3_c2" ~ "three"))

df_threebox_1 <- df_threebox %>% 
  filter(answerbox=="one") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh2_11:Iseh2_14,lhappro_nonresponse) %>% 
  rename(label_1 = lseh2_11,
         label_2 = lseh2_12,
         label_3 = Iseh2_13,
         label_4 = Iseh2_14)
df_threebox_2 <- df_threebox %>% 
  filter(answerbox=="two") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh2_21:lseh2_22,lhappro_nonresponse) %>% 
  rename(label_1 = lseh2_21,
         label_2 = lseh2_22) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
df_threebox_3 <- df_threebox %>% 
  filter(answerbox=="three") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh2_31:lseh2_32,lhappro_nonresponse) %>% 
  rename(label_1 = lseh2_31,
         label_2 = lseh2_32) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
  
df_threebox <- rbind(df_threebox_1,df_threebox_2,df_threebox_3)
rm(df_threebox_1,df_threebox_2,df_threebox_3)


df_fivebox <- df_fivebox %>% 
  select(lfdn, lhap_ec, lhappro1_c3:lhappro5_c3, lseh3_11:lseh3_52,lhappro_nonresponse)

df_fivebox <- gather(df_fivebox, answerbox, text, lhappro1_c3:lhappro5_c3, factor_key=TRUE)
df_fivebox <- df_fivebox %>% 
  mutate(answerbox = case_when(
    answerbox=="lhappro1_c3" ~ "one",
    answerbox=="lhappro2_c3" ~ "two",
    answerbox=="lhappro3_c3" ~ "three",
    answerbox=="lhappro4_c3" ~ "four",
    answerbox=="lhappro5_c3" ~ "five"))



### five boxes
df_fivebox_1 <- df_fivebox %>% 
  filter(answerbox=="one") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh3_11:lseh3_12,lhappro_nonresponse) %>% 
  rename(label_1 = lseh3_11,
         label_2 = lseh3_12) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
df_fivebox_2 <- df_fivebox %>% 
  filter(answerbox=="two") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh3_21:Iseh3_24,lhappro_nonresponse) %>% 
  rename(label_1 = lseh3_21,
         label_2 = Iseh3_22,
         label_3 = Iseh3_23,
         label_4 = Iseh3_24)
df_fivebox_3 <- df_fivebox %>% 
  filter(answerbox=="three") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh3_31:lseh3_32,lhappro_nonresponse) %>% 
  rename(label_1 = lseh3_31,
         label_2 = lseh3_32) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
df_fivebox_4 <- df_fivebox %>% 
  filter(answerbox=="four") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh3_41:lseh3_42,lhappro_nonresponse) %>% 
  rename(label_1 = lseh3_41,
         label_2 = lseh3_42) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
df_fivebox_5 <- df_fivebox %>% 
  filter(answerbox=="five") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh3_51:lseh3_52,lhappro_nonresponse) %>% 
  rename(label_1 = lseh3_51,
         label_2 = lseh3_52) %>% 
  mutate(label_3 = NA,
         label_4 = NA)

df_fivebox <- rbind(df_fivebox_1,df_fivebox_2,df_fivebox_3,df_fivebox_4,df_fivebox_5)
rm(df_fivebox_1,df_fivebox_2,df_fivebox_3,df_fivebox_4,df_fivebox_5)



## 10 boxes

df_tenbox <- df_tenbox %>% 
  select(lfdn, lhap_ec, lhappro1_c4:lhappro10_c4, lseh4_11:lseh4_102,lhappro_nonresponse)

df_tenbox <- gather(df_tenbox, answerbox, text, lhappro1_c4:lhappro10_c4, factor_key=TRUE)
df_tenbox <- df_tenbox %>% 
  mutate(answerbox = case_when(
    answerbox == "lhappro1_c4" ~ "one",
    answerbox == "lhappro2_c4" ~ "two",
    answerbox == "lhappro3_c4" ~ "three",
    answerbox == "lhappro4_c4" ~ "four",
    answerbox == "lhappro5_c4" ~ "five",
    answerbox == "lhappro6_c4" ~ "six",
    answerbox == "lhappro7_c4" ~ "seven",
    answerbox == "lhappro8_c4" ~ "eight",
    answerbox == "lhappro9_c4" ~ "nine",
    answerbox == "lhappro10_c4" ~ "ten"
  ))


df_tenbox_1 <- df_tenbox %>% 
  filter(answerbox=="one") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_11:lseh4_12,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_11,
         label_2 = lseh4_12) %>% 
  mutate(label_3 = NA)

df_tenbox_2 <- df_tenbox %>% 
  filter(answerbox=="two") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_21:lseh4_22,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_21,
         label_2 = lseh4_22) %>% 
  mutate(label_3 = NA)

df_tenbox_3 <- df_tenbox %>% 
  filter(answerbox=="three") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_31:Iseh4_33,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_31,
         label_2 = lseh4_32,
         label_3 = Iseh4_33)

df_tenbox_4 <- df_tenbox %>% 
  filter(answerbox=="four") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_41:lseh4_42,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_41,
         label_2 = lseh4_42) %>% 
  mutate(label_3 = NA)

df_tenbox_5 <- df_tenbox %>% 
  filter(answerbox=="five") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_51,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_51) %>% 
  mutate(label_2 = NA,
         label_3 = NA)

df_tenbox_6 <- df_tenbox %>% 
  filter(answerbox=="six") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_61,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_61) %>% 
  mutate(label_2 = NA,
         label_3 = NA)

df_tenbox_7 <- df_tenbox %>% 
  filter(answerbox=="seven") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_71,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_71) %>% 
  mutate(label_2 = NA,
         label_3 = NA)

df_tenbox_8 <- df_tenbox %>% 
  filter(answerbox=="eight") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_81,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_81) %>% 
  mutate(label_2 = NA,
         label_3 = NA)

df_tenbox_9 <- df_tenbox %>% 
  filter(answerbox=="nine") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_91,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_91) %>% 
  mutate(label_2 = NA,
         label_3 = NA)

df_tenbox_10 <- df_tenbox %>% 
  filter(answerbox=="ten") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_101:lseh4_102,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_101,
         label_2 = lseh4_102) %>% 
  mutate(label_3 = NA)


df_tenbox <- rbind(df_tenbox_1, df_tenbox_2, df_tenbox_3, df_tenbox_4, df_tenbox_5,
                   df_tenbox_6, df_tenbox_7, df_tenbox_8, df_tenbox_9, df_tenbox_10)
rm(df_tenbox_1, df_tenbox_2, df_tenbox_3, df_tenbox_4, df_tenbox_5,
   df_tenbox_6, df_tenbox_7, df_tenbox_8, df_tenbox_9, df_tenbox_10)

print(df[df$lfdn==3064,])

table(df_onebox$label_1, df_onebox$lhappro_nonresponse)


df_onebox <- df_onebox %>%
  mutate(label_1 = as.character(label_1)) %>%
  mutate(new_label_1 = case_when(
    str_detect(label_1, "^9") ~ "Rest",
    str_detect(label_1, "^8") ~ "Time references",
    str_detect(label_1, "^7") ~ "Life event",
    str_detect(label_1, "^6") ~ "Politics, security & society",
    str_detect(label_1, "^5") ~ "Life situation & living conditions",
    str_detect(label_1, "^4") ~ "Financial situation",
    str_detect(label_1, "^3") ~ "Job",
    str_detect(label_1, "^2") ~ "Health",
    str_detect(label_1, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_1  # Keep the original value if no condition is met
  ))

df_onebox <- df_onebox %>%
  mutate(label_2 = as.character(label_2)) %>%
  mutate(new_label_2 = case_when(
    str_detect(label_2, "^9") ~ "Rest",
    str_detect(label_2, "^8") ~ "Time references",
    str_detect(label_2, "^7") ~ "Life event",
    str_detect(label_2, "^6") ~ "Politics, security & society",
    str_detect(label_2, "^5") ~ "Life situation & living conditions",
    str_detect(label_2, "^4") ~ "Financial situation",
    str_detect(label_2, "^3") ~ "Job",
    str_detect(label_2, "^2") ~ "Health",
    str_detect(label_2, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_2  # Keep the original value if no condition is met
  ))

df_onebox <- df_onebox %>%
  mutate(label_3 = as.character(label_3)) %>%
  mutate(new_label_3 = case_when(
    str_detect(label_3, "^9") ~ "Rest",
    str_detect(label_3, "^8") ~ "Time references",
    str_detect(label_3, "^7") ~ "Life event",
    str_detect(label_3, "^6") ~ "Politics, security & society",
    str_detect(label_3, "^5") ~ "Life situation & living conditions",
    str_detect(label_3, "^4") ~ "Financial situation",
    str_detect(label_3, "^3") ~ "Job",
    str_detect(label_3, "^2") ~ "Health",
    str_detect(label_3, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_3  # Keep the original value if no condition is met
  ))

df_onebox <- df_onebox %>%
  mutate(label_4 = as.character(label_4)) %>%
  mutate(new_label_4 = case_when(
    str_detect(label_4, "^9") ~ "Rest",
    str_detect(label_4, "^8") ~ "Time references",
    str_detect(label_4, "^7") ~ "Life event",
    str_detect(label_4, "^6") ~ "Politics, security & society",
    str_detect(label_4, "^5") ~ "Life situation & living conditions",
    str_detect(label_4, "^4") ~ "Financial situation",
    str_detect(label_4, "^3") ~ "Job",
    str_detect(label_4, "^2") ~ "Health",
    str_detect(label_4, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_4  # Keep the original value if no condition is met
  ))

df_onebox <- df_onebox %>%
  mutate(label_5 = as.character(label_5)) %>%
  mutate(new_label_5 = case_when(
    str_detect(label_5, "^9") ~ "Rest",
    str_detect(label_5, "^8") ~ "Time references",
    str_detect(label_5, "^7") ~ "Life event",
    str_detect(label_5, "^6") ~ "Politics, security & society",
    str_detect(label_5, "^5") ~ "Life situation & living conditions",
    str_detect(label_5, "^4") ~ "Financial situation",
    str_detect(label_5, "^3") ~ "Job",
    str_detect(label_5, "^2") ~ "Health",
    str_detect(label_5, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_5  # Keep the original value if no condition is met
  ))

df_onebox <- df_onebox %>%
  mutate(label_6 = as.character(label_6)) %>%
  mutate(new_label_6 = case_when(
    str_detect(label_6, "^9") ~ "Rest",
    str_detect(label_6, "^8") ~ "Time references",
    str_detect(label_6, "^7") ~ "Life event",
    str_detect(label_6, "^6") ~ "Politics, security & society",
    str_detect(label_6, "^5") ~ "Life situation & living conditions",
    str_detect(label_6, "^4") ~ "Financial situation",
    str_detect(label_6, "^3") ~ "Job",
    str_detect(label_6, "^2") ~ "Health",
    str_detect(label_6, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_6  # Keep the original value if no condition is met
  ))

df_onebox <- df_onebox %>%
  mutate(label_7 = as.character(label_7)) %>%
  mutate(new_label_7 = case_when(
    str_detect(label_7, "^9") ~ "Rest",
    str_detect(label_7, "^8") ~ "Time references",
    str_detect(label_7, "^7") ~ "Life event",
    str_detect(label_7, "^6") ~ "Politics, security & society",
    str_detect(label_7, "^5") ~ "Life situation & living conditions",
    str_detect(label_7, "^4") ~ "Financial situation",
    str_detect(label_7, "^3") ~ "Job",
    str_detect(label_7, "^2") ~ "Health",
    str_detect(label_7, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_7  # Keep the original value if no condition is met
  ))

df_onebox <- df_onebox %>%
  mutate(label_8 = as.character(label_8)) %>%
  mutate(new_label_8 = case_when(
    str_detect(label_8, "^9") ~ "Rest",
    str_detect(label_8, "^8") ~ "Time references",
    str_detect(label_8, "^7") ~ "Life event",
    str_detect(label_8, "^6") ~ "Politics, security & society",
    str_detect(label_8, "^5") ~ "Life situation & living conditions",
    str_detect(label_8, "^4") ~ "Financial situation",
    str_detect(label_8, "^3") ~ "Job",
    str_detect(label_8, "^2") ~ "Health",
    str_detect(label_8, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_8  # Keep the original value if no condition is met
  ))

df_onebox <- df_onebox %>%
  mutate(label_9 = as.character(label_9)) %>%
  mutate(new_label_9 = case_when(
    str_detect(label_9, "^9") ~ "Rest",
    str_detect(label_9, "^8") ~ "Time references",
    str_detect(label_9, "^7") ~ "Life event",
    str_detect(label_9, "^6") ~ "Politics, security & society",
    str_detect(label_9, "^5") ~ "Life situation & living conditions",
    str_detect(label_9, "^4") ~ "Financial situation",
    str_detect(label_9, "^3") ~ "Job",
    str_detect(label_9, "^2") ~ "Health",
    str_detect(label_9, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_9  # Keep the original value if no condition is met
  ))




df_threebox <- df_threebox %>%
  mutate(label_1 = as.character(label_1)) %>%
  mutate(new_label_1 = case_when(
    str_detect(label_1, "^9") ~ "Rest",
    str_detect(label_1, "^8") ~ "Time references",
    str_detect(label_1, "^7") ~ "Life event",
    str_detect(label_1, "^6") ~ "Politics, security & society",
    str_detect(label_1, "^5") ~ "Life situation & living conditions",
    str_detect(label_1, "^4") ~ "Financial situation",
    str_detect(label_1, "^3") ~ "Job",
    str_detect(label_1, "^2") ~ "Health",
    str_detect(label_1, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_1  # Keep the original value if no condition is met
  ))

df_threebox <- df_threebox %>%
  mutate(label_2 = as.character(label_2)) %>%
  mutate(new_label_2 = case_when(
    str_detect(label_2, "^9") ~ "Rest",
    str_detect(label_2, "^8") ~ "Time references",
    str_detect(label_2, "^7") ~ "Life event",
    str_detect(label_2, "^6") ~ "Politics, security & society",
    str_detect(label_2, "^5") ~ "Life situation & living conditions",
    str_detect(label_2, "^4") ~ "Financial situation",
    str_detect(label_2, "^3") ~ "Job",
    str_detect(label_2, "^2") ~ "Health",
    str_detect(label_2, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_2  # Keep the original value if no condition is met
  ))

df_threebox <- df_threebox %>%
  mutate(label_3 = as.character(label_3)) %>%
  mutate(new_label_3 = case_when(
    str_detect(label_3, "^9") ~ "Rest",
    str_detect(label_3, "^8") ~ "Time references",
    str_detect(label_3, "^7") ~ "Life event",
    str_detect(label_3, "^6") ~ "Politics, security & society",
    str_detect(label_3, "^5") ~ "Life situation & living conditions",
    str_detect(label_3, "^4") ~ "Financial situation",
    str_detect(label_3, "^3") ~ "Job",
    str_detect(label_3, "^2") ~ "Health",
    str_detect(label_3, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_3  # Keep the original value if no condition is met
  ))

df_threebox <- df_threebox %>%
  mutate(label_4 = as.character(label_4)) %>%
  mutate(new_label_4 = case_when(
    str_detect(label_4, "^9") ~ "Rest",
    str_detect(label_4, "^8") ~ "Time references",
    str_detect(label_4, "^7") ~ "Life event",
    str_detect(label_4, "^6") ~ "Politics, security & society",
    str_detect(label_4, "^5") ~ "Life situation & living conditions",
    str_detect(label_4, "^4") ~ "Financial situation",
    str_detect(label_4, "^3") ~ "Job",
    str_detect(label_4, "^2") ~ "Health",
    str_detect(label_4, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_4  # Keep the original value if no condition is met
  ))






df_fivebox <- df_fivebox %>%
  mutate(label_1 = as.character(label_1)) %>%
  mutate(new_label_1 = case_when(
    str_detect(label_1, "^9") ~ "Rest",
    str_detect(label_1, "^8") ~ "Time references",
    str_detect(label_1, "^7") ~ "Life event",
    str_detect(label_1, "^6") ~ "Politics, security & society",
    str_detect(label_1, "^5") ~ "Life situation & living conditions",
    str_detect(label_1, "^4") ~ "Financial situation",
    str_detect(label_1, "^3") ~ "Job",
    str_detect(label_1, "^2") ~ "Health",
    str_detect(label_1, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_1  # Keep the original value if no condition is met
  ))

df_fivebox <- df_fivebox %>%
  mutate(label_2 = as.character(label_2)) %>%
  mutate(new_label_2 = case_when(
    str_detect(label_2, "^9") ~ "Rest",
    str_detect(label_2, "^8") ~ "Time references",
    str_detect(label_2, "^7") ~ "Life event",
    str_detect(label_2, "^6") ~ "Politics, security & society",
    str_detect(label_2, "^5") ~ "Life situation & living conditions",
    str_detect(label_2, "^4") ~ "Financial situation",
    str_detect(label_2, "^3") ~ "Job",
    str_detect(label_2, "^2") ~ "Health",
    str_detect(label_2, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_2  # Keep the original value if no condition is met
  ))

df_fivebox <- df_fivebox %>%
  mutate(label_3 = as.character(label_3)) %>%
  mutate(new_label_3 = case_when(
    str_detect(label_3, "^9") ~ "Rest",
    str_detect(label_3, "^8") ~ "Time references",
    str_detect(label_3, "^7") ~ "Life event",
    str_detect(label_3, "^6") ~ "Politics, security & society",
    str_detect(label_3, "^5") ~ "Life situation & living conditions",
    str_detect(label_3, "^4") ~ "Financial situation",
    str_detect(label_3, "^3") ~ "Job",
    str_detect(label_3, "^2") ~ "Health",
    str_detect(label_3, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_3  # Keep the original value if no condition is met
  ))

df_fivebox <- df_fivebox %>%
  mutate(label_4 = as.character(label_4)) %>%
  mutate(new_label_4 = case_when(
    str_detect(label_4, "^9") ~ "Rest",
    str_detect(label_4, "^8") ~ "Time references",
    str_detect(label_4, "^7") ~ "Life event",
    str_detect(label_4, "^6") ~ "Politics, security & society",
    str_detect(label_4, "^5") ~ "Life situation & living conditions",
    str_detect(label_4, "^4") ~ "Financial situation",
    str_detect(label_4, "^3") ~ "Job",
    str_detect(label_4, "^2") ~ "Health",
    str_detect(label_4, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_4  # Keep the original value if no condition is met
  ))








df_tenbox <- df_tenbox %>%
  mutate(label_1 = as.character(label_1)) %>%
  mutate(new_label_1 = case_when(
    str_detect(label_1, "^9") ~ "Rest",
    str_detect(label_1, "^8") ~ "Time references",
    str_detect(label_1, "^7") ~ "Life event",
    str_detect(label_1, "^6") ~ "Politics, security & society",
    str_detect(label_1, "^5") ~ "Life situation & living conditions",
    str_detect(label_1, "^4") ~ "Financial situation",
    str_detect(label_1, "^3") ~ "Job",
    str_detect(label_1, "^2") ~ "Health",
    str_detect(label_1, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_1  # Keep the original value if no condition is met
  ))

df_tenbox <- df_tenbox %>%
  mutate(label_2 = as.character(label_2)) %>%
  mutate(new_label_2 = case_when(
    str_detect(label_2, "^9") ~ "Rest",
    str_detect(label_2, "^8") ~ "Time references",
    str_detect(label_2, "^7") ~ "Life event",
    str_detect(label_2, "^6") ~ "Politics, security & society",
    str_detect(label_2, "^5") ~ "Life situation & living conditions",
    str_detect(label_2, "^4") ~ "Financial situation",
    str_detect(label_2, "^3") ~ "Job",
    str_detect(label_2, "^2") ~ "Health",
    str_detect(label_2, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_2  # Keep the original value if no condition is met
  ))

df_tenbox <- df_tenbox %>%
  mutate(label_3 = as.character(label_3)) %>%
  mutate(new_label_3 = case_when(
    str_detect(label_3, "^9") ~ "Rest",
    str_detect(label_3, "^8") ~ "Time references",
    str_detect(label_3, "^7") ~ "Life event",
    str_detect(label_3, "^6") ~ "Politics, security & society",
    str_detect(label_3, "^5") ~ "Life situation & living conditions",
    str_detect(label_3, "^4") ~ "Financial situation",
    str_detect(label_3, "^3") ~ "Job",
    str_detect(label_3, "^2") ~ "Health",
    str_detect(label_3, "^1") ~ "Social network & surrounding",  # New condition
    TRUE ~ label_3  # Keep the original value if no condition is met
  ))

write_csv(df_fivebox, "~/bwSyncShare/Multilabel open q/Happy_fivebox.csv")
write_csv(df_threebox, "~/bwSyncShare/Multilabel open q/Happy_threebox.csv")
write_csv(df_tenbox, "~/bwSyncShare/Multilabel open q/Happy_tenbox.csv")
write_csv(df_onebox, "~/bwSyncShare/Multilabel open q/Happy_onebox.csv")

