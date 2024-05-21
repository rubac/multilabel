# setup
library(tidyverse)
library(haven)

# read data from Katharina
df <- read_dta("~/bwSyncShare/Multilabel open q/Dataset_Happy.dta")

# make some changes to the weird formats resulting from the dta import
df$lhappro_character <- haven::as_factor(df$lhappro_character)
df$lhap_ec <- haven::as_factor(df$lhap_ec)
df$lhap <- haven::as_factor(df$lhap)
df$lfdn <- as.numeric(df$lfdn)

df$lhappro_nonresponse <- haven::as_factor(df$lhappro_nonresponse)

# remove hard nonresponses
df <- df %>% 
  filter(lhappro_nonresponse!="hard nonresponse")
table(df$lhappro_nonresponse)

# more changes to dta format
df$lhappro_themes <- haven::as_factor(df$lhappro_themes)
table(df$lhappro_themes)

df$lhappro_character <- haven::as_factor(df$lhappro_character)
table(df$lhappro_character)

# split up data by experimental condition
df_onebox <- df %>% 
  filter(lhap_ec=="one_box")
df_threebox <- df %>% 
  filter(lhap_ec=="three_boxes")
df_fivebox <- df %>% 
  filter(lhap_ec=="five_boxes")
df_tenbox <- df %>% 
  filter(lhap_ec=="ten_boxes")



### one box
df_onebox$text <- as.character(df_onebox$lhappro_c1_neu)
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
# drop unnecessary columns
df_onebox <- df_onebox %>% 
  select(lfdn, lhap_ec, text, starts_with("label_"),lhappro_nonresponse)


## three boxes
# drop unnecessary columns
df_threebox <- df_threebox %>% 
  select(lfdn, lhap_ec, lhappro1_c2:lhappro3_c2, lseh2_11:lseh2_32,lhappro_nonresponse)

# from wide to long format and indicate which box text comes from
df_threebox <- gather(df_threebox, answerbox, text, lhappro1_c2:lhappro3_c2, factor_key=TRUE)
df_threebox <- df_threebox %>% 
  mutate(answerbox = case_when(
    answerbox=="lhappro1_c2" ~ "box one",
    answerbox=="lhappro2_c2" ~ "box two",
    answerbox=="lhappro3_c2" ~ "box three")) %>% 
  filter(text!="-99")

df_threebox_1 <- df_threebox %>% 
  filter(answerbox=="box one") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh2_11:Iseh2_14,lhappro_nonresponse) %>% 
  rename(label_1 = lseh2_11,
         label_2 = lseh2_12,
         label_3 = Iseh2_13,
         label_4 = Iseh2_14)
df_threebox_2 <- df_threebox %>% 
  filter(answerbox=="box two") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh2_21:lseh2_22,lhappro_nonresponse) %>% 
  rename(label_1 = lseh2_21,
         label_2 = lseh2_22) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
df_threebox_3 <- df_threebox %>% 
  filter(answerbox=="box three") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh2_31:lseh2_32,lhappro_nonresponse) %>% 
  rename(label_1 = lseh2_31,
         label_2 = lseh2_32) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
  
df_threebox <- rbind(df_threebox_1,df_threebox_2,df_threebox_3)
rm(df_threebox_1,df_threebox_2,df_threebox_3)

### now same with five boxes
df_fivebox <- df_fivebox %>% 
  select(lfdn, lhap_ec, lhappro1_c3:lhappro5_c3, lseh3_11:lseh3_52,lhappro_nonresponse)

df_fivebox <- gather(df_fivebox, answerbox, text, lhappro1_c3:lhappro5_c3, factor_key=TRUE)
df_fivebox <- df_fivebox %>% 
  mutate(answerbox = case_when(
    answerbox=="lhappro1_c3" ~ "box one",
    answerbox=="lhappro2_c3" ~ "box two",
    answerbox=="lhappro3_c3" ~ "box three",
    answerbox=="lhappro4_c3" ~ "box four",
    answerbox=="lhappro5_c3" ~ "box five")) %>% 
  filter(text!="-99")



### five boxes
df_fivebox_1 <- df_fivebox %>% 
  filter(answerbox=="box one") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh3_11:lseh3_12,lhappro_nonresponse) %>% 
  rename(label_1 = lseh3_11,
         label_2 = lseh3_12) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
df_fivebox_2 <- df_fivebox %>% 
  filter(answerbox=="box two") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh3_21:Iseh3_24,lhappro_nonresponse) %>% 
  rename(label_1 = lseh3_21,
         label_2 = Iseh3_22,
         label_3 = Iseh3_23,
         label_4 = Iseh3_24)
df_fivebox_3 <- df_fivebox %>% 
  filter(answerbox=="box three") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh3_31:lseh3_32,lhappro_nonresponse) %>% 
  rename(label_1 = lseh3_31,
         label_2 = lseh3_32) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
df_fivebox_4 <- df_fivebox %>% 
  filter(answerbox=="box four") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh3_41:lseh3_42,lhappro_nonresponse) %>% 
  rename(label_1 = lseh3_41,
         label_2 = lseh3_42) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
df_fivebox_5 <- df_fivebox %>% 
  filter(answerbox=="box five") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh3_51:lseh3_52,lhappro_nonresponse) %>% 
  rename(label_1 = lseh3_51,
         label_2 = lseh3_52) %>% 
  mutate(label_3 = NA,
         label_4 = NA)

df_fivebox <- rbind(df_fivebox_1,df_fivebox_2,df_fivebox_3,df_fivebox_4,df_fivebox_5)
rm(df_fivebox_1,df_fivebox_2,df_fivebox_3,df_fivebox_4,df_fivebox_5)



## and with 10 boxes

df_tenbox <- df_tenbox %>% 
  select(lfdn, lhap_ec, lhappro1_c4:lhappro10_c4, lseh4_11:lseh4_102,lhappro_nonresponse)

df_tenbox <- gather(df_tenbox, answerbox, text, lhappro1_c4:lhappro10_c4, factor_key=TRUE)
df_tenbox <- df_tenbox %>% 
  mutate(answerbox = case_when(
    answerbox == "lhappro1_c4" ~ "box one",
    answerbox == "lhappro2_c4" ~ "box two",
    answerbox == "lhappro3_c4" ~ "box three",
    answerbox == "lhappro4_c4" ~ "box four",
    answerbox == "lhappro5_c4" ~ "box five",
    answerbox == "lhappro6_c4" ~ "box six",
    answerbox == "lhappro7_c4" ~ "box seven",
    answerbox == "lhappro8_c4" ~ "box eight",
    answerbox == "lhappro9_c4" ~ "box nine",
    answerbox == "lhappro10_c4" ~ "box ten"
  )) %>% 
  filter(text!="-99")


df_tenbox_1 <- df_tenbox %>% 
  filter(answerbox=="box one") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_11:lseh4_12,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_11,
         label_2 = lseh4_12) %>% 
  mutate(label_3 = NA)

df_tenbox_2 <- df_tenbox %>% 
  filter(answerbox=="box two") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_21:lseh4_22,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_21,
         label_2 = lseh4_22) %>% 
  mutate(label_3 = NA)

df_tenbox_3 <- df_tenbox %>% 
  filter(answerbox=="box three") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_31:Iseh4_33,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_31,
         label_2 = lseh4_32,
         label_3 = Iseh4_33)

df_tenbox_4 <- df_tenbox %>% 
  filter(answerbox=="box four") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_41:lseh4_42,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_41,
         label_2 = lseh4_42) %>% 
  mutate(label_3 = NA)

df_tenbox_5 <- df_tenbox %>% 
  filter(answerbox=="box five") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_51,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_51) %>% 
  mutate(label_2 = NA,
         label_3 = NA)

df_tenbox_6 <- df_tenbox %>% 
  filter(answerbox=="box six") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_61,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_61) %>% 
  mutate(label_2 = NA,
         label_3 = NA)

df_tenbox_7 <- df_tenbox %>% 
  filter(answerbox=="box seven") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_71,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_71) %>% 
  mutate(label_2 = NA,
         label_3 = NA)

df_tenbox_8 <- df_tenbox %>% 
  filter(answerbox=="box eight") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_81,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_81) %>% 
  mutate(label_2 = NA,
         label_3 = NA)

df_tenbox_9 <- df_tenbox %>% 
  filter(answerbox=="box nine") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_91,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_91) %>% 
  mutate(label_2 = NA,
         label_3 = NA)

df_tenbox_10 <- df_tenbox %>% 
  filter(answerbox=="box ten") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh4_101:lseh4_102,lhappro_nonresponse) %>% 
  rename(label_1 = lseh4_101,
         label_2 = lseh4_102) %>% 
  mutate(label_3 = NA)


df_tenbox <- rbind(df_tenbox_1, df_tenbox_2, df_tenbox_3, df_tenbox_4, df_tenbox_5,
                   df_tenbox_6, df_tenbox_7, df_tenbox_8, df_tenbox_9, df_tenbox_10)
rm(df_tenbox_1, df_tenbox_2, df_tenbox_3, df_tenbox_4, df_tenbox_5,
   df_tenbox_6, df_tenbox_7, df_tenbox_8, df_tenbox_9, df_tenbox_10)





# update label categories /characters instead of numeric codes, repeat for each label
table(df_onebox$label_1, df_onebox$lhappro_nonresponse)


df_onebox <- df_onebox %>%
  mutate(label_1 = as.character(label_1)) %>%
  mutate(new_label_1 = case_when(
    str_detect(label_1, "^-9") ~ "nonresponse",
    str_detect(label_1, "^9") ~ "rest",
    str_detect(label_1, "^8") ~ "time_references",
    str_detect(label_1, "^7") ~ "life_event",
    str_detect(label_1, "^6") ~ "politics_security_society",
    str_detect(label_1, "^5") ~ "life_situation_living_conditions",
    str_detect(label_1, "^4") ~ "financial_situation",
    str_detect(label_1, "^3") ~ "job",
    str_detect(label_1, "^2") ~ "health",
    str_detect(label_1, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_1  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_1))

df_onebox <- df_onebox %>%
  mutate(label_2 = as.character(label_2)) %>%
  mutate(new_label_2 = case_when(
    str_detect(label_2, "^-9") ~ "nonresponse",
    str_detect(label_2, "^9") ~ "rest",
    str_detect(label_2, "^8") ~ "time_references",
    str_detect(label_2, "^7") ~ "life_event",
    str_detect(label_2, "^6") ~ "politics_security_society",
    str_detect(label_2, "^5") ~ "life_situation_living_conditions",
    str_detect(label_2, "^4") ~ "financial_situation",
    str_detect(label_2, "^3") ~ "job",
    str_detect(label_2, "^2") ~ "health",
    str_detect(label_2, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_2  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_2))

df_onebox <- df_onebox %>%
  mutate(label_3 = as.character(label_3)) %>%
  mutate(new_label_3 = case_when(
    str_detect(label_3, "^-9") ~ "nonresponse",
    str_detect(label_3, "^9") ~ "rest",
    str_detect(label_3, "^8") ~ "time_references",
    str_detect(label_3, "^7") ~ "life_event",
    str_detect(label_3, "^6") ~ "politics_security_society",
    str_detect(label_3, "^5") ~ "life_situation_living_conditions",
    str_detect(label_3, "^4") ~ "financial_situation",
    str_detect(label_3, "^3") ~ "job",
    str_detect(label_3, "^2") ~ "health",
    str_detect(label_3, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_3  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_3))

df_onebox <- df_onebox %>%
  mutate(label_4 = as.character(label_4)) %>%
  mutate(new_label_4 = case_when(
    str_detect(label_4, "^-9") ~ "nonresponse",
    str_detect(label_4, "^9") ~ "rest",
    str_detect(label_4, "^8") ~ "time_references",
    str_detect(label_4, "^7") ~ "life_event",
    str_detect(label_4, "^6") ~ "politics_security_society",
    str_detect(label_4, "^5") ~ "life_situation_living_conditions",
    str_detect(label_4, "^4") ~ "financial_situation",
    str_detect(label_4, "^3") ~ "job",
    str_detect(label_4, "^2") ~ "health",
    str_detect(label_4, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_4  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_4))

df_onebox <- df_onebox %>%
  mutate(label_5 = as.character(label_5)) %>%
  mutate(new_label_5 = case_when(
    str_detect(label_5, "^-9") ~ "nonresponse",
    str_detect(label_5, "^9") ~ "rest",
    str_detect(label_5, "^8") ~ "time_references",
    str_detect(label_5, "^7") ~ "life_event",
    str_detect(label_5, "^6") ~ "politics_security_society",
    str_detect(label_5, "^5") ~ "life_situation_living_conditions",
    str_detect(label_5, "^4") ~ "financial_situation",
    str_detect(label_5, "^3") ~ "job",
    str_detect(label_5, "^2") ~ "health",
    str_detect(label_5, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_5  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_5))

df_onebox <- df_onebox %>%
  mutate(label_6 = as.character(label_6)) %>%
  mutate(new_label_6 = case_when(
    str_detect(label_6, "^-9") ~ "nonresponse",
    str_detect(label_6, "^9") ~ "rest",
    str_detect(label_6, "^8") ~ "time_references",
    str_detect(label_6, "^7") ~ "life_event",
    str_detect(label_6, "^6") ~ "politics_security_society",
    str_detect(label_6, "^5") ~ "life_situation_living_conditions",
    str_detect(label_6, "^4") ~ "financial_situation",
    str_detect(label_6, "^3") ~ "job",
    str_detect(label_6, "^2") ~ "health",
    str_detect(label_6, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_6  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_6))

df_onebox <- df_onebox %>%
  mutate(label_7 = as.character(label_7)) %>%
  mutate(new_label_7 = case_when(
    str_detect(label_7, "^-9") ~ "nonresponse",
    str_detect(label_7, "^9") ~ "rest",
    str_detect(label_7, "^8") ~ "time_references",
    str_detect(label_7, "^7") ~ "life_event",
    str_detect(label_7, "^6") ~ "politics_security_society",
    str_detect(label_7, "^5") ~ "life_situation_living_conditions",
    str_detect(label_7, "^4") ~ "financial_situation",
    str_detect(label_7, "^3") ~ "job",
    str_detect(label_7, "^2") ~ "health",
    str_detect(label_7, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_7  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_7 ))

df_onebox <- df_onebox %>%
  mutate(label_8 = as.character(label_8)) %>%
  mutate(new_label_8 = case_when(
    str_detect(label_8, "^-9") ~ "nonresponse",
    str_detect(label_8, "^9") ~ "rest",
    str_detect(label_8, "^8") ~ "time_references",
    str_detect(label_8, "^7") ~ "life_event",
    str_detect(label_8, "^6") ~ "politics_security_society",
    str_detect(label_8, "^5") ~ "life_situation_living_conditions",
    str_detect(label_8, "^4") ~ "financial_situation",
    str_detect(label_8, "^3") ~ "job",
    str_detect(label_8, "^2") ~ "health",
    str_detect(label_8, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_8  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_8))

df_onebox <- df_onebox %>%
  mutate(label_9 = as.character(label_9)) %>%
  mutate(new_label_9 = case_when(
    str_detect(label_9, "^-9") ~ "nonresponse",
    str_detect(label_9, "^9") ~ "rest",
    str_detect(label_9, "^8") ~ "time_references",
    str_detect(label_9, "^7") ~ "life_event",
    str_detect(label_9, "^6") ~ "politics_security_society",
    str_detect(label_9, "^5") ~ "life_situation_living_conditions",
    str_detect(label_9, "^4") ~ "financial_situation",
    str_detect(label_9, "^3") ~ "job",
    str_detect(label_9, "^2") ~ "health",
    str_detect(label_9, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_9  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_9, lhap_ec, lhappro_nonresponse))



# same with three box format, five box, ten box
df_threebox <- df_threebox %>%
  mutate(label_1 = as.character(label_1)) %>%
  mutate(new_label_1 = case_when(
    str_detect(label_1, "^-9") ~ "nonresponse",
    str_detect(label_1, "^9") ~ "rest",
    str_detect(label_1, "^8") ~ "time_references",
    str_detect(label_1, "^7") ~ "life_event",
    str_detect(label_1, "^6") ~ "politics_security_society",
    str_detect(label_1, "^5") ~ "life_situation_living_conditions",
    str_detect(label_1, "^4") ~ "financial_situation",
    str_detect(label_1, "^3") ~ "job",
    str_detect(label_1, "^2") ~ "health",
    str_detect(label_1, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_1  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_1))

df_threebox <- df_threebox %>%
  mutate(label_2 = as.character(label_2)) %>%
  mutate(new_label_2 = case_when(
    str_detect(label_2, "^-9") ~ "nonresponse",
    str_detect(label_2, "^9") ~ "rest",
    str_detect(label_2, "^8") ~ "time_references",
    str_detect(label_2, "^7") ~ "life_event",
    str_detect(label_2, "^6") ~ "politics_security_society",
    str_detect(label_2, "^5") ~ "life_situation_living_conditions",
    str_detect(label_2, "^4") ~ "financial_situation",
    str_detect(label_2, "^3") ~ "job",
    str_detect(label_2, "^2") ~ "health",
    str_detect(label_2, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_2  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_2))

df_threebox <- df_threebox %>%
  mutate(label_3 = as.character(label_3)) %>%
  mutate(new_label_3 = case_when(
    str_detect(label_3, "^-9") ~ "nonresponse",
    str_detect(label_3, "^9") ~ "rest",
    str_detect(label_3, "^8") ~ "time_references",
    str_detect(label_3, "^7") ~ "life_event",
    str_detect(label_3, "^6") ~ "politics_security_society",
    str_detect(label_3, "^5") ~ "life_situation_living_conditions",
    str_detect(label_3, "^4") ~ "financial_situation",
    str_detect(label_3, "^3") ~ "job",
    str_detect(label_3, "^2") ~ "health",
    str_detect(label_3, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_3  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_3))

df_threebox <- df_threebox %>%
  mutate(label_4 = as.character(label_4)) %>%
  mutate(new_label_4 = case_when(
    str_detect(label_4, "^-9") ~ "nonresponse",
    str_detect(label_4, "^9") ~ "rest",
    str_detect(label_4, "^8") ~ "time_references",
    str_detect(label_4, "^7") ~ "life_event",
    str_detect(label_4, "^6") ~ "politics_security_society",
    str_detect(label_4, "^5") ~ "life_situation_living_conditions",
    str_detect(label_4, "^4") ~ "financial_situation",
    str_detect(label_4, "^3") ~ "job",
    str_detect(label_4, "^2") ~ "health",
    str_detect(label_4, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_4  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_4, lhap_ec, lhappro_nonresponse))






df_fivebox <- df_fivebox %>%
  mutate(label_1 = as.character(label_1)) %>%
  mutate(new_label_1 = case_when(
    str_detect(label_1, "^-9") ~ "nonresponse",
    str_detect(label_1, "^9") ~ "rest",
    str_detect(label_1, "^8") ~ "time_references",
    str_detect(label_1, "^7") ~ "life_event",
    str_detect(label_1, "^6") ~ "politics_security_society",
    str_detect(label_1, "^5") ~ "life_situation_living_conditions",
    str_detect(label_1, "^4") ~ "financial_situation",
    str_detect(label_1, "^3") ~ "job",
    str_detect(label_1, "^2") ~ "health",
    str_detect(label_1, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_1  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_1))

df_fivebox <- df_fivebox %>%
  mutate(label_2 = as.character(label_2)) %>%
  mutate(new_label_2 = case_when(
    str_detect(label_2, "^-9") ~ "nonresponse",
    str_detect(label_2, "^9") ~ "rest",
    str_detect(label_2, "^8") ~ "time_references",
    str_detect(label_2, "^7") ~ "life_event",
    str_detect(label_2, "^6") ~ "politics_security_society",
    str_detect(label_2, "^5") ~ "life_situation_living_conditions",
    str_detect(label_2, "^4") ~ "financial_situation",
    str_detect(label_2, "^3") ~ "job",
    str_detect(label_2, "^2") ~ "health",
    str_detect(label_2, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_2  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_2))

df_fivebox <- df_fivebox %>%
  mutate(label_3 = as.character(label_3)) %>%
  mutate(new_label_3 = case_when(
    str_detect(label_3, "^-9") ~ "nonresponse",
    str_detect(label_3, "^9") ~ "rest",
    str_detect(label_3, "^8") ~ "time_references",
    str_detect(label_3, "^7") ~ "life_event",
    str_detect(label_3, "^6") ~ "politics_security_society",
    str_detect(label_3, "^5") ~ "life_situation_living_conditions",
    str_detect(label_3, "^4") ~ "financial_situation",
    str_detect(label_3, "^3") ~ "job",
    str_detect(label_3, "^2") ~ "health",
    str_detect(label_3, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_3  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_3))

df_fivebox <- df_fivebox %>%
  mutate(label_4 = as.character(label_4)) %>%
  mutate(new_label_4 = case_when(
    str_detect(label_4, "^-9") ~ "nonresponse",
    str_detect(label_4, "^9") ~ "rest",
    str_detect(label_4, "^8") ~ "time_references",
    str_detect(label_4, "^7") ~ "life_event",
    str_detect(label_4, "^6") ~ "politics_security_society",
    str_detect(label_4, "^5") ~ "life_situation_living_conditions",
    str_detect(label_4, "^4") ~ "financial_situation",
    str_detect(label_4, "^3") ~ "job",
    str_detect(label_4, "^2") ~ "health",
    str_detect(label_4, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_4  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_4, lhap_ec, lhappro_nonresponse))








df_tenbox <- df_tenbox %>%
  mutate(label_1 = as.character(label_1)) %>%
  mutate(new_label_1 = case_when(
    str_detect(label_1, "^-9") ~ "nonresponse",
    str_detect(label_1, "^9") ~ "rest",
    str_detect(label_1, "^8") ~ "time_references",
    str_detect(label_1, "^7") ~ "life_event",
    str_detect(label_1, "^6") ~ "politics_security_society",
    str_detect(label_1, "^5") ~ "life_situation_living_conditions",
    str_detect(label_1, "^4") ~ "financial_situation",
    str_detect(label_1, "^3") ~ "job",
    str_detect(label_1, "^2") ~ "health",
    str_detect(label_1, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_1  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_1))

df_tenbox <- df_tenbox %>%
  mutate(label_2 = as.character(label_2)) %>%
  mutate(new_label_2 = case_when(
    str_detect(label_2, "^-9") ~ "nonresponse",
    str_detect(label_2, "^9") ~ "rest",
    str_detect(label_2, "^8") ~ "time_references",
    str_detect(label_2, "^7") ~ "life_event",
    str_detect(label_2, "^6") ~ "politics_security_society",
    str_detect(label_2, "^5") ~ "life_situation_living_conditions",
    str_detect(label_2, "^4") ~ "financial_situation",
    str_detect(label_2, "^3") ~ "job",
    str_detect(label_2, "^2") ~ "health",
    str_detect(label_2, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_2  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_2))

df_tenbox <- df_tenbox %>%
  mutate(label_3 = as.character(label_3)) %>%
  mutate(new_label_3 = case_when(
    str_detect(label_3, "^-9") ~ "nonresponse",
    str_detect(label_3, "^9") ~ "rest",
    str_detect(label_3, "^8") ~ "time_references",
    str_detect(label_3, "^7") ~ "life_event",
    str_detect(label_3, "^6") ~ "politics_security_society",
    str_detect(label_3, "^5") ~ "life_situation_living_conditions",
    str_detect(label_3, "^4") ~ "financial_situation",
    str_detect(label_3, "^3") ~ "job",
    str_detect(label_3, "^2") ~ "health",
    str_detect(label_3, "^1") ~ "social_network_surrounding",  # New condition
    TRUE ~ label_3  # Keep the original value if no condition is met
  )) %>% 
  select(-c(label_3, lhap_ec, lhappro_nonresponse))


df_fivebox <- df_fivebox %>% 
  filter(!is.na(new_label_1))
df_threebox <- df_threebox %>% 
  filter(!is.na(new_label_1))
df_tenbox <- df_tenbox %>% 
  filter(!is.na(new_label_1))


##### here we drop cases that have multiple labels
df_fivebox <- df_fivebox %>% 
  filter(is.na(new_label_2)) %>%
  filter(is.na(new_label_3))

df_threebox <- df_threebox %>% 
filter(is.na(new_label_2)) %>%
  filter(is.na(new_label_3))


df_tenbox <- df_tenbox %>% 
filter(is.na(new_label_2)) %>%
  filter(is.na(new_label_3))



####### 
library(caret)
# one-hot encoding to put labels into right format for python
df_onebox_label1 <- predict(dummyVars(~ new_label_1, df_onebox), newdata = df_onebox)
colnames(df_onebox_label1) <- gsub("new_label_1", "", colnames(df_onebox_label1))
df_onebox_label1 <- as.data.frame(df_onebox_label1)
df_onebox_label1$label <- "One"
df_onebox_label1$lfdn <- df_onebox$lfdn

df_onebox_label2 <- predict(dummyVars(~ new_label_2, df_onebox), newdata = df_onebox)
colnames(df_onebox_label2) <- gsub("new_label_2", "", colnames(df_onebox_label2))
df_onebox_label2 <- as.data.frame(df_onebox_label2)
df_onebox_label2$label <- "Two"
df_onebox_label2$lfdn <- df_onebox$lfdn

df_onebox_label3 <- predict(dummyVars(~ new_label_3, df_onebox), newdata = df_onebox)
colnames(df_onebox_label3) <- gsub("new_label_3", "", colnames(df_onebox_label3))
df_onebox_label3 <- as.data.frame(df_onebox_label3)
df_onebox_label3$label <- "Three"
df_onebox_label3$lfdn <- df_onebox$lfdn

df_onebox_label4 <- predict(dummyVars(~ new_label_4, df_onebox), newdata = df_onebox)
colnames(df_onebox_label4) <- gsub("new_label_4", "", colnames(df_onebox_label4))
df_onebox_label4 <- as.data.frame(df_onebox_label4)
df_onebox_label4$label <- "Four"
df_onebox_label4$lfdn <- df_onebox$lfdn

df_onebox_label5 <- predict(dummyVars(~ new_label_5, df_onebox), newdata = df_onebox)
colnames(df_onebox_label5) <- gsub("new_label_5", "", colnames(df_onebox_label5))
df_onebox_label5 <- as.data.frame(df_onebox_label5)
df_onebox_label5$label <- "Five"
df_onebox_label5$lfdn <- df_onebox$lfdn

df_onebox_label6 <- predict(dummyVars(~ new_label_6, df_onebox), newdata = df_onebox)
colnames(df_onebox_label6) <- gsub("new_label_6", "", colnames(df_onebox_label6))
df_onebox_label6 <- as.data.frame(df_onebox_label6)
df_onebox_label6$label <- "Six"
df_onebox_label6$lfdn <- df_onebox$lfdn

df_onebox_label7 <- predict(dummyVars(~ new_label_7, df_onebox), newdata = df_onebox)
colnames(df_onebox_label7) <- gsub("new_label_7", "", colnames(df_onebox_label7))
df_onebox_label7 <- as.data.frame(df_onebox_label7)
df_onebox_label7$label <- "Seven"
df_onebox_label7$lfdn <- df_onebox$lfdn

df_onebox_label8 <- predict(dummyVars(~ new_label_8, df_onebox), newdata = df_onebox)
colnames(df_onebox_label8) <- gsub("new_label_8", "", colnames(df_onebox_label8))
df_onebox_label8 <- as.data.frame(df_onebox_label8)
df_onebox_label8$label <- "Eight"
df_onebox_label8$lfdn <- df_onebox$lfdn

df_onebox$new_label_9[which(is.na(df_onebox$new_label_9))] <- "NOOO"
df_onebox_label9 <- predict(dummyVars(~ new_label_9, df_onebox), newdata = df_onebox)
colnames(df_onebox_label9) <- gsub("new_label_9", "", colnames(df_onebox_label9))
df_onebox_label9 <- as.data.frame(df_onebox_label9)
df_onebox_label9$label <- "Nine"
df_onebox_label9$lfdn <- df_onebox$lfdn
df_onebox_label9$NOOO <- NULL


df_onebox2 <- bind_rows(df_onebox_label1,df_onebox_label2,df_onebox_label3,df_onebox_label4
                       ,df_onebox_label5,df_onebox_label6,df_onebox_label7,df_onebox_label8
                       ,df_onebox_label9)

df_onebox2 <- df_onebox2 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
         nonresponse = max(nonresponse),
         time_references = max(time_references),
         life_event = max(life_event),
         politics_security_society = max(politics_security_society),
         life_situation_living_conditions = max(life_situation_living_conditions),
         financial_situation = max(financial_situation),
         job = max(job),
         health = max(health),
         social_network_surrounding = max(social_network_surrounding))
         
df_onebox <- merge(df_onebox, df_onebox2)
df_onebox <- df_onebox %>% 
  select(-c(starts_with("new_lab")))
rm(df_onebox2, df_onebox_label1, df_onebox_label2, df_onebox_label3, df_onebox_label4
   , df_onebox_label5, df_onebox_label6, df_onebox_label7, df_onebox_label8, df_onebox_label9)




############
# one-hot encoding to put labels into right format for python
df_threebox_label1 <- predict(dummyVars(~ new_label_1, df_threebox), newdata = df_threebox)
colnames(df_threebox_label1) <- gsub("new_label_1", "", colnames(df_threebox_label1))
df_threebox_label1 <- as.data.frame(df_threebox_label1)
df_threebox_label1$label <- "One"
df_threebox_label1$lfdn <- df_threebox$lfdn
df_threebox_label1$answerbox <- df_threebox$answerbox


### for MultiCLASS Single Label
df_threebox_single <- df_threebox %>% 
#   filter(is.na(new_label_2)) %>% 
  select(-c(new_label_2, new_label_3))
df_threebox_single$new_label_1[which(is.na(df_threebox_single$new_label_1))] <- "nonresponse"

df_threebox2 <- df_threebox_label1

df_threebox2_A1 <- df_threebox2 %>% 
  filter(answerbox=="box one")
df_threebox2_A2 <- df_threebox2 %>% 
  filter(answerbox=="box two")
df_threebox2_A3 <- df_threebox2 %>% 
  filter(answerbox=="box three")

df_threebox2_A1 <- df_threebox2_A1 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_threebox2_A1$answerbox <- "box one"

df_threebox2_A2 <- df_threebox2_A2 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_threebox2_A2$answerbox <- "box two"

df_threebox2_A3 <- df_threebox2_A3 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_threebox2_A3$answerbox <- "box three"

df_threebox2 <- rbind(df_threebox2_A1, df_threebox2_A2, df_threebox2_A3)

df_threebox <- merge(df_threebox, df_threebox2)
df_threebox <- df_threebox %>% 
  select(-c(starts_with("new_lab")))
rm(df_threebox2, df_threebox_label1,
   df_threebox2_A1, df_threebox2_A2, df_threebox2_A3)


#######

############
# one-hot encoding to put labels into right format for python
df_fivebox_label1 <- predict(dummyVars(~ new_label_1, df_fivebox), newdata = df_fivebox)
colnames(df_fivebox_label1) <- gsub("new_label_1", "", colnames(df_fivebox_label1))
df_fivebox_label1 <- as.data.frame(df_fivebox_label1)
df_fivebox_label1$label <- "One"
df_fivebox_label1$lfdn <- df_fivebox$lfdn
df_fivebox_label1$answerbox <- df_fivebox$answerbox


df_fivebox2 <- df_fivebox_label1


df_fivebox2_A1 <- df_fivebox2 %>% 
  filter(answerbox=="box one")
df_fivebox2_A2 <- df_fivebox2 %>% 
  filter(answerbox=="box two")
df_fivebox2_A3 <- df_fivebox2 %>% 
  filter(answerbox=="box three")
df_fivebox2_A4 <- df_fivebox2 %>% 
  filter(answerbox=="box four")
df_fivebox2_A5 <- df_fivebox2 %>% 
  filter(answerbox=="box five")

### for MultiCLASS Single Label
df_fivebox_single <- df_fivebox %>% 
  select(-c(new_label_2, new_label_3, new_label_4))
df_fivebox_single$new_label_1[which(is.na(df_fivebox_single$new_label_1))] <- "nonresponse"

df_fivebox2_A1 <- df_fivebox2_A1 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_fivebox2_A1$answerbox <- "box one"

df_fivebox2_A2 <- df_fivebox2_A2 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_fivebox2_A2$answerbox <- "box two"

df_fivebox2_A3 <- df_fivebox2_A3 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_fivebox2_A3$answerbox <- "box three"

df_fivebox2_A4 <- df_fivebox2_A4 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_fivebox2_A4$answerbox <- "box four"

df_fivebox2_A5 <- df_fivebox2_A5 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_fivebox2_A5$answerbox <- "box five"

df_fivebox2 <- rbind(df_fivebox2_A1, df_fivebox2_A2, df_fivebox2_A3, df_fivebox2_A4, df_fivebox2_A5)

df_fivebox <- merge(df_fivebox, df_fivebox2)
df_fivebox <- df_fivebox %>% 
  select(-c(starts_with("new_lab")))
rm(df_fivebox2, df_fivebox_label1,
   df_fivebox2_A1, df_fivebox2_A2, df_fivebox2_A3, df_fivebox2_A4, df_fivebox2_A5)


############ tenbox
#######

############
# one-hot encoding to put labels into right format for python
df_tenbox_label1 <- predict(dummyVars(~ new_label_1, df_tenbox), newdata = df_tenbox)
colnames(df_tenbox_label1) <- gsub("new_label_1", "", colnames(df_tenbox_label1))
df_tenbox_label1 <- as.data.frame(df_tenbox_label1)
df_tenbox_label1$label <- "One"
df_tenbox_label1$lfdn <- df_tenbox$lfdn
df_tenbox_label1$answerbox <- df_tenbox$answerbox



### for MultiCLASS Single Label
df_tenbox_single <- df_tenbox %>% 
  #   filter(is.na(new_label_2)) %>% 
  select(-c(new_label_2, new_label_3))
df_tenbox_single$new_label_1[which(is.na(df_tenbox_single$new_label_1))] <- "nonresponse"

df_tenbox2 <- df_tenbox_label1
df_tenbox2_A1 <- df_tenbox2 %>% 
  filter(answerbox=="box one")
df_tenbox2_A2 <- df_tenbox2 %>% 
  filter(answerbox=="box two")
df_tenbox2_A3 <- df_tenbox2 %>% 
  filter(answerbox=="box three")
df_tenbox2_A4 <- df_tenbox2 %>% 
  filter(answerbox=="box four")
df_tenbox2_A5 <- df_tenbox2 %>% 
  filter(answerbox=="box five")
df_tenbox2_A6 <- df_tenbox2 %>% 
  filter(answerbox=="box six")
df_tenbox2_A7 <- df_tenbox2 %>% 
  filter(answerbox=="box seven")
df_tenbox2_A8 <- df_tenbox2 %>% 
  filter(answerbox=="box eight")
df_tenbox2_A9 <- df_tenbox2 %>% 
  filter(answerbox=="box nine")
df_tenbox2_A10 <- df_tenbox2 %>% 
  filter(answerbox=="box ten")

df_tenbox2_A1 <- df_tenbox2_A1 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_tenbox2_A1$answerbox <- "box one"

df_tenbox2_A2 <- df_tenbox2_A2 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_tenbox2_A2$answerbox <- "box two"

df_tenbox2_A3 <- df_tenbox2_A3 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_tenbox2_A3$answerbox <- "box three"

df_tenbox2_A4 <- df_tenbox2_A4 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_tenbox2_A4$answerbox <- "box four"

df_tenbox2_A5 <- df_tenbox2_A5 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_tenbox2_A5$answerbox <- "box five"

df_tenbox2_A6 <- df_tenbox2_A6 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_tenbox2_A6$answerbox <- "box six"

df_tenbox2_A7 <- df_tenbox2_A7 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_tenbox2_A7$answerbox <- "box seven"

df_tenbox2_A8 <- df_tenbox2_A8 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_tenbox2_A8$answerbox <- "box eight"

df_tenbox2_A9 <- df_tenbox2_A9 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_tenbox2_A9$answerbox <- "box nine"

df_tenbox2_A10 <- df_tenbox2_A10 %>%
  mutate_all(~na_if(., NA) %>% replace_na(0)) %>% 
  group_by(lfdn) %>%
  summarize(rest = max(rest),
            nonresponse = max(nonresponse),
            time_references = max(time_references),
            life_event = max(life_event),
            politics_security_society = max(politics_security_society),
            life_situation_living_conditions = max(life_situation_living_conditions),
            financial_situation = max(financial_situation),
            job = max(job),
            health = max(health),
            social_network_surrounding = max(social_network_surrounding))
df_tenbox2_A10$answerbox <- "box ten"

df_tenbox2 <- rbind(df_tenbox2_A1, df_tenbox2_A2, df_tenbox2_A3, df_tenbox2_A4, df_tenbox2_A5,
                    df_tenbox2_A6,df_tenbox2_A7,df_tenbox2_A8,df_tenbox2_A9,df_tenbox2_A10)

df_tenbox <- merge(df_tenbox, df_tenbox2)
df_tenbox <- df_tenbox %>% 
  select(-c(starts_with("new_lab")))
rm(df_tenbox2, df_tenbox_label1, df_tenbox_label2, df_tenbox_label3,
   df_tenbox2_A1, df_tenbox2_A2, df_tenbox2_A3, df_tenbox2_A4, df_tenbox2_A5
   , df_tenbox2_A6, df_tenbox2_A7, df_tenbox2_A8, df_tenbox2_A9, df_tenbox2_A10)

# check if there are multilabel answers in the nonmultilabel setting
df_fivebox$multilabel <- rowSums(df_fivebox[, c("rest","nonresponse", "time_references", 
                                          "life_event", "politics_security_society", "life_situation_living_conditions", 
                                          "financial_situation", "job", "health", 
                                          "social_network_surrounding")])
table(df_fivebox$multilabel)

df_multi_five <- df_fivebox$text[which(df_fivebox$multilabel>1)]

df_fivebox <- df_fivebox %>% 
  filter(!(multilabel!=1)) %>% 
  select(-multilabel)

`%nin%` = Negate(`%in%`)
df_fivebox_single <- df_fivebox_single[df_fivebox_single$text %nin% df_multi_five,]

# check if there are multilabel answers in the nonmultilabel setting
df_threebox$multilabel <- rowSums(df_threebox[, c("rest","nonresponse", "time_references", 
                                                "life_event", "politics_security_society", "life_situation_living_conditions", 
                                                "financial_situation", "job", "health", 
                                                "social_network_surrounding")])
table(df_threebox$multilabel)
df_threebox <- df_threebox %>% 
  filter(!(multilabel!=1)) %>% 
  select(-multilabel)

# check if there are multilabel answers in the nonmultilabel setting
df_tenbox$multilabel <- rowSums(df_tenbox[, c("rest","nonresponse", "time_references", 
                                                "life_event", "politics_security_society", "life_situation_living_conditions", 
                                                "financial_situation", "job", "health", 
                                                "social_network_surrounding")])
table(df_tenbox$multilabel)
df_tenbox <- df_tenbox %>% 
  filter(!(multilabel!=1)) %>% 
  select(-multilabel)

df_fivebox$exp_cond <- "five box"
df_threebox$exp_cond <- "three box"
df_tenbox$exp_cond <- "ten box"

df_fivebox_single$exp_cond <- "five box"
df_threebox_single$exp_cond <- "three box"
df_tenbox_single$exp_cond <- "ten box"

df_threebox_single <- df_threebox_single %>% 
  select(-new_label_4)

df_single <- rbind(df_tenbox_single, df_fivebox_single, df_threebox_single)
df_concat <- rbind(df_tenbox, df_fivebox, df_threebox)
df_concat_text <- df_concat %>% 
  group_by(lfdn) %>% 
  summarize(concat_text = paste(text, collapse = ", "))

df_concat_cats <- df_concat %>% 
  group_by(lfdn) %>% 
  summarize(rest = max(rest),
  time_references = max(time_references),
  life_event = max(life_event),
  politics_security_society = max(politics_security_society),
  life_situation_living_conditions = max(life_situation_living_conditions),
  financial_situation = max(financial_situation),
  job = max(job),
  health = max(health),
  social_network_surrounding = max(social_network_surrounding),
  nonresponse = max(nonresponse))

df_concat2 <- merge(df_concat_cats, df_concat_text)
df_concat2 <- df_concat2 %>% 
  rename(text = concat_text)
df_lfdn_unique <- df_concat %>% 
  group_by(lfdn) %>% 
  summarise(exp_cond = max(exp_cond))
df_concat2$exp_cond <- df_lfdn_unique$exp_cond

df_concatsss <- rowSums(df_concat2[,c("nonresponse",
                               "rest",
                               "time_references",
                               "life_event",
                               "politics_security_society",
                               "life_situation_living_conditions",
                               "financial_situation",
                               "job",
                               "health",
                               "social_network_surrounding")])

min(df_concatsss)

write_csv(df_onebox, "~/bwSyncShare/Multilabel open q/Happy_onebox.csv")
write_csv(df_single, "~/bwSyncShare/Multilabel open q/all_single.csv")
names(df_onebox)

df_concat2 <- select(df_concat2, lfdn, text, rest, nonresponse, time_references, life_event,
                    politics_security_society, life_situation_living_conditions, 
                    financial_situation, job, health, social_network_surrounding)
names(df_concat2)

write_csv(df_concat2, "~/bwSyncShare/Multilabel open q/all_concat.csv")

head(df_onebox)

### descriptive stats for Table 1 and 2
df_onebox %>%
  summary()

table(df_onebox$social_network_surrounding)
table(df_onebox$rest)
table(df_onebox$nonresponse)
table(df_onebox$time_references)
table(df_onebox$life_event)
table(df_onebox$politics_security_society)
table(df_onebox$life_situation_living_conditions)
table(df_onebox$financial_situation)
table(df_onebox$job)
table(df_onebox$health)

table(row_sums <- rowSums(df_onebox[c("social_network_surrounding", "rest", "nonresponse", "time_references", "life_event", "politics_security_society", "life_situation_living_conditions", "financial_situation","job", "health" ) ]))


table(df_single$new_label_1)
