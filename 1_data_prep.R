# setup
library(haven)

df <- read_dta("~/bwSyncShare/Multilabel open q/Dataset_Happy.dta")

df$lhappro_character <- haven::as_factor(df$lhappro_character)
df$lhap_ec <- haven::as_factor(df$lhap_ec)
df$lhap <- haven::as_factor(df$lhap)

df$lhappro_nonresponse <- haven::as_factor(df$lhappro_nonresponse)
# remove hard nonresponses
library(tidyverse)
df <- df %>% 
  filter(lhappro_nonresponse!="hard nonresponse")
table(df$lhappro_nonresponse)

df$lhappro_themes <- haven::as_factor(df$lhappro_themes)
table(df$lhappro_themes)

df$lhappro_character <- haven::as_factor(df$lhappro_character)
table(df$lhappro_character)


df_onebox <- df %>% 
  filter(lhap_ec=="one_box")
df_threebox <- df %>% 
  filter(lhap_ec=="three_boxes")
df_fivebox <- df %>% 
  filter(lhap_ec=="five_boxes")
df_tenbox <- df %>% 
  filter(lhap_ec=="ten_boxes")


df_onebox$text <- df_onebox$lhappro_c1_neu
df_onebox$label_1 <- haven::as_factor(df_onebox$lseh1_1)
df_onebox$label_2 <- haven::as_factor(df_onebox$lseh1_2)
df_onebox$label_3 <- haven::as_factor(df_onebox$lseh1_3)
df_onebox$label_4 <- haven::as_factor(df_onebox$lseh1_4)
df_onebox$label_5 <- haven::as_factor(df_onebox$lseh1_5)
df_onebox$label_6 <- haven::as_factor(df_onebox$lseh1_6)
df_onebox$label_7 <- haven::as_factor(df_onebox$lseh1_7)
df_onebox$label_8 <- haven::as_factor(df_onebox$lseh1_8)
df_onebox$label_9 <- haven::as_factor(df_onebox$lseh1_9)
names(df_onebox)
df_onebox <- df_onebox %>% 
  select(lfdn, lhap_ec, text, starts_with("label_"))

df_threebox <- df_threebox %>% 
  select(lfdn, lhap_ec, lhappro1_c2:lhappro3_c2, lseh2_11:lseh2_32)

df_threebox <- gather(df_threebox, answerbox, text, lhappro1_c2:lhappro3_c2, factor_key=TRUE)
df_threebox <- df_threebox %>% 
  mutate(answerbox = case_when(
    answerbox=="lhappro1_c2" ~ "one",
    answerbox=="lhappro2_c2" ~ "two",
    answerbox=="lhappro3_c2" ~ "three"))

df_threebox_1 <- df_threebox %>% 
  filter(answerbox=="one") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh2_11:Iseh2_14) %>% 
  rename(label_1 = lseh2_11,
         label_2 = lseh2_12,
         label_3 = Iseh2_13,
         label_4 = Iseh2_14)
df_threebox_2 <- df_threebox %>% 
  filter(answerbox=="two") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh2_21:lseh2_22) %>% 
  rename(label_1 = lseh2_21,
         label_2 = lseh2_22) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
df_threebox_3 <- df_threebox %>% 
  filter(answerbox=="three") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh2_31:lseh2_32) %>% 
  rename(label_1 = lseh2_31,
         label_2 = lseh2_32) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
  
df_threebox <- rbind(df_threebox_1,df_threebox_2,df_threebox_3)
rm(df_threebox_1,df_threebox_2,df_threebox_3)


df_fivebox <- df_fivebox %>% 
  select(lfdn, lhap_ec, lhappro1_c3:lhappro5_c3, lseh3_11:lseh3_52)

df_fivebox <- gather(df_fivebox, answerbox, text, lhappro1_c3:lhappro5_c3, factor_key=TRUE)
df_fivebox <- df_fivebox %>% 
  mutate(answerbox = case_when(
    answerbox=="lhappro1_c3" ~ "one",
    answerbox=="lhappro2_c3" ~ "two",
    answerbox=="lhappro3_c3" ~ "three",
    answerbox=="lhappro4_c3" ~ "four",
    answerbox=="lhappro5_c3" ~ "five"))


STOPPED HERE WITH EDITING CODE. CODE BELOW IS COPY PASTE FROM THREEBOX THAT NEEDS TO BE ADAPTED TO FIVEBOX


df_threebox_1 <- df_threebox %>% 
  filter(answerbox=="one") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh2_11:Iseh2_14) %>% 
  rename(label_1 = lseh2_11,
         label_2 = lseh2_12,
         label_3 = Iseh2_13,
         label_4 = Iseh2_14)
df_threebox_2 <- df_threebox %>% 
  filter(answerbox=="two") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh2_21:lseh2_22) %>% 
  rename(label_1 = lseh2_21,
         label_2 = lseh2_22) %>% 
  mutate(label_3 = NA,
         label_4 = NA)
df_threebox_3 <- df_threebox %>% 
  filter(answerbox=="three") %>% 
  select(lfdn, lhap_ec, answerbox, text, lseh2_31:lseh2_32) %>% 
  rename(label_1 = lseh2_31,
         label_2 = lseh2_32) %>% 
  mutate(label_3 = NA,
         label_4 = NA)

df_threebox <- rbind(df_threebox_1,df_threebox_2,df_threebox_3)

