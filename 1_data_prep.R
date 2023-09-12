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
    answerbox=="lhappro3_c2" ~ "three")) %>% 

df_threebox_1 <- df_threebox %>% 
  filter(answerbox=="one")
  
  
  
df_threebox3 <- gather(df_threebox, answerbox, text, lhappro1_c2:lhappro3_c2, factor_key=TRUE)


names(df_threebox2)
table(df_threebox2$answerbox)

df_threebox$label_1 <- haven::as_factor(df_threebox$lseh1_1)
df_threebox$label_2 <- haven::as_factor(df_threebox$lseh1_2)
df_threebox$label_3 <- haven::as_factor(df_threebox$lseh1_3)
df_threebox$label_4 <- haven::as_factor(df_threebox$lseh1_4)
df_threebox$label_5 <- haven::as_factor(df_threebox$lseh1_5)
df_threebox$label_6 <- haven::as_factor(df_threebox$lseh1_6)
df_threebox$label_7 <- haven::as_factor(df_threebox$lseh1_7)
df_threebox$label_8 <- haven::as_factor(df_threebox$lseh1_8)
df_threebox$label_9 <- haven::as_factor(df_onebox$lseh1_9)
names(df_onebox)

lseh2_11"                   "lseh2_12"                  
 [57] "Iseh2_13"                   "Iseh2_14"                   "lseh2_21"                   "lseh2_22"                  
 [61] "lseh2_31"                   "lseh2_32"





names(df)

<- haven::as_factor(df$lhappro_c1_neu)


df$lhappro1_c2


lhappro2_c2
lhappro3_c2