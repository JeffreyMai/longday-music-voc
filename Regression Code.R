library(lme4)
library(lmerTest)
library(sjPlot)
library(sjmisc)

#predicting canonical infant vocalizations
CC_model_log <-  lmer(Child_C_log ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = Descriptives_Table)
summary(CC_model_log)
tab_model(CC_model_log)
CC_model_count <- glmer(Child_C_count ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_count + Adult_Lyrical_N_count + Adult_NoMusic_N_count + Adult_NoMusic_T_count + Adult_NonLyrical_N_count + Adult_NonLyrical_T_count, data = Descriptives_Table, family = poisson)
summary(CC_model_count)

CC_model_18_log <- lmer(Child_C_log ~ (1|Child_ID) + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = subset_18)
summary(CC_model_18_log)
CC_model_18_count <- glmer(Child_C_count ~ (1|Child_ID) + Adult_Lyrical_T_count + Adult_Lyrical_N_count + Adult_NoMusic_N_count + Adult_NoMusic_T_count + Adult_NonLyrical_N_count + Adult_NonLyrical_T_count, data = subset_18, family = poisson)
summary(CC_model_18_count)

CC_model_9_18_log <- lmer(Child_C_log ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = subset_9_18)
summary(CC_model_9_18_log)
CC_model_9_18_count <- glmer(Child_C_count ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_count + Adult_Lyrical_N_count + Adult_NoMusic_N_count + Adult_NoMusic_T_count + Adult_NonLyrical_N_count + Adult_NonLyrical_T_count, data = subset_9_18, family = poisson)
summary(CC_model_9_18_count)

#predicting non-canonical infant vocalizations
CX_model_log <-  lmer(Child_X_log ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = Descriptives_Table)
summary(CX_model_log)
CX_model_count <- glmer(Child_X_count ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_count + Adult_Lyrical_N_count + Adult_NoMusic_N_count + Adult_NoMusic_T_count + Adult_NonLyrical_N_count + Adult_NonLyrical_T_count, data = Descriptives_Table, family = poisson)
summary(CX_model_count)

CX_model_18_log <- lmer(Child_X_log ~ (1|Child_ID) + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = subset_18)
summary(CX_model_18_log)
CX_model_18_count <- glmer(Child_X_count ~ (1|Child_ID) + Adult_Lyrical_T_count + Adult_Lyrical_N_count + Adult_NoMusic_N_count + Adult_NoMusic_T_count + Adult_NonLyrical_N_count + Adult_NonLyrical_T_count, data = subset_18, family = poisson)
summary(CX_model_18_count)

CX_model_9_18_log <- lmer(Child_X_log ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = subset_9_18)
summary(CX_model_9_18_log)
CX_model_9_18_count <- glmer(Child_X_count ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_count + Adult_Lyrical_N_count + Adult_NoMusic_N_count + Adult_NoMusic_T_count + Adult_NonLyrical_N_count + Adult_NonLyrical_T_count, data = subset_9_18, family = poisson)
summary(CC_model_9_18_count)

#predicting C/(C+X)
CCX_model_log <- lmer(Child_C_log/(Child_C_log + Child_X_log)~ (1|Child_ID) + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = Descriptives_Table)
summary(CCX_model_log)
CCX_model_count <- lmer(Child_C_count/(Child_C_count + Child_X_count)~ (1|Child_ID) + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = Descriptives_Table, family = poisson)
summary(CCX_model_count)

CCX_model_18_log <- lmer(Child_C_log/(Child_C_log + Child_X_log) ~ (1|Child_ID) + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = subset_18)
summary(CCX_model_18_log)
CCX_model_18_count <- glmer(Child_C_count/(Child_C_count + Child_X_count) ~ (1|Child_ID) + Adult_Lyrical_T_count + Adult_Lyrical_N_count + Adult_NoMusic_N_count + Adult_NoMusic_T_count + Adult_NonLyrical_N_count + Adult_NonLyrical_T_count, data = subset_18, family = poisson)
summary(CCX_model_18_count)

CCX_model_9_18_log <- lmer(Child_C_log/(Child_C_log + Child_X_log)~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = subset_9_18)
summary(CCX_model_9_18_log)
CCX_model_9_18_count <- glmer((Child_C_count/(Child_C_count + Child_X_count)) ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_count + Adult_Lyrical_N_count + Adult_NoMusic_N_count + Adult_NoMusic_T_count + Adult_NonLyrical_N_count +Adult_NonLyrical_T_count, data = subset_9_18, family = poisson)
summary(CCX_model_9_18_count)

#predicting infant laughter
CL_model_log <- lmer(Child_L_log ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = Descriptives_Table)
summary(CL_model_log)
tab_model(CL_model_log)
CL_model_count <- glmer(Child_L_count ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_count + Adult_Lyrical_N_count + Adult_NoMusic_N_count + Adult_NoMusic_T_count + Adult_NonLyrical_N_count +Adult_NonLyrical_T_count, data = Descriptives_Table, family = poisson)
summary(CL_model_count)

#predicting infant reflexive cry
CR_model_log <- lmer(Child_R_log ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = Descriptives_Table)
summary(CR_model_log)
CR_model_count <- glmer(Child_R_count ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_count + Adult_Lyrical_N_count + Adult_NoMusic_N_count + Adult_NoMusic_T_count + Adult_NonLyrical_N_count +Adult_NonLyrical_T_count, data = Descriptives_Table, family = poisson)
summary(CR_model_count)

#predicting directed singing
ALT_model_log <- lmer(Adult_Lyrical_T_log ~ (1|Child_ID) + Child_Age + Child_C_log + Child_X_log + Child_L_log + Child_R_log, data = Descriptives_Table)
summary(ALT_model_log)
tab_model(ALT_model_log)
ALT_model_count <- glmer(Adult_Lyrical_T_count ~ (1|Child_ID) + Child_Age + Child_C_count + Child_X_count + Child_L_count + Child_R_log, data= Descriptives_Table, family = poisson)
summary(ALT_model_count)

#predicting non-directed singing
ALN_model_log <- lmer(Adult_Lyrical_N_log ~ (1|Child_ID) + Child_Age + Child_C_log + Child_X_log + Child_L_log + Child_R_log, data = Descriptives_Table)
summary(ALN_model_log)
ALN_model_count <- glmer(Adult_Lyrical_N_count ~ (1|Child_ID) + Child_Age + Child_C_count + Child_X_count + Child_L_count + Child_R_log, data= Descriptives_Table, family = poisson)
summary(ALN_model_count)

#predicting directed speech
ANMT_model_log <-lmer(Adult_NoMusic_T_log ~ (1|Child_ID) + Child_Age + Child_C_log + Child_X_log + Child_L_log + Child_R_log, data = Descriptives_Table)
summary(ANMT_model_log)
tab_model(ANMT_model_log)
ANMT_model_count <- glmer(Adult_NoMusic_T_count ~ (1|Child_ID) + Child_Age + Child_C_count + Child_X_count + Child_L_count + Child_R_log, data= Descriptives_Table, family = poisson)
summary(ANMT_model_count)

#predicting non-directed speech
ANMN_model <-lmer(Adult_NoMusic_N_log ~ (1|Child_ID) + Child_Age + Child_C_log + Child_X_log + Child_L_log + Child_R_log, data = Descriptives_Table)
summary(ANMN_model)
ANMN_model_count <- glmer(Adult_NoMusic_N_count ~ (1|Child_ID) + Child_Age + Child_C_count + Child_X_count + Child_L_count + Child_R_log, data= Descriptives_Table, family = poisson())
summary(ANMN_model_count)


