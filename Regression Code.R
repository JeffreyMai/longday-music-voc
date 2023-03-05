library(lme4)
library(lmerTest)

#predicting canonical infant vocalizations
CC_model_log <-  lmer(Child_C_log ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = Descriptives_Table)
summary(CC_model_log)
CC_model_count <- glmer(Child_C_count ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_count + Adult_Lyrical_N_count + Adult_NoMusic_N_count + Adult_NoMusic_T_count + Adult_NonLyrical_N_count + Adult_NonLyrical_T_count, data = Descriptives_Table, family = poisson)
summary(CC_model_count)

#predicting non-canonical infant vocalizations
CX_model_log <-  lmer(Child_X_log ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = Descriptives_Table)
summary(CX_model_log)
CX_model_count <- glmer(Child_X_count ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_count + Adult_Lyrical_N_count + Adult_NoMusic_N_count + Adult_NoMusic_T_count + Adult_NonLyrical_N_count + Adult_NonLyrical_T_count, data = Descriptives_Table, family = poisson)
summary(CX_model_count)

#predicting infant laughter
CL_model_log <- lmer(Child_L_log ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T_log + Adult_Lyrical_N_log + Adult_NoMusic_N_log + Adult_NoMusic_T_log + Adult_NonLyrical_N_log +Adult_NonLyrical_T_log, data = Descriptives_Table)
summary(CL_model_log)
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
ANMT_model_count <- glmer(Adult_NoMusic_T_count ~ (1|Child_ID) + Child_Age + Child_C_count + Child_X_count + Child_L_count + Child_R_log, data= Descriptives_Table, family = poisson)
summary(ANMT_model_count)

#predicting non-directed speech
ANMN_model <-lmer(Adult_NoMusic_N_log ~ (1|Child_ID) + Child_Age + Child_C_log + Child_X_log + Child_L_log + Child_R_log, data = Descriptives_Table)
summary(ANMN_model)
ANMN_model_count <- glmer(Adult_NoMusic_N_count ~ (1|Child_ID) + Child_Age + Child_C_count + Child_X_count + Child_L_count + Child_R_log, data= Descriptives_Table, family = poisson())
summary(ANMN_model_count)

