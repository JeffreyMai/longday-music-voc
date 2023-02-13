library(lme4)
library(lmerTest)

#predicting canonical infant vocalizations
CC_model_duration_log <-  lmer(Child_C_log ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T + Adult_Lyrical_N + Adult_NoMusic_N + Adult_NoMusic_T + Adult_NonLyrical_N +Adult_NonLyrical_T, data = Descriptives_Table)
summary(CC_model_duration_log)
CC_model_count <- glmer(Child_C_count ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T + Adult_Lyrical_N + Adult_NoMusic_N + Adult_NoMusic_T + Adult_NonLyrical_N +Adult_NonLyrical_T, data = Descriptives_Table, family = poisson)
summary(CC_model_count)

#predicting infant laughter
CL_model_duration_log <- lmer(Child_L_log ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T + Adult_Lyrical_N + Adult_NoMusic_N + Adult_NoMusic_T + Adult_NonLyrical_N +Adult_NonLyrical_T, data = Descriptives_Table)
summary(CL_model_duration_log)
CL_model_count <- glmer(Child_L_count ~ (1|Child_ID) + Child_Age + Adult_Lyrical_T + Adult_Lyrical_N + Adult_NoMusic_N + Adult_NoMusic_T + Adult_NonLyrical_N +Adult_NonLyrical_T, data = Descriptives_Table, family = poisson)
summary(CL_model_count)

#predicting directed singing
ALT_model <- lmer(Adult_Lyrical_T_log ~ (1|Child_ID) + Child_Age + Child_L + Child_R + Child_C + Child_X, data = Descriptives_Table)
summary(ALT_model)

#predicting nondirected singing
ALN_model <- lmer(Adult_Lyrical_N_log ~ (1|Child_ID) + Child_Age + Child_L + Child_R + Child_C + Child_X, data = Descriptives_Table)
summary(ALN_model)

#predicting directed speech
ANMT_model <-lmer(Adult_NoMusic_T_log ~ (1|Child_ID) + Child_Age + Child_L + Child_R + Child_C + Child_X, data = Descriptives_Table)
summary(ANMT_model)

