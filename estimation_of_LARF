#Estimation of LARF in R ---- Results as reported in Figure 2

set.seed(10021989)

library(LARF)
library(readstata13)
library(plyr)
library(foreign)

ivdat <- read.dta13("..../data-work/IVdata_final.dta")

ls(ivdat)

ivdat$last_job <- as.numeric(ivdat$last_job)
ivdat$ALG2_total <- as.numeric(ivdat$ALG2_total)
ivdat$LEH_total <- as.numeric(ivdat$LEH_total)
ivdat$emp_total <- as.numeric(ivdat$emp_total)
ivdat$unemp_total <- as.numeric(ivdat$unemp_total)
ivdat$almp_total <- as.numeric(ivdat$almp_total)
ivdat$LEH_total <- as.numeric(ivdat$LEH_total)

table(ivdat$D3)
table(ivdat$Z)

#######################################################################
#LARF estimates for count outcome
# Falsification test (before wave 1)
summary(m_num_0 <- larf(Y_t_w0 ~ ALG2_total + ALG2_m_dur + LEH_total + LEH_m_dur + emp_total + emp_m_dur + 
                unemp_total + unemp_m_dur +  CITIZ + diff_empyer + male +
                last_job + west + age + schuleFitz ,
                treatment = ivdat$D3,
                instrument =  ivdat$Z,
                method = "LS",
                data = ivdat))
# After wave 1 and before wave 2
summary(m_num_1 <- larf(Y_t_w1 ~ ALG2_total + ALG2_m_dur + LEH_total + LEH_m_dur + emp_total + emp_m_dur + 
                unemp_total + unemp_m_dur + male  + CITIZ + diff_empyer + 
                last_job + west + age + schuleFitz + Y_t_w0 ,
                treatment = ivdat$D1,
                instrument =  ivdat$Z,
                method = "LS",
                data = ivdat))
# After wave 2 and before wave 3
summary(m_num_2 <- larf(Y_t_w2 ~ ALG2_total + ALG2_m_dur + LEH_total + LEH_m_dur + emp_total + emp_m_dur + 
                unemp_total + unemp_m_dur + male + CITIZ + diff_empyer + 
                last_job + west + age + schuleFitz + Y_t_w0 ,
                treatment = ivdat$D2,
                instrument =  ivdat$Z,
                method = "LS",
                data = ivdat))
# After wave 3 and before wave 4
summary(m_num_3 <- larf(Y_t_w3 ~ ALG2_total + ALG2_m_dur + LEH_total + LEH_m_dur + emp_total + emp_m_dur + 
                unemp_total + unemp_m_dur + male + CITIZ + diff_empyer + 
                last_job + west + age + schuleFitz + Y_t_w0,
                treatment = ivdat$D3,
                instrument =  ivdat$Z,
                method = "LS",
                data = ivdat))

# LARF estimates for binary outcome
# Falsification test (before wave 1)
summary(m_bin_0 <- larf(Y_b_w0 ~ ALG2_total + ALG2_m_dur + LEH_total + LEH_m_dur + emp_total + emp_m_dur + 
                          unemp_total + unemp_m_dur +  CITIZ + diff_empyer + male +
                          last_job + west + age + schuleFitz  ,
                        treatment = ivdat$D3,
                        instrument =  ivdat$Z,
                        data = ivdat))
# After wave 1 and before wave 2
summary(m_bin_1 <- larf(Y_b_w1 ~ ALG2_total + ALG2_m_dur + LEH_total + LEH_m_dur + emp_total + emp_m_dur + 
                  unemp_total + unemp_m_dur + male + Y_b_w0 + CITIZ + diff_empyer + 
                  last_job + west + age + schuleFitz ,
                treatment = ivdat$D1,
                instrument =  ivdat$Z,
                data = ivdat))
# After wave 2 and before wave 3
summary(m_bin_2 <- larf(Y_b_w2 ~ ALG2_total + ALG2_m_dur + LEH_total + LEH_m_dur + emp_total + emp_m_dur + 
                  unemp_total + unemp_m_dur + male + Y_b_w0 + CITIZ + diff_empyer + 
                  last_job + west + age + schuleFitz ,
                treatment = ivdat$D2,
                instrument =  ivdat$Z,
                data = ivdat))
# After wave 3 and before wave 4
summary(m_bin_3 <- larf(Y_b_w3 ~ ALG2_total + ALG2_m_dur + LEH_total + LEH_m_dur + emp_total + emp_m_dur + 
                  unemp_total + unemp_m_dur + male + Y_b_w0 + CITIZ + diff_empyer + 
                  last_job + west + age + schuleFitz ,
                treatment = ivdat$D3,
                instrument =  ivdat$Z,
                data = ivdat))

# LARF estimates for job search behavior
summary(larf ~ t_n_job ~ ALG2_total + ALG2_m_dur + LEH_total + LEH_m_dur + emp_total + emp_m_dur + 
                unemp_total + unemp_m_dur + male + CITIZ + diff_empyer + 
                last_job + west + age + schuleFitz + Y_t_w0,
                treatment = ivdat$D3,
                instrument =  ivdat$Z,
                method = "LS",
                data = ivdat))

summary(larf(found ~ ALG2_total + ALG2_m_dur + LEH_total + LEH_m_dur + emp_total + emp_m_dur + 
                unemp_total + unemp_m_dur + male + Y_b_w0 + CITIZ + diff_empyer + 
                last_job + west + age + schuleFitz ,
                treatment = ivdat$D3,
                instrument =  ivdat$Z,
                data = ivdat))

# Put coefficients and SEs together and export for later use in Stata
b_se <- m_num_0$coefficients[1]
b_se<-cbind(b_se, m_num_0$SE[1])
b_se<-cbind(b_se, m_num_1$coefficients[1])
b_se<-cbind(b_se, m_num_1$SE[1])
b_se<-cbind(b_se, m_num_2$coefficients[1])
b_se<-cbind(b_se, m_num_2$SE[1])
b_se<-cbind(b_se, m_num_3$coefficients[1])
b_se<-cbind(b_se, m_num_3$SE[1])

b_se<-cbind(b_se, m_bin_0$MargEff[1])
b_se<-cbind(b_se, m_bin_0$MargSE[1])
b_se<-cbind(b_se, m_bin_1$MargEff[1])
b_se<-cbind(b_se, m_bin_1$MargSE[1])
b_se<-cbind(b_se, m_bin_2$MargEff[1])
b_se<-cbind(b_se, m_bin_2$MargSE[1])
b_se<-cbind(b_se, m_bin_3$MargEff[1])
b_se<-cbind(b_se, m_bin_3$MargSE[1])


colnames(b_se) <- c("n0_coef", "n0_SE", "n1_coef", "n1_SE", "n2_coef", "n2_SE", "n3_coef", "n3_SE" ,
                    "b0_coef", "b0_SE", "b1_coef", "b1_SE", "b2_coef", "b2_SE", "b3_coef", "b3_SE" )

ATT_SE <- as.data.frame(mat_A <- matrix(data = b_se, nrow = 8, byrow = TRUE))
#Confidence Intervals
ATT_SE$lb <- ATT_SE$V1 - 1.96*ATT_SE$V2
ATT_SE$ub <- ATT_SE$V1 + 1.96*ATT_SE$V2
colnames(ATT_SE) <- c("b", "se", "lb", "ub")

write.dta(ATT_SE, ".../LARF_SE.dta")
