library(survival)
library(survminer)

# UPLOAD, FACTORIZATION AND ATTACHMENT
topi <- read.table("topi.dat", header=TRUE)
names(topi)
str(topi)
topi$group <- as.factor(topi$group)
topi$days1 <- topi$days - 100
days1 = days - 100
attach(topi)

# NON PARAMETRIC ANALYSIS
S_Hat <- survfit(Surv(days,status)~1)
summary(S_Hat)
plot(S_Hat, ylab = "S(t)", xlab="t (giorni)", conf.int = TRUE, lty = 1)
ggsurvplot(S_Hat, data = topi, conf.int = TRUE, pval = TRUE, pval.method = TRUE, log.rank.weights = "1")

# IC COMPARISON FOR THE ESTIMATED SURVIVAL CURVE
plot(S_Hat, ylab = "S(t)", xlab="t(giorni)")
S_Hat_loglog <- survfit(Surv(days,status)~1, conf.type="log-log")
lines(S_Hat_loglog, col=2)
S_Hat_wald <- survfit(Surv(days,status)~1, conf.type="plain")
lines(S_Hat_wald, col=3)
legend("bottomleft", c("IC log", "IC loglog", "Wald"), col = c("black", "red","green"), lwd = 2)

# INCLUDING GROUP REGRESSOR
S_Hat_Group <- survfit(Surv(days1,status)~group)
summary(S_Hat_Group)
plot(S_Hat_Group, col=c(1,2), lwd = 2, ylab="S(t)", xlab="t(giorni)")
legend("bottomleft", c("gruppo 0", "gruppo 1"), col=c("black","red"))
ggsurvplot(S_Hat_Group, data = topi, conf.int = FALSE, pval = TRUE, pval.method = TRUE, log.rank.weights = "1") # molto meglio

LOG-RANK AND PETO-PETO TEST
survdiff(Surv(days1,status)~group)
survdiff(Surv(days,status)~group, rho=1) 

# PARAMETRIC MODELS AND PARAMETERS ESTIMATION
ripar_exp = function(coef){
  lambda <-  exp(- coef[1]) 
  gamma <- - coef[-1]
  return(c(lambda,gamma))
}

ripar_wei = function(coef, sigma){
  mu <- coef[1]
  beta <- coef[-1]
  alpha <-  1/sigma 
  lambda <- exp(-mu/sigma)
  gamma <- -beta/sigma
  return(c(alpha,lambda,gamma))
}

# WEIBULL MODEL, NO REGRESSORS
Mod_Weibull<-survreg(Surv(days1,status)~1)
summary(Mod_Weibull)

# WEIBULL PARAMETERS ESTIMATION AND ADAPTATION
alpha_modwei = par[1] # alpha
lambda_modwei = par[2] # lambda
alpha_modwei
lambda_modwei
S_Hat <- survfit(Surv(days1,status)~1)
plot(S_Hat, conf.int = FALSE, lwd = 2)
curve(exp(-lambda_modwei*t^alpha), 0, max(days1), xname="t", lwd = 2, col = "purple", add = TRUE)

# EXPONENTIAL MODEL, NO REGRESSORS
Mod_Exp<-survreg(Surv(days1,status)~1, scale=1)
summary(Mod_Exp)
Mod_Exp$coefficients

# EXPONENTIAL PARAMETERS ESTIMATION AND ADAPTATION
lambda_modexp <- ripar_exp(Mod_Exp$coefficients)   
lambda_modexp
S_Hat = survfit(Surv(days1,status)~1)
plot(S_Hat, conf.int = FALSE, lwd = 2)
curve(exp(-lambda_modexp*t), 0, max(days1), xname="t", ylab="S(t)", xlab="t(giorni)", lwd=2, col="dark green", add = TRUE)