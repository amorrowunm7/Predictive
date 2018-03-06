library(MASS)
heart <- read.table("SAheart.txt",sep="\t",header=T)
dim(heart)
str(heart)
summary(heart)

null=glm(chd ~ 1,data=heart,family ="binomial")
fwd_model<- stepAIC(null,scope=list(lower= ~ 1,upper= ~ sbp + tobacco +ldl + adiposity + famhist + typea + obesity + alcohol + age),direction="forward")

full=glm(chd ~ sbp + tobacco +ldl + adiposity + famhist + typea + obesity + alcohol + age,data=heart, family ="binomial")
back_model <- stepAIC(full,scope=list(lower= ~ 1,upper= ~ sbp + tobacco +ldl + adiposity + famhist + typea + obesity + alcohol + age,family ="binomial")
