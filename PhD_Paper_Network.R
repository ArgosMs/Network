dat <- PhD_Dataset

dev.off()

par(mfrow=c(1, 4))

# How many

neighbors_many_insurance <- table(dat$NEIGHBORS_INSURANCE)

barplot(main = "INSURANCE", table(dat$NEIGHBORS_INSURANCE))

neighbors_many_improvements <- table(dat$NEIGHBORS_IMPROVEMENTS)

barplot(main = "IMPROVEMENTS", table(dat$NEIGHBORS_IMPROVEMENTS))

neighbors_many_raising <- table(dat$NEIGHBORS_RAISING)

barplot(main = "HOUSE RAISING", table(dat$NEIGHBORS_RAISING))

neighbors_many_relocation <- table(dat$NEIGHBORS_RELOCATION)

barplot(main = "RELOCATION", table(dat$NEIGHBORS_RELOCATION))

# How close

neighbors_close_insurance <- table(dat$CLOSE_NEIGHBORS_INSURANCE)

barplot(main = "INSURANCE", table(dat$CLOSE_NEIGHBORS_INSURANCE))

neighbors_close_improvements <- table(dat$CLOSE_NEIGHBORS_IMPROVEMENTS)

barplot(main = "IMPROVEMENTS", table(dat$CLOSE_NEIGHBORS_IMPROVEMENTS))

neighbors_close_raising <- table(dat$CLOSE_NEIGHBORS_RAISING)

barplot(main = "HOUSE RAISING", table(dat$CLOSE_NEIGHBORS_RAISING))

neighbors_close_relocation <- table(dat$CLOSE_NEIGHBORS_RELOCATION)

barplot(main = "RELOCATION", table(dat$CLOSE_NEIGHBORS_RELOCATION))

# Effective

neighbors_effective_insurance <- table(dat$EFFECTIVE_NEIGHBORS_INSURANCE)

barplot(main = "INSURANCE", table(dat$EFFECTIVE_NEIGHBORS_INSURANCE))

neighbors_efffective_improvements <- table(dat$EFFECTIVE_NEIGHBORS_IMPROVEMENTS)

barplot(main = "IMPROVEMENTS", table(dat$EFFECTIVE_NEIGHBORS_IMPROVEMENTS))

neighbors_effective_raising <- table(dat$EFFECTIVE_NEIGHBORS_RAISING)

barplot(main = "RAISING", table(dat$EFFECTIVE_NEIGHBORS_RAISING))

neighbors_effective_relocation <- table(dat$EFFECTIVE_NEIGHBORS_RELOCATION)

barplot(main = "RELOCATION", table(dat$EFFECTIVE_NEIGHBORS_RELOCATION))

# Influence

neighbors_influence_insurance <- table(dat$INFLUENCE_NEIGHBORS_INSURANCE)

barplot(main = "INSURANCE", table(dat$INFLUENCE_NEIGHBORS_INSURANCE))

neighbors_influence_improvements <- table(dat$INFLUENCE_NEIGHBORS_IMPROVEMENTS)

barplot(main = "IMPROVEMENTS", table(dat$INFLUENCE_NEIGHBORS_IMPROVEMENTS))

neighbors_influence_raising <- table(dat$INFLUENCE_NEIGHBORS_RAISING)

barplot(main = "RAISING", table(dat$INFLUENCE_NEIGHBORS_RAISING))

neighbors_influence_relocation <- table(dat$INFLUENCE_NEIGHBORS_RELOCATION)

barplot(main = "RELOCATION", table(dat$INFLUENCE_NEIGHBORS_RELOCATION))

# Regression

logitInsurance_neighbors <- glm(INSURANCE_DEPENDENT ~ 
                                  NEIGHBORS_INSURANCE +
                                  CLOSE_NEIGHBORS_INSURANCE +
                                  EFFECTIVE_NEIGHBORS_INSURANCE +
                                  INFLUENCE_NEIGHBORS_INSURANCE,
                                  data = dat)

summary(logitInsurance_neighbors)

library(car)

vifInsurance 

dev.off()

scatter.smooth(x=dat$INSURANCE_DEPENDENT, y=dat$CLOSE_NEIGHBORS_INSURANCE, xlab = NA, ylab = NA, main="INSURANCE ~ CLOSE")

logitMeasure_neighbors <- glm(MEASURE_DEPENDENT ~ 
                                  NEIGHBORS_IMPROVEMENTS +
                                  CLOSE_NEIGHBORS_IMPROVEMENTS +
                                  EFFECTIVE_NEIGHBORS_IMPROVEMENTS +
                                  INFLUENCE_NEIGHBORS_IMPROVEMENTS,
                                data = dat)

summary(logitMeasure_neighbors)

vifMeasure <- vif(logitMeasure_neighbors)
vifMeasure

scatter.smooth(x=dat$MEASURE_DEPENDENT, y=dat$NEIGHBORS_IMPROVEMENTS, xlab = NA, ylab = NA, main="MEASURE ~ NEIGHBORS")
scatter.smooth(x=dat$MEASURE_DEPENDENT, y=dat$CLOSE_NEIGHBORS_IMPROVEMENTS, xlab = NA, ylab = NA, main="MEASURE ~ CLOSE")

logitRaising_neighbors <- glm(HOUSE_RAISING_DEPENDENT ~ 
                                  NEIGHBORS_RAISING +
                                  CLOSE_NEIGHBORS_RAISING +
                                  EFFECTIVE_NEIGHBORS_RAISING +
                                  INFLUENCE_NEIGHBORS_RAISING,
                                data = dat)

summary(logitRaising_neighbors)

vifRaising <- vif(logitRaising_neighbors)
vifRaising

scatter.smooth(x=dat$HOUSE_RAISING_DEPENDENT, y=dat$CLOSE_NEIGHBORS_RAISING, xlab = NA, ylab = NA, main="RAISING ~ CLOSE")
scatter.smooth(x=dat$HOUSE_RAISING_DEPENDENT, y=dat$INFLUENCE_NEIGHBORS_RAISING, xlab = NA, ylab = NA, main="RAISING ~ INFLUENCE")

logitRelocation_neighbors <- glm(RELOCATION_DEPENDENT ~ 
                                NEIGHBORS_RELOCATION +
                                CLOSE_NEIGHBORS_RELOCATION +
                                EFFECTIVE_NEIGHBORS_RELOCATION +
                                INFLUENCE_NEIGHBORS_RELOCATION,
                              data = dat)

summary(logitRelocation_neighbors)

vifRelocation <- vif(logitRelocation_neighbors)

# Tables for Outputs

library(xtable)

# Regression Outputs

Insurance_table <- xtable(logitInsurance_neighbors)
print.xtable(Insurance_table, type="html", file="PhD_Paper_Network_Insurancetable.html")

House_raising_table <- xtable(logitRaising_neighbors)
print.xtable(House_raising_table, type="html", file="PhD_Paper_Network_Raisingtable.html")

Measure_table <- xtable(logitMeasure_neighbors)
print.xtable(Measure_table, type="html", file="PhD_Paper_Network_Measuretable.html")

# Hosmer-Lemeshow Goodness of Fit

install.packages("ResourceSelection")

library(ResourceSelection)

hoslem.test(mtcars$vs, fitted(model))

hoslem.test(dat$INSURANCE_DEPENDENT, fitted(logitInsurance_neighbors))

# Frequency of Dependent Variables

dat1 <- dat

dependent_insurance <- table(dat1$INSURANCE_DEPENDENT)
dependent_house_raising <- table(dat1$HOUSE_RAISING_DEPENDENT)
dependent_measure <- table(dat1$MEASURE_DEPENDENT)

dependent_combined <- cbind(dependent_insurance, dependent_house_raising, dependent_measure)

dev.off()

barplot(dependent_combined, 
width = 0.53,
mgp=c(-4, .5, -7), # First: ylab location. Second: Tick-mark labels. Third: Tick marks.
cex.main = 1.3,
cex.names = .8,
legend = c("No", "Yes"),
xlim = c(2.5, -.45),               
names.arg = c("Insurance", "House Raising", "Home Improvements"),
ylab = "Frequency", 
main = "Comparative Analysis for Dependent Variables",
las = 1,
args.legend = 
  list("topright",
       bty = "n")) # 1:hor 2:perpendicular to axis 3:vertical

plot(yaxt="n")
axis(2, mgp=c(3, -6, 0))





