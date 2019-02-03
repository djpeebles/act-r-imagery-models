packages <- c("here")

for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package, repos="https://www.stats.bris.ac.uk/R/")
        library(package, character.only=TRUE)
    }
}

dat <- read.csv(here("scanning-output.csv"), header=TRUE)

pdf(here("scanning-data-humans.pdf"), width=8.0, height=8.0, onefile=FALSE, paper="special")
plot(dat$Xvariable, dat$Humans, xlim = c(0,20), ylim = c(0.5,2.5), pch=16, cex=1.0, col="black",xlab = "Distance (cm)", ylab = "Response Time (s)", cex.axis=1.3, cex.lab=1.3)
abline(lm(dat$Humans ~ dat$Xvariable))
lm_mod <- lm(dat$Humans ~ dat$Xvariable)
abline(lm_mod)
lm_coef <- round(coef(lm_mod), 3) # extract coefficients
# mtext(bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), adj=1, padj=0)
text(18, 0.5, bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), cex=1.2)
dev.off()

pdf(here("scanning-data-model.pdf"), width=8.0, height=8.0, onefile=FALSE, paper="special")
plot(dat$Xvariable, dat$Model, xlim = c(0,20), ylim = c(0.5,2.5), pch=16, cex=1.0, col="black",xlab = "Distance (cm)", ylab = "Response Time (s)", cex.axis=1.3, cex.lab=1.3)
lm_mod <- lm(dat$Model ~ dat$Xvariable)
abline(lm_mod)
lm_coef <- round(coef(lm_mod), 3) # extract coefficients
# mtext(bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), adj=1, padj=0)
text(18, 0.5, bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), cex=1.2)
dev.off()

