load("experimental_data.Rdat")

plot.ablines <- function() {
    abline(h=750)
    abline(h=750*1.15,lty=3)
    abline(h=750*0.85,lty=3,col="red")
    
    abline(v=50.5,lty=3)
    abline(v=100.5,lty=3)
    abline(v=150.5,lty=3)
}

plot_rt <- function(plotdat)
{
    

    plot(plotdat$Trial_Nr, plotdat$mRT, type="b", ylim=c(min(plotdat$mRT)-10,max(plotdat$mRT)+10), xlab="Trials", ylab="Mean Response Time [ms]", main="Response Times", col = "indianred1")
    points(experimental_data$Trial_Nr, experimental_data$mRT, type="b", ylim=c(min(experimental_data$mRT)-10,max(experimental_data$mRT)+10), xlab="Trials", ylab="Mean Response Time [ms]", main="Response Times", col = "cadetblue2", pch = 3)
    
    plot.ablines()
    for (i in 1:4) 
    {
        lines(lowess(plotdat$Trial_Nr[plotdat$Cond==i], plotdat$mRT[plotdat$Cond==i]),lwd=2,col="red")
        lines(lowess(experimental_data$Trial_Nr[experimental_data$Cond==i], experimental_data$mRT[experimental_data$Cond==i]),lwd=2,col="blue")
    }
    plot.ablines()
    legend('topleft', 'top', c("Simulated Data", "Experimental Data"), col=c('red','blue'), lty=c(1,1), bty = 'n', cex = 0.6)
    
}

plot_sd <- function(plotdat)
{
    
    plot(plotdat$Trial_Nr, plotdat$sdRT, type="b", ylim=c(min(plotdat$sdRT)-10,max(plotdat$sdRT)+10), xlab="Trials", ylab="Mean Response Time SD [ms]", main="Response Times SD", col = "indianred1")
    points(experimental_data$Trial_Nr, experimental_data$sdRT, type="b", ylim=c(min(experimental_data$sdRT)-10,max(experimental_data$sdRT)+10), xlab="Trials", ylab="Mean Response Time SD [ms]", main="Response Times SD", col = "cadetblue2", pch = 3)
    
    plot.ablines()
    for (i in 1:4) 
    {
        lines(lowess(plotdat$Trial_Nr[plotdat$Cond==i], plotdat$sdRT[plotdat$Cond==i]),lwd=2,col="red")
        lines(lowess(experimental_data$Trial_Nr[experimental_data$Cond==i], experimental_data$sdRT[experimental_data$Cond==i]),lwd=2,col="blue")
    }
    plot.ablines()
    legend('topleft', 'top', c("Simulated Data", "Experimental Data"), col=c('red','blue'), lty=c(1,1), bty = 'n', cex = 0.6)
    
}

plot_mScore <- function(plotdat)
{
    
    plot(plotdat$Trial_Nr, plotdat$mScore, type="b", ylim=c(min(plotdat$mScore) - 10, max(plotdat$mScore) + 10), xlab="Trials", ylab="Mean Score", main="Mean Scores")
    for (i in 1:4) {lines(lowess(plotdat$Trial_Nr[plotdat$Cond==i], plotdat$mScore[plotdat$Cond==i]),lwd=2,col="red")}
    plot.ablines()
    
}

plot_mCurPoints <- function(plotdat)
{
    
    plot(plotdat$Trial_Nr, plotdat$mCurPoints, type="b", ylim=c(min(plotdat$mCurPoints) - 10, max(plotdat$mCurPoints) + 10), xlab="Trials", ylab="Mean Point Increase", main="Mean Point Increase")
    plot.ablines()
    for (i in 1:4) {lines(lowess(plotdat$Trial_Nr[plotdat$Cond==i], plotdat$mCurPoints[plotdat$Cond==i]),lwd=2,col="red")}
    plot.ablines()
    abline(h=5, lty=3)
    abline(h=-5, lty=3)
    abline(h=-25, lty=3)
    axis(side = 2, at = c(-25, -5, 5))

}

plot_mCorrect <- function(plotdat)
{
    
    plot(plotdat$Trial_Nr, plotdat$mCorrect, type="b", ylim=c(min(plotdat$mCorrect) - 0.5, max(plotdat$mCorrect) + 0.5), xlab="Trials", ylab="Proportion of Correct Trials", main="Proportion of Correct Trials", col = "indianred1")
    points(experimental_data$Trial_Nr, experimental_data$mCorrect, type="b", ylim=c(min(experimental_data$mCorrect)-10,max(experimental_data$mCorrect)+10), col = "cadetblue2", pch = 3)
    
    plot.ablines()
    for (i in 1:4) 
    {
        lines(lowess(plotdat$Trial_Nr[plotdat$Cond==i], plotdat$mCorrect[plotdat$Cond==i]),lwd=2,col="red")
        lines(lowess(experimental_data$Trial_Nr[experimental_data$Cond==i], experimental_data$mCorrect[experimental_data$Cond==i]),lwd=2,col="blue")
    }
    plot.ablines()
    legend('topleft', 'top', c("Simulated Data", "Experimental Data"), col=c('red','blue'), lty=c(1,1), bty = 'n', cex = 0.6)
    
    
    
    
    
}