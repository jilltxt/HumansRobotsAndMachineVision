# Making a set of plots using par() and base R instead of facet_wrap:
# 

# par() is a base R way of putting multiple plots in a grid.
# I'm using it cos I can't work out how to use ggplot to run the same 
# thing over multiple columns while removing NAs but only for an individual graph.
# And actually this is pretty efficient.
# mar() is the margins - mar(bottom, left, top, right) for each plot.

par(mfrow=c(4,1), mar=c(4,10,1,1), cex=0.7)
barplot(table(Situations$target, Situations$Genre), col = c("dark gray", "orange"))
barplot(table(Situations$target, Situations$Entity), col = c("dark gray", "orange"), las=1, horiz=TRUE)
barplot(table(Situations$target, Situations$Technology), col = c("dark gray", "orange"), las=1, horiz=TRUE)
barplot(table(Situations$target, Situations$target), col = c("dark gray", "orange"), las=1, horiz=TRUE)
legend(x="bottom", y = "right", ncol=2,legend=c("Passive","Active"),
       fill=c("dark gray","orange"), title="Actions are")
