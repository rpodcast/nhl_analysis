# load hockey data analysis project

library(ProjectTemplate)
load.project()

#-------------------------------------------------------------------------------------
# create combination of box plot, violin plot using ggplot2 to visualize attendance
# numbers
# - subset to only include games not played in outdoor stadiums and years 1994-2012
#-------------------------------------------------------------------------------------

plot.data <- subset(reg.game, subset=(attendance < 30000 & year >= 1994))

library(ggplot2)

myplot <- ggplot(data=plot.data, aes(x=home.team, y=attendance))

myplot

summary(myplot)

myplot + geom_boxplot(aes(fill=home.conference), width=0.3, outlier.colour=NA)

last_plot() + coord_cartesian(ylim=c(10000, 23000))

last_plot() + theme_bw()

last_plot() + labs(x="Team", y="Attendance", fill="Conference")

last_plot() +  opts(title = "Distributions of Game Attendance",
                    plot.title = theme_text(size=20),
                    legend.position="top",
                    legend.title = theme_text(size=20),
                    legend.text = theme_text(size=12),
                    axis.text.x = theme_text(angle=90, size=12),
                    axis.title.x = theme_text(size=20),
                    axis.text.y = theme_text(size=12),
                    axis.title.y = theme_text(angle=90, size=16),
                    strip.text.y = theme_text(angle=90, size=12))

last_plot() + facet_grid(era.ind ~ ., margins=FALSE)

myplot.labs <- labs(x="Team", y="Attendance", fill="Conference", colour="Conference")
myplot.theme <- list(theme_bw(),
                     opts(title = "Distributions of Game Attendance"),
                     opts(plot.title = theme_text(size=20)),
                     opts(legend.position="top"),
                     opts(legend.title = theme_text(size=20)),
                     opts(legend.text = theme_text(size=12)),
                     opts(axis.text.x = theme_text(angle=90, size=12)),
                     opts(axis.title.x = theme_text(size=20)),
                     opts(axis.text.y = theme_text(size=12)),
                     opts(axis.title.y = theme_text(angle=90, size=16)),
                     opts(strip.text.y = theme_text(angle=90, size=12)))
myplot.zoom <- coord_cartesian(ylim=c(10000, 23000))
myplot.box <- geom_boxplot(aes(fill=home.conference), width=0.3, outlier.colour=NA)

myplot <- ggplot(data=plot.data, aes(x=home.team, y=attendance))
myplot + myplot.box + myplot.zoom + myplot.labs + myplot.theme + facet_grid(era.ind ~ ., margins=FALSE)
