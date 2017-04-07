############################################################################
# DS250 - Introduction to Data Science
# Lab7  - Data Visualization

# install.packages("ggplot2")
# install.packages("maps")
# install.packages("ggmap")
# install.packages("ggplot2movies")

require(ggplot2)
require(maps)
require(ggmap)
require(ggplot2movies)

############################################################################
# 1 - Bar Charts
# Prep data from movies data set
# Review data in the package
data(package="ggplot2movies")

d1 <-data.frame(movies[movies$Action==1, c("budget", "Short", "year")])
d1$Type <- "Animation"
d2 <-data.frame(movies[movies$Animation==1, c("budget", "Short", "year")])
d2$Type <- "Animation"
d3 <-data.frame(movies[movies$Comedy==1, c("budget", "Short", "year")])
d3$Type <- "Comedy"
d4 <-data.frame(movies[movies$Drama==1, c("budget", "Short", "year")])
d4$Type <- "Drama"
d5 <-data.frame(movies[movies$Documentary==1, c("budget", "Short", "year")])
d5$Type <- "Documentary"
d6 <-data.frame(movies[movies$Romance==1, c("budget", "Short", "year")])
d6$Type <- "Romance"
myMovieData <- rbind(d1, d2, d3, d4, d5, d6)
names(myMovieData) <- c("Budget", "Short", "Year", "Type" )

qplot(Type, data=myMovieData , geom="bar", fill=Type)

############################################################################
# 2 - Line Chart
# Economics data set
head(economics)

qplot(date, unemploy, data=economics, geom="line")

############################################################################
#3 - Using Maps 

#a Cities
data(us.cities)
big_cities <- subset(us.cities, pop > 500000)
qplot(long, lat, data = big_cities) + borders("state", size = 0.5)

#b California
ca_cities <- subset(us.cities, country.etc == "CA")
ggplot(ca_cities, aes(long, lat)) + borders(database="county", regions="california", color = "grey70") + geom_point()

#c
data(world.cities)
capitals <- subset(world.cities, capital == 1)
ggplot(capitals, aes(long, lat)) + borders("world", fill="lightblue", col="cornflowerblue") + geom_point(aes(size = pop),col="darkgreen")

############################################################################
#4 Using Google Maps

#a
data(us.cities)
state.capitals <- subset(us.cities, capital == 2)
state.capitals.coord <- state.capitals[,c("long","lat")]

mapUSA <- get_googlemap("USA", scale = 2,zoom = 4, markers=state.capitals.coord)
USAmap <- ggmap(mapUSA, extent = "device")
USAmap + geom_text(aes(x = long, y = lat, label=name), data = state.capitals,hjust=-0.1,size=3.5)

