### exploting ggplot2
# install.packages("ggplot2")
library(ggplot2)

data(mtcars, package="datasets")
str(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
str(mtcars)
plot(mpg ~hp, data = mtcars, col=cyl)

# make first ggplot
ggplot(data =mtcars, mapping = aes(x=hp, y=mpg, color=cyl, size = mtcars$wt))+
  geom_point()

ggplot(data =mtcars, mapping = aes(x=hp, y=mpg, color=cyl))+
  geom_point()+
  geom_smooth(method = "lm", aes(fill=cyl))+
  geom_smooth(method = "loess", color = "black", se=FALSE)+
  theme_bw()

ggplot(data =mtcars, mapping = aes(x=hp, y=mpg, color=cyl))+
  geom_point()+
  geom_smooth(method = "lm", aes(fill=cyl))+
  facet_wrap(~cyl)+
  theme_bw()
