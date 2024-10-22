
#---------- Données en cercle --------
radius = 1
theta = runif(50, min = 0, max = 2*pi)
x1 = radius * cos(theta)
y1 = radius * sin(theta)
#
radius = 0.8
theta = runif(50, min = 0, max = 2*pi)
x2 = radius * cos(theta)
y2 = radius * sin(theta)

radius = 0.5
theta = runif(50, min = 0, max = 2*pi)
x3 = radius * cos(theta)
y3 = radius * sin(theta)

radius = 0.2
theta = runif(50, min = 0, max = 2*pi)
x4 = radius * cos(theta)
y4 = radius * sin(theta)


plot(x1,y1,col="red", xlab = "", ylab = "")
points(x2,y2,col="green")
points(x3,y3,col="blue")
points(x4,y4,col="black")


# --------- Profondeur de Tukey -----------
library(pBrackets)
x <- c(1, 1.1,1.15, 1.6, 1.7, 1.75 ,1.8, 2,2.1, 2.2, 2.5, 2.8, 3)
y <- rep(2, length(x))

plot(x,y, pch = 19, cex=2, col="black", lwd=2)
points(2.4,2, pch = 19, cex=2, col="red", lwd=2)

# Caote droite
brackets(2.4, 2.1, 3, 2.1, h = NULL,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = 1, lty = 1, xpd = FALSE)
text(x = 2.7, y = 2.3, '#{    >=    }')
points(2.85,2.3, pch = 19, cex=1.5, col="red", lwd=2)
points(2.6,2.3, pch = 19, cex=1.5, col="black", lwd=2)
# Cote gauche
# Caote droite
brackets(1, 2.1, 2.37, 2.1, h = NULL,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = 1, lty = 1, xpd = FALSE)
text(x = 1.68, y = 2.3, '#{    <=    }')
points(1.84,2.3, pch = 19, cex=1.5, col="red", lwd=2)
points(1.6,2.3, pch = 19, cex=1.5, col="black", lwd=2)

# --------- Profondeur Simpliciale ------------
x <- c(0.5, 4, 2, 4 ,4,1,3,3.5, 2,3, 2.5)
y <- c(6, 3, 3.5,3, 4.5, 4,2.5,4.5, 5.2,5.35, 4.8)
plot(x,y, pch = 19, cex=1, col="black", lwd=2, xlim = c(0,5), ylim = c(1.5,7))
x1 <- c(2,3,3.5)
y1 <- c(4,4.5,3.5)
polygon(x1,y1)
points(x1,y1, pch = 19, cex=1, col="red", lwd=2)

x1 <- c(1,3,4.5)
y1 <- c(5,3,5)
polygon(x1,y1)
points(x1,y1, pch = 19, cex=1, col="blue", lwd=2)

x1 <- c(1.5,3,4)
y1 <- c(5.5,3,5.5)
polygon(x1,y1)
points(x1,y1, pch = 19, cex=1, col="green", lwd=2)
points(3,4, pch = 19, cex=1, col="darkslateblue", lwd=2)
text(x = 2.5, y = 6.5, '#{       contenant       }')
points(3.4,6.5, pch = 19, cex=1.5, col="darkslateblue", lwd=2)
points(1.8,6.5, pch = 24, cex=1.5, col="black", lwd=2)


# ---------- curves -------
n <- 50 # number of data points
t <- seq(0,4*pi,,100)
a <- 3
b <- 2
c.unif <- runif(n)
c.norm <- rnorm(n)
amp <- 2

# generate data and calculate "y"

y1 <- a*sin(b*t)+c.unif*amp # uniform error
y2 <- a*sin(2*b*t)+c.unif*amp # Gaussian/normal error
y3 <- sin(b*t)+c.unif*amp # Gaussian/normal error
y4 <- a*sin(0.5*b*t)+c.unif*amp # Gaussian/normal error

# plot results
plot(t, y1, t="l", ylim=range(y1,y2)*c(1,1.2))
#lines(t, y2, col=2)
lines(t, y3, col=3)
lines(t, y4, col=4)
abline(v = 4, col="red", lwd=3, lty=2)






