#In this script, want to test creating a simple Earth orbiting the Sun simulation

library(dplyr)
library(ggplot2)


#Define params 
M_S <- 2e30
M_E <- 6e24
G <- 6.67e-11

#Initial earth position is (in a 2D plane) 'on the right of the Sun'
x <- 1.5e11
y <- 0   

#Initial velocity of Earth (perpendicular to Earth-Sun line)
vx <- 0
vy <- 30000


#Want to update each day. 
dt <- 60*60*24
T <- 60*60*24*365

day <- c()
x_positions <- c()
y_positions <- c()


#For each day
for (i in 1:(T/dt)) {

    r <- sqrt(x^2 + y^2)

    #Magnitude of Gravitational Force on Earth  
    F <- G*M_E*M_S/r^2

    #X-component of Force - F*cos(theta) = F*x/r
    Fx <- -F*x/r 
    
    #Y-component of Force - F*sin(theta) = F*y/r
    Fy <- -F*y/r


    #Update velocities: 
    vx <- vx + (Fx/M_E)*dt 
    vy <- vy + (Fy/M_E)*dt 


    #Update positions: 
    x <- x + vx*dt 
    y <- y + vy*dt 

    day[i] <- i
    x_positions[i] <- x 
    y_positions[i] <- y

}


#Turn into DF 
earth_orbit_df <- data.frame(day = day, x = x_positions, y = y_positions) %>%
    as_tibble()



ggplot(earth_orbit_df, aes(x = x, y = y, colour = day)) +
    geom_point() +
    geom_point(x = 0, y = 0, color = 'red', size = 10) +
    theme_bw() +
    xlab("X (m)") +
    ylab("Y (m)")


