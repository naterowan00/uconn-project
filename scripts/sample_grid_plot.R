################
### Script from the Exploring Baseball Data with R script
###############

# setup

library(tidyverse)
sc2017 <- read_csv("statcast2017.csv")
sc2017 %>% filter(type == "X") %>%
  mutate(Count = paste(balls, strikes, sep="-"),
         ball_strikes = balls + strikes) -> sc2017_ip

sc2017_ip <- mutate(sc2017_ip,
                    adj_plate_x = ifelse(stand == "L",
                                         -plate_x, plate_x))

# theme for title in ggplot graphic

TH <- theme(
  plot.title = element_text(
    colour = "blue",
    size = 18,
    hjust = 0.5,
    vjust = 0.8,
    angle = 0
  )
)

# count effects

sc2017_ip %>% group_by(Count) %>% 
  summarize(M_LS = mean(launch_speed, na.rm = TRUE),
            S_LS = sd(launch_speed, na.rm = TRUE),
            M_LA = mean(launch_angle, na.rm = TRUE),
            S_LA = sd(launch_angle, na.rm = TRUE),
            Balls_Strikes = first(ball_strikes)) ->
  S

ggplot(S, aes(Balls_Strikes, M_LS, label=Count)) +
  geom_hline(yintercept = 
               mean(sc2017_ip$launch_speed, na.rm = TRUE),
             color="red") + TH +
  geom_label(fill = "yellow", color= "blue") +
  xlab("Count of Balls and Strikes") +
  ylab("Mean Launch Velocity") +
  ggtitle("Mean Launch Speed by Count")

ggplot(S, aes(Balls_Strikes, M_LA, label=Count)) +
  TH +
  geom_hline(yintercept = 
               mean(sc2017_ip$launch_angle, na.rm = TRUE),
             color="red") +
  geom_label(fill = "yellow", color= "blue") +
  xlab("Count of Balls and Strikes") +
  ylab("Mean Launch Angle") +
  ggtitle("Mean Launch Angle by Count")

# location effects

# this function will construct a graph of average launch
#  speed (or average launch angle) by zone location
# sc is the Statcast data and ngrid is the size of the grid

myplot <- function(sc, ngrid = 10, LA = FALSE){
  scnew <- filter(sc, adj_plate_x > -1.5,
                  adj_plate_x <  1.5,
                  plate_z > 1, plate_z < 4)
  scnew$cx <- cut(scnew$adj_plate_x, 
                  seq(-1.5, 1.5, length.out = ngrid))
  scnew$cz <- cut(scnew$plate_z, 
                  seq(1, 4, length.out = ngrid))
  scnew %>% group_by(cx, cz) %>% 
    summarize(N = n(),
              Mean_Launch_Speed = mean(launch_speed, na.rm=TRUE),
              Mean_Launch_Angle = mean(launch_angle, na.rm=TRUE)) -> S1
  
  grid_x <- seq(-1.5, 1.5, length.out = ngrid)
  mid_x <- (grid_x[-ngrid] + grid_x[-1]) / 2
  grid_z <- seq(1, 4, length.out = ngrid)
  mid_z <- (grid_z[-ngrid] + grid_z[-1]) / 2
  S1$x <- rep(mid_x, each = ngrid - 1)
  S1$z <- rep(mid_z, ngrid - 1)
  #S1$c_LS <- cut(S1$M_LS, c(50, 78, 82, 86, 90, 100))
  
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.85
  outKzone <- 0.85
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  
  if (LA == FALSE){
    p <- ggplot(kZone, aes(x, y)) +
      geom_tile(data=S1, 
                aes(x=x, y=z, fill= Mean_Launch_Speed)) +
      scale_fill_distiller(palette = "Spectral") +
      geom_path(lwd=1.5, col="black") + TH +
      ggtitle("Mean Launch Speed by Location") +
      coord_fixed() + xlab("Adjusted Plate X") + ylab("Plate Z")
  } else {
    p <- ggplot(kZone, aes(x, y)) +
      geom_tile(data=S1, 
                aes(x=x, y=z, fill= Mean_Launch_Angle)) +
      scale_fill_distiller(palette = "Spectral") +
      geom_path(lwd=1.5, col="black") + TH +
      ggtitle("Mean Launch Angle by Location") +
      coord_fixed() + xlab("Adjusted Plate X") + ylab("Plate Z")
  }
  p
}

# here I apply this function using a 13 x 13 grid

myplot(sc2017_ip, 13)
myplot(sc2017_ip, 13, LA = TRUE)

# this function plots several contour graphs of the
# zone locations of the swings on balls in play
# across different counts

swing_count <- function(sc2017_ip,
                        several_counts, NCOL = 2){
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.85
  outKzone <- 0.85
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  scnew <- filter(sc2017_ip, 
                  Count %in% several_counts)
  scnew$COUNT = paste("Count:", scnew$Count)
  ggplot(scnew,
         aes(adj_plate_x, plate_z)) +
    #  stat_density_2d(geom = "raster", 
    #                  aes(fill = ..density..), 
    #                  contour = FALSE) +
    stat_density2d(geom = "contour",
                   bins=5) +
    geom_path(data = kZone,
              aes(x = x, y = y),
              lwd=1, col="red") +
    xlim(-1.5, 1.5) + ylim(1, 4) + 
    facet_wrap(~ COUNT, ncol=NCOL) + TH +
    ggtitle("Location of In-Play Swings 
      for Different Counts") +
    coord_fixed()
}

# try this function out 

swing_count(sc2017_ip,
            c("0-0", "0-1", "0-2", 
              "1-0", "1-1", "1-2",
              "2-0",  "2-1", "2-2",
              "3-0", "3-1", "3-2"), NCOL=3)