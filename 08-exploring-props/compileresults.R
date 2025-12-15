library(tidyverse)
################################################################################
################################################################################
################################################################################
dat.linear <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                 mean.err=numeric(23),
                 median.err=numeric(23),
                 mode.err=numeric(23))

files <- list.files("reg-linear/pred/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.linear$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.linear$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.linear$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}

dat.linear <- dat.linear |>
  mutate(mean.err = mean.err/dat.linear$mean.err[1],
         median.err = median.err/dat.linear$median.err[1],
         mode.err = mode.err/dat.linear$mode.err[1])

################################################################################
################################################################################
################################################################################

dat.poly <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                 mean.err=numeric(23),
                 median.err=numeric(23),
                 mode.err=numeric(23))

files <- list.files("reg-poly/pred/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.poly$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.poly$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.poly$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}

dat.poly <- dat.poly |>
  mutate(mean.err = mean.err/dat.poly$mean.err[1],
         median.err = median.err/dat.poly$median.err[1],
         mode.err = mode.err/dat.poly$mode.err[1])

################################################################################
################################################################################
################################################################################

dat.nonlinear <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                 mean.err=numeric(23),
                 median.err=numeric(23),
                 mode.err=numeric(23))

files <- list.files("reg-nonlinear/pred/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.nonlinear$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.nonlinear$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.nonlinear$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}

dat.nonlinear <- dat.nonlinear |>
  mutate(mean.err = mean.err/dat.nonlinear$mean.err[1],
         median.err = median.err/dat.nonlinear$median.err[1],
         mode.err = mode.err/dat.nonlinear$mode.err[1])

################################################################################
################################################################################
################################################################################

dat.heart <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                 mean.err=numeric(23),
                 median.err=numeric(23),
                 mode.err=numeric(23))

files <- list.files("reg-heartunif/pred/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.heart$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.heart$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.heart$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}
dat.heart <- dat.heart |>
  mutate(mean.err = mean.err/dat.heart$mean.err[1],
         median.err = median.err/dat.heart$median.err[1],
         mode.err = mode.err/dat.heart$mode.err[1])

################################################################################
################################################################################
################################################################################
dat.motorcycle <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                         mean.err=numeric(23),
                         median.err=numeric(23),
                         mode.err=numeric(23))
files <- list.files("motorcycle/pred/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.motorcycle$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.motorcycle$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.motorcycle$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}
dat.motorcycle <- dat.motorcycle |>
  mutate(mean.err = mean.err/dat.motorcycle$mean.err[1],
         median.err = median.err/dat.motorcycle$median.err[1],
         mode.err = mode.err/dat.motorcycle$mode.err[1])

################################################################################
################################################################################
################################################################################
dat.bodyfat <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                      mean.err=numeric(23),
                      median.err=numeric(23),
                      mode.err=numeric(23))
files <- list.files("bodyfat/pred/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.bodyfat$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.bodyfat$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.bodyfat$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}
dat.bodyfat <- dat.bodyfat |>
  mutate(mean.err = mean.err/dat.bodyfat$mean.err[1],
         median.err = median.err/dat.bodyfat$median.err[1],
         mode.err = mode.err/dat.bodyfat$mode.err[1])

################################################################################
################################################################################
################################################################################
dat.diabetes <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                       mean.err=numeric(23),
                       median.err=numeric(23),
                       mode.err=numeric(23))
files <- list.files("diabetes/pred/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.diabetes$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.diabetes$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.diabetes$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}
dat.diabetes <- dat.diabetes |>
  mutate(mean.err = mean.err/dat.diabetes$mean.err[1],
         median.err = median.err/dat.diabetes$median.err[1],
         mode.err = mode.err/dat.diabetes$mode.err[1])

################################################################################
################################################################################
################################################################################
dat.laser <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                       mean.err=numeric(23),
                       median.err=numeric(23),
                       mode.err=numeric(23))
files <- list.files("laser/pred/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.laser$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.laser$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.laser$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}
dat.laser <- dat.laser |>
  mutate(mean.err = mean.err/dat.laser$mean.err[1],
         median.err = median.err/dat.laser$median.err[1],
         mode.err = mode.err/dat.laser$mode.err[1])

################################################################################
################################################################################
dat.plasma <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                          mean.err=numeric(23),
                          median.err=numeric(23),
                          mode.err=numeric(23))
files <- list.files("plasma/pred/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.plasma$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.plasma$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.plasma$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}
dat.plasma <- dat.plasma |>
  mutate(mean.err = mean.err/dat.plasma$mean.err[1],
         median.err = median.err/dat.plasma$median.err[1],
         mode.err = mode.err/dat.plasma$mode.err[1])

################################################################################
################################################################################
################################################################################
dat.Boston <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                     mean.err=numeric(23),
                     median.err=numeric(23),
                     mode.err=numeric(23))
files <- list.files("Boston/pred/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.Boston$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.Boston$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.Boston$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}
dat.Boston <- dat.Boston |>
  mutate(mean.err = mean.err/dat.Boston$mean.err[1],
         median.err = median.err/dat.Boston$median.err[1],
         mode.err = mode.err/dat.Boston$mode.err[1])

################################################################################
################################################################################
################################################################################
dat.cholesterol <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                     mean.err=numeric(23),
                     median.err=numeric(23),
                     mode.err=numeric(23))
files <- list.files("cholesterol/pred/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.cholesterol$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.cholesterol$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.cholesterol$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}
dat.cholesterol <- dat.cholesterol |>
  mutate(mean.err = mean.err/dat.cholesterol$mean.err[1],
         median.err = median.err/dat.cholesterol$median.err[1],
         mode.err = mode.err/dat.cholesterol$mode.err[1])

################################################################################
################################################################################
################################################################################
dat.student <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                      mean.err=numeric(23),
                      median.err=numeric(23),
                      mode.err=numeric(23))
files <- list.files("student/pred-1000/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.student$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.student$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.student$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}
dat.student <- dat.student |>
  mutate(mean.err = mean.err/dat.student$mean.err[1],
         median.err = median.err/dat.student$median.err[1],
         mode.err = mode.err/dat.student$mode.err[1])

################################################################################
################################################################################
################################################################################
dat.abalone <- tibble(prop=c(0,0.01,0.025, seq(0.05, 1, 0.05)),
                      mean.err=numeric(23),
                      median.err=numeric(23),
                      mode.err=numeric(23))
files <- list.files("abalone/pred/", full.names = T)
for(i in 1:length(files)){
  curr.dat <- read_csv(paste(files[i]))
  dat.abalone$mean.err[i] <- sqrt(mean((curr.dat$pt2.mean-curr.dat$y)^2))
  dat.abalone$median.err[i] <- sqrt(mean((curr.dat$pt2.median-curr.dat$y)^2))
  dat.abalone$mode.err[i] <- sqrt(mean((curr.dat$pt2.mode-curr.dat$y)^2))
}
dat.abalone <- dat.abalone |>
  mutate(mean.err = mean.err/dat.abalone$mean.err[1],
         median.err = median.err/dat.abalone$median.err[1],
         mode.err = mode.err/dat.abalone$mode.err[1])
################################################################################
################################################################################
################################################################################
pdf("exploringprops.pdf", width=6,height=3.5)
ggplot()+
  geom_hline(yintercept = 1.00)+
  #abalone
  geom_line(data=dat.bodyfat, aes(x=prop, y=mean.err, color="Body Fat"))+
  geom_line(data=dat.Boston, aes(x=prop, y=mean.err, color="Boston"))+
  geom_line(data=dat.cholesterol, aes(x=prop, y=mean.err, color="Cholesterol"))+
  geom_line(data=dat.diabetes, aes(x=prop, y=mean.err, color="Diabetes"))+
  geom_line(data=dat.laser, aes(x=prop, y=mean.err, color="Laser"))+
  geom_line(data=dat.motorcycle, aes(x=prop, y=mean.err, color="Motorcycle"))+
  geom_line(data=dat.plasma, aes(x=prop, y=mean.err, color="Plasma"))+
  geom_line(data=dat.heart, aes(x=prop, y=mean.err, color="Heartbeat"))+
  geom_line(data=dat.linear, aes(x=prop, y=mean.err, color="Linear"))+
  geom_line(data=dat.nonlinear, aes(x=prop, y=mean.err, color="Nonlinear"))+
  geom_line(data=dat.poly, aes(x=prop, y=mean.err, color="Polynomial"))+
  geom_line(data=dat.student, aes(x=prop, y=mean.err, color="Student"))+
  geom_line(data=dat.abalone, aes(x=prop, y=mean.err, color="Abalone"))+
  geom_vline(xintercept = 0.05, linetype="dotted")+
  theme_bw()+
  xlab("Proportion of O Proposals Randomly Accepted")+
  ylab("Percent Change in RMSE")+
  labs(color="")
dev.off()
