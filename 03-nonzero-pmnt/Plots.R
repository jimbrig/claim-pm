data <- readRDS("../00-data/model-claims.RDS")
str(data)

model_data <- dplyr::filter(data, eval != "2015-11-30") 
model_data <- model_data[!model_data$tot_pd_incr_act == 0, ]

full <- readRDS("../00-data/bi-claims-full.RDS")
rx <- full[, c("claim_number", "devt", "tot_rx")]
pd <- full[, c("claim_number", "devt", "tot_pd")]
model_data <- left_join(model_data, rx, by = c("claim_number", "devt"))
model_data <- left_join(model_data, pd, by = c("claim_number", "devt"))


theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)


names(model_data)
featurePlot(x = model_data[ ,c(8:13,15,16) ],
            y = log(model_data$tot_pd_incr_act),
            plot = "scatter",
            type = c("p", "smooth"),
            span = 1.5,
            layout = c(3, 3))





sample <- readRDS("../00-data/03-nzm/nzm-model-data.RDS")

str(sample)

hist(sample$tot_pd_incr_act)
with(sample, scatter.smooth(tot_pd_incr_act ~ bi_rx))
with(sample, scatter.smooth(log(tot_pd_incr_act) ~ bi_rx, span = 1))
with(sample, scatter.smooth(log(tot_pd_incr_act) ~ log(bi_rx + 1), span = 1))



mod <- lm(tot_pd_incr_act ~ bi_rx, data = sample[sample$devt == 18, ])
mod1 <- lm(tot_pd_incr_act ~ bi_rx + I(bi_rx ^ .5) , data = sample[sample$devt == 18, ])
summary(mod1)

plot(mod)
plot(mod1, 2)











mod2 <- lm(log(tot_pd_incr_act) ~ bi_rx + 1, data = sample)
plot(mod2, 1)

mod3 <- lm(lm(log(tot_pd_incr_act) ~ log(bi_rx + 1), data = sample))
plot(mod3, 1)

summary(mod)
summary(mod2)
summary(mod3)

mod <- lm(tot_pd_incr_act ~ exp_pd + bi_rx, data = sample[sample$devt == 18, ])
summary(mod)

plot(mod, 1)

mod <- update(mod, data = sample[-36350, ])
plot(mod, 4)

plot(mod, 1)


boxplot(log(tot_pd_incr_act) ~ status_0_act, data = sample[sample$devt == 18, ])

mod <- lm(tot_pd_incr_act ~ status_0_act, data = sample)
summary(mod)
levels(sample$status_0_act) 
levels(sample$status_0_act) <- c("C","C", "O_C", "O_O")

boxplot(log(tot_pd_incr_act) ~ suit_lag_group, data = sample[sample$devt == 18, ])

mod <- lm(tot_pd_incr_act ~ suit_lag_group, data= sample)
summary(mod)

mod <- lm(tot_pd_incr_act ~ suit_lag_group + bi_rx + exp_pd, data = sample[sample$devt == 18, ])
summary(mod)
levels(sample$suit_lag_group) <- c("NS", "lag_12", "lag_18", "lag_18", "lag_18")

boxplot(log(tot_pd_incr_act) ~ newclm, data = sample)

mod <- lm(tot_pd_incr_act ~ newclm, data = sample)
summary(mod)

mod <- lm(tot_pd_incr_act ~ newclm + suit_lag_group )