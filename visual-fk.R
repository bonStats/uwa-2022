library(ggplot2)
library(dplyr)
library(ggdist)
library(distributional)
library(forcats)

# mu ~ N(-1,2), G(x) = N(x,1,2), M(x,y) ~ N(x + 1,1)
# mu(dx) G(x) ~ N(0,1)
# int_X mu(dx) G(x) M(x,y) ~ N(1,2)

anno_size = 7

d_initial <- dist_normal(mu = -1, sigma = sqrt(2))
potfun <- dist_normal(mu = 1, sigma = sqrt(2))
kerdist <- dist_normal(mu = -2, sigma = 1)

d_update <- dist_normal(mu = 0, sigma = 1)
d_predict <- dist_normal(mu = -2, sigma = sqrt(2))

d_df <- tibble(name = as_factor(c("Initial", "Updated", "Predictive")),
       dist = c(d_initial, d_update, d_predict),
       type = "analytical"
       )

N <- 100

# initial
x <- generate(d_initial, N)
s_initial <- dist_sample(x)

# updated
w <- density(potfun, x[[1]])[[1]]
x_updated <- sample(x[[1]], replace = T, prob = w)
s_update <- dist_sample(list(x_updated))

# predictive
x_predict <- x_updated + generate(kerdist, N)[[1]]
s_predict <- dist_sample(list(x_predict))

s_df <- tibble(name = ordered(c("Initial", "Updated", "Predictive")),
               dist = c(s_initial, s_update, s_predict),
               type = "empirical"
)

d_plot <- ggplot() + ggdist::stat_slab(aes(y = name, dist = dist), data = d_df) +
  ggdist::stat_slab(aes(y = "Initial", dist = potfun), fill = NA, colour = "red") + 
  ggdist::stat_slab(aes(y = "Updated", dist = kerdist), fill = NA, colour = "blue", normalize = "all", linetype = "dashed") + 
  theme_bw() +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-12.5,12.5)) +
  ylab("") +
  xlab("") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  geom_text(aes(x = 9, y = "Initial"), label = "G[0](x)==phi(textstyle(frac(x-1,sqrt(2))))", parse = T, size=anno_size, nudge_y = 0.45, colour = "red") +
  geom_text(aes(x = -9, y = "Initial"), label = "eta[0]==N(-1, 2)", parse = T, size=anno_size, nudge_y = 0.45) +
  geom_text(aes(x = -9, y = "Updated"), label = "hat(eta)[0]==N(0, 1)", parse = T, size=anno_size, nudge_y = 0.45) +
  geom_text(aes(x = -9, y = "Predictive"), label = "eta[1]==N(-2, 2)", parse = T, size=anno_size, nudge_y = 0.45) +
  geom_text(aes(x = 9, y = "Updated"), label = "atop(M[1](x[0],.)==phantom(0),N(x[0]-2, 1))", parse = T, size=anno_size, nudge_y = 0.45, colour = "blue")


#d_plot + ggtitle("Normalised densities")

ds_plot <- d_plot + 
  ggdist::stat_dots(aes(y = name, dist = dist), data = s_df, colour = "black", fill = "black")


#ds_plot + ggtitle("Normalised densities + samples")

# Twisted 

d_t_initial <- d_update
t_potfun <- dist_normal(mu = 0, sigma = 100)

d_t_update <- d_t_initial
d_t_predict <- d_predict

d_t_df <- tibble(name = as_factor(c("Initial", "Updated", "Predictive")),
               dist = c(d_t_initial, d_t_update, d_t_predict),
               type = "analytical"
)

# initial
x_t <- generate(d_t_initial, N)
s_t_initial <- dist_sample(x_t)

# updated
x_t_updated <- x_t[[1]]
s_t_update <- s_t_initial

# predictive
x_t_predict <- x_t_updated + generate(kerdist, N)[[1]]
s_t_predict <- dist_sample(list(x_t_predict))

s_t_df <- tibble(name = ordered(c("Initial", "Updated", "Predictive")),
               dist = c(s_t_initial, s_t_update, s_t_predict),
               type = "empirical"
)


d_t_plot <- ggplot() + ggdist::stat_slab(aes(y = name, dist = dist), data = d_t_df) +
  ggdist::stat_slab(aes(y = "Initial", dist = t_potfun), fill = NA, colour = "red") + 
  ggdist::stat_slab(aes(y = "Updated", dist = kerdist), fill = NA, colour = "blue", normalize = "all", linetype = "dashed") + 
  theme_bw() + 
  scale_y_discrete(limits = rev) +
  ylab("") +
  xlab("") +
  xlim(layer_scales(d_plot)$x$range$range) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  geom_text(aes(x = 9, y = "Initial"), label = "{G[0]^psi}(x)==1", parse = T, size=anno_size, nudge_y = 0.45, colour = "red") +
  geom_text(aes(x = -9, y = "Initial"), label = "eta[0]^psi==N(0, 1)", parse = T, size=anno_size, nudge_y = 0.45) +
  geom_text(aes(x = -9, y = "Updated"), label = "hat(eta)[0]^psi==N(0, 1)", parse = T, size=anno_size, nudge_y = 0.45) +
  geom_text(aes(x = -9, y = "Predictive"), label = "eta[1]^psi==N(-2, 2)", parse = T, size=anno_size, nudge_y = 0.45) +
  geom_text(aes(x = 9, y = "Updated"), label = "atop({M[1]^psi}(x[0],.)==phantom(0),N(x[0]-2, 1))", parse = T, size=anno_size, nudge_y = 0.45, colour = "blue")


ds_t_plot <- d_t_plot + 
  ggdist::stat_dots(aes(y = name, dist = dist), data = s_t_df, colour = "black", fill = "black")
ds_t_plot
