# 



load(file='160414_Volta.RData')
str(dat)
summary(dat)


dat <- filter(dat, Year == 2002:2009)
dat <- filter(dat, crop == 'Maize' | crop == 'Millet'| 
                crop == 'Rice'| crop == 'Yam'| crop == 'Sorghum'| 
                crop == 'Cowpea'| crop == 'Soy' )

dat$Year <- as.factor(dat$Year)


p1 <- ggplot(data=dat, aes(crop, prod)) +
  geom_jitter(aes(colour=crop, alpha=0.2)) + geom_boxplot(notch=T) +
  facet_grid( Year ~ country)  +
  theme (text = element_text(size = 8), axis.text.x = element_text(angle=90)) +
  ggtitle('Production of crops (Tons) in the Volta basin')

p1

dat %>%
  filter (Year == 2003) %>%
  select(TAI_ID2, crop, prod) %>%
  spread (key = crop, value = prod) %>%
  ncol()
  