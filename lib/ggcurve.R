require(ggplot2)
require(dplyr)
require(tidyr)
require(scales)
require(ustyc)

# fetch the data
yc = getYieldCurve()

# build and clean
x = yc$df %>% mutate(Date=as.Date(rownames(yc$df))) %>% gather(Product,Yield,starts_with("BC"))
# x = x[complete.cases(x),]

x = x[-(which(x$Product=="BC_30YEARDISPLAY" & x$Yield==0)),]

# 30-year gaps, boundaries 2002-02-15 and 2006-02-09
# 30-year display starts at 0 until 2011-01-03
# 1-month has NAs
# 6-month has one NA
# 3m,1,2,3,5,7-year has one NA on 2010-10-11
# 20-year has NAs

# plot all products with dates axis
p <- ggplot(x,aes(x=Date,y=Yield,group=Product))
p + geom_line(aes(colour=Product)) + scale_x_date()

# facets
# + theme_bw()
p <- ggplot(x,aes(Date,Yield)) 
p + geom_line(aes(colour=Product)) + 
  facet_wrap(~Product,ncol=4,scales="fixed") + 
  scale_x_date() +
  theme(legend.position="none")

# plot spreads
xs <- yc$df %>% 
  mutate(Date=as.Date(rownames(yc$df))) %>%
  mutate(SP_2_5=BC_5YEAR-BC_2YEAR) %>%
  mutate(SP_2_10=BC_10YEAR-BC_2YEAR) %>% 
  mutate(SP_2_20=BC_20YEAR-BC_2YEAR) %>%
  mutate(SP_2_30=BC_30YEAR-BC_2YEAR) %>% 
  gather(Product,Spread,starts_with("SP"))

p <- ggplot(xs,aes(x=Date,y=Spread,group=Product))
p <- p + geom_line(aes(colour=Product)) + 
  scale_x_date() + 
  ggtitle("Yield Spread History") +
  theme(plot.title=element_text(size=20)) +
  xlab(NULL) +
  ylab("Yield Spread %")
p

# recent years
p + scale_x_date(limits=c(as.Date("2007-1-1"),max(xs$Date)))

# facets
p <- ggplot(xs,aes(Date,Spread)) 
p + geom_line(aes(colour=Product)) + 
  facet_wrap(~Product,ncol=2,scales="fixed") + 
  scale_x_date() +
  ggtitle("Yield Spread History") +
  theme(plot.title=element_text(size=20),legend.position="none")

# six-month changes
# TBD
