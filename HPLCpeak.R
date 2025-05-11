library(readxl)
library(tidyverse)


# setwd('yourDirectory/inVitroNitriteReactionsData/')


# 220326_dA -----
dA <- read_xlsx('./nitrite_dN_reactions.xlsx', sheet = '220326_dA', range = 'R1C1:R10C3') %>% 
  pivot_longer(cols = -`incubation_time(min)`, names_to = 'analytes', values_to = 'peakArea')

reactant_max <- dA %>% filter(analytes == factor(dA$analytes) %>% levels(.) %>% .[1]) %>% summarize(max(peakArea)) %>% .[[1]]

p <- nls(peakArea ~ SSasymp(`incubation_time(min)`, asym, resp0, lrc), data = dA %>% filter(analytes == factor(dA$analytes) %>% levels(.) %>% .[2])) # dI_14min
product_asym <- summary(p)$parameters[1] # obtain asymptote of product

dA <- dA %>% mutate(`amount(nmol)` = if_else(analytes == factor(dA$analytes) %>% levels(.) %>% .[1], peakArea / reactant_max * 6, peakArea / product_asym * 6)) # calc amount in nmol. total reactant 1 nmol/uL x 6 uL = 6 nmol

dA %>% 
  ggplot(aes(x = `incubation_time(min)`, y = `amount(nmol)`, color = analytes)) + 
  geom_point()+
  geom_smooth(data = . %>% filter(analytes == factor(dA$analytes) %>% levels(.) %>% .[1]), se = FALSE, method = "nls", formula = y ~ SSasymp(x, asym, resp0, lrc))+
  geom_smooth(data = . %>% filter(analytes == factor(dA$analytes) %>% levels(.) %>% .[2]), se = FALSE, method = "nls", formula =  y ~ SSasymp(x, asym, resp0, lrc))

ggsave('./graphs/dArxn.svg', device = 'svg', width = 2000, height = 1236, units = 'px')


# 220323_N6mdA ------
N6mdA <- read_xlsx('./nitrite_dN_reactions.xlsx', sheet = '220323_N6mdA', range = 'R1C1:R7C3') %>% 
  pivot_longer(cols = -`incubation_time(min)`, names_to = 'analytes', values_to = 'peakArea')

reactant_max <- N6mdA %>% filter(analytes == factor(N6mdA$analytes) %>% levels(.) %>% .[1]) %>% summarize(max(peakArea)) %>% .[[1]]

p <- nls(peakArea ~ SSasymp(`incubation_time(min)`, asym, resp0, lrc), data = N6mdA %>% filter(analytes == factor(N6mdA$analytes) %>% levels(.) %>% .[2])) # dI_14min
product_asym <- summary(p)$parameters[1] # obtain asymptote of product

N6mdA <- N6mdA %>% mutate(`amount(nmol)` = if_else(analytes == factor(N6mdA$analytes) %>% levels(.) %>% .[1], peakArea / reactant_max * 6, peakArea / product_asym * 6)) # calc amount in nmol. total reactant 1 nmol/uL x 6 uL = 6 nmol

N6mdA %>% 
  ggplot(aes(x = `incubation_time(min)`, y = `amount(nmol)`, color = analytes)) + 
  geom_point()+
  geom_smooth(data = . %>% filter(analytes == factor(N6mdA$analytes) %>% levels(.) %>% .[1]), se = FALSE, method = "nls", formula = y ~ SSasymp(x, asym, resp0, lrc))+
  geom_smooth(data = . %>% filter(analytes == factor(N6mdA$analytes) %>% levels(.) %>% .[2]), se = FALSE, method = "nls", formula =  y ~ SSasymp(x, asym, resp0, lrc))

ggsave('./graphs/N6mdArxn.svg', device = 'svg', width = 2000, height = 1236, units = 'px')


# 220331_dC -----
dC <- read_xlsx('./nitrite_dN_reactions.xlsx', sheet = '220331_dC', range = 'R1C1:R6C3') %>% 
  pivot_longer(cols = -`incubation_time(min)`, names_to = 'analytes', values_to = 'peakArea')

reactant_max <- dC %>% filter(analytes == factor(dC$analytes) %>% levels(.) %>% .[1]) %>% summarize(max(peakArea)) %>% .[[1]]

p <- nls(peakArea ~ SSasymp(`incubation_time(min)`, asym, resp0, lrc), data = dC %>% filter(analytes == factor(dC$analytes) %>% levels(.) %>% .[2])) # dI_14min
product_asym <- summary(p)$parameters[1] # obtain asymptote of product

dC <- dC %>% mutate(`amount(nmol)` = if_else(analytes == factor(dC$analytes) %>% levels(.) %>% .[1], peakArea / reactant_max * 6, peakArea / product_asym * 6)) # calc amount in nmol. total reactant 1 nmol/uL x 6 uL = 6 nmol

dC %>% 
  ggplot(aes(x = `incubation_time(min)`, y = `amount(nmol)`, color = analytes)) + 
  geom_point()+
  geom_smooth(data = . %>% filter(analytes == factor(dC$analytes) %>% levels(.) %>% .[1]), se = FALSE, method = "nls", formula = y ~ SSasymp(x, asym, resp0, lrc))+
  geom_smooth(data = . %>% filter(analytes == factor(dC$analytes) %>% levels(.) %>% .[2]), se = FALSE, method = "nls", formula =  y ~ SSasymp(x, asym, resp0, lrc))

ggsave('./graphs/dCrxn.svg', device = 'svg', width = 2000, height = 1236, units = 'px')


# 220320_N4mdC ------
N4mdC <- read_xlsx('./nitrite_dN_reactions.xlsx', sheet = '220320_N4mdC', range = 'R1C1:R6C3') %>% 
  pivot_longer(cols = -`incubation_time(min)`, names_to = 'analytes', values_to = 'peakArea')

reactant_max <- N4mdC %>% filter(analytes == factor(N4mdC$analytes) %>% levels(.) %>% .[1]) %>% summarize(max(peakArea)) %>% .[[1]]

p <- nls(peakArea ~ SSasymp(`incubation_time(min)`, asym, resp0, lrc), data = N4mdC %>% filter(analytes == factor(N4mdC$analytes) %>% levels(.) %>% .[2])) # dI_14min
product_asym <- summary(p)$parameters[1] # obtain asymptote of product

N4mdC <- N4mdC %>% mutate(`amount(nmol)` = if_else(analytes == factor(N4mdC$analytes) %>% levels(.) %>% .[1], peakArea / reactant_max * 6, peakArea / product_asym * 6)) # calc amount in nmol. total reactant 1 nmol/uL x 6 uL = 6 nmol

N4mdC %>% 
  ggplot(aes(x = `incubation_time(min)`, y = `amount(nmol)`, color = analytes)) + 
  geom_point()+
  geom_smooth(data = . %>% filter(analytes == factor(N4mdC$analytes) %>% levels(.) %>% .[1]), se = FALSE, method = "nls", formula = y ~ SSasymp(x, asym, resp0, lrc))+
  geom_smooth(data = . %>% filter(analytes == factor(N4mdC$analytes) %>% levels(.) %>% .[2]), se = FALSE, method = "nls", formula =  y ~ SSasymp(x, asym, resp0, lrc))

ggsave('./graphs/N4mdCrxn.svg', device = 'svg', width = 2000, height = 1236, units = 'px')
