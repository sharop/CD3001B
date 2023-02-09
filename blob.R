library(tidyverse)
library(knitr)
library(tidyverse)
library(kableExtra)
library(tsibble)
library(lubridate)
library(hrbrthemes)
library(tsibbledata)
library(wesanderson)
library(mxmaps)


# read datasets

df_inv <- 
  read_csv('introduccionDatosPanel/juguetes_mexico_dataset/inventory.csv')

df_prod <- 
  read_csv('introduccionDatosPanel/juguetes_mexico_dataset/products.csv')

df_sales <- 
  read_csv('introduccionDatosPanel/juguetes_mexico_dataset/sales.csv')

df_stores <- 
  read_csv('introduccionDatosPanel/juguetes_mexico_dataset/stores.csv')

# check out data sets 

df_inv %>% 
  head %>%
  kable(.,
        format = "html",
        booktabs = TRUE) %>%
  kable_styling(font_size = 12) %>%
  scroll_box(width = "100%")

df_prod %>% 
  head %>%
  kable(.,
        format = "html",
        booktabs = TRUE) %>%
  kable_styling(font_size = 12) %>%
  scroll_box(width = "100%")

df_sales %>% 
  head %>%
  kable(.,
        format = "html",
        booktabs = TRUE) %>%
  kable_styling(font_size = 12) %>%
  scroll_box(width = "100%")

df_stores %>% 
  head %>%
  kable(.,
        format = "html",
        booktabs = TRUE) %>%
  kable_styling(font_size = 12) %>%
  scroll_box(width = "100%")


# join relevant tables

df <- 
df_prod %>% 
  left_join(df_sales,
            by='Product_ID') %>% 
  left_join(df_stores,
            by='Store_ID')


## revenue report -- revenue as metric

rev_report <-
df %>% 
  mutate(
    Product_Price = str_remove(Product_Price, "\\$") %>% as.numeric(),
    Product_Cost =  str_remove(Product_Cost, "\\$") %>% as.numeric(),
  ) %>% 
  group_by(
    Date,
    Product_Category,
    Product_Cost,
    Product_Price
  ) %>% 
  summarise(
    costs = sum(Product_Cost),
    sales = sum(Product_Price),
    items_sold = n(),
    total_revenue = sales - costs
  ) 

## geo report -- revenue as metric

geo_report <-
  df %>% 
  mutate(
    Product_Price = str_remove(Product_Price, "\\$") %>% as.numeric(),
    Product_Cost =  str_remove(Product_Cost, "\\$") %>% as.numeric(),
  ) %>% 
  group_by(
    Date,
    Store_City,
    Store_Location,
  ) %>% 
  summarise(
    costs = sum(Product_Cost),
    sales = sum(Product_Price),
    items_sold = n(),
    total_revenue = sales - costs
  ) 

## product report -- revenue as metric

prod_report <-
  df %>% 
  mutate(
    Product_Price = str_remove(Product_Price, "\\$") %>% as.numeric(),
    Product_Cost =  str_remove(Product_Cost, "\\$") %>% as.numeric(),
  ) %>% 
  group_by(
    Date,
    Product_Category,
    Product_Name,
  ) %>% 
  summarise(
    costs = sum(Product_Cost),
    sales = sum(Product_Price),
    items_sold = n(),
    total_revenue = sales - costs
  ) 

# show reports

rev_report %>% 
  head %>%
  kable(.,
        format = "html",
        booktabs = TRUE) %>%
  kable_styling(font_size = 12) %>%
  scroll_box(width = "100%")

geo_report %>% 
  head %>%
  kable(.,
        format = "html",
        booktabs = TRUE) %>%
  kable_styling(font_size = 12) %>%
  scroll_box(width = "100%")

prod_report  %>% 
  head %>%
  kable(.,
        format = "html",
        booktabs = TRUE) %>%
  kable_styling(font_size = 12) %>%
  scroll_box(width = "100%")


## general viz

# by category

df %>% 
  ggplot(aes(x=Date, colour=Product_Category)) +
  geom_bar() +
  facet_wrap(~ Product_Category) +
  theme_ipsum() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
  axis.text.x = element_text(
    angle = 45,
    vjust = 0.5,
    hjust = 1,
    size = 9
    )
  )

# drill into Art and Crafts

df %>% 
  filter(Product_Category == 'Art & Crafts') %>% 
  ggplot(aes(x=Date, y=Units, colour=Product_Name, group=Product_Name, fill=Product_Name)) +
  geom_area() +
  facet_wrap(~ Product_Name) +
  theme_ipsum() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1,
          size = 9
        )
  )

# viz reports

rev_report %>% 
  ggplot(aes(Date, total_revenue, group=Product_Category,col=Product_Category)) +
  geom_smooth(method = 'loess', se = F) +
  theme_ipsum() +
  scale_colour_manual(name = '', values=wes_palettes$FantasticFox1) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

rev_report %>% 
  ggplot(aes(Date, total_revenue/items_sold, group=Product_Category,col=Product_Category)) +
  geom_smooth(method = 'loess', se = F) +
  theme_ipsum() +
  scale_colour_manual(name = '', values=wes_palettes$FantasticFox1) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  ylab("average rev per product")

geo_report %>% 
  ggplot(aes(Date, total_revenue, group=Store_City,col=Store_City)) +
  geom_smooth(method = 'loess', se = F) +
  theme_ipsum() +
  scale_colour_manual(name = '', values=colorRampPalette(wes_palettes$FantasticFox1)(29)) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

geo_report %>% 
  ggplot(aes(Date, total_revenue/items_sold, group=Store_City,col=Store_City)) +
  geom_smooth(method = 'loess', se = F) +
  theme_ipsum() +
  scale_colour_manual(name = '', values=colorRampPalette(wes_palettes$FantasticFox1)(29)) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  ylab("average rev per product")

geo_report %>% 
  ggplot(aes(Date, total_revenue, group=Store_Location,col=Store_Location)) +
  geom_smooth(method = 'loess', se = F) +
  theme_ipsum() +
  scale_colour_manual(name = '', values=wes_palettes$FantasticFox1) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

geo_report %>% 
  ggplot(aes(Date, total_revenue/items_sold, group=Store_Location,col=Store_Location)) +
  geom_smooth(method = 'loess', se = F) +
  theme_ipsum() +
  scale_colour_manual(name = '', values=wes_palettes$FantasticFox1) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  ylab("average rev per product")

# regional revenue 
#Valle-Jones D (2022). mxmaps: Create Maps of Mexico. https://www.diegovalle.net/mxmaps/, https://github.com/diegovalle/mxmaps.

revenue_choropleth <- tibble(
  region =
    geo_report %>% 
    left_join(read_csv('introduccionDatosPanel/state.csv'),
            by=c('Store_City'='state')) %>% ungroup %>% 
  select(code) %>% 
    unlist
) %>% 
  mutate(value = geo_report$total_revenue) %>% 
  group_by(region) %>% 
  summarise(
    value = sum(value)
  )

mxstate_choropleth(
  revenue_choropleth,
  num_colors = 5,
  title = "",
  legend = "revenue"
  )


prod_report %>% 
  filter(Product_Category == 'Art & Crafts') %>% 
  ggplot(aes(Date, total_revenue, group=Product_Name,col=Product_Name)) +
  geom_smooth(method = 'loess', se = F) +
  theme_ipsum() +
  scale_colour_manual(name = '', values=colorRampPalette(wes_palettes$FantasticFox1)(8)) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

prod_report %>% 
  filter(Product_Category == 'Art & Crafts') %>% 
  ggplot(aes(Date, total_revenue/items_sold, group=Product_Name,col=Product_Name)) +
  geom_smooth(method = 'loess', se = F) +
  theme_ipsum() +
  scale_colour_manual(name = '', values=colorRampPalette(wes_palettes$FantasticFox1)(8)) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  ylab("average rev per product")

