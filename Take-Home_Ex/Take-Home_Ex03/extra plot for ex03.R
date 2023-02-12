---
  title: "take home exercise 3 extra"
author: "huoda"
---
  
  
  
  ## 2. How does the resale price of HDB with different flat types change over time?
  
  Data Preparation
```{r}
prop_data <- prop_data %>%
  # Filter dataset: keep only for 3,4,5 rooms.
  filter(flat_type == '3 ROOM' | flat_type == '4 ROOM' |flat_type =='5 ROOM') %>%
  # Add a column of the price per square meter
  mutate(price_psm = resale_price/floor_area_sqm) %>%
  # add a column of date by converting the month column from string to date
  mutate(date = as.Date(paste(month,"-01", sep="")))
prop_data
```

```{r}
p2<- prop_data %>%
  group_by(flat_type) %>%
  filter(flat_type == '3 ROOM' | flat_type == '4 ROOM' |flat_type =='5 ROOM') %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = flat_type, y = price_psm)) +
  transition_time(date)

anim_save('price2_animation.gif',p2)
```


![](price2_animation.gif)


From the animation, we come to the same conclusion that the price goes up. However, if the check the different flat type in different residential towns, we could spot some differences.

```{r}
p2<- prop_data %>%
  group_by(flat_type) %>%
  filter(flat_type == '3 ROOM' | flat_type == '4 ROOM' |flat_type =='5 ROOM') %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = flat_type, y = price_psm)) +
  facet_wrap(~town)
plot(p2)
```

```{r}
p3<- prop_data %>%
  group_by(flat_type) %>%
  filter(flat_type == '3 ROOM' | flat_type == '4 ROOM' |flat_type =='5 ROOM') %>%
  filter(town == 'WOODLANDS' | town == 'CENTRAL AREA') %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = flat_type, y = price_psm)) +
  facet_wrap(~town) +
  theme_bw() +
  
  labs(title = "HDB resale prices in {frame_time} in Woodlands vs Central Area", 
       x = "", y = "Price psm SGD")+
  transition_time(date)

anim_save('price3_animation.gif',p3)
```

![](price3_animation.gif)



```{r}
prop_data_p4<- prop_data %>%
  mutate(age = 99-as.integer(substr(prop_data$remaining_lease,0,2))) %>%
  group_by(town)

```

```{r}
prop_data_tool4 <- prop_data %>%
  mutate(age = 99-as.integer(substr(prop_data$remaining_lease,0,2))) %>%
  group_by(town) %>%
  summarise(ave_age = round(mean(age),1))



tooltip_p4 <-  c(paste( "Town:", prop_data_tool4$town,
                        "\n Mean Age:" , prop_data_tool4$ave_age
))

```



```{r}

p4<- prop_data_p4 %>%
  ggplot(aes(x = reorder(town,age), 
             y = age)) +
  geom_boxplot_interactive(aes(tooltip = town)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  theme_bw() +
  
  theme(legend.position = "none") +
  theme(panel.grid = element_blank()) +
  labs(title = "Age of HDBs by residential towns, 2017 - 2023 Singapore", 
       y = "Age (years)", 
       x = "Residential Town") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


girafe(
  ggobj = p4,
  width_svg = 12
)

```




```{r}

p4<- prop_data_p4 %>%
  ggplot(aes(x = reorder(town,age), 
             y = age)) +
  geom_boxplot_interactive(aes(tooltip = town)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  theme_bw() +
  
  theme(legend.position = "none") +
  theme(panel.grid = element_blank()) +
  labs(title = "Age of HDBs by residential towns, 2017 - 2023 Singapore", 
       y = "Age (years)", 
       x = "Residential Town") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


girafe(
  ggobj = p4,
  width_svg = 12
)

```




```{r}  
p4<- prop_data_p4 %>%
  ggplot(mapping = aes(x=age, 
                       y = reorder(as.factor(town),-age),
                       fill = after_stat(x)
  )
  ) +
  geom_density_ridges_gradient( color = "white") +
  scale_fill_viridis_c_interactive(aes(tooltip = tooltip_p4))+
  #scale_fill_viridis_c(option = "C") +
  theme_bw() +
  
  theme(legend.position = "none") +
  theme(panel.grid = element_blank()) +
  labs(title = "Age of HDBs by residential towns, 2017 - 2023 Singapore", 
       x = "Age (years)", 
       y = "Residential Town")
p4
```



