library(devtools)
library(redist)
library(dplyr)
library(ggplot2)
library(geomander)
library(tidyverse)
library(scales)
library(patchwork)
library(sf)
library(broom)
library(geomander)
library(tidyverse)
library(tidycensus)
library(patchwork)

census_api_key("0ad7e84c93c23d79cd10d1eafb450c5f30c7c958", install = TRUE)

# load precincts
data("precincts")

# create block data
block <- create_block_table(state = 'al')

# head(block)

# I want to categorise them based on county 
county_geometries2 <- block %>% 
  group_by(county) %>% 
  summarise(all_pop = sum(pop), all_pop_white = sum(pop_white), all_pop_black = sum(pop_black), 
            all_pop_hisp = sum(pop_hisp), all_pop_asian = sum(pop_asian))

head(county_geometries2)

# Read other relevant data here 

al_2020_vtd <- read.csv("al_2020_vtd.csv")
al_dist_blocks <- st_read("al_pl2020_b/al_pl2020_p1_b.shp")
al_cong_dist <- st_read("al_pl2020_cd/al_pl2020_cd.shp")
al_cong_prec <- st_read("al_gen_22_prec 2/al_gen_22_cong_prec.shp")

# Processing the data here a bit 
al_cong_prec <- al_cong_prec %>% mutate(CONG_DIST = as.numeric(CONG_DIST))

al_dist_test <- al_dist %>% mutate(party = ifelse(GEOID20=="0107","D","R")) # only 7th district is D
al_cong_dist <- al_cong_dist %>% mutate(GEOID20 = str_sub(GEOID20, 03))
al_cong_dist <- al_cong_dist %>% mutate(GEOID20 = as.numeric(GEOID20))


# Let's merge the block county data and congressional districts here 
shape1 <- block
shape2 <- al_cong_dist

shape2 <- st_transform(shape2, st_crs(shape1))
merged_shape <- st_join(shape1, shape2, join = st_intersects)

head(merged_shape)

merged_shape_county <- merged_shape %>% 
  group_by(county) %>%
  summarise(all_pop = sum(pop), all_pop_white = sum(pop_white), all_pop_black = sum(pop_black), 
            all_pop_hisp = sum(pop_hisp), all_pop_asian = sum(pop_asian), GEOID = mean(GEOID20))

merged_shape_county %>% 
  mutate(GEOID = round(GEOID))

save(merged_shape_county, file = "cong_on_county_al")

head(al_dist_blocks) # There is GEOID20 
head(al_cong_prec) # THERE IS NO GEOID??? 
head(al_2020_vtd)

test <- al_cong_prec %>% 
  group_by(COUNTYFP) %>% 
  summarize(geometry = st_union(geometry))

plot(test)

al_dist <- al_cong_dist
al_race <- st_read("al_race_2021_bg/al_race_2021_bg.shp")

head(al_dist_blocks)
head(al_dist)
head(al_race)

alabama_map <- redist_map(merged_shape_county, existing_plan = GEOID, pop_tol = 0.05, total_pop = all_pop)
plot(alabama_map, adj=T) + plot(alabama_map)

alabama_plans = redist_smc(alabama_map, 500, compactness = 1, runs = 2)
redist.plot.plans(alabama_plans, draws=1:6, shp=alabama_map)

summary(alabama_plans)

### adding some summary statisitcs 

county_perims = prep_perims(alabama_map, alabama_map$adj)

alabama_plans = alabama_plans %>%
  mutate(pop_dev = abs(total_pop / get_target(alabama_map) - 1),
         comp = distr_compactness(alabama_map, "PolsbyPopper", perim_df=county_perims),
         pct_min = group_frac(alabama_map, all_pop - all_pop_white, all_pop))
print(alabama_plans)

plan_sum = group_by(alabama_plans, draw) %>%
  summarize(max_dev = max(pop_dev),
            avg_comp = mean(comp),
            max_pct_min = max(pct_min))

hist(plan_sum, max_dev) + hist(alabama_plans, comp) +
  plot_layout(guides="collect")

head(al_race)

test <- left_join(al_race, al_dist %>% select(STATEFP20, STATEFP), by = "STATEFP")

iowa_map = redist_map(iowa, existing_plan=cd_2010, pop_tol=0.01, total_pop = pop)
iowa_plans = redist_smc(iowa_map, 500, compactness=1, runs=2)

redist.plot.plans(iowa_plans, draws=1:6, shp=iowa_map)


head(al_dist_test_2 )
p <- ggplot() + 
  geom_sf(data = al_dist, aes(fill = GEOID20), show.legend = FALSE) + 
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )
p

q <- ggplot() + 
  geom_sf(data = al_dist_test, aes(fill = party), show.legend = FALSE) + 
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) + 
  scale_fill_manual(values = c("dodgerblue", "firebrick"), name = "party", labels = c("D", "R"))
q

r <- ggplot() +
  geom_sf(data = al_dist, show.legend = FALSE, alpha = 0, color="black") + 
  geom_sf(data = al_race, aes(fill = BLK_ALL21 / TOT_POP21), color=NA, lwd = 0, show.legend = FALSE) + 
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) + 
  scale_fill_gradient(low = "white", high = "darkolivegreen") 
r

s <- ggplot() +
  geom_sf(data = al_race, aes(fill = BLK_ALL21 / TOT_POP21), color=NA, show.legend = FALSE) + 
  geom_sf(data = al_dist, show.legend = FALSE, alpha = 0, color="black") + 
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) + 
  scale_fill_gradient(low = "white", high = "darkolivegreen") 
s

w <- ggplot() +
  # Base layer for all districts with demographic information
  geom_sf(data = al_race, aes(fill = BLK_ALL21 / TOT_POP21), color = NA, show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "darkolivegreen") +
  
  # Overlay for non-highlighted districts
  geom_sf(data = al_dist[al_dist$GEOID20 != "0107", ], fill = "black", alpha = 0.5, color = NA, show.legend = FALSE) +
  
  geom_sf(data = al_dist, show.legend = FALSE, alpha = 0, color="black") + 
  
  # Theme settings
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )

w

y <- ggplot() +
  # Base layer for all districts with demographic information
  geom_sf(data = al_race, aes(fill = BLK_ALL21 / TOT_POP21), color = NA, show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "darkolivegreen") +
  
  # Overlay for non-highlighted districts
  geom_sf(data = al_dist[!(al_dist$GEOID20 %in% c("0102", "0103")), ], fill = "black", alpha = 0.5, color = NA, show.legend = FALSE) +
  
  geom_sf(data = al_dist, show.legend = FALSE, alpha = 0, color="black") + 
  
  # Theme settings
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )

y

### Maryland 
md_dist <- st_read("md_cvap_2020_cd/md_cvap_2020_cd.shp")
md_dist3 <- md_dist %>% subset(GEOID20 == "000000002403")
head(md_dist3)

t <- ggplot() + 
  geom_sf(data = md_dist3, aes(fill = GEOID20), show.legend = FALSE) + 
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) + 
  scale_fill_manual(values = c("plum1"), name = "GEOID20", labels = c("3"))
t

### Texas 
tx_dist <- st_read("tx_cong_2021/PLANC2193.shp")
tx_dist33 <- tx_dist %>% subset(District == 33)
tx_dist33$District <- as.factor(tx_dist33$District)
u <- ggplot() + 
  geom_sf(data = tx_dist33, aes(fill = District), show.legend = FALSE) + 
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) + 
  scale_fill_manual(values = c("plum1"), name = "District", labels = c("33"))
u

### Illinois 
il_dist <- st_read("il_cong_2011_to_2021/il_cong_2011_to_2021.shp")
head(il_dist)
il_dist4 <- il_dist %>% subset(OBJECTID == 4)
il_dist4$OBJECTID <- as.factor(il_dist4$OBJECTID)

v <- ggplot() + 
  geom_sf(data = il_dist4, aes(fill = OBJECTID), show.legend = FALSE) + 
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) + 
  scale_fill_manual(values = c("plum1"), name = "OBJECTID", labels = c("4"))
v

### More Demographic Data 

tn_race <- st_read("tn_cong_adopted_2022/Congress.shp")
tn.dist <- tn_race %>% select("OBJECTID", "geometry")
tn.census <- st_read("tn_pl2020_b/tn_pl2020_p1_b.shp")
head(tn.census)


z <- ggplot() + 
  geom_sf(data = tn.census, aes(fill = P0010004/P0010001), color = NA, show.legend = FALSE) + 
  geom_sf(data = tn.dist, show.legend = FALSE, alpha = 0, color="black") + 
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) + 
  scale_fill_gradient(low = "white", high = "darkolivegreen")
z

ggplot()

### Save the files 

ggsave(
  plot = r,
  filename = "pop_percentage.png",
  bg = "transparent"
)

ggsave(
  plot = s,
  filename = "pop_and_distr.png",
  bg = "transparent"
)

ggsave(
  plot = t,
  filename = "md.png",
  bg = "transparent"
)

ggsave(
  plot = u,
  filename = "tx.png",
  bg = "transparent"
)

ggsave(
  plot = v,
  filename = "il.png",
  bg = "transparent"
)

ggsave(
  plot = w,
  filename = "highlight7.png",
  bg = "transparent"
)

ggsave(
  plot = y,
  filename = "highlight2_3.png",
  bg = "transparent"
)

ggsave(
  plot = z,
  filename = "TEN E SEE.png",
  bg = "transparent"
)

### TESTING 

data <- data.frame(
  Category = c("Districts", "Population"),
  Percentage = c(14, 27),
  Fill = c("purple", "grey")
)

# Create the plot
ggplot2_plot <- ggplot(data, aes(x = Category, y = Percentage, fill = Fill)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("purple", "grey")) +
  labs(title = "Percentage of Black Population in Alabama",
       x = "", y = "") +
  theme_minimal() +
  theme(text = element_text(color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        legend.position = "none") +
  coord_flip()

# Display the plot
print(ggplot2_plot)