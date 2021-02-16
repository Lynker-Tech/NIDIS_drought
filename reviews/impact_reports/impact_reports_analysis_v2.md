Synopsis of Drought Impact Reporter Media Reports
================
Keith Jennings

This script takes raw impact data from the [Drought Impact
Reporter](https://droughtreporter.unl.edu/map/) and analyzes the number
of reports for each impact category within the Intermountain West. For
this project, the region consists of Arizona, New Mexcio, Colorado,
Utah, and Wyoming.

## 1\) Load packages and import data

``` r
library(tidyverse)
library(cowplot); theme_set(theme_cowplot()) # I like the cowplot because it makes plot pretty
```

``` r
drive_dir =  "/Volumes/GoogleDrive/My Drive/noaa-cpo-cwd/" # change for local machine
impacts <- read.csv(paste0(drive_dir,
                           "data/impacts/misc/drought_impact_reporter/dir_impacts_from_media_all.csv"))
```

These impact data include unique impact IDs, titles, descriptions,
places, and other relevant information. For the lit review paper, we are
assessing what percent each category comprises of the total number of
reports.

``` r
head(impacts) %>% knitr::kable()
```

|   Id | Title                                                                          | Description                                                                                                                                                                                                                             | Post.Date  | Start.Date | End.Date   | Level  | FIPS     | Place              | Categories                                             | X  |
| ---: | :----------------------------------------------------------------------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :--------- | :--------- | :--------- | :----- | :------- | :----------------- | :----------------------------------------------------- | :- |
| 1624 | Voluntary water restrictions in Greensboro, Burlington, and Thomasville, NC    | Voluntary water restrictions are in effect for Greensboro, Burlington, and Thomasville as drought conditions linger in the region.                                                                                                      | 11/18/2008 | 11/17/2008 |            | city   | 37-09060 | Burlington,NC      | Water Supply & Quality,Relief, Response & Restrictions | NA |
| 1624 | Voluntary water restrictions in Greensboro, Burlington, and Thomasville, NC    | Voluntary water restrictions are in effect for Greensboro, Burlington, and Thomasville as drought conditions linger in the region.                                                                                                      | 11/18/2008 | 11/17/2008 |            | city   | 37-28000 | Greensboro,NC      | Water Supply & Quality,Relief, Response & Restrictions | NA |
| 1624 | Voluntary water restrictions in Greensboro, Burlington, and Thomasville, NC    | Voluntary water restrictions are in effect for Greensboro, Burlington, and Thomasville as drought conditions linger in the region.                                                                                                      | 11/18/2008 | 11/17/2008 |            | city   | 37-67420 | Thomasville,NC     | Water Supply & Quality,Relief, Response & Restrictions | NA |
| 1625 | Drought lowered dry pea and lentil production in Montana and North Dakota      | Drought hurt the lentil crop in Montana and North Dakota causing production to fall by 30 % compared to 2007.                                                                                                                           | 11/19/2008 | 01/01/2008 | 11/19/2008 | state  | 30       | Montana            | Agriculture                                            | NA |
| 1625 | Drought lowered dry pea and lentil production in Montana and North Dakota      | Drought hurt the lentil crop in Montana and North Dakota causing production to fall by 30 % compared to 2007.                                                                                                                           | 11/19/2008 | 01/01/2008 | 11/19/2008 | state  | 38       | North Dakota       | Agriculture                                            | NA |
| 1626 | Somerset County Drought Task Force advocates 10 % voluntary water conservation | The voluntary water conservation level remains 10 % in Somerset County at the behest of the Somerset County Drought Task Force, although the Department of Environmental Protection recommended a 5 % voluntary reduction in water use. | 11/19/2008 | 11/18/2008 |            | county | 42111    | Somerset County,PA | Water Supply & Quality,Relief, Response & Restrictions | NA |

## 2\) Work with the data

First we’ll need to subset the data to only locations within the
Intermountain West. Let’s make a list of state abbreviations and names
as they appear in the dataframe.

``` r
states <- c("Arizona", "AZ",
            "New Mexico", "NM",
            "Colorado", "CO",
            "Utah", "UT",
            "Wyoming", "WY")
```

We’ll then filter the data to include only locations including those
strings.

``` r
impacts_IMW <- impacts %>% 
  filter(str_detect(Place, paste(states, collapse = "|")))
```

This is good, but includes some false positives like Colorado County,
TX. To fix this, we’ll remove any entries that include other state
abbreviations.

``` r
states_omit = state.abb[-c(3, 6, 31, 44, 50)] # numbers remove IMW states from vector

# filter to exlcude states in states_omit
impacts_IMW2 <- impacts_IMW %>% 
  filter(str_detect(Place, paste(states_omit, collapse = "|")) == FALSE)
```

Now we’ll filter down to one row per unique ID to ensure impact reports
aren’t double-counted.

``` r
impacts_IMW3 <- impacts_IMW2 %>% 
  distinct(Id, .keep_all = TRUE)
```

## 3\) Summarize the data

Now we’ll summarise the number of reports per category

``` r
impacts_summary <- impacts_IMW3 %>% 
  summarize(agriculture = sum(str_detect(Categories, "Agriculture")),
            energy = sum(str_detect(Categories, "Energy")),
            fire = sum(str_detect(Categories, "Fire")),
            restrictions = sum(str_detect(Categories, "Restrictions")),
            plants = sum(str_detect(Categories, "Plants")),
            recreation = sum(str_detect(Categories, "Recreation")),
            water_supply = sum(str_detect(Categories, "Water Supply")),
            public_health = sum(str_detect(Categories, "Public Health")),
            business = sum(str_detect(Categories, "Business"))) %>% 
  pivot_longer(cols = everything(),
               values_to = "n") %>% 
  mutate(pct_of_total = n/length(impacts_IMW3$Id) * 100) %>% 
  arrange(desc(n))
```

Note: the above code will produce a pct\_of\_total sum greater than 100
because each unique ID can contain multiple categories.

``` r
impacts_summary %>% knitr::kable()
```

| name           |   n | pct\_of\_total |
| :------------- | --: | -------------: |
| restrictions   | 879 |     36.6708385 |
| agriculture    | 804 |     33.5419274 |
| water\_supply  | 650 |     27.1172299 |
| fire           | 638 |     26.6166041 |
| plants         | 442 |     18.4397163 |
| public\_health | 308 |     12.8493951 |
| recreation     | 136 |      5.6737589 |
| business       |  75 |      3.1289111 |
| energy         |   9 |      0.3754693 |
