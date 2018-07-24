
## Certificates {.tabset}

```{r cert}
certificates <- read_feather('data/education/certificates.feather')

certificates <- certificates[!duplicated(certificates$CPSIDP),]

certificates %<>%
  rename(MSA = METFIPS,
         year = YEAR) %>%
  pull_peers_MSA() %>%
  mutate(
    certification = if_else(PROFCERT == 2, 1, 0),
    certification = replace(certification, PROFCERT == 99, NA),
    degree = if_else(EDUC >= 91, 1, 0),
    cert_degree = if_else(certification == 1 | degree == 1, 1, 0))

certificates$city <- NA
certificates$city[certificates$METAREA == 3001] = "Grand Rapids"
certificates$city[certificates$METAREA == 7040] = "St. Louis"
certificates$city[certificates$METAREA == 5880] = "Oklahoma City"
certificates$city[certificates$METAREA == 8560] = "Tulsa"
certificates$city[certificates$METAREA == 3162] = "Greenville"
certificates$city[certificates$METAREA == 3840] = "Knoxville"
certificates$city[certificates$METAREA == 1001] = "Birmingham"
certificates$city[certificates$METAREA == 4520] = "Louisville"
certificates$city[certificates$METAREA == 3480] = "Indianapolis"
certificates$city[certificates$METAREA == 3760] = "Kansas City"
certificates$city[certificates$METAREA == 5921] = "Omaha"
certificates$city[certificates$METAREA == 3122] = "Greensboro"
certificates$city[certificates$METAREA == 1521] = "Charlotte"
certificates$city[certificates$METAREA == 1840] = "Columbus"
certificates$city[certificates$METAREA == 1641] = "Cincinnati"
certificates$city[certificates$METAREA == 5361] = "Nashville"
certificates$city[certificates$METAREA == 4920] = "Memphis"
certificates$city[certificates$METAREA == 3590] = "Jacksonville"
certificates$city[certificates$METAREA == 6642] = "Raleigh"
certificates$city[certificates$METAREA == 2002] = "Dayton"
certificates$city[certificates$METAREA == 6761] = "Richmond"

certificates %<>%
  filter(AGE > 24 & AGE < 65)

cert_svy <- svydesign(ids = ~1, weights = ~WTFINL, data = certificates)

cert_df <- svyby(~certification+cert_degree, ~city+year, design = cert_svy, svymean, na.rm = TRUE) %>%
  select(-se.certification, -se.cert_degree) %>%
  mutate(certification = certification * 100,
         cert_degree = cert_degree * 100)

certificates_young <- certificates %<>%
  filter(AGE < 35)

cert_svy <- svydesign(ids = ~1, weights = ~WTFINL, data = certificates_young)

cert_df_young <- svyby(~certification+cert_degree, ~city+year, design = cert_svy, svymean, na.rm = TRUE) %>%
  select(-se.certification, -se.cert_degree) %>%
  mutate(certification_young = certification * 100,
         cert_degree_young = cert_degree * 100)

cert_df_lou =  svyby(~certification, ~year, design = subset(cert_svy, COUNTY == 21111), svymean, na.rm = TRUE) 

names <- read_csv('data/MSA to FIPS.csv') %>% select(-MSA)

cert_df %<>% left_join(names) %>% select(-city)
cert_df_young %<>% left_join(names) %>% select(-city, -certification, -cert_degree)


data %<>% 
  select(-city, -baseline, - current) %>%
  full_join(cert_df, by = c('FIPS', 'year')) %>%
  pull_peers_FIPS()

```
### Ranking

```{r graphcert1}
rank_and_nb_group(data[data$year == 2017,], 
                  'certification', 
                  plot_title = 'Certificates, 2017',
                  subtitle_text = 'Ages 25-64')

rank_and_nb_group(data[data$year == 2017,], 
                  'cert_degree', 
                  plot_title = 'Certification or Degree, 2017',
                  subtitle_text = 'Ages 25-64')

```

### Trendline

```{r graphcert2}
graph_trendline(data,
                'certification',
                plot_title = 'Certifications',
                xmin = 2015,
                xmax = 2018)

graph_trendline(data,
                'cert_degree',
                plot_title = 'Certification or Degree',
                xmin = 2015,
                xmax = 2018)
```

### Ranking - Young
```{r graphcert3}
rank_and_nb_group(data[data$year == 2017,], 
                  'certification_young', 
                  plot_title = 'Certificates, 2017',
                  subtitle_text = 'Ages 25-34')

rank_and_nb_group(data[data$year == 2017,], 
                  'cert_degree_young', 
                  plot_title = 'Certification or Degree, 2017',
                  subtitle_text = 'Ages 25-34')

```

### Trendline - Young

```{r graphcert4}
graph_trendline(data,
                'certification_young',
                plot_title = 'Certifications',
                xmin = 2015,
                xmax = 2018)

graph_trendline(data,
                'cert_degree_young',
                plot_title = 'Certification or Degree',
                xmin = 2015,
                xmax = 2018)
```
