acs_time <- function(folder, starting_year = 2005){
  wd <- getwd()
  directory <- paste0(wd, folder)
  file_names <- list.files(directory)
  n <- length(file_names)
  y <- starting_year
  for (i in 1:n){
    file_path <- paste0(wd, folder, file_names[i])
    data <- read_csv(file_path, skip=1, col_types = cols("Id2" = col_double()))
    names(data)[names(data) == 'Id2'] <- 'FIPS'
    all_peers <- data %>% filter(FIPS %in% c(1073, 12031, 18097, 21111, 26081, 
                                             29095, 29189, 29510, 31055, 37081, 
                                             37119, 37183, 39049, 39061, 39113, 
                                             40109, 40143, 45045, 47037, 47093, 
                                             47157, 51760))
    
    all_peers$year <- y
    y <- y + 1
    
    if(i == 1){
      df <- all_peers 
    }
    else{
      names(all_peers) <- names(df)
      df <- rbind(df, all_peers)
    }
  }
  df
}

wonder_time <- function(folder, geog_type){
  wd <- getwd()
  wd <- paste(wd, folder, sep = "/")
  file_names <- list.files(wd)
  file_geog <- substr(file_names, 1, 5)
  n <- length(file_names)
  
  for (i in 1:n){
    file_path <- paste(wd, file_names[i], sep = "/")
    data <- read_tsv(file_path)
    
    output <- data %>% filter(!is.na(Year))
    output[[geog_type]] <- file_geog[i]
    
    if(i == 1){df <- output}
    else{df <- rbind(df, output)}
    
  }
  df
}

brfss_time <- function(start_year = 2002){
  wd <- getwd()
  wd <- paste(wd, "brfss_download", sep = "/")
  file_names <- list.files(wd)
  n <- length(file_names)
  y <- start_year
  
  for (i in 1:n){
    file_path <- paste(wd, file_names[i], sep = "/")
    data <- sasxport.get(file_path)
    
    data <- map_df(data, remove_var_label)
    
    if(y == 2002){
      output <- data.frame(msa = data$a.mmsa, 
                           wgt = data$a.mmsawt, 
                           obs = data$seqno,
                           age = data$age.mmsa,
                           hlth = data$genhlth, 
                           physdays = data$physhlth, 
                           mentdays = data$menthlth)
    } 
    else if(y == 2003){
      output <- data.frame(msa = data$x.mmsa, 
                           wgt = data$x.lmmsawt,
                           obs = data$seqno,
                           age = data$x.ageg.,
                           hlth = data$genhlth, 
                           physdays = data$physhlth, 
                           mentdays = data$menthlth)
    }
    else if(y >= 2004 & y <= 2010){
      output <- data.frame(msa = data$x.mmsa, 
                           wgt = data$x.mmsawt,
                           obs = data$seqno,
                           age = data$age.mmsa,
                           hlth = data$genhlth, 
                           physdays = data$physhlth, 
                           mentdays = data$menthlth)
    }
    else if(y >= 2011){
      output <- data.frame(msa = data$x.mmsa, 
                           wgt = data$x.mmsawt,
                           obs = data$seqno,
                           age = data$x.age.g,
                           hlth = data$genhlth, 
                           physdays = data$physhlth, 
                           mentdays = data$menthlth)
      
    }
    
    output$year <- y
    y <- y + 1
    
    if(i == 1){df <- output}
    else{df <- rbind(df, output)}
    
  }
  df
}

insurance_time <- function(directory = "", starting.year=2008){
  wd <- getwd()
  file_names <- list.files(directory)
  n<-length(file_names)
  y<-starting.year
  for (i in 1:n){
    file_path <- paste(directory, file_names[i], sep = "")
    data<-read_csv(file_path, skip=79)
    
    if(y > 2007){ data <- data %>% select(-X26) }
    
    data$statefips <- as.character(data$statefips)
    data$countyfips <- as.character(data$countyfips)
    
    data$statefips <- if_else( nchar(data$statefips) < 2, paste0("0",data$statefips), data$statefips)
    data$countyfips <- if_else( nchar(data$countyfips) < 3, paste0("0",data$countyfips), data$countyfips)
    data$countyfips <- if_else( nchar(data$countyfips) < 3, paste0("0",data$countyfips), data$countyfips)
    
    data$FIPS <- as.numeric(paste0(data$statefips, data$countyfips))
    all.peers <-subset(data, data$FIPS == 1073 |data$FIPS == 37119
                       |data$FIPS == 39061 |data$FIPS == 39049
                       |data$FIPS == 26081 |data$FIPS == 37081
                       |data$FIPS == 45045 |data$FIPS == 18097
                       |data$FIPS == 29095 |data$FIPS == 47093
                       |data$FIPS == 21111 |data$FIPS == 47157
                       |data$FIPS == 47037 |data$FIPS == 40109
                       |data$FIPS == 31055 |data$FIPS == 29189
                       |data$FIPS == 29510
                       |data$FIPS == 40143 |data$FIPS == 39113
                       |data$FIPS == 12031 |data$FIPS == 37183
                       |data$FIPS == 51760)
    
    all.peers$year<-y
    y<-y+1
    
    if(i==1){
      df<-all.peers 
    }
    else{
      names(all.peers)<-names(df)
      df<-rbind(df, all.peers)
    }
  }
  df
}

pull_peers_MSA<-function(data){
  all.peers <- filter(data, data$MSA %in% c("24340", "41180", "36420", "46140", "24860", "28940", "13820", "26900", "31140", "28140", "36540", "24660", "16740", "18140", "17140", "34980", "32820", "27260", "39580", "19380", "40060"))
  
  all.peers$baseline <- 1
  all.peers$current  <- 1
  
  all.peers$baseline[all.peers$MSA == 24340 | all.peers$MSA == 41180
                     |all.peers$MSA == 36420 | all.peers$MSA == 46140
                     |all.peers$MSA == 24860 | all.peers$MSA == 28940] <-0
  
  all.peers$current[all.peers$MSA == 27260 | all.peers$MSA == 39580
                    |all.peers$MSA == 19380 | all.peers$MSA == 40060] <-0
  all.peers
}

pull_peers_FIPS <- function(dat){
  all.peers <- subset(dat, dat$FIPS == 1073 | dat$FIPS == 37119
                      |dat$FIPS == 39061 |dat$FIPS == 39049
                      |dat$FIPS == 26081 |dat$FIPS == 37081
                      |dat$FIPS == 45045 |dat$FIPS == 18097
                      |dat$FIPS == 29095 |dat$FIPS == 47093
                      |dat$FIPS == 21111 |dat$FIPS == 47157
                      |dat$FIPS == 47037 |dat$FIPS == 40109
                      |dat$FIPS == 31055 |dat$FIPS == 29189
                      |dat$FIPS == 29510 |dat$FIPS == 40143
                      |dat$FIPS == 12031 |dat$FIPS == 37183
                      |dat$FIPS == 39113 |dat$FIPS == 51760
                      |dat$FIPS == "01073" | dat$FIPS == "MERGED")
  all.peers$baseline <- 1
  all.peers$current <- 1
  all.peers$baseline[all.peers$FIPS==26081|all.peers$FIPS==29189
                     |all.peers$FIPS==29510|all.peers$FIPS==40109
                     |all.peers$FIPS==40143|all.peers$FIPS==45045
                     |all.peers$FIPS==47093|all.peers$FIPS=="MERGED"]<-0
  all.peers$current[all.peers$FIPS== 12031|all.peers$FIPS==37183|
                      all.peers$FIPS==39113|all.peers$FIPS==51760]<-0
  
  city <- c('Grand Rapids', 'St. Louis', 'Oklahoma City', 'Tulsa', 'Greenville', 'Knoxville', 
            'Birmingham', 'Indianapolis', 'Louisville', 'Kansas City', 'Omaha', 'Greensboro', 
            'Charlotte', 'Columbus', 'Cincinnati', 'Nashville', 'Memphis', 'Jacksonville', 
            'Raleigh', 'Dayton', 'Richmond')
  
  FIPS_codes <- c(26081, 'MERGED', 40109, 40143, 45045, 47093, 1073, 18097, 21111, 29095, 31055, 
                  37081, 37119, 39049, 39061, 47037, 47157, 12031, 37183, 39113, 51760)
  
  names_df <- data.frame(city, FIPS_codes)
  
  all.peers <- left_join(all.peers, names_df, by = c('FIPS' = 'FIPS_codes'))
  all.peers
}

weight_stl <- function(df_original, variables, weight_variable = ""){
  
  n <- 1
  
  if(weight_variable == ""){
    population_data <- read_csv("data/population_data.csv")
    if(typeof(df_original$FIPS) == 'character'){
      population_data$FIPS <- as.character(population_data$FIPS)
    }
    df_original <- df_original %>% left_join(population_data, by = c("FIPS", "year"))
    weight_variable <- 'population'
  }
  
  for(v in 1:length(variables)){
    df  <- df_original[,c('FIPS', 'year', variables[n], weight_variable)]
    
    df$var <- df[[variables[n]]]
    df$weight_var <- df[[weight_variable]]
    
    df %<>%
      mutate(FIPS = replace(FIPS, FIPS == '29189' | FIPS == '29510', 'MERGED')) %>%
      group_by(FIPS, year) %>%
      summarise(var = weighted.mean(var, weight_var)) %>%
      ungroup()
    
    df[[variables[n]]] <- df$var
    
    df %<>% select(-var)
    
    if(n == 1){
      output <- df
    }else{
      output <- full_join(output, df, by = c('FIPS', 'year'))
    }
    
    n = n + 1
    
  }
  output
}

bind_df <- function(...){
  data_frames <- list(...)
  output <- reduce(data_frames, full_join, by = c('FIPS', 'year'))
  output
}

rpp_col_adj <- function(x){
  x *  data$rpp_index * data$cpi_index
}
