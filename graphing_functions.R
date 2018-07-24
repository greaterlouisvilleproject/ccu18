
#RANKING GRAPHS

#plots ranked bar chart of cities
rank_and_nb_group<-function(df, var, order="Descending", peers="Current",
                            plot_title="", y_title = "Percent", caption_text = "",
                            sigfig = 3, num_dec = 1, text = TRUE, h_line = FALSE,
                            subtitle_text = "", thousands_comma = T){
  #SET GRAPH PARAMETERS
  
  #copy variable var to a new column for use with the '$' operator
  df$var <- df[[var]]
  
  #subset df to peer parameter
  if(peers=="Current"){
    df<-subset(df,current ==1)
  }
  if(peers=="Baseline"){
    df<-subset(df,baseline ==1)
  }
  
  #sort df according to order parameter
  if(order=="Descending"){
    d.order<-df[order(-df$var),]
  }
  if(order=="Ascending"){
    d.order<-df[order(df$var),]
  }
  
  #create numbered city labels for left side of graph
  ranks<-1:length(df$var)
  d.rank<-cbind(d.order,ranks)
  names<-paste(d.rank$ranks,".",sep="")
  names<-paste(names,d.rank$city)
  d.graph<-cbind(d.rank,names)
  
  #set bar color
  breaks <- classIntervals(d.graph$var,3,style="jenks")
  d.graph$color <- NA
  d.graph$color[d.graph$var <= breaks$brks[2]] <- "green"
  d.graph$color[d.graph$var > breaks$brks[2] & d.graph$var <= breaks$brks[3]] <- "yellow"
  d.graph$color[d.graph$var > breaks$brks[3]] <- "red"
  
  #round numbers in graph according to sigfif, num_dec, and thousands_comma parameters
  thousands_char <- if_else(thousands_comma == TRUE, ",", "")
  d.graph$round <- d.graph$var %>%
                    signif(digits = sigfig) %>%
                    round(digits = num_dec) %>%
                    format(big.mark = thousands_char)
  
  #Set text format, highlight and italicise Louisville text, highlight Louisville bar
  d.graph$textfont <- "Museo Sans 300"
  d.graph$textfont[d.graph$city == "Louisville"] <- "Museo Sans 300 Italic"
  d.graph$textcolor <- "black"
  d.graph$textcolor[d.graph$city == "Louisville"] <- "#00a9b7"
  d.graph$linecolor <- "white"
  d.graph$linecolor[d.graph$city == "Louisville"] <- "#00a9b7"

  #PLOT GRAPH
  
  #initial plot
  p <- ggplot(data=d.graph,aes(x=factor(names, levels=rev(unique(names))),
                               y=var,fill=factor(color)))+guides(fill=FALSE)
  
  #add  bars
  p <- p+geom_bar(stat="identity",color=rev(d.graph$linecolor), size = 1)+coord_flip()+theme_tufte()
  if(order=="Ascending"){
    p <- p+scale_fill_manual(values=c("#96ca4f","#db2834","#ffd600"))
  }
  if(order=="Descending"){
    p <- p+scale_fill_manual(values=c("#db2834","#96ca4f","#ffd600"))
  }
  
  #add features
  p <- p + theme(text = element_text(family = "Museo Sans 300"),
                 plot.title = element_text(size = 36, hjust = 0.5),
                 axis.text.y = element_text(hjust=0, family = rev(d.graph$textfont),
                                            size=20, color = rev(d.graph$textcolor)),
                 axis.title.x = element_text(size = 24),
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(),
                 plot.caption = element_text(),
                 plot.subtitle = element_text(hjust = 0.5, size = 18))
  
  #add numeric labels to bars based on text parameter
  if (text == TRUE) {
    p <-
      p + geom_text(
        aes(label = round),
        hjust = 1.1,
        size = 8,
        family = "Museo Sans 300"
      )
  }
  
  #add vertical line to the left side of the bars based on the h_line parameter
  if (h_line == TRUE){
    p <- p + geom_hline(yintercept = 0, linetype = "longdash", size = 1)
  }
  p <- p+labs(title = plot_title, y= y_title,
              x = "", caption = caption_text,
              subtitle = subtitle_text)
  p
}

#TRENDLINE GRAPHS

##rolling mean functions for 3-year and 5-year trendlines
rollmean3 <- function(x){
  n <- length(x)
  y <- NA
  for(i in 1:n){
    y[i] <- mean(c(x[i-1],x[i],x[i+1]))
    y[1] <- NA
  }
  y
}

rollmean5 <- function(x){
  n <- length(x)
  y <- NA
  for(i in 1:n){
    y[i] <- mean(c(x[i-2],x[i-1],x[i],x[i+1],x[i+2]))
    y[1] <- NA
    y[2] <- NA
  }
  y
}

#Basic trendline graph
graph_trendline<-function(df,var, plot_title="",y_title="Percent", peers = "Current", 
                          caption_text = "", subtitle_text = "",
                          rollmean = 1, xmin = 2005, xmax = 2016,
                          break_settings = ""){
  
  #create a new variable to use var with the '$' operator
  df$var <- df[[var]]
  
  #if(is.na(df$var[df$year == 2016 & df$city == "Louisville"]))  xmax = 2015
  
  #subset to peers and remove Louisville
  if(peers=="Current"){
    df.wol <- subset(df,current == 1 & FIPS!=21111)
  }
  
  if(peers=="Baseline"){
    df.wol <- subset(df,baseline == 1 & FIPS!=21111)
  }
  
  #calculate 25th and 75th percentiles
  output_wol = df %>% 
    group_by(year) %>%
    summarise(first_quarter = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var, na.rm = TRUE),
              third_quarter = quantile(var, prob = 0.75, na.rm = TRUE))
  
  #extract Louisville values
  lville = df %>% 
    filter(FIPS == 21111) %>% 
    select(var, year)
  
  #join 25th percentile, 75th percentile, and Louisville values
  dat = full_join(lville, output_wol, by = "year")
  
  if(xmin == 2000 & rollmean == 3){
    var_2000 <- dat$var[dat$year == 2000]
    first_quarter_2000 <- dat$first_quarter[dat$year == 2000]
    mean_2000 <- dat$mean[dat$year == 2000]
    third_quarter_2000 <- dat$third_quarter[dat$year == 2000]
  }
  
  #Calculate 3- or 5-year rolling average
  if (rollmean == 3){
    dat$var = rollmean3(dat$var)
    dat$first_quarter = rollmean3(dat$first_quarter)
    dat$mean = rollmean3(dat$mean)
    dat$third_quarter = rollmean3(dat$third_quarter)
    
    if(xmin != 2000){
      dat <- dat %>% filter((year > xmin))
      xmin = xmin +1
    }
    
    dat <- dat %>% filter((year < xmax))
    xmax = xmax -1
    subtitle_text = paste0(subtitle_text, "3-year rolling average")
  }
  if (rollmean == 5){
    dat$var = rollmean5(dat$var)
    dat$first_quarter = rollmean5(dat$first_quarter)
    dat$mean = rollmean5(dat$mean)
    dat$third_quarter = rollmean5(dat$third_quarter)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = paste0(subtitle_text, "5-year rolling average")
  }
  
  if(xmin == 2000 & rollmean == 3){
    dat$var[dat$year == 2000] <- var_2000
    dat$first_quarter[dat$year == 2000] <- first_quarter_2000
    dat$mean[dat$year == 2000] <- mean_2000
    dat$third_quarter[dat$year == 2000] <- third_quarter_2000
  }
  
  #use to write out 2016 child poverty statistics (also write out the line above filtering out 2016 data)
  #write_csv(dat, 'C:/Users/Harrison Kirby/Desktop/GLP/child_pov_output2.csv')
  
  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    } 
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }
  
  #reshape data
  data_long <- melt(dat, id="year")
  data_long$variable = factor(data_long$variable, levels = c("var", "third_quarter", "mean", "first_quarter"))
  data_long <- data_long[!is.na(data_long$value),]
  
  #initial line plot
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=variable,linetype=variable, alpha=variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  p<-p+scale_y_continuous(labels = comma)

  #add color and line types
  cPalette <- c("#00a9b7", "grey50", "black","grey50")#"#f44542", "black","#00a346")
  p <- p + scale_colour_manual(
    values = cPalette,
    labels = c(
      "Louisville",
      "75th Percentile",
      "Peer City Mean",
      "25th Percentile"
    )
  ) +
    scale_linetype_manual(
      values = c("solid", "dashed", "dashed", "dashed"),
      labels = c(
        "Louisville",
        "75th Percentile",
        "Peer City Mean",
        "25th Percentile"
      )
    ) +
    scale_alpha_manual(
      values = c(1, .65, .8, .65),
      labels = c(
        "Louisville",
        "75th Percentile",
        "Peer City Mean",
        "25th Percentile"
      )
    )
  
  #add remaining style and elements
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=24, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=36, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             axis.title = element_text(size = 24),
             legend.text=element_text(size=24, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 18))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}

graph_trendline_change<-function(df,var, plot_title="",y_title = "", peers = "Current", 
                          caption_text = "", subtitle_text = "",
                          newest_year = 2016, oldest_year = 2005,
                          xmin = 2005, xmax = 2016,
                          break_settings = ""){
  
  #create a new variable to use var with the '$' operator
  df$var <- df[[var]]
  
  #subset to peers
  if(peers=="Current"){
    df <- subset(df, current == 1)
  }
  
  if(peers=="Baseline"){
    df <- subset(df, baseline == 1)
  }
  
  #select vars needed
  df = df %>% 
    select(year, city, var) %>%
    filter(year >= oldest_year & year <= newest_year)
  
  #calculate which peers had the biggest change
  city_list <- df %>%
    filter(year == newest_year | year == oldest_year) %>%
    spread(year, var) 
  
  city_list$change <- city_list[ ,3] - city_list[ ,2] #hard coding by column position as year will create different var names
  
  city_list <- city_list %>%
    filter(change == max(change) | change == min(change) |  city == "Louisville") %>%
    mutate(category = case_when(
      change == max(change) ~ paste0("Max: ", city),
      change == min(change) ~ paste0("Min: ", city),
      city == "Louisville" ~ paste0("", city),
      TRUE ~ ""
    ))  %>%
    select(city, category)
  
  df <- df %>%
    filter(city %in% city_list$city) %>%
    left_join(city_list, by = "city")
  
  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    break_settings = seq(xmin, xmax, 2)
  }
  
  #initial line plot
  p <- ggplot(data = df,aes(x = year, y = var, colour = category))+
    geom_point(size = 1.8)+
    geom_line(size = 1) + scale_colour_manual(values = c("blue", "darkgreen", "red"))
  p <- p + theme_bw()
  midpoint <- (max(df$var, na.rm = TRUE) + min(df$var, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(df$var, na.rm = TRUE) - border_space, max(df$var, na.rm=TRUE) + border_space))
  p <- p + scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  
  #add remaining style and elements
  p <- p + theme(text = element_text(family = "Museo Sans 300"),
                 legend.title=element_blank(),
                 legend.position = "top",
                 axis.text=element_text(size = 24, family = "Museo Sans 300"),
                 axis.title.x =element_text(size = 24),
                 axis.ticks.y=element_blank(),
                 plot.title=element_text(size = 36, hjust=.5, family = "Museo Sans 300",
                                         margin=margin(b = 10, unit="pt")),
                 legend.text=element_text(size = 24, family = "Museo Sans 300"),
                 plot.caption = element_text(family = "Museo Sans 300"),
                 plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 18))
  p <- p + labs(title=plot_title,x="Year",
                y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}

#Trendline graph for white, black, and hispanic including peer city
graph_trendline_race_peer<-function(df,vars, plot_title="",y_title="Percent", peers = "Current", 
                               caption_text = "", subtitle_text = "",
                               rollmean = 1, xmin = 2005, xmax = 2016,
                               break_settings = ""){
  
  #create a new variable to use var with the '$' operator
  df$var_white <- df[[vars[1]]]
  df$var_black <- df[[vars[2]]]
  df$var_hisp  <- df[[vars[3]]]
  
  #if(is.na(df$var[df$year == 2016 & df$city == "Louisville"]))  xmax = 2015
  
  #subset to peers and remove Louisville
  if(peers=="Current"){
    df.wol <- subset(df,current == 1 & FIPS!=21111)
  }
  
  if(peers=="Baseline"){
    df.wol <- subset(df,baseline == 1 & FIPS!=21111)
  }
  
  #calculate 25th and 75th percentiles
  output_wol = df %>% 
    group_by(year) %>%
    summarise(
      white_q1   = quantile(var_white, prob = 0.25, na.rm = TRUE),
      white_mean = mean(var_white, na.rm = TRUE),
      white_q3   = quantile(var_white, prob = 0.75, na.rm = TRUE),
      black_q1   = quantile(var_black, prob = 0.25, na.rm = TRUE),
      black_mean = mean(var_black, na.rm = TRUE),
      black_q3    = quantile(var_black, prob = 0.75, na.rm = TRUE),
      hisp_q1    = quantile(var_hisp, prob = 0.25, na.rm = TRUE),
      hisp_mean  = mean(var_hisp, na.rm = TRUE),
      hisp_q3    = quantile(var_hisp, prob = 0.75, na.rm = TRUE))
  
  #extract Louisville values
  lville = df %>% 
    filter(FIPS == 21111) %>% 
    select(var_white, var_black, var_hisp, year)
  
  #join 25th percentile, 75th percentile, and Louisville values
  dat = full_join(lville, output_wol, by = "year")
  
  if(xmin == 2000 & rollmean == 3){
    var_2000 <- dat$var[dat$year == 2000]
    first_quarter_2000 <- dat$first_quarter[dat$year == 2000]
    mean_2000 <- dat$mean[dat$year == 2000]
    third_quarter_2000 <- dat$third_quarter[dat$year == 2000]
  }
  
  #Calculate 3- or 5-year rolling average
  if (rollmean == 3){
    dat <- dat %>%
      mutate_at(vars(var_white, var_black, var_hisp,
                white_q1, white_mean, white_q3,
                black_q1, black_mean, black_q3,
                hisp_q1, hisp_mean, hisp_q3), rollmean3)
    if(xmin != 2000){
      dat <- dat %>% filter((year > xmin))
      xmin = xmin +1
    }
    
    dat <- dat %>% filter((year < xmax))
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat <- dat %>%
      mutate_at(vars(var_white, var_black, var_hisp,
                     white_q1, white_mean, white_q3,
                     black_q1, black_mean, black_q3,
                     hisp_q1, hisp_mean, hisp_q3), rollmean5)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }
  
  if(xmin == 2000 & rollmean == 3){
    dat$var[dat$year == 2000] <- var_2000
    dat$first_quarter[dat$year == 2000] <- first_quarter_2000
    dat$mean[dat$year == 2000] <- mean_2000
    dat$third_quarter[dat$year == 2000] <- third_quarter_2000
  }
  
  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    } 
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }
  
  #reshape data
  data_long <- melt(dat, id="year")
  data_long <- data_long[!is.na(data_long$value),]
  
  data_long$race[data_long$variable %in% c('var_white', 'white_q1', 'white_mean', 'white_q3')] <- "White"
  data_long$race[data_long$variable %in% c('var_black', 'black_q1', 'black_mean', 'black_q3')] <- "African American"
  data_long$race[data_long$variable %in% c('var_hisp', 'hisp_q1', 'hisp_mean', 'hisp_q3')] <- "Hispanic"
  
  #initial line plot
  p <- ggplot(data=data_long,aes(x=year, y=value, color = variable, linetype = variable, alpha = variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1, aes(group = variable))
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  p<-p+scale_y_continuous(labels = comma)
  
  #var_white var_black var_hisp white_q1 white_mean white_q3 black_q1 black_mean blackq3 hisp_q1 hisp_mean hisp_q3
  
  #add color and line types
  cPalette <- c('blue', 'red', 'darkgreen',
                "black", "blue", "black",
                "black", "red", "black",
                "black", "darkgreen", "black")
  p <- p + 
    scale_colour_manual(
      values = cPalette)
  
   p <- p + scale_linetype_manual(
     values = c("solid", "solid", "solid",
                "dashed", "dashed", "dashed",
                "dashed", "dashed", "dashed",
                "dashed", "dashed", "dashed"))
   
   p <- p + scale_alpha_manual(
     values = c(1, 1, 1, 0, .8, 0,
                0, .8, 0, 0, .8, 0))
  
  #add remaining style and elements
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=24, family = "Museo Sans 300"),
             axis.title = element_text(size = 24),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=36, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=24, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 20))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  
  ribbon_dat <- data_long %>% 
    filter(variable %in% c('white_q1', 'black_q1', 'hisp_q1', 'white_q3', 'black_q3', 'hisp_q3'))
  
  positions <- data.frame(
    variable = ribbon_dat$variable,
    race = ribbon_dat$race,
    x  = ribbon_dat$year,
    y  = ribbon_dat$value)
  
  positions <- positions %>% 
  {
    x <- .
    bind_rows(
      x %>% filter(variable %in% c('white_q1', 'black_q1', 'hisp_q1')),
      x %>% filter(variable %in% c('white_q3', 'black_q3', 'hisp_q3')) %>% arrange(desc(x)))
  }
  
  p <- p + geom_polygon(data = positions, 
                        aes(x = x, y = y, group = race, fill = factor(race), color = factor(race)), 
                        col = NA, alpha = 0.3)
  
  #p <- p + scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086'))
  
  p <- p + guides(linetype = FALSE, color = FALSE, alpha = FALSE)
  
  p
}

#Trendline graph for white, black, and hispanic including peer city
graph_trendline_race_peer_test<-function(df,vars, plot_title="",y_title="Percent", peers = "Current", 
                                    caption_text = "", subtitle_text = "",
                                    rollmean = 1, xmin = 2005, xmax = 2016,
                                    break_settings = ""){
  
  #create a new variable to use var with the '$' operator
  df$white <- df[[vars[1]]]
  df$black <- df[[vars[2]]]
  df$hisp  <- df[[vars[3]]]
  
  df %<>% select(FIPS, year, current, baseline, white, black, hisp) %>%
    gather(white:hisp, key = "race", value = "value")
  
  #subset to peers and remove Louisville
  if(peers=="Current"){
    df.wol <- subset(df,current == 1 & FIPS!=21111)
  }
  
  if(peers=="Baseline"){
    df.wol <- subset(df,baseline == 1 & FIPS!=21111)
  }
  
  #calculate 25th and 75th percentiles
  output_wol = df %>% 
    group_by(year, race) %>%
    summarise(
      q1   = quantile(value, prob = 0.25, na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      q3   = quantile(value, prob = 0.75, na.rm = TRUE))
  
  #extract Louisville values
  lville = df %>% 
    filter(FIPS == 21111) %>% 
    mutate(lou = value) %>%
    select(year, race, lou)
  
  #join 25th percentile, 75th percentile, and Louisville values
  dat = full_join(lville, output_wol, by = c("year", "race"))
  
  if(xmin == 2000 & rollmean == 3){
    var_2000 <- dat$var[dat$year == 2000]
    first_quarter_2000 <- dat$first_quarter[dat$year == 2000]
    mean_2000 <- dat$mean[dat$year == 2000]
    third_quarter_2000 <- dat$third_quarter[dat$year == 2000]
  }
  
  #Calculate 3- or 5-year rolling average
  if (rollmean == 3){
    dat <- dat %>%
      mutate_at(vars(lou, q1, mean, q3), rollmean3)
    if(xmin != 2000){
      dat <- dat %>% filter((year > xmin))
      xmin = xmin +1
    }
    
    dat <- dat %>% filter((year < xmax))
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat <- dat %>%
      mutate_at(vars(lou, q1, mean, q3), rollmean5)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }
  
  if(xmin == 2000 & rollmean == 3){
    dat$var[dat$year == 2000] <- var_2000
    dat$first_quarter[dat$year == 2000] <- first_quarter_2000
    dat$mean[dat$year == 2000] <- mean_2000
    dat$third_quarter[dat$year == 2000] <- third_quarter_2000
  }
  
  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    } 
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }
  
  print(dat)
  #reshape data
  data_long <- dat %>% select(year, race, everything()) %>% gather(lou:q3, key ='key', value ='value')
  data_long <- data_long[!is.na(data_long$value),]
  
  #data_long$race[data_long$variable %in% c('var_white', 'white_q1', 'white_mean', 'white_q3')] <- "White"
  #data_long$race[data_long$variable %in% c('var_black', 'black_q1', 'black_mean', 'black_q3')] <- "African American"
  #data_long$race[data_long$variable %in% c('var_hisp', 'hisp_q1', 'hisp_mean', 'hisp_q3')] <- "Hispanic"
  
  data_long %<>%
    mutate(grouping = paste0(race, key))
  
  #initial line plot
  p <- ggplot(data = data_long,aes(x = year, y = value, color = grouping, linetype = key, alpha = key, group = grouping))+
    geom_point(size = 1.8)+
    geom_line(size = 1, aes(group = grouping))
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  p<-p+scale_y_continuous(labels = comma)
  
  print(unique(data_long$grouping))
  print(unique(data_long$key))

  # "whitelou"  "blacklou"  "hisplou"   "whiteq1"   "blackq1"   "hispq1"    "whitemean" "blackmean" "hispmean"  "whiteq3"   "blackq3"   "hispq3" 
  
  cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")
  p + scale_colour_manual(values = cols)
  
  #add color and line types
  cPalette <- c('whitelou' ='blue',   'blacklou' = 'red',  'hisplou' = 'darkgreen',
                'whiteq1' = "black",  'blackq1' = "black", 'hispq1' = "black",
                'whitemean' = "blue", 'blackmean' = "red", 'hispmean' = "black",
                'whiteq3' = "black",  'blackq3' = "black", 'hispq3' = "black")
  p <- p + 
    scale_colour_manual(values = cPalette)
  
  shapes <- c('lou' = 'solid', 'q1' = 'dashed', 'q3' = 'dashed', 'mean' = 'dashed')
  
  p <- p + scale_linetype_manual(values = shapes)
  
  p <- p + scale_alpha_manual(values = c('lou' = 1, 'q1' = 0, 'q3' = 0, 'mean' = 1))
  
  #add remaining style and elements
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=24, family = "Museo Sans 300"),
             axis.title = element_text(size = 24),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=36, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=24, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 20))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  
  ribbon_dat <- data_long %>% 
    filter(key %in% c('q1', 'q3'))
  
  positions <- data.frame(
    key = ribbon_dat$key,
    race = ribbon_dat$race,
    x  = ribbon_dat$year,
    y  = ribbon_dat$value)
  
  positions <- positions %>% 
  {
    x <- .
    bind_rows(
      x %>% filter(key %in% c('q1')) %>% arrange(x), 
      x %>% filter(key %in% c('q3')) %>% arrange(desc(x)))
  }
  
  p <- p + geom_polygon(data = positions, 
                        aes(x = x, y = y, group = race, fill = factor(race), color = factor(race)), 
                        col = NA, alpha = 0.3)
  
  #p <- p + scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086'))
  
  p <- p + guides(color = FALSE, alpha = FALSE)
  
  p <- p+ theme(  legend.background = element_rect(fill="white"))
  
  p
}

graph_trendline_race_peer_two<-function(df,vars, plot_title="",y_title="Percent", peers = "Current", 
                                    caption_text = "", subtitle_text = "",
                                    rollmean = 1, xmin = 2005, xmax = 2016,
                                    break_settings = ""){
  
  #create a new variable to use var with the '$' operator
  df$var_white <- df[[vars[1]]]
  df$var_black <- df[[vars[2]]]
  
  #if(is.na(df$var[df$year == 2016 & df$city == "Louisville"]))  xmax = 2015
  
  #subset to peers and remove Louisville
  if(peers=="Current"){
    df.wol <- subset(df,current == 1 & FIPS!=21111)
  }
  
  if(peers=="Baseline"){
    df.wol <- subset(df,baseline == 1 & FIPS!=21111)
  }
  
  #calculate 25th and 75th percentiles
  output_wol = df %>% 
    group_by(year) %>%
    summarise(
      white_q1   = quantile(var_white, prob = 0.25, na.rm = TRUE),
      white_mean = mean(var_white, na.rm = TRUE),
      white_q3   = quantile(var_white, prob = 0.75, na.rm = TRUE),
      black_q1   = quantile(var_black, prob = 0.25, na.rm = TRUE),
      black_mean = mean(var_black, na.rm = TRUE),
      black_q3    = quantile(var_black, prob = 0.75, na.rm = TRUE))
  
  #extract Louisville values
  lville = df %>% 
    filter(FIPS == 21111) %>% 
    select(var_white, var_black, year)
  
  #join 25th percentile, 75th percentile, and Louisville values
  dat = full_join(lville, output_wol, by = "year")
  
  if(xmin == 2000 & rollmean == 3){
    var_2000 <- dat$var[dat$year == 2000]
    first_quarter_2000 <- dat$first_quarter[dat$year == 2000]
    mean_2000 <- dat$mean[dat$year == 2000]
    third_quarter_2000 <- dat$third_quarter[dat$year == 2000]
  }
  
  #Calculate 3- or 5-year rolling average
  if (rollmean == 3){
    dat <- dat %>%
      mutate_at(vars(var_white, var_black,
                     white_q1, white_mean, white_q3,
                     black_q1, black_mean, black_q3), rollmean3)
    if(xmin != 2000){
      dat <- dat %>% filter((year > xmin))
      xmin = xmin +1
    }
    
    dat <- dat %>% filter((year < xmax))
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat <- dat %>%
      mutate_at(vars(var_white, var_black,
                     white_q1, white_mean, white_q3,
                     black_q1, black_mean, black_q3), rollmean5)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }
  
  if(xmin == 2000 & rollmean == 3){
    dat$var[dat$year == 2000] <- var_2000
    dat$first_quarter[dat$year == 2000] <- first_quarter_2000
    dat$mean[dat$year == 2000] <- mean_2000
    dat$third_quarter[dat$year == 2000] <- third_quarter_2000
  }
  
  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    } 
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }
  
  #reshape data
  data_long <- melt(dat, id="year")
  data_long <- data_long[!is.na(data_long$value),]
  
  data_long$race[data_long$variable %in% c('var_white', 'white_q1', 'white_mean', 'white_q3')] <- "White"
  data_long$race[data_long$variable %in% c('var_black', 'black_q1', 'black_mean', 'black_q3')] <- "African American"
  
  #initial line plot
  p <- ggplot(data=data_long,aes(x = year, y = value, color = variable, linetype = variable, alpha = variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1, aes(group = variable))
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  p<-p+scale_y_continuous(labels = comma)
  
  #var_white var_black var_hisp white_q1 white_mean white_q3 black_q1 black_mean blackq3 hisp_q1 hisp_mean hisp_q3
  
  #add color and line types
  cPalette <- c('blue', 'red',
                "black", "blue", "black",
                "black", "red", "black")
  p <- p + 
    scale_colour_manual(
      values = cPalette)
  
  p <- p + scale_linetype_manual(
    values = c("solid", "solid",
               "dashed", "dashed", "dashed",
               "dashed", "dashed", "dashed"))
  
  p <- p + scale_alpha_manual(
    values = c(1, 1, .2, .8, .2,
               .2, .8, .2))
  
  #add remaining style and elements
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=24, family = "Museo Sans 300"),
             axis.title = element_text(size = 24),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=36, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=24, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 20))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  
  ribbon_dat <- data_long %>% 
    filter(variable %in% c('white_q1', 'black_q1', 'white_q3', 'black_q3'))
  
  positions <- data.frame(
    variable = ribbon_dat$variable,
    race = ribbon_dat$race,
    x  = ribbon_dat$year,
    y  = ribbon_dat$value)
  
  positions <- positions %>% 
  {
    x <- .
    bind_rows(
      x %>% filter(variable %in% c('white_q1', 'black_q1')),
      x %>% filter(variable %in% c('white_q3', 'black_q3')) %>% arrange(desc(x)))
  }
  
  p <- p + geom_polygon(data = positions, 
                        aes(x = x, y = y, group = race, fill = factor(race), color = factor(race)), 
                        col = NA, alpha = 0.2)
  
  #p <- p + scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086'))
  
  p <- p + guides(linetype = FALSE, color = FALSE, alpha = FALSE)
  
  p
}

graph_trendline_mig<-function(df,vars, plot_title="",y_title="Percent", peers = "Current", 
                                    caption_text = "", subtitle_text = "",
                                    rollmean = 1, xmin = 2005, xmax = 2016,
                                    break_settings = ""){
  
  #create a new variable to use var with the '$' operator
  df$var_white <- df[[vars[1]]]
  df$var_black <- df[[vars[2]]]
  df$var_hisp  <- df[[vars[3]]]
  
  #if(is.na(df$var[df$year == 2016 & df$city == "Louisville"]))  xmax = 2015
  
  #subset to peers and remove Louisville
  if(peers=="Current"){
    df.wol <- subset(df,current == 1 & FIPS!=21111)
  }
  
  if(peers=="Baseline"){
    df.wol <- subset(df,baseline == 1 & FIPS!=21111)
  }
  
  #calculate 25th and 75th percentiles
  output_wol = df %>% 
    group_by(year) %>%
    summarise(
      white_q1   = quantile(var_white, prob = 0.25, na.rm = TRUE),
      white_mean = mean(var_white, na.rm = TRUE),
      white_q3   = quantile(var_white, prob = 0.75, na.rm = TRUE),
      black_q1   = quantile(var_black, prob = 0.25, na.rm = TRUE),
      black_mean = mean(var_black, na.rm = TRUE),
      black_q3    = quantile(var_black, prob = 0.75, na.rm = TRUE),
      hisp_q1    = quantile(var_hisp, prob = 0.25, na.rm = TRUE),
      hisp_mean  = mean(var_hisp, na.rm = TRUE),
      hisp_q3    = quantile(var_hisp, prob = 0.75, na.rm = TRUE))
  
  #extract Louisville values
  lville = df %>% 
    filter(FIPS == 21111) %>% 
    select(var_white, var_black, var_hisp, year)
  
  #join 25th percentile, 75th percentile, and Louisville values
  dat = full_join(lville, output_wol, by = "year")
  
  if(xmin == 2000 & rollmean == 3){
    var_2000 <- dat$var[dat$year == 2000]
    first_quarter_2000 <- dat$first_quarter[dat$year == 2000]
    mean_2000 <- dat$mean[dat$year == 2000]
    third_quarter_2000 <- dat$third_quarter[dat$year == 2000]
  }
  
  #Calculate 3- or 5-year rolling average
  if (rollmean == 3){
    dat <- dat %>%
      mutate_at(vars(var_white, var_black, var_hisp,
                     white_q1, white_mean, white_q3,
                     black_q1, black_mean, black_q3,
                     hisp_q1, hisp_mean, hisp_q3), rollmean3)
    if(xmin != 2000){
      dat <- dat %>% filter((year > xmin))
      xmin = xmin +1
    }
    
    dat <- dat %>% filter((year < xmax))
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat <- dat %>%
      mutate_at(vars(var_white, var_black, var_hisp,
                     white_q1, white_mean, white_q3,
                     black_q1, black_mean, black_q3,
                     hisp_q1, hisp_mean, hisp_q3), rollmean5)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }
  
  if(xmin == 2000 & rollmean == 3){
    dat$var[dat$year == 2000] <- var_2000
    dat$first_quarter[dat$year == 2000] <- first_quarter_2000
    dat$mean[dat$year == 2000] <- mean_2000
    dat$third_quarter[dat$year == 2000] <- third_quarter_2000
  }
  
  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    } 
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }
  
  #reshape data
  data_long <- melt(dat, id="year")
  data_long <- data_long[!is.na(data_long$value),]
  
  data_long$race[data_long$variable %in% c('var_white', 'white_q1', 'white_mean', 'white_q3')] <- "Net Migration"
  data_long$race[data_long$variable %in% c('var_black', 'black_q1', 'black_mean', 'black_q3')] <- "Inmigration"
  data_long$race[data_long$variable %in% c('var_hisp', 'hisp_q1', 'hisp_mean', 'hisp_q3')] <- "Outmigration"
  
  #initial line plot
  p <- ggplot(data=data_long,aes(x=year, y=value, color = variable, linetype = variable, alpha = variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1, aes(group = variable))
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  p<-p+scale_y_continuous(labels = comma)
  
  #var_white var_black var_hisp white_q1 white_mean white_q3 black_q1 black_mean blackq3 hisp_q1 hisp_mean hisp_q3
  
  #add color and line types
  cPalette <- c('blue', 'red', 'darkgreen',
                "black", "blue", "black",
                "black", "red", "black",
                "black", "darkgreen", "black")
  p <- p + 
    scale_colour_manual(
      values = cPalette)
  
  p <- p + scale_linetype_manual(
    values = c("solid", "solid", "solid",
               "dashed", "dashed", "dashed",
               "dashed", "dashed", "dashed",
               "dashed", "dashed", "dashed"))
  
  p <- p + scale_alpha_manual(
    values = c(1, 1, 1, 0, .8, 0,
               0, .8, 0, 0, .8, 0))
  
  #add remaining style and elements
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=24, family = "Museo Sans 300"),
             axis.title = element_text(size = 24),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=36, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=24, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 20))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  
  ribbon_dat <- data_long %>% 
    filter(variable %in% c('white_q1', 'black_q1', 'hisp_q1', 'white_q3', 'black_q3', 'hisp_q3'))
  
  positions <- data.frame(
    variable = ribbon_dat$variable,
    race = ribbon_dat$race,
    x  = ribbon_dat$year,
    y  = ribbon_dat$value)
  
  positions <- positions %>% 
  {
    x <- .
    bind_rows(
      x %>% filter(variable %in% c('white_q1', 'black_q1', 'hisp_q1')),
      x %>% filter(variable %in% c('white_q3', 'black_q3', 'hisp_q3')) %>% arrange(desc(x)))
  }
  
  p <- p + geom_polygon(data = positions, 
                        aes(x = x, y = y, group = race, fill = factor(race), color = factor(race)), 
                        col = NA, alpha = 0.3)
  
  #p <- p + scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086'))
  
  p <- p + guides(linetype = FALSE, color = FALSE, alpha = FALSE)
  
  p
}

#Trneldine graph by race
graph_trendline_race <- function(data_long, var = "var", value = "value", plot_title="",y_title="Percent", 
                                  caption_text = "", subtitle_text = "", rollmean = 1,
                                  break_settings = "", xmin = 1996, xmax = 2016,
                                  labels, color_pal){
  data_long$var <- data_long[[var]]
  data_long$value<-data_long[[value]]
  data_long %<>% select(year, var, value)
  data_long <- arrange(data_long, as.character(var))
  
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    } 
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }
  
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=var))+
    geom_point(size = 1.8)+
    geom_line(data=data_long[!is.na(data_long$value),], size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  p<-p+scale_y_continuous(labels = comma)
  cPalette <- color_pal
  p <- p + scale_colour_manual(values = cPalette, labels = labels)
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=24, family = "Museo Sans 300"),
             axis.title = element_text(size = 24),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=36, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=24, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 20))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}

#Just Louisville
graph_trendline_lou<-function(df,var, plot_title="",y_title="Percent", 
                          caption_text = "", subtitle_text = "",
                          rollmean = 1, xmin = 2005, xmax = 2016,
                          break_settings = ""){
  
  #create a new variable to use var with the '$' operator
  df$var <- df[[var]]
  
  #extract Louisville values
  lville = df %>% 
    filter(FIPS == 21111) %>% 
    select(var, year)
  
  #join 25th percentile, 75th percentile, and Louisville values
  dat = lville
  
  if(xmin == 2000 & rollmean == 3){
    var_2000 <- dat$var[dat$year == 2000]
    first_quarter_2000 <- dat$first_quarter[dat$year == 2000]
    mean_2000 <- dat$mean[dat$year == 2000]
    third_quarter_2000 <- dat$third_quarter[dat$year == 2000]
  }
  
  #Calculate 3- or 5-year rolling average
  if (rollmean == 3){
    dat$var = rollmean3(dat$var)
    dat$first_quarter = rollmean3(dat$first_quarter)
    dat$mean = rollmean3(dat$mean)
    dat$third_quarter = rollmean3(dat$third_quarter)
    
    if(xmin != 2000){
      dat <- dat %>% filter((year > xmin))
      xmin = xmin +1
    }
    
    dat <- dat %>% filter((year < xmax))
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat$var = rollmean5(dat$var)
    dat$first_quarter = rollmean5(dat$first_quarter)
    dat$mean = rollmean5(dat$mean)
    dat$third_quarter = rollmean5(dat$third_quarter)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }
  
  if(xmin == 2000 & rollmean == 3){
    dat$var[dat$year == 2000] <- var_2000
    dat$first_quarter[dat$year == 2000] <- first_quarter_2000
    dat$mean[dat$year == 2000] <- mean_2000
    dat$third_quarter[dat$year == 2000] <- third_quarter_2000
  }
  
  #use to write out 2016 child poverty statistics (also write out the line above filtering out 2016 data)
  #write_csv(dat, 'C:/Users/Harrison Kirby/Desktop/GLP/child_pov_output2.csv')
  
  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    } 
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }
  
  #reshape data
  data_long <- melt(dat, id="year")
  data_long <- data_long[!is.na(data_long$value),]
  
  #initial line plot
  p <- ggplot(data=data_long,aes(x=year,y=value, color = variable))+
    geom_point(size = 1.8, show.legend = FALSE)+
    geom_line(size = 1, show.legend = FALSE)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + scale_y_continuous(labels = scales::comma,
              limits = c(min(data_long$value, na.rm = TRUE) - border_space, 
                         max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  
  #add color and line types
  cPalette <- c("#00a9b7")
  p <- p + scale_colour_manual(
    values = cPalette,
    labels = c(
      "Louisville"
    )
  )
  
  #add remaining style and elements
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 20))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}
#Trendline graph adapted for MSAs
graph_trendline_msa<-function(df,var, plot_title="",y_title="Percent", 
                              peers = "Current", caption_text = "", 
                              subtitle_text = "", rollmean = 1, break_settings = "", 
                              xmin = 2005, xmax = 2016){
  df$var <- df[[var]]
  df = df %>% filter(year != 2016)
  if(peers=="Current"){
    df.wol <- filter(df,current == 1 & MSA!=31140)
  }
  if(peers=="Baseline"){
    df.wol <- filter(df,baseline == 1 & MSA!=31140)
  }
  output_wol = df %>% 
    group_by(year) %>%
    summarise(first_quarter = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var),
              third_quarter = quantile(var, prob = 0.75, na.rm = TRUE))
  lville = df %>% 
    filter(MSA == 31140) %>% 
    select(var, year)
  dat = full_join(lville, output_wol, by = "year")
  if (rollmean == 3){
    dat$var = rollmean3(dat$var)
    dat$first_quarter = rollmean3(dat$first_quarter)
    dat$mean = rollmean3(dat$mean)
    dat$third_quarter = rollmean3(dat$third_quarter)
    dat <- dat %>% filter((year > xmin) & (year < xmax))
    xmin = xmin +1
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat$var = rollmean5(dat$var)
    dat$first_quarter = rollmean5(dat$first_quarter)
    dat$mean = rollmean5(dat$mean)
    dat$third_quarter = rollmean5(dat$third_quarter)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }
  
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    } 
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }
  
  dat
  data_long <- melt(dat, id="year")
  data_long$variable = factor(data_long$variable, levels = c("var", "third_quarter", "mean", "first_quarter"))
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=variable,linetype=variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value)+min(data_long$value))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value) - border_space, max(data_long$value + border_space)))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- c("#00a9b7","grey50", "black","grey50")
  p <- p + scale_colour_manual(
    values = cPalette,
    labels = c(
      "Louisville",
      "75th Percentile",
      "Peer City Mean",
      "25th Percentile"
    )
  ) +
    scale_linetype_manual(
      values = c("solid", "dashed", "dashed", "dashed"),
      labels = c(
        "Louisville",
        "75th Percentile",
        "Peer City Mean",
        "25th Percentile"
      )
    )
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=12, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}

#Trendline graph adapted for ky_ed data
graph_trendline_ky_ed<-function(df,var, plot_title="",y_title="Percent", 
                                caption_text = "", subtitle_text = "", rollmean = 1,
                                break_settings = "", xmin = 2005, xmax = 2015){
  df$var <- df[[var]]
  output_wol = df %>% 
    group_by(year) %>%
    summarise(first_quarter = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var, na.rm = TRUE),
              third_quarter = quantile(var, prob = 0.75, na.rm = TRUE))
  lville = df %>% 
    filter(area == "Louisville") %>% 
    select(var, year)
  dat = full_join(lville, output_wol, by = "year")
  
  if (rollmean == 3){
    dat$var = rollmean3(dat$var)
    dat$first_quarter = rollmean3(dat$first_quarter)
    dat$mean = rollmean3(dat$mean)
    dat$third_quarter = rollmean3(dat$third_quarter)
    dat <- dat %>% filter((year > xmin) & (year < xmax))
    xmin = xmin +1
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat$var = rollmean5(dat$var)
    dat$first_quarter = rollmean5(dat$first_quarter)
    dat$mean = rollmean5(dat$mean)
    dat$third_quarter = rollmean5(dat$third_quarter)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }
  
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    } 
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }
  
  data_long <- melt(dat, id="year")
  data_long$variable = factor(data_long$variable, levels = c("var", "third_quarter", "mean", "first_quarter"))
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=variable,linetype=variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- c("#00a9b7","grey50", "black","grey50")
  p <- p + scale_colour_manual(
    values = cPalette,
    labels = c(
      "JCPS",
      "75th Percentile",
      "KY School District Mean",
      "25th Percentile"
    )
  ) +
    scale_linetype_manual(
      values = c("solid", "dashed", "dashed", "dashed"),
      labels = c(
        "JCPS",
        "75th Percentile",
        "KY School District Mean",
        "25th Percentile"
      )
    )
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=12, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}

#Trendline graph adapted for more years of ky_ed data
ky_ed_data_long_trendline <- function(data_long, var = "var", value = "value", plot_title="",y_title="Percent", 
                                      caption_text = "", subtitle_text = "", rollmean = 1,
                                      break_settings = "", xmin = 1996, xmax = 2016,
                                      labels, color_pal){
  data_long$var <- data_long[[var]]
  data_long$value<-data_long[[value]]
  data_long %<>% select(year, var, value)
  data_long <- arrange(data_long, as.character(var))
  
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    } 
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }
  
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=var))+
    geom_point(size = 1.8)+
    geom_line(data=data_long[!is.na(data_long$value),], size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- color_pal
  p <- p + scale_colour_manual(values = cPalette, labels = labels)
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=12, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}

#MAPS

make_map <- function(var, name, units = "Percent",
                     map_style = "sequential", legend_title = ""){
  
  #renames var for use with the '$' operator
  map_jc@data$var <- map_jc@data[[var]]
  
  #concatenate third line of text for tract labels using units parameter
  if(units == "Percent"){
    map_jc@data$l_line3 <- paste(name, ": ", round(map_jc@data$var, 2),"%", sep = "")
  }
  if(units == "Dollars"){
    map_jc@data$l_line3 <- paste(name, ": $",
                                 prettyNum(
                                   signif(map_jc@data$var, 3),
                                   big.mark = ",",
                                   preserve.width = "none"
                                 ),
                                 sep = "")
  }
  if(units == "minutes"){
    map_jc@data$l_line3 <- paste(name, ": ", round(map_jc@data$var, 2)," minutes", sep = "")
  }
  if(units == "none"){
    map_jc@data$l_line3 <- paste(name, ": ", round(map_jc@data$var, 2), sep = "")
  }
  
  #combine lines of text into full formatted label
  labels <- sprintf("%s<br/>%s<br/>%s",
                    map_jc@data$l_line1, map_jc@data$l_line2, map_jc@data$l_line3
                   ) %>% 
            lapply(htmltools::HTML)
  
  labels[[190]] <- htmltools::HTML(sprintf("%s<br/>%s<br/>%s",
                                           "Tract #: 980000",
                                           "Louisville International Airport",
                                           "No residents"
                                          )
                                  )
                 
  #Define palette using map_style parameter
  if(map_style == "sequential" | map_style == "Sequential"){
    col_palette = "BuPu"
  }
  if(map_style == "divergent" | map_style == "Divergent"){
    col_palette = "RdYlGn"
  }
  pal <- brewer.pal(11, col_palette)
  pal <- colorNumeric(
    palette = pal,
    domain = map_jc@data$var
  )
  
  #Create map title using legend_title parameter
  if(units == "Percent") {
    title_text <- paste(legend_title, "(%)", sep = ' ')
  }
  if(units == "Dollars") {
    title_text <- paste(legend_title, "($)", sep = ' ')
  }
  if(units == "minutes"){
    title_text <- paste(legend_title, "(minutes)", sep = ' ')
  }
  if(units == "none"){
    title_text <- legend_title
  }
  
  #create map
  m <- leaflet(map_jc) %>%
    addTiles() %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~pal(var),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))%>%
    addLegend(pal = pal, values = ~var, opacity = 0.7, title = title_text,
              position = "bottomright")
  
  m
}

make_map_nh <- function(var, name, units = "Percent",
                     map_style = "sequential", legend_title = ""){
  
  #renames var for use with the '$' operator
  map_jc_nh@data$var <- map_jc_nh@data[[var]]
  
  #concatenate third line of text for tract labels using units parameter
  if(units == "Percent"){
    map_jc_nh@data$l_line2 <- paste(name, ": ", round(map_jc_nh@data$var, 2),"%", sep = "")
  }
  if(units == "Dollars"){
    map_jc_nh@data$l_line2 <- paste(name, ": $",
                                 prettyNum(
                                   signif(map_jc_nh@data$var, 3),
                                   big.mark = ",",
                                   preserve.width = "none"
                                 ),
                                 sep = "")
  }
  if(units == "none"){
    map_jc_nh@data$l_line2 <- paste(name, ": ", round(map_jc_nh@data$var, 2), sep = "")
  }
  
  #combine lines of text into full formatted label
  labels <- sprintf("%s<br/>%s",
                    map_jc_nh@data$l_line1, map_jc_nh@data$l_line2
  ) %>% 
    lapply(htmltools::HTML)
  
  labels[[25]] <- htmltools::HTML(sprintf("%s<br/>%s",
                                           "Louisville International Airport",
                                           "No residents"
  )
  )
  
  #Define palette using map_style parameter
  if(map_style == "sequential" | map_style == "Sequential"){
    col_palette = "BuPu"
  }
  if(map_style == "divergent" | map_style == "Divergent"){
    col_palette = "RdYlGn"
  }
  pal <- brewer.pal(11, col_palette)
  pal <- colorNumeric(
    palette = pal,
    domain = map_jc_nh@data$var
  )
  
  #Create map title using legend_title parameter
  if(units == "Percent") {
    title_text <- paste(legend_title, "(%)", sep = ' ')
  }
  if(units == "Dollars") {
    title_text <- paste(legend_title, "($)", sep = ' ')
  }
  if(units == "none"){
    title_text <- legend_title
  }
  
  #create map
  m <- leaflet(map_jc_nh) %>%
    addTiles() %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~pal(var),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))%>%
    addLegend(pal = pal, values = ~var, opacity = 0.7, title = title_text,
              position = "bottomright")
  m
}

make_map_zip <- function(var, name, units = "Percent",
                         map_style = "sequential", legend_title = ""){
  
  #renames var for use with the '$' operator
  map_jc_zip@data$var <- map_jc_zip@data[[var]]
  
  #concatenate third line of text for tract labels using units parameter
  if(units == "Percent"){
    map_jc_zip@data$l_line2 <- paste(name, ": ", round(map_jc_zip@data$var, 2),"%", sep = "")
  }
  if(units == "Dollars"){
    map_jc_zip@data$l_line2 <- paste(name, ": $",
                                    prettyNum(
                                      signif(map_jc_zip@data$var, 3),
                                      big.mark = ",",
                                      preserve.width = "none"
                                    ),
                                    sep = "")
  }
  if(units == "none"){
    map_jc_zip@data$l_line2 <- paste(name, ": ", round(map_jc_zip@data$var, 2), sep = "")
  }
  
  #combine lines of text into full formatted label
  labels <- sprintf("%s<br/>%s",
                    map_jc_zip@data$l_line1, map_jc_zip@data$l_line2
  ) %>% 
    lapply(htmltools::HTML)
  
  #Define palette using map_style parameter
  if(map_style == "sequential" | map_style == "Sequential"){
    col_palette = "BuPu"
  }
  if(map_style == "divergent" | map_style == "Divergent"){
    col_palette = "RdYlGn"
  }
  pal <- brewer.pal(11, col_palette)
  pal <- colorNumeric(
    palette = pal,
    domain = map_jc_zip@data$var
  )
  
  #Create map title using legend_title parameter
  if(units == "Percent") {
    title_text <- paste(legend_title, "(%)", sep = ' ')
  }
  if(units == "Dollars") {
    title_text <- paste(legend_title, "($)", sep = ' ')
  }
  if(units == "none"){
    title_text <- legend_title
  }
  
  #create map
  m <- leaflet(map_jc_zip) %>%
    addTiles() %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~pal(var),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))%>%
    addLegend(pal = pal, values = ~var, opacity = 0.7, title = title_text,
              position = "bottomright")
  m
}