
data("Grunfeld", package = "plm")
highchart() %>%
  hc_add_series()

hchart(Grunfeld, "line", hcaes(x = year, y = value, group = firm))


data(economics_long, package = "ggplot2")

economics_long2 <- dplyr::filter(economics_long, variable %in% c("pop", "uempmed", "unemploy"))

head(economics_long2)

# scrap 1 -----------------------------------------------------------------


data(diamonds)
data <- subset(diamonds, color %in% c("E", "F", "G") & cut %in% c("Ideal", "Premium", "Good"))
data$indicator <- ifelse(data$color %in% c("G" ), 1, 0)

colors_hc <- c("#7CB5EC", "#313131", "#F7A35C",
               "#90EE7E", "#7798BF", "#AAEEEE",
               "#FF0066", "#EEAAEE", "#55BF3B",
               "#DF5353", "#7798BF", "#AAEEEE")

font_import(paths="~/Downloads/open-sans/")
loadfonts()
fonts()

theme_hc <- function(){
  theme(
    text                = element_text( size = 10),
    title               = element_text(hjust=0), 
    axis.title.x        = element_text(hjust=.5),
    axis.title.y        = element_text(hjust=.5),
    panel.grid.major.y  = element_line(color='gray', size = .3),
    panel.grid.minor.y  = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    panel.border        = element_blank(),
    panel.background    = element_blank(),
    legend.position     = "bottom",
    legend.title        = element_blank()
  )
}

p1 <- ggplot(data) +
  geom_bar(aes(cut), width =.4, fill = colors_hc[1]) +
  ggtitle("An interesting title for a bar plot") +
  xlab("Cut") + ylab("Amount") +
  #scale_y_continuous(labels = comma) +
  theme_hc()
p1



p2 <- ggplot(data) +
  geom_bar(aes(color, fill=cut), position="dodge", width=.4) +
  ggtitle("Another interesting title") +
  xlab("Cut") + ylab("Amount") +
  # scale_y_continuous(labels = comma) +
  scale_fill_manual(values=colors_hc) 
#theme_hc()
p2

dat_sum <-data %>% group_by(color,cut) %>%summarise(tot=n())

highchart() %>%
  hc_add_series(dat_sum,type="column", hcaes(color,tot,group=cut))  %>%
  hc_title(text="Another interesting title")
hc_xAxis(type="category") 



#%>% hc_plotOptions(column=list(stacking="normal"))




p3 <- ggplot(data) + geom_density(aes(x, fill=cut, color=cut), alpha=I(0.5)) +
  ggtitle("Density plot") +  xlab("x") + ylab("Density") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values=colors_hc) +
  xlim(4, 8) +
  theme_hc()
p3


data("Grunfeld", package = "plm")


# scrap 2 -----------------------------------------------------------------



data(Salaries)
library(ggplot2)
library(tidyverse)
ggplot(data=Salaries, aes(x=salary, fill=rank)) +
  geom_density(alpha=.3)

data(singer, package="lattice")
library(ggplot2)
ggplot(data=singer, aes(x=height)) +
  geom_histogram() +
  facet_wrap(~voice.part, nrow=4)


library(highcharter)
hchart(Salaries,hcaes(salary,group=rank))
dens_sal= density(Salaries$salary)
hchart(Salaries,hcaes(salary)
       Salaries %>% pull(salary,rank)%>%hchart(hcaes(salary,group=rank))
       
       
       
       harden = 
         gamelogs %>%
         filter(namePlayer == "James Harden") %>%
         select(numberGameTeamSeason, pts, treb, ast)      
hchart(harden, "column", hcaes(x = numberGameTeamSeason, y = pts), yAxis = 0) %>%
         hc_yAxis_multiples(
           create_yaxis(
             naxis = 3,                                                       # 3 different y axes
             heights = c(1,1,1),                                              # All equal
             turnopposite = TRUE,                                             # Alternate left-right sides of the axes
             title = map(c("Points", "Asissts", "Rebounds"), ~list(text = .)) # Needs a list like: list(list(text = 'Points'), list(text = 'Assists'),...)
           )
         ) %>%
         hc_add_series(harden, "column", hcaes(x = numberGameTeamSeason, y = ast), yAxis = 1) %>%
         hc_add_series(harden, "column", hcaes(x = numberGameTeamSeason, y = treb), yAxis = 2)








```{r}
mod_name_14_17.fun<-function(data){
  data$Method[14]<-"Logistic + RF Gini 10"
  data$Method[15]<-"Logistic + RF Gini 5"
  data$Method[16]<-"Logistic + RF Gini 1"
  data$Method[17]<-"Logistic + RF Gini 5 + Return Details"
  data
}



DATA_PATH<-"~/Dropbox/Onica/2019/AMAZON/06_Tag_Voting_Models/Performance_Progression/DATA/"
holdout_or_sample_for_model<- "holdout"
box_holdout<-read_csv(paste0(DATA_PATH,"Box_",holdout_or_sample_for_model,"_performance_progression.csv"))
env_holdout<- read_csv(paste0(DATA_PATH,"Envelope_",holdout_or_sample_for_model,"_performance_progression.csv"))
sioc_holdout<-read_csv(paste0(DATA_PATH,"SIOC_",holdout_or_sample_for_model,"_performance_progression.csv"))

box_holdout<- mod_name_14_17.fun(box_holdout)
env_holdout<- mod_name_14_17.fun(env_holdout)
sioc_holdout<- mod_name_14_17.fun(sioc_holdout)

holdout_or_sample_for_model<- "sample_for_model"
box_large_sample<-read_csv(paste0(DATA_PATH,"Box_",holdout_or_sample_for_model,"_performance_progression.csv"))
env_large_sample<- read_csv(paste0(DATA_PATH,"Envelope_",holdout_or_sample_for_model,"_performance_progression.csv"))
sioc_large_sample<-read_csv(paste0(DATA_PATH,"SIOC_",holdout_or_sample_for_model,"_performance_progression.csv"))
box_large_sample<- mod_name_14_17.fun(box_large_sample)
env_large_sample<- mod_name_14_17.fun(env_large_sample)
sioc_large_sample<-mod_name_14_17.fun(sioc_large_sample)

sioc_holdout<- sioc_holdout%>% mutate(Method=gsub("CCC",'A',Method)   )%>% 
  mutate(Method=gsub("TM",'B',Method)   ) %>% 
  mutate(Method=gsub("No Restock",'Filtered',Method)   )  %>% 
  mutate(Method=gsub("Defect_id",'C',Method)   ) %>% 
  mutate(Method=gsub("Return Details",'D',Method)   )%>% 
  mutate(Method=gsub("No inclusion_tm_tag",'w/out feature X',Method)   )

sioc_holdout<- sioc_holdout %>% rename(Group=Package.Group)%>% mutate(Group="Redacted")
write_csv(sioc_holdout, "model_progression.csv")
```



holdout<- sioc_holdout %>% mutate(id=row_number()) %>% mutate(x_label=paste0(id,": ",Method))

# first observation is the baseline model performance
recall_baseline<-holdout$Recall[1]
precision_baseline<- holdout$Precision[1]

recall_baseline

highchart() %>%
  hc_add_series(holdout[-1,], "line",hcaes(x_label,Precision),name="Precision" ,color="maroon"
                ,zones=list(  #list(value=0,color='maroon'),
                  list(value=precision_baseline,color='lightgrey')
                  ,  list(value=0.8,color='maroon')
                  , list(value=1.01,color='green',dashStyle='shortdashdot',dashWidth=3)
                ) )%>% 
  
  
  hc_add_series(holdout[-1,], "line",hcaes(x_label,Recall),name="Recall", color="slateblue"
                ,zones=list(#list(value=0,color='slateblue') , #f7a35c''slategrey')
                  list(value=holdout$Recall[1],color='lightgrey')
                  , list(value=0.8,color='slateblue')
                  , list(value=1.01,color='geen'))
  )%>% 
  hc_xAxis(type="category") %>%   hc_yAxis(max=1)%>%
  hc_yAxis(plotLines = list(
    list(label = list(text = "CPEX Recall Baseline"),
         color = "slateblue", #"#FF0000",
         width = 1.75,
         dashStyle= 'shortdashdot',
         value = holdout$Recall[1])
    , list(label = list(text = "CPEX Precision Baseline", y=13),
           color = "maroon", #"#FF0000",
           width = 2,
           dashStyle= 'shortdashdot',
           value = holdout$Precision[1])
    , list(label = list(text = "GOAL"),
           color = "green", #"#FF0000",
           width = 2.25,
           dashStyle= 'shortdashdot',
           value = 0.8)
    
    
  )
  
  ) %>%  hc_legend(align = "right", verticalAlign = "top",
                   layout = "vertical") %>% hc_size(height=700)


```



### Holdout

```{r}
holdout<- env_holdout %>% mutate(id=row_number()) %>% mutate(x_label=paste0(id,": ",Method))
recall_baseline<-holdout$Recall[1]
precision_baseline<- holdout$Precision[1]
highchart() %>%
  hc_add_series(holdout[-1,], "line",hcaes(x_label,Precision),name="Precision" ,color="maroon"
                ,zones=list(  #list(value=0,color='maroon'),
                  list(value=precision_baseline,color='lightgrey')
                  ,  list(value=0.8,color='maroon')
                  , list(value=1.01,color='green',dashStyle='shortdashdot',dashWidth=3)
                ) )%>% 
  
  
  hc_add_series(holdout[-1,], "line",hcaes(x_label,Recall),name="Recall", color="slateblue"
                ,zones=list(#list(value=0,color='slateblue') , #f7a35c''slategrey')
                  list(value=holdout$Recall[1],color='lightgrey')
                  , list(value=0.8,color='slateblue')
                  , list(value=1.01,color='geen'))
  )%>% 
  hc_xAxis(type="category") %>%   hc_yAxis(max=1)%>%
  hc_yAxis(plotLines = list(
    list(label = list(text = "CPEX Recall Baseline"),
         color = "slateblue", #"#FF0000",
         width = 1.75,
         dashStyle= 'shortdashdot',
         value = holdout$Recall[1])
    , list(label = list(text = "CPEX Precision Baseline", y=13),
           color = "maroon", #"#FF0000",
           width = 2,
           dashStyle= 'shortdashdot',
           value = holdout$Precision[1])
    , list(label = list(text = "GOAL"),
           color = "green", #"#FF0000",
           width = 2.25,
           dashStyle= 'shortdashdot',
           value = 0.8)
    
    
  )
  
  ) %>%  hc_legend(align = "right", verticalAlign = "top",
                   layout = "vertical") %>% hc_size(height=700)


```

       



# scrap 3 -----------------------------------------------------------------


Row {data-height=50}
-------------------------------------
  
  <h3> 3. MOVE THE HIGHCHART LEGEND  </h3>
  
  
  
  
  Row {data-height=250}
-------------------------------------
  
  
  ```{r, message=FALSE,eval=FALSE,echo=TRUE}
ggplot(data) +
  geom_bar(aes(color, fill=cut), position="dodge") +
  ggtitle("Another interesting title") +
  xlab("Cut") + ylab("Amount")


```




```{r, message=FALSE,eval=FALSE,echo=TRUE}

dat_sum <-data %>% group_by(color,cut) %>%summarise(tot=n())

highchart() %>%
  hc_add_series(dat_sum , type="column", hcaes(color,tot,group=cut))  %>%
  hc_title(text="Another interesting title")%>%
  hc_xAxis(type="category", title=list(text="Cut")) %>%
  hc_legend( align = "right"
             ,  verticalAlign = "middle"
             ,  layout = "vertical" )

```




Row {data-height=500}
-------------------------------------
  ### ggplot2 
  
  ```{r, message=FALSE}
ggplot(data) +
  geom_bar(aes(color, fill=cut), position="stack") +
  ggtitle("Another interesting title") +
  xlab("Cut") + ylab("Amount")


```


### highcharter

```{r, message=FALSE}

dat_sum <-data %>% group_by(color,cut) %>%summarise(tot=n())

highchart() %>%
  hc_add_series(dat_sum , type="column", hcaes(color,tot,group=cut) , stacking = "normal")  %>%
  hc_title(text="Another interesting title")%>%
  hc_xAxis(type="category", title=list(text="Cut"))%>%
  hc_legend( align = "right"
             ,  verticalAlign = "middle"
             ,  layout = "vertical") # %>% hc_plotOptions(column=list(opacity=.5))

```

Row {data-height=500, data-width=600}
-------------------------------------
  
  ```{r, message=FALSE}

details1%>%filter(id==3)%>%select(!c(id, example))%>%
  datatable(class = "compact",options = list(
    bPaginate = FALSE,dom = 't'), rownames=FALSE) 

```


Row
-------------------------------------
  
  
  # 1
  ```{r}


```


# 1

```{r}


```

# 1


```{r}


```


# 1

```{r}


```


Row
-------------------------------------
  
  
  # The data {data-height=700}
  
  ```{r,message=FALSE, warning=FALSE}
holdout<- read_csv("model_progression.csv")
holdout<- holdout%>%select(!c(Group,Dataset))
datatable(holdout, class = "compact",options = list(pageLength = 18))

# first observation is the baseline model performance
```

Row
-------------------------------------
  
  # add numbers to the Method names
  ```{r,message=FALSE, warning=FALSE}

holdout<- holdout %>% mutate(x_label=paste0(row_number()-1,": ",Method))
datatable(holdout,  class = "compact",options = list(pageLength = 18))
``` 


Row
-------------------------------------
  
  
  # first observation is the baseline model performance
  ```{r,message=FALSE, warning=FALSE}

recall_baseline<-holdout$Recall[1]
precision_baseline<- holdout$Precision[1]
holdout <- holdout[-1,]

recall_baseline
precision_baseline
```


Row
-------------------------------------
  
  # Here's where we're headed:
  ```{r, echo=FALSE}
performance_threshold_dash= 'shortdashdot'
precision_color='maroon'
recall_color= 'slateblue'

below_baseline_color='lightgrey'
above_goal_color='green'

highchart() %>%
  hc_add_series(holdout, "line", hcaes(x_label,Precision)
                , name="Precision" , color=precision_color 
                # conditional line coloring: let the line plot change colors and dash style based on y-values
                , zones=list(  
                  list( value = precision_baseline , color= below_baseline_color)
                  ,  list( value = 0.8  , color = precision_color )
                  ,  list( value = 1.01 , color = above_goal_color , dashStyle='shortdashdot' , dashWidth=3 )
                ) 
  ) %>% 
  
  
  hc_add_series(holdout, "line",  hcaes(x_label,Recall)  
                ,  name="Recall"  , color=recall_color
                ,  zones=list(
                  list(value=recall_baseline,color= below_baseline_color)
                  , list(value = 0.8,color = recall_color)
                  , list(value = 1.01,color = above_goal_color ) )
  )%>% 
  hc_xAxis(type="category") %>%   
  hc_yAxis(max=1)%>% 
  
  hc_yAxis(plotLines = list(
    
    list(label = list(text = "Recall Baseline"),
         color = "slateblue",
         width = 1.75,
         dashStyle= performance_threshold_dash,
         value = recall_baseline)
    
    , list(label = list(text = "Precision Baseline", y=13),
           color = "maroon", 
           width = 2,
           dashStyle= performance_threshold_dash,
           value = precision_baseline)
    , list(label = list(text = "GOAL"),
           color = above_goal_color , 
           width = 2.25,
           dashStyle= performance_threshold_dash,
           value = 0.8)
    
    
  ))%>%    
  hc_legend(align = "right", verticalAlign = "top", layout = "vertical") %>% 
  hc_size(height=700)%>%
  hc_title(text="Precision/Recall Tradeoff") %>% hc_subtitle(text="ML Classification Model Progression")

```























# Bubble plot

```{r}
data(stars)
stars %>% hchart("scatter", hcaes(temp, lum, size = radiussun)) 
```


# axis type = "logarithmic"
```{r}
stars %>% 
  hchart("scatter", hcaes(temp, lum, size = radiussun))  %>% 
  hc_xAxis(type = "logarithmic") %>%                                    ####
  hc_yAxis(type = "logarithmic")                                        ####
```




# Adding colors via colorize
```{r}
colors <- c("#FB1108","#FD150B","#FA7806","#FBE426","#FCFB8F",                     #####
            "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1")                              #####
stars$color <- colorize(log(stars$distance), colors)                               #####
stars %>%
  hchart( "scatter", hcaes(temp, lum, size = radiussun, color=color)) %>%          #####
hc_xAxis(type = "logarithmic") %>% 
  hc_yAxis(type = "logarithmic", gridLineWidth = 0) 
```


# Axis titles
```{r}
colors <- c("#FB1108","#FD150B","#FA7806","#FBE426","#FCFB8F",
            "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1")
stars$color <- colorize(log(stars$distance), colors)
stars %>% 
  hchart( "scatter", hcaes(temp, lum, size = radiussun, color=color)) %>% 
  hc_xAxis(type = "logarithmic" , title = list(text ="Temperature")) %>%       ######
hc_yAxis(type = "logarithmic", title = list(text ="Luminosity")) %>%         ######
hc_title(text='Sweet bubble plot')                                           ######
```




# Custom hover text

```{r}
colors <- c("#FB1108","#FD150B","#FA7806","#FBE426","#FCFB8F",
            "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1")
stars$color <- colorize(log(stars$distance), colors)
x <- c("Luminosity", "Temperature", "Distance")                #####  what do you want to name the values
y <- sprintf("{point.%s:.2f}", c("lum", "temp", "distance"))   #####  column names of the values & formatting to cut off at 2 decimal places
tltip <- tooltip_table(x, y)                                   #####  creates the tooltip
stars %>% 
  hchart( "scatter", hcaes(temp, lum, size = radiussun, color=color)) %>% 
  hc_xAxis(type = "logarithmic" , title = list(text ="Temperature")) %>% 
  hc_yAxis(type = "logarithmic", title = list(text ="Luminosity")) %>% 
  hc_title(text='Sweet bubble plot')   %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip)     ##### use the tooltip
```


