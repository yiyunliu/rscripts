library(stringr)
library(tidyverse)
library(purrr)
library(readxl)
library(rlang)
library(forcats)
library(lubridate)
library(openxlsx)

## Code for sanitizing data

is_chart <- function(sheet_name){
  str_sub(sheet_name,-6)==" Chart" && str_sub(sheet_name,1,5)!="Daily"
}
extract_sheet <- function(sheet_name){
  str_sub(sheet_name,1,-7)
}
get_sheets <- function(svec){
  svec %>% str_trim %>% keep(is_chart) %>% map_chr(extract_sheet)
}

is_date <- function(str){str == "日期"}


load_sheet <- function(path,sheet){
  cols <- sheet %>% read_excel(path=path,range=cell_limits(c(3,92),c(NA,NA))) %>% colnames
  ct <- if_else(is_date(cols),"date","numeric")
  read_excel(path,sheet=sheet,col_types = ct,range=cell_limits(c(3,92),c(NA,NA))) %>% 
    mutate(product=sheet) %>% 
    select(product,everything(),-contains("PPM")) %>%
    rename(date=日期)
}

load_sheets <- function(path){
  sheets <- path %>% excel_sheets() %>% get_sheets()
  sheets %>% map_dfr (partial(load_sheet,path=path))
}


## Query helpers

tbetween <- function(t,t1,t2){
  between(t,as.POSIXct(t1,"UTC"),as.POSIXct(t2,"UTC"))
}

ppmf <- function(x,nd,d){x/(nd+d)*10^6} # Computes ppm


## Query functions

### Query 1
query1_plot <- function(dt,p,beg,end){
  dt %>%
    ggplot(aes(x=fct_inorder(ErrCode),y=PPM)) +
    geom_bar(mapping=aes(fill=fct_inorder(ErrCode)),stat = "identity") + 
    geom_text(aes(label=round(PPM))) + 
    xlab("Error Code") + 
    labs(fill = "Error Code", title = paste(paste(p,collapse=" "),"Top 10 from",beg,"to",end))
}

query1 <- function(dt,p,beg,end){
  dt %>% 
    filter(tbetween(date,beg,end)) %>%
    group_by(product) %>% 
    select(everything(),-date)  %>% 
    summarise_all(sum,na.rm=TRUE) %>% 
    mutate_at(.vars=vars(everything(),-(product:不良品数量)),.funs=funs(ppmf(.,良品数量,不良品数量))) %>%
    filter(product %in% p) %>% 
    select(-(1:5)) %>% 
    summarise_all(sum,na.rm=TRUE)%>%
    gather(key="ErrCode",value="PPM")  %>% 
    arrange(desc(PPM)) %>% 
    slice(1:10)
}

query1_then_plot <- function(dt,p,beg,end){
  dt %>% query1(p,beg,end) %>% query1_plot(p,beg,end)
}

date_to_season <- function(d){
  (month(d)-1) %/% 3
}
### Query 2

# How I wish I had reader monad in R x_x
query2A_M <- function(dt,pd,beg,end,err_code){
  dt %>% 
    filter(tbetween(date,beg,end)) %>%
    mutate(date=floor_date(date,unit="month")) %>%
    filter(product %in% pd) %>%
    group_by(product,date) %>%
    summarise_all(sum,na.rm=TRUE) %>%
    mutate_at(.vars=vars(everything(),-(date:不良品数量)),.funs=funs(ppmf(.,良品数量,不良品数量))) %>%
    select(product,date,one_of(err_code)) %>% 
    gather(one_of(c(err_code)),key="err_code",value="PPM")
}

query2A_M_plot <- function(dt,pd,beg,end,err_code){
  dt %>%
    mutate(PPM=if_else(is.na(PPM),0,PPM)) %>%
    ggplot(mapping=aes(x=date,y=PPM)) +
    geom_col(mapping=aes(fill=product)) +
    labs(title=paste(paste(pd,collapse=" "),"Monthly",beg,"to",end))
}

query2A_M_then_plot <- function(dt,pd,beg,end,err_code){
  dt %>% query2A_M(pd,beg,end,err_code) %>% query2A_M_plot(pd,beg,end,err_)
}

# Season
query2A_S <- function(dt,pd,beg,end,err_code){
  err_codeS <- sym(err_code)
  dt %>%
    filter(tbetween(date,beg,end)&product==pd) %>%
    mutate(season=date_to_season(date),year = year(date)) %>%
    group_by(year,season) %>% 
    summarise(nf_total=sum(良品数量+不良品数量,na.rm=TRUE),err_total=sum(UQ(err_codeS),na.rm=TRUE)) %>%
    transmute(season=rank(season+year*4),PPM = err_total/nf_total*10^6)
}

query2A_S_plot <- function(dt,pd,beg,end,err_code){
  dt %>%
    mutate(PPM=if_else(is.na(PPM),0,PPM),Season=paste("S",season,sep="")) %>% 
    ggplot(mapping=aes(x=Season,y=PPM))+
    geom_col(mapping=aes(fill=Season))+
    geom_text(aes(label=round(PPM))) + 
    labs(title=paste(pd,err_code,"Quarterly",beg,"to",end)) +
    theme(legend.position="none")
}

query2A_S_then_plot <- function(dt,pd,beg,end,err_code){
  dt %>%
    query2A_S(pd,beg,end,err_code) %>%
    query2A_S_plot(pd,beg,end,err_code)
}

# Day
query2A_D <- function(dt,pd,beg,end,err_code){
  err_codeS <- sym(err_code)
  dt %>%
    filter(tbetween(date,beg,end)&product==pd) %>%
    transmute(day=rank(date),PPM = ppmf(UQ(err_codeS),良品数量,不良品数量))
}

query2A_D_plot <- function(dt,pd,beg,end,err_code){
  dt %>%
    mutate(PPM=if_else(is.na(PPM),0,PPM)) %>% 
    ggplot(mapping=aes(x=day,y=PPM)) + 
    geom_line(color="purple",size=2) +
    xlab("Day")+
    labs(title=paste(pd,err_code,"Daily",beg,"to",end))
}

query2A_D_then_plot <- function(dt,pd,beg,end,err_code){
  dt %>%
    query2A_D(pd,beg,end,err_code) %>%
    query2A_D_plot(pd,beg,end,err_code)
}

# Week
query2A_W <- function(dt,pd,beg,end,err_code){
  err_codeS <- sym(err_code)
  dt %>%
    filter(tbetween(date,beg,end)&product==pd) %>%
    mutate(week=floor_date(date,unit="week")) %>%
    group_by(week) %>% 
    summarise(nf_total=sum(良品数量+不良品数量,na.rm=TRUE),err_total=sum(UQ(err_codeS),na.rm=TRUE)) %>%
    transmute(week=rank(week),PPM = err_total/nf_total*10^6)
}
query2A_W_plot <- function(dt,pd,beg,end,err_code){
  dt %>% 
    mutate(PPM=if_else(is.na(PPM),0,PPM),week = fct_inorder(paste("W",week,sep=""))) %>% 
    ggplot(mapping=aes(x=week,y=PPM)) + 
    geom_col(mapping=aes(fill=week)) + 
    geom_text(aes(label=round(PPM))) + 
    xlab("Week") + 
    labs(fill = "Week", title = paste(pd,err_code,"Weekly",beg,"to",end)) +
    theme(legend.position="none")
}
query2A_W_then_plot <- function(dt,pd,beg,end,err_code){
  dt %>%
    query2A_W(pd,beg,end,err_code) %>%
    query2A_W_plot(pd,beg,end,err_code)
}
## Daily
daily <- function(dt){
  dt %>% 
    group_by(date) %>% 
    select(-product) %>% 
    summarise_all(sum,na.rm=TRUE)
}

## Export sanitized data as a csv file
export_sheets <- function(dt,path){
  write.xlsx(dt,path,asTable=TRUE)
}

## This function should have been used as default.
## But!
## "Don't fix it if it ain't broken" x_x
tidy_sheets <- function(dt){
  dt %>%
    gather(everything(),-(1:4),key="err_code",value="err_count")
}

convert_sheets <- function(orig,dest){
  load_sheets(orig) %>% export_sheets(dest)
}

convert_sheets_tidy <- function(orig,dest){
  load_sheets(orig) %>% tidy_sheets %>% export_sheets(dest)
}

convert_sheets_xlsx <- function(orig,dest){
  load_sheets(orig) %>% tidy_sheets %>% export_sheets(dest)
}