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
    cols <- sheet %>%
        read_excel(path=path,range=cell_limits(c(3,92),c(NA,NA))) %>%
        colnames
    ct <- if_else(is_date(cols),"date","numeric")
    read_excel(path,sheet=sheet,col_types = ct,
                       range=cell_limits(c(3,92),c(NA,NA))) %>%
        mutate(product=sheet,date=as_date(日期)) %>% 
        select(date,product,everything(),-contains("PPM"),-contains("不良品数量"),-日期) %>%
        rename(good_count = 良品数量) %>%
        mutate_at(vars(everything(),-(date:product)),funs(as_integer))
}

## Let's rock
load_sheets <- function(path){
    sheets <- path %>% excel_sheets() %>% get_sheets()
    data <- sheets %>% map_dfr (partial(load_sheet,path=path))
    good_data <- data %>% select(date,product,good_count)
    defect_data <- data %>%
        select(-good_count) %>%
        gather(everything(),-(date:product),key="error_code",value="count")
    list(good_data,defect_data)
}


## Query helpers

ppmf <- function(x,nd,d){x/(nd+d)*10^6} # Computes ppm


## Query functions

### Query 1
query1 <- function(data,products,date_begin,date_end){
    good_total <- data[[1]] %>%
        summarise(good_total=sum(good_count,na.rm=TRUE)) %>%
        pull(good_total)
    defect_data <- data[[2]] %>%
        filter(between(date,date_begin,date_end) & product %in% products) %>%
        group_by(error_code, product) %>%
        summarise(defect_count=sum(count,na.rm=TRUE)) %>%
        ungroup(error_code)
    defect_total <- defect_data %>%
        summarise(defect_total=sum(defect_count)) %>%
        pull(defect_total)
    plot_data <- defect_data %>%
        transmute(product,error_code,PPM=defect_count*10^6/(good_total+defect_total))
    top_ten <- plot_data %>%
        group_by(error_code) %>%
        summarise(PPM=sum(PPM)) %>%
        arrange(desc(PPM)) %>%
        slice(1:10) %>%
        distinct(error_code) %>%
        pull(error_code)
    plot_data %>%
        filter(error_code%in% top_ten) %>%
        ggplot(aes(x=reorder(error_code,-PPM,sum),y=PPM,fill=product)) +
        geom_col() +
        xlab("Error Code")
}

### Query 2

## Top-level query2_round function
query2_round <- function(data,unit,products=c(),date_beg,date_end,err_code){
    query2_natural(data,
                   unit,
                   products,
                   as_date(floor_date(date_beg,unit=unit)),
                   as_date(ceiling_date(date_end,unit=unit)),
                   err_code)
    ## might want to add some labels here
}
## Helper
date_quotient <- function(data,unit){
    data %>% mutate(date=as_date(floor_date(date,unit=unit)))
}

## Top-level query2_natural function
query2_natural <- function(data,unit,products=c(),date_beg,date_end,err_code){
    good_total <- data[[1]] %>%
        filter(between(date,date_beg,date_end) & product %in% products) %>%
        date_quotient(unit) %>%
        group_by(date) %>%
        summarise(good_count=sum(good_count,na.rm=TRUE))
    defect_data <- data[[2]] %>%
        filter(between(date,date_beg,date_end) & product %in% products) %>%
        date_quotient(unit) %>%
        group_by(date,product,error_code) %>%
        summarise(count=sum(count,na.rm=TRUE)) %>%
        ungroup(date,product,error_code)
    defect_total <- defect_data %>%
        group_by(date) %>%
        summarise(defect_count=sum(count,na.rm=TRUE))
    
    plot_data <- defect_data %>%
        filter(error_code %in% err_code) %>%
        left_join(.,good_total) %>%
        left_join(.,defect_total) %>%
        transmute(date,product,error_code,PPM=count*10^6/(good_count+defect_count))
    plot_data %>% ggplot(aes(date,PPM,fill=error_code)) +
        geom_col()
}

## Support functions for shiny
min_max_date <- function(data){
    data %>% summarise(max=max(date),min=min(date)) 
}

products <- function(data){
    data %>% distinct(product) %>% pull(product)
}

error_codes <- function(data){
    data %>% distinct(error_code) %>% pull(error_code)
}

## Daily
daily <- function(data){
    good_daily <- data[[1]] %>%
        group_by(date) %>% summarise(good_count=sum(good_count,na.rm=TRUE))
    defect_daily <- data[[2]] %>%
        group_by(date,error_code) %>%
        summarise(defect_count=sum(count,na.rm=TRUE)) %>%
        spread(error_code,defect_count)
    left_join(good_daily,defect_daily)
}

## Export sanitized data as a csv file
export_sheets <- function(dt,path){
    write.xlsx(dt,path,asTable=TRUE)
}

## Deprecated
tidy_sheets <- function(dt){
    dt %>%
        gather(everything(),-(1:4),key="err_code",value="err_count")
}

## File conversion
convert_sheets <- function(orig,dest){
    load_sheets(orig) %>% export_sheets(dest)
}

convert_sheets_tidy <- function(orig,dest){
    load_sheets(orig) %>% tidy_sheets %>% export_sheets(dest)
}

convert_sheets_xlsx <- function(orig,dest){
    load_sheets(orig) %>% tidy_sheets %>% export_sheets(dest)
}
