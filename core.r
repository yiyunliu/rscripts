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
        select(date,product,everything(),-contains("PPM"),-不良品数量,-日期) %>%
        rename(good_count = 良品数量) %>%
        mutate_at(vars(everything(),-(date:product)),funs(as_integer))
}

## Let's rock
load_sheets_neo <- function(path){
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
query1_plot <- function(dt,p,beg,end){
    dt %>%
        ggplot(aes(x=fct_inorder(ErrCode),y=PPM)) +
        geom_bar(mapping=aes(fill=fct_inorder(ErrCode)),stat = "identity") + 
        geom_text(aes(label=round(PPM))) + 
        xlab("Error Code") + 
        labs(fill = "Error Code", title = paste(paste(p,collapse=" "),"Top 10 from",beg,"to",end))
}



query1_tidy_plot <- function(plot_data,p,beg,end){
    top_ten <- plot_data %>% 
        select(-product) %>% 
        group_by(err_code) %>% 
        summarise_all(sum) %>%
        transmute(err_code,PPM=ppmf(err_count,good_count,)) %>% 
        arrange(desc(PPM)) %>%
        slice(1:10) %>% 
        pull(err_code)
    
    total <- plot_data %>% 
        group_by(product) %>% 
        summarise(good = first(good_count), bad = first(不良品数量)) %>%
        select(-product) %>%
        summarise_all(sum) %>% 
        unlist(use.names=FALSE)
    
    total <- total[1]+total[2]
    
    plot_data %>% 
        filter(err_code %in% top_ten) %>% 
        
        ggplot(aes(x=reorder(err_code,-err_count/total,FUN=sum),y=err_count/total*10^6,fill=product))+
        geom_col()+
        xlab("Error Code") + 
        ylab("PPM")+
        labs(fill = "Product", title = paste(paste(p,collapse=" "),"Top 10 from",beg,"to",end))
}

query1_tidy <- function(tidy_data,p,beg,end){
    plot_data <- tidy_data %>%
        mutate(date=as_date(date)) %>%
        filter(between(date,beg,end) & product %in% p) %>%
        select(-date) %>% 
        group_by(err_code,product) %>% 
        summarise_all(sum,na.rm=TRUE) %>% 
        ungroup(err_code) 
}

query1 <- function(dt,p,beg,end){
    dt %>% 
        filter(tbetween(date,beg,end)) %>%
        group_by(product) %>% 
        select(everything(),-date)  %>% 
        summarise_all(sum,na.rm=TRUE) %>% 
        mutate_at(.vars=vars(everything(),-(product:不良品数量)),.funs=funs(ppmf(.,good_count,不良品数量))) %>%
        filter(product %in% p) %>% 
        select(-(1:5)) %>% 
        summarise_all(sum,na.rm=TRUE)%>%
        gather(key="ErrCode",value="PPM")  %>% 
        arrange(desc(PPM)) %>% 
        slice(1:10)
}

query1_tidy_then_plot <- function(tidy_data,p,beg,end){
    tidy_data %>% query1_tidy(p,beg,end) %>% query1_tidy_plot(p,beg,end)
}


query1_then_plot <- function(dt,p,beg,end){
    dt %>% query1(p,beg,end) %>% query1_plot(p,beg,end)
}

date_to_season <- function(d){
    (month(d)-1) %/% 3
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
    return (plot_data)
}

## Support functions for shiny
min_max_date <- function(tidy_dt){
    tidy_dt %>% summarise(max=max(date),min=min(date)) 
}

products <- function(tidy_dt){
    l <- tidy_dt %>% distinct(product)
    l$product
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
