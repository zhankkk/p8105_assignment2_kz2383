kz2383\_assignment2
================

## problem 2

read and clean sheet mr\_trash\_wheel

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

``` r
 mr_trash_wheel = 
  read_excel(
    "~/Dropbox/Mac/Downloads/Trash Wheel Collection Data.xlsx",
    sheet = "Mr. Trash Wheel", range = cell_cols("A:N")) %>%
  janitor::clean_names() %>%
  drop_na(dumpster)%>%
  mutate(
    sports_balls = round(sports_balls),
    sports_balls = as.integer(sports_balls))%>%
  mutate(dumpster=as.character(dumpster),
         trash_wheel=c("mr"))
```

read and clean professor trash wheel data

``` r
professor_trash_wheel = 
  read_excel(
    "~/Dropbox/Mac/Downloads/Trash Wheel Collection Data.xlsx",
    sheet = "Professor Trash Wheel", range = cell_cols("A:M")) %>%
  janitor::clean_names() %>%
  drop_na(dumpster)%>%
  mutate(
         dumpster=as.character(dumpster),
         year=as.character(year),
          trash_wheel=c("prof"))
```

combine mr. trash wheel and professor trash wheel data

``` r
data=mr_trash_wheel%>%full_join(professor_trash_wheel)
```

    ## Joining, by = c("dumpster", "month", "year", "date", "weight_tons",
    ## "volume_cubic_yards", "plastic_bottles", "polystyrene", "cigarette_butts",
    ## "glass_bottles", "grocery_bags", "chip_bags", "homes_powered", "trash_wheel")

``` r
#total weight in dataset professor trash wheel

total_weight=sum(professor_trash_wheel$weight_tons)
total_weight
```

    ## [1] 190.12

``` r
# total number of sports balls in 2020 in mr. trash wheel 
num=mr_trash_wheel%>%filter(year=="2020")%>%pull(sports_balls)
total_ball=sum(num)
total_ball
```

    ## [1] 856

For Mr. Trash Wheel there are 547 observations and 15 variables. Except
for `month` and `date`, all variables are `integer` or `numeric`
variables. Number of sports balls is rounded to nearest integer so is
the only integer variable in the dataset.

As for professor trash wheel, there are 94 observations and 14
variables.Except for `month` and `date`, all variables are `integer` or
`numeric` variables. Number of sports balls is rounded to nearest
integer so is the only integer variable in the dataset.

\#\#\# There are a total of 190.12 tons of trash collected by professor trash
wheel and 856 sports balls collected by mr. trash wheel in 2020.

## problem 3

clean pols-month dataset, separate variable mon and create new variable
president

``` r
pols = 
  read_csv("/Users/zhankeming/Desktop/zhank/Desktop/data science/solution for data science/p8105_hw2_ajg2202/data/fivethirtyeight_datasets/pols-month.csv") %>%
  separate(mon, into = c("year", "month", "day"), convert = TRUE) %>%
  mutate(
    month = month.name[month], 
    president = recode(prez_gop, "0" = "dem", "1" = "gop", "2" = "gop")) %>%
  select(-day, -starts_with("prez"))
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pols
```

    ## # A tibble: 822 × 9
    ##     year month     gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <int> <chr>       <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1  1947 January        23      51     253      23      45     198 dem      
    ##  2  1947 February       23      51     253      23      45     198 dem      
    ##  3  1947 March          23      51     253      23      45     198 dem      
    ##  4  1947 April          23      51     253      23      45     198 dem      
    ##  5  1947 May            23      51     253      23      45     198 dem      
    ##  6  1947 June           23      51     253      23      45     198 dem      
    ##  7  1947 July           23      51     253      23      45     198 dem      
    ##  8  1947 August         23      51     253      23      45     198 dem      
    ##  9  1947 September      23      51     253      23      45     198 dem      
    ## 10  1947 October        23      51     253      23      45     198 dem      
    ## # … with 812 more rows

clean dataset snp and arrange year and month as leading columns

``` r
snp = 
  read_csv("/Users/zhankeming/Desktop/zhank/Desktop/data science/solution for data science/p8105_hw2_ajg2202/data/fivethirtyeight_datasets/snp.csv") %>%
  separate(date, into = c("month", "day", "year"), convert = TRUE) %>%
  arrange(year, month) %>%
  mutate(month = month.name[month]) %>%
  select(year, month, close) 
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp
```

    ## # A tibble: 787 × 3
    ##     year month     close
    ##    <int> <chr>     <dbl>
    ##  1  1950 January    17.0
    ##  2  1950 February   17.2
    ##  3  1950 March      17.3
    ##  4  1950 April      18.0
    ##  5  1950 May        18.8
    ##  6  1950 June       17.7
    ##  7  1950 July       17.8
    ##  8  1950 August     18.4
    ##  9  1950 September  19.5
    ## 10  1950 October    19.5
    ## # … with 777 more rows

transform unemployment data from wide to long format

``` r
unemployment = 
  read_csv("/Users/zhankeming/Desktop/zhank/Desktop/data science/solution for data science/p8105_hw2_ajg2202/data/fivethirtyeight_datasets/unemployment.csv") %>%
  gather(key = month, value = unemployment, Jan:Dec) %>%
  mutate(
    month = 
      recode(month, 
             Jan = "January", Feb = "February",
             Mar = "March", Apr = "April",
             May = "May", Jun = "June",
             Jul = "July", Aug = "August",
             Sep = "September", Oct = "October",
             Nov = "November", Dec = "December"))
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemployment 
```

    ## # A tibble: 816 × 3
    ##     Year month   unemployment
    ##    <dbl> <chr>          <dbl>
    ##  1  1948 January          3.4
    ##  2  1949 January          4.3
    ##  3  1950 January          6.5
    ##  4  1951 January          3.7
    ##  5  1952 January          3.2
    ##  6  1953 January          2.9
    ##  7  1954 January          4.9
    ##  8  1955 January          4.9
    ##  9  1956 January          4  
    ## 10  1957 January          4.2
    ## # … with 806 more rows

merge pols-month, snp and unemployment datasets

``` r
data = 
  left_join(pols, snp) %>%
  left_join(., unemployment)
```

    ## Joining, by = c("year", "month")
    ## Joining, by = "month"

``` r
data
```

    ## # A tibble: 55,896 × 12
    ##     year month   gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president close
    ##    <int> <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>     <dbl>
    ##  1  1947 January      23      51     253      23      45     198 dem          NA
    ##  2  1947 January      23      51     253      23      45     198 dem          NA
    ##  3  1947 January      23      51     253      23      45     198 dem          NA
    ##  4  1947 January      23      51     253      23      45     198 dem          NA
    ##  5  1947 January      23      51     253      23      45     198 dem          NA
    ##  6  1947 January      23      51     253      23      45     198 dem          NA
    ##  7  1947 January      23      51     253      23      45     198 dem          NA
    ##  8  1947 January      23      51     253      23      45     198 dem          NA
    ##  9  1947 January      23      51     253      23      45     198 dem          NA
    ## 10  1947 January      23      51     253      23      45     198 dem          NA
    ## # … with 55,886 more rows, and 2 more variables: Year <dbl>, unemployment <dbl>

The first dataset `pols` includes the number of national politicians who
are democratic or republican at any given time.

The second dataset `snp` indicates the stock market index (S&P), which
usually represents the situation of stock market as a whole.

The third dataset `unemployment` showed the percentage of unemployment
in each month of the associated year.

In the final merged dataset `data`,

-   there are 55896 rows and 12 columns;
-   it range from year 1947 to year 2015;
-   it have key variables named “gov\_gop, sen\_gop, rep\_gop, gov\_dem,
    sen\_dem, rep\_dem, president, close, Year, unemployment”.
