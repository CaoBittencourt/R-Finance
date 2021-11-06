pkg <- c(
  'tidyverse'
  ,'data.table'
  ,'quantmod'
  ,'rvest'
  ,'progress'
  ,'lubridate'
  ,'feather'
  ,'zoo'
)

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

getTickers <- function(){
  sp500_wiki <- read_html(
    "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
  
  symbols_table <- sp500_wiki %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
    html_table()
  
  symbols_table <- symbols_table[[1]]
  tickers <- as.character(symbols_table$`Ticker symbol`)
  
  sp1000_wiki <- read_html(
    "https://en.wikipedia.org/wiki/List_of_S%26P_1000_companies")
  
  symbols_table <- sp1000_wiki %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[3]') %>%
    html_table()
  
  symbols_table <- symbols_table[[1]]
  tickers2 <- as.character(symbols_table$`Ticker Symbol`)
  
  tickers <- c(tickers,tickers2)
  
  for(i in 1:length(tickers)) tickers[[i]] <- gsub('\\.', '-', tickers[[i]])
  
  return(tickers)
}

getFinancials <- function(ticker){
  stock <- ticker
  output <- tibble()
  
  for (i in 1:length(stock)) {
    tryCatch(
      {
        url <- "https://finance.yahoo.com/quote/"
        url <- paste0(url,stock[i],"/financials?p=",stock[i])
        wahis.session <- html_session(url)
        p <- wahis.session %>%
          html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
          html_table(fill = TRUE)
        IS <- p[[1]]
        colnames(IS) <- paste(IS[1,])
        IS <- IS[-c(1,5,12,20,25),]
        names_row <- paste(IS[,1])
        IS <- IS[,-1]
        IS <- apply(IS,2,function(x){gsub(",","",x)})
        IS <- as.data.frame(apply(IS,2,as.numeric))
        rownames(IS) <- paste(names_row)
        temp1 <- IS
        url <- "https://finance.yahoo.com/quote/"
        url <- paste0(url,stock[i],"/balance-sheet?p=",stock[i])
        wahis.session <- html_session(url)
        p <- wahis.session %>%
          html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
          html_table(fill = TRUE)
        BS <- p[[1]]
        colnames(BS) <- BS[1,]
        BS <- BS[-c(1,2,17,28),]
        names_row <- BS[,1]
        BS <- BS[,-1]
        BS <- apply(BS,2,function(x){gsub(",","",x)})
        BS <- as.data.frame(apply(BS,2,as.numeric))
        rownames(BS) <- paste(names_row)
        temp2 <- BS
        url <- "https://finance.yahoo.com/quote/"
        url <- paste0(url,stock[i],"/cash-flow?p=",stock[i])
        wahis.session <- html_session(url)
        p <- wahis.session %>%
          html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
          html_table(fill = TRUE)
        CF <- p[[1]]
        colnames(CF) <- CF[1,]
        CF <- CF[-c(1,3,11,16),]
        names_row <- CF[,1]
        CF <- CF[,-1]
        CF <- apply(CF,2,function(x){gsub(",","",x)})
        CF <- as.data.frame(apply(CF,2,as.numeric))
        rownames(CF) <- paste(names_row)
        temp3 <- CF
        #assign(paste0(stock[i],'.f'),value = list(IS = temp1,BS = temp2,CF = temp3),envir = parent.frame())
        total_df <- as.data.frame(rbind(temp1,temp2,temp3))
        total_df$value <- rownames(total_df)
        for(k in 1:length(total_df$value)) total_df$value[[k]] <- gsub(" ", "",total_df$value[[k]])
        financial_camps <- total_df$value
        df <- total_df %>% t()
        df <- df[!rownames(df) %in% c("value"),]
        dates <- rownames(df)
        df <- df %>% data.table()
        colnames(df) <- financial_camps
        df$date <- mdy(dates)
        df$firm <- stock[[i]]
      },
      error = function(cond){
        message(stock[i], "Give error ",cond)
      }
    )
    df[is.na(df)] <- 0
    output <- rbind(output, df)
  }
  return(output)
}

getPrices <- function(tickers, from = "1950-01-01"){
  stock_prices <- new.env()
  getSymbols(tickers, src = "yahoo", from = "1950-01-01", auto.assign = TRUE, env = stock_prices)
  
  all_prices <- tibble()
  
  for(i in 1:length(tickers)){
    raw_df <- stock_prices[[tickers[[i]]]][,6] %>% data.frame()
    dates <- raw_df %>% row.names() %>% as.Date()
    prices <- raw_df[,1]
    
    new_df <- data.frame(price = as.numeric(prices), date = dates) %>%
      data.table()
    
    new_df$firm <- tickers[[i]]
    
    all_prices <- rbind(all_prices,new_df)
  }
  return(all_prices)
}

getMerge <- function(prices, financials){
  
  # Get common days
  k <- 1
  absences <- which(!(financials$date %in% prices$date))
  while(sum(!(unique(financials$date) %in% unique(prices$date))) > 0){
    count <- sum(!(unique(financials$date) %in% unique(prices$date)))
    financials$date[[absences[k]]] <- financials$date[[absences[k]]] + 1
    if(sum(!(unique(financials$date) %in% unique(prices$date))) < count) k <- k + 1
  }
  
  stocks <- unique(prices$firm)
  tidy_merged <- tibble()
  merged <- left_join(prices,financials, by = c("date", "firm"))
  for(s in stocks){
    df <- merged %>% filter(firm == s)
    tidy_merged <- df %>%
      na.locf() %>%
      na.omit() %>%
      distinct() %>%
      as_tibble() %>%
      rbind(tidy_merged)
  }
  
  tidy_merged$date <- as.Date(tidy_merged$date)
  tidy_merged$firm <- as.factor(tidy_merged$firm)
  cols <- colnames(tidy_merged)
  for(c in cols) if(is.character(tidy_merged[[c]])) tidy_merged[[c]] <- as.numeric(tidy_merged[[c]])
  
  tidy_merged <- tidy_merged %>% distinct()
  
  return(tidy_merged)
}

tickers <- getTickers(index = "all")

pb <- progress_bar$new(total = length(tickers))

for(t in tickers){
  tryCatch({
    fins <- getFinancials(t)
    prices <- getPrices(t)
    whole_data <- getMerge(financials = fins, prices = prices)
    path <- paste0("data/",t)
    write_feather(whole_data,path)
  }, error=function(e){})
  
  pb$tick()
}

invest <- function(method, data, date, amount_to_invest = 1, stocks_owned, specificity = 1){
  d <- date
  stocks_available <- data %>%
    filter(date == d) %>%
    .$firm %>%
    unique() %>%
    as.character()
  
  if(method == "random"){
    selected_stock <- stocks_available %>%
      sample(specificity, replace = TRUE)
  }
  
  
  if(method == "per"){
    selected_stock <- data %>%
      filter(date == d) %>%
      filter(firm %in% stocks_available) %>%
      mutate(per = price*CommonStock/TotalRevenue) %>%
      top_n(specificity, -per) %>%
      .$firm %>%
      unique() %>%
      as.character()
  }
  
  selected_stock <- data %>%
    filter(date == d) %>%
    filter(firm %in% stocks_available) %>%
    mutate(evebitda = (EarningsBeforeInterestandTaxes)/(price*CommonStock + PreferredStock + MinorityInterest + LongTermDebt - CashAndCashEquivalents)) %>%
    top_n(specificity, evebitda) %>%
    .$firm %>%
    unique() %>%
    as.character()
  
  if(method == "dividends_ev"){
    selected_stock <- data %>%
      filter(date == d) %>%
      filter(firm %in% stocks_available) %>%
      mutate(dividends_ev = (DividendsPaid)/(price*CommonStock + PreferredStock + MinorityInterest + LongTermDebt - CashAndCashEquivalents)) %>%
      top_n(specificity, dividends_ev) %>%
      .$firm %>%
      unique() %>%
      as.character()
  }
  
  if(method == "div_and_rev"){
    selected_stock <- data %>%
      filter(date == d) %>%
      filter(firm %in% stocks_available) %>%
      mutate(div_and_rev = (EarningsBeforeInterestandTaxes)*(DividendsPaid)/(price*CommonStock + PreferredStock + MinorityInterest + LongTermDebt - CashAndCashEquivalents)) %>%
      top_n(specificity, div_and_rev) %>%
      .$firm %>%
      unique() %>%
      as.character()
  }
  
  simulate_multiple_strategies <- function(data, amount_to_invest = 1, specificity = 1, strategies){
    
    count_strategy <- 0
    
    history_value <- c()
    history_strategies <- c()
    history_dates <- c()
    
    dates <- unique(data$date) %>% sort()
    dates_array <- data$date
    prices_array <- data$price
    firms_array <- data$firm
    portfolio <- tibble()
    stocks_owned <- list()
    
    pb <- progress_bar$new(
      format = "(:spin) [:bar] :percent eta: :eta",
      total = length(dates), clear = FALSE, width = 60)
    
    for(s in strategies) stocks_owned[[s]] <- list()
    
    for(i in 1:length(dates)){
      pb$tick()
      d <- dates[[i]]
      
      pos_date <- which(dates_array == d)
      
      today_price <- prices_array[pos_date] %>% as.numeric()
      today_firm <- firms_array[pos_date] %>% as.character()
      
      for(strategy in (strategies)){
        
        # Invest first day of each month
        if(!(class(try(dates[[i-1]], silent = TRUE)) == 'try-error')){
          if(day(dates[[i]]) < day(dates[[i-1]])){
            stocks_owned[[strategy]] <- invest(method = strategy, data = data, date = d, amount_to_invest = amount_to_invest, stocks_owned = stocks_owned[[strategy]], specificity = specificity)
          }
        }else{
          stocks_owned[[strategy]] <- invest(method = strategy, data = data, date = d, amount_to_invest = amount_to_invest, stocks_owned = stocks_owned[[strategy]], specificity = specificity)
        }
        
        # Compute Portfolio value
        tmp_value <- 0
        tmp_benchmark <- 0
        
        stocks <- names(stocks_owned[[strategy]])
        for(n in 1:length(stocks)){
          st <- stocks[[n]]
          pos_firm <- which(firms_array == st)
          today_price <- prices_array[intersect(pos_date, pos_firm)]
          tmp_value <- tmp_value + sum(stocks_owned[[strategy]][[st]]*today_price)
        }
        
        history_value[[i + count_strategy]] <- tmp_value
        history_strategies[[i + count_strategy]] <- strategy
        history_dates[[i + count_strategy]] <- d
        count_strategy <- count_strategy + 1
      }
    }
    results <- data.table(value = history_value,
                          strategy = history_strategies,
                          date = as.Date(history_dates)) %>%
      na.omit()
    
    return(list(results = results, portfolio = stocks_owned))
  }
  
  n_stocks <- 100
  specificity <- 25
  
  strategies_list <- c("random", "per", "evebitda", "dividends_ev", "div_and_rev")
  
  
  stocks_universe <- getTickers("all") %>% sample(n_stocks) %>% intersect(list.files("data/"))
  files <- paste0("data/",stocks_universe)
  dt <- rbindlist(lapply(files, read_feather)) %>% filter(price >= 0.1) %>% data.table()
  investment <- simulate_multiple_strategies_fasto(data = dt,
                                                   amount_to_invest = 1,
                                                   strategies = strategies_list,
                                                   specificity = specificity)
  
  investment$result %>%
    ggplot(aes(x = date, y = value, colour = strategy)) +
    geom_line(size = 1) +
    theme_minimal() +
    scale_colour_hp(discrete = TRUE, house = "ravenclaw") +
    ylab("Value") +
    xlab("")
  
  # Compute Internal Rate of Return
  irr <- function(cash_flows, frequency = 30.5, error = 0.01){
    l <- length(cash_flows)
    cf_zero <- cash_flows[[1]]
    cf_final <- cash_flows[[l]]
    irr_bot <- -100
    irr_top <- 100
    npv <- 0
    while(abs(cf_final - npv) > error){
      npv <- 0
      irr_trial <- (irr_bot+irr_top)/2
      for(i in 1:(floor(l/frequency))){
        npv <- npv + cf_zero*((1 + irr_trial/100)^(i))
      }
      if(cf_final > npv) irr_bot <- irr_trial
      if(cf_final < npv) irr_top <- irr_trial
    }
    return(irr_trial/100)
  }
  
  # Compute relative Difference between the Return of the portfolio and the benchmark
  alpha_ret <- function(portfolio, benchmark){
    r_port <- irr(portfolio)
    r_ben <- irr(benchmark)
    a <- (r_port - r_ben)/(r_ben)
    return(a)
  }
  
  # Compute beta with the benchmark
  beta <- function(portfolio, benchmark){
    return(cov(portfolio,benchmark/sd(benchmark)^2))
  }
  
  # Compute Jensen's alpha with the benchmark
  alpha <- function(portfolio, benchmark){
    r_port <- irr(portfolio)
    r_ben <- irr(benchmark)
    a <- (r_port - beta(portfolio, benchmark)*r_ben)
    return(a)
  }
  
  trials <- 50
  n_stocks <- 100
  specificity <- 25
  alpha_result <- list()
  diff_result <- list()
  
  strategies_list <- c("random", "per", "evebitda", "dividends_ev", "div_and_rev")
  
  for(t in 1:trials){
    
    stocks_universe <- getTickers("all") %>% sample(n_stocks) %>% intersect(list.files("data/"))
    files <- paste0("data/",stocks_universe)
    dt <- rbindlist(lapply(files, read_feather)) %>% filter(price >= 0.1) %>% data.table()
    investment <- simulate_multiple_strategies_fasto(data = dt, amount_to_invest = 1, strategies = strategies_list, specificity = specificity)
    
    for(s in strategies_list) {
      diff_result[[s]][[t]] <- alpha_ret(portfolio = investment[["results"]] %>% filter(strategy == s) %>% .$value,
                                         benchmark = investment[["results"]] %>% filter(strategy == "random") %>% .$value
      )
    }
  }
  
  diff_result %>%
    as_tibble() %>%
    melt() %>%
    ggplot(aes(x = as.factor(variable), y = value)) +
    geom_boxplot(aes(fill = variable), colour = "black", alpha = 0.35) +
    geom_point(aes(colour = variable), size = 2) +
    theme_minimal() +
    xlab("Strategy") +
    ylab("Returns Relative Difference")+
    scale_colour_hp(discrete = TRUE, house = "ravenclaw", direction = -1, name = "Strategy") +
    scale_fill_hp(discrete = TRUE, house = "ravenclaw", direction = -1, name = "Strategy")
  
  alpha_result %>%
    as_tibble() %>%
    melt() %>%
    ggplot(aes(x = as.factor(variable), y = value)) +
    geom_boxplot(aes(fill = variable), colour = "black", alpha = 0.35) +
    geom_point(aes(colour = variable), size = 2) +
    theme_minimal() +
    xlab("Strategy") +
    ylab("Alpha")+
    scale_colour_hp(discrete = TRUE, house = "ravenclaw", direction = -1, name = "Strategy") +
    scale_fill_hp(discrete = TRUE, house = "ravenclaw", direction = -1, name = "Strategy")