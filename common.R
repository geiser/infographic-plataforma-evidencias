# concat columns from a df
concat.columns <- function(df, columns, sep='', prefix = list()) {
  to_return <- df[[columns[1]]]
  if (columns[[1]] %in% names(prefix)) {
    to_return <- paste0(prefix[[columns[[1]]]], df[[columns[1]]])
  }
  if (length(columns) > 1) {
    to_return <- paste0(
      to_return
      , do.call(
        paste0
        , lapply(columns[2:length(columns)], FUN = function(x) {
          v <- df[[x]]; v[is.na(v)] <-'';
          if (x %in% names(prefix)) {
            v <- paste0(prefix[[x]], v)
          }
          return(paste0(sep,v))
        })))
  }
  return(to_return)
}

# parameters to get data from CSVs
default.csv.params <- function() {
  
  to_return <- list(
    assessmentValue = list(src='data/assessment.csv', id='Avaliação da Evidência', name='Avaliação da Evidência')
    
    , period = list(src='data/evidencias.csv', id='Cód. Ciclo', name='Ciclo')
    , technologyType = list(src='data/evidencias.csv', id='Tipo de Tecnologia', name="Tipo de Tecnologia"
      , extra.fields=list(period='Cód. Ciclo'))
    , technology = list(src='data/evidencias.csv', id='Cód. Tecnologia', name="Cód. Tecnologia"
      , extra.fields=list(technologyType='Tipo de Tecnologia', period='Cód. Ciclo'))
    , evidence = list(src='data/evidencias.csv', id='Cód. Evidência', name='Evidência'
      , extra.fields=list(technology='Cód. Tecnologia', technologyType='Tipo de Tecnologia', period='Cód. Ciclo'))

    , dimension = list(src='data/criterios.csv', id='Dimensão', name='Dimensão')
    , criteria = list(src='data/criterios.csv', id='Cód. Critério', name=c('Cód. Item Edital','Critério')
      , extra.fields=list(dimension='Dimensão'))
  )
  
  for (key in names(to_return)) {
    if (!is.null(to_return[[key]][['src']])) {
      df <- read_csv(to_return[[key]][['src']])
      df[['id']] <- df[[to_return[[key]][['id']]]]
      df[['name']] <- concat.columns(df, to_return[[key]][['name']], '.')
      for(nfield in names(to_return[[key]][['extra.fields']])) {
        df[[nfield]] <- df[[to_return[[key]][['extra.fields']][[nfield]]]]
      }
      to_return[[key]][['df']] <- unique(df)
      
      u_df <- unique(df[,c('id','name')])
      u_list <- as.list(u_df[['name']])
      names(u_list) <- u_df[['id']]
      to_return[[key]][['unique']] <- u_list
    }
  }
  
  return(to_return)
}

# returns a pair-list <name,id> for a key
get_choices <- function(key, filters = list(), csv.params = default.csv.params()) {
  df <-  csv.params[[key]][['df']]
  
  idx <- !is.na(df[['id']])
  for (nfilter in names(filters)) {
    if (all(nfilter %in% colnames(df)) && length(filters[[nfilter]]) > 0) {
      idx <- idx & (df[[nfilter]] %in% filters[[nfilter]])
    }
  }
  
  df <- df[idx,]
  df <- unique(df[,c('id','name')])
  to_return <- as.list(df[['id']])
  names(to_return) <- df[['name']]
  return(to_return)
}

# get data
get_data <- function(dtype, filters = list(), options = list(), csv.params = default.csv.params()) {
  library(pivottabler)
  if (length(options$assessmentValue) == 0) return(NULL)
  
  if (dtype == 'assessment-result') {
    
    df <- read_csv('data/assessment.csv')
    fields <- list(
      id=list(period = 'Cód. Ciclo'
              , technologyType = 'Tipo de Tecnologia'
              , technology = 'Cód. Tecnologia'
              , evidence = 'Cód. Evidência'
              , dimension = 'Dimensão'
              , criteria = 'Cód. Critério'
              , assessmentFrom = 'Avaliação consolidadora?'
              , assessmentValue = 'Avaliação da Evidência')
      , name=list(period = 'Ciclo'
                  , technologyType = 'Tipo de Tecnologia'
                  , technology = 'Cód. Tecnologia'
                  , evidence = 'Cód. Evidência'
                  , dimension = 'Dimensão'
                  , criteria = 'Critério'
                  , assessmentFrom = 'Avaliação consolidadora?'
                  , assessmentValue = 'Avaliação da Evidência')
    )
    
    ## Apply filters
    idx <- rep(T, nrow(df))
    if (options$assessmentFrom != 'all') {
      if (options$assessmentFrom == 'main') {
        idx <- idx & (df[[fields$id$assessmentFrom]] == 'S')
      } else {
        idx <- idx & (df[[fields$id$assessmentFrom]] == 'N')
      }
    }
    
    for (nfilter in names(filters)) {
      if (all(nfilter %in% names(fields$id)) && length(filters[[nfilter]]) > 0) {
        idx <- idx & (df[[fields$id[[nfilter]]]] %in% filters[[nfilter]])
      }
    }
    df <- df[idx,]
    
    ## getting data
    rdata <- data.frame()
    if (nrow(df) == 0) return(rdata)
    
    grouping_by = c('period')
    if (length(filters[['technology']]) > 0) {
      grouping_by <- c(grouping_by, 'technology')
    } else if (length(filters[['technologyType']]) > 0) {
      grouping_by <- c(grouping_by, 'technologyType')
    }
    #if (length(filters$criteria) > 0) {
    #  grouping_by <- c(grouping_by, 'criteria')
    #} else
    if (length(filters[['dimension']]) > 0) {
      grouping_by <- c(grouping_by, 'dimension')
    }
    cids <- as.vector(sapply(grouping_by, FUN = function(x) { return(fields$id[[x]]) }))
    columns <- as.vector(sapply(grouping_by, FUN = function(x) { return(fields$name[[x]]) }))
    prefix <- list('Tec-'); names(prefix) <- fields$name$technology
    df[['GId']] <- concat.columns(df, cids, sep = ':')
    df[['Group']] <- concat.columns(df, columns, sep = ':', prefix = prefix)
    grouping.df <- unique(df[,c('GId','Group', unique(c(cids,columns)))])
    
    lgroupingby <- list(id=c('GId',cids), name=c('Group',columns))
    
    r_cols <- options$assessmentValue
    r_cols <- c(r_cols, paste0('Pct.', r_cols))
    display_columns <- unique(c('id','name',r_cols,lgroupingby$name))
    
    ## iterate by groups
    for (gid in unique(df[['GId']])) {
      vdf <- df[(df[['GId']] == gid),]
      if (nrow(vdf) == 0) next;
      
      vdata <- PivotTable$new()
      vdata$addData(vdf)
      
      vdata$addColumnDataGroups(fields$id$assessmentValue)
      vdata$addRowDataGroups(fields$id$criteria)
      vdata$defineCalculation(calculationName = "Total", summariseExpression="n()")
      vdata$evaluatePivot()
      
      pdata <- vdata$asDataFrame()
      if (nrow(pdata) < 2) next;
      pdata[['id']] <- rownames(pdata)
      pdata[is.na(pdata)] <- 0
      for (value in options$assessmentValue) {
        if (!(value %in% colnames(pdata))) pdata[[value]] <- rep(0,nrow(pdata))
        pdata[[paste0('Pct.',value)]] <- round((100*pdata[[value]])/pdata$Total,2)
      }
      pdata <- pdata[(1:nrow(pdata)-1),]
      
      pdata <- merge(pdata, csv.params$criteria$df[,c('id','name')], by='id')
      pdata[['GId']] <- rep(gid, nrow(pdata))
      pdata <- merge(pdata, grouping.df, by='GId')
      
      ## sorting and select the n-first data 
      if (nrow(pdata) > 1) {
        orderBy <- options$assessmentValue
        if (!is.null(options$orderBy) && length(options$orderBy)>0) {
          orderBy <- options$orderBy
        }
        if (('measurement' %in% names(options)) && options$measurement == 'pct') {
          orderBy <- paste0('Pct.',orderBy)
        }
        
        pdata <- data.table::setorderv(pdata, orderBy, rep(-1,length(orderBy)))
        if (!is.null(options$limit) && !is.na(options$limit) && options$limit > 0) {
          pdata <- head(pdata, options$limit)
        }
        
        if (nrow(rdata) == 0) { rdata <- pdata } else { rdata <- rbind(rdata, pdata) }
      }
    }
    
    return(list(df=rdata, grouping_by=lgroupingby, display_columns = display_columns))
  }
}

# get data as data frame
get_data.as.df <- function(dtype, filters = list(), options = list(), csv.params = default.csv.params()) {
  params_code <- digest::digest(list(
    dtype = dtype
    , filters = filters
    , options = options
    , csv.params = csv.params))
  filename <- paste0(getwd(),'/tmp/get_data_',params_code,'.rds')
  if (!file.exists(filename)) {
    sd <- get_data(dtype, filters = filters, options = options, csv.params = csv.params)
    saveRDS(sd, filename)
  } else {
    sd <- readRDS(filename)
  }
  return(sd$df[,sd$display_columns])
}

# get plotly object for a query data
get_plotly <- function(dtype, ctype, filters = list(), options = list(), csv.params = default.csv.params()) {
  library(plotly)
  params_code <- digest::digest(list(
    dtype = dtype
    , filters = filters
    , options = options
    , csv.params = csv.params))
  filename <- paste0(getwd(),'/tmp/get_data_',params_code,'.rds')
  if (!file.exists(filename)) {
    sd <- get_data(dtype, filters = filters, options = options, csv.params = csv.params)
    saveRDS(sd, filename)
  } else {
    sd <- readRDS(filename)
  }
  
  # RadarChart, Dot Chart
  p <- plot_ly(type=ctype, mode='lines+markers')
  
  if (dtype == 'assessment-result') {
    
    if (ctype == 'scatterpolar') {
      p <- plot_ly(type = ctype, fill = 'toself')
      for (group in unique(sd$df[['Group']])) {
        for (value in options$assessmentValue) {
          name <- paste0('Qty of evidences assessed as ', value)
          if (options$measurement == 'pct') {
            name <- paste0('Pct of evidences assessed as ', value)
            value <- paste0('Pct.',value)
          }
          
          p_idx <- (sd$df[['Group']] == group)
          r <- sd$df[[value]][p_idx]
          
          theta <- sd$df$name[p_idx]
          p <- add_trace(
            p, r = r, theta = theta
            , name = paste0(name,' in ',group)
          )
        }
      }
      p <- layout(p, legend=list(orientation = 'h'))
      
    } else if (ctype == 'stackedbar') {
      
      p <- plot_ly(type='bar')
      axis_x <- unique(unique(sd$df[['Group']]))
      
      for (value in options$assessmentValue) {
        if (options$measurement == 'pct') value <- paste0('Pct.',value)
        axis_y <- as.vector(sapply(axis_x, FUN = function(x) {
          if (options$formula == 'sum') {
            return(sum(sd$df[[value]][sd$df[['Group']] == x]))
          } else {
            return(mean(sd$df[[value]][sd$df[['Group']] == x]))
          }
        }))
        p <- add_trace(p, x=axis_x, y=axis_y, name=value)
      }
      p <- layout(p, barmode = 'stack', legend=list(orientation = 'l'))

    } else if (ctype == 'pie') {
      p <- plot_ly()
      groups <- unique(unique(sd$df[['Group']]))
      
      i <- 0
      max_col <- ifelse((length(groups)%%2 == 0), 2, 1) 
      for (group in groups) {
        p <- add_pie(
          p
          , labels = options$assessmentValue
          , title = group
          , values =  as.vector(sapply(options$assessmentValue, FUN = function(value) {
            if (options$formula == 'sum') {
              return(sum(sd$df[[value]][sd$df[['Group']] == group]))
            } else {
              return(mean(sd$df[[value]][sd$df[['Group']] == group]))
            }
          }))
          , domain = list(row = i %/% max_col, column = i %% max_col)
          , hole = 0.8
          , sort = F
        )
        i <- i + 1
      }
      
      p <- layout(
        p, grid=list(rows=length(groups)/max_col, columns=max_col)
        , xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        , yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        , legend=list(orientation = 'h'))
      
    }
    
    
  }
  return(p)
}
