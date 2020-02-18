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
    , criteria = list(src='data/criterios.csv', id='Cód. Critério', name='Cód. Critério'
      , extra.fields=list(dimension='Dimensão', cod.edital='Cód. Item Edital'))
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
  params_code <- digest::digest(list(
    dtype = dtype
    , filters = filters
    , options = list(assessmentValue = options$assessmentValue
                     , assessmentFrom = options$assessmentFrom
                     , measurement = options$measurement
                     , limit = options$limit
                     , orderBy = options$orderBy)
    , csv.params = csv.params))
  
  dir.create(file.path(getwd(), 'tmp'), showWarnings = FALSE)
  filename <- paste0(getwd(),'/tmp/get_data_',params_code,'.rds')
  if (file.exists(filename)) {
    return(readRDS(filename))
  }
  
  library(pivottabler)
  if (length(options$assessmentValue) == 0) return(NULL)
 
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
  
  
  lgroupingby <- c()
  rdata <- data.frame()
  display_columns <- c()
  
  
  if (dtype == 'about-evidences' ||
      dtype == 'about-assessments') {
    
    ## Apply filters
    idx <- rep(T, nrow(df))
    if (options$assessmentFrom != 'all') {
      if (options$assessmentFrom == 'main') {
        idx <- idx & (df[[fields$id$assessmentFrom]] == 'S')
      } else {
        idx <- idx & (df[[fields$id$assessmentFrom]] == 'N')
      }
    }
    
    ## (TODO): Remove ugly hack to generate pie charts about cicle 2  
    if (length(filters$period) == 1 && (filters$period == "61" || filters$period == "2")) {
      df <- read_csv('data/evidencias.csv')
      rel <- read_csv('data/evidencias-pre2.csv')[,c(csv.params$evidence$id,csv.params$criteria$id)]
      df <- merge(df, rel)
      df <- merge(df, read_csv('data/criterios.csv'))
      idx <- rep(T, nrow(df))
    }
    
    for (nfilter in names(filters)) {
      if (all(nfilter %in% names(fields$id)) && length(filters[[nfilter]]) > 0) {
        idx <- idx & (df[[fields$id[[nfilter]]]] %in% filters[[nfilter]])
      }
    }
    df <- df[idx,]
    
    rdata <- df
    display_columns <- c(as.vector(sapply(fields$name, FUN = function(x) x)), 'Evidência')
    
  } else if ( dtype == 'result-distribution' || dtype == 'result-criteria' || dtype == 'assessment-result') {
    
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
    display_columns <- unique(c('id', 'name', r_cols, lgroupingby$name))
    
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
  }
  
  sd <- list(df=rdata, grouping_by=lgroupingby, display_columns=display_columns)
  saveRDS(sd, filename)
  return(sd)
  
}

# get data as data frame
get_data.as.df <- function(dtype, filters = list(), options = list(), csv.params = default.csv.params()) {
  sd <- get_data(dtype, filters = filters, options = options, csv.params = csv.params)
  return(sd$df[,sd$display_columns])
}

# get plotly object for a query data
get_plotly <- function(dtype, ctype, filters = list(), options = list(), csv.params = default.csv.params()) {
  library(plotly)
  
  sd <- get_data(dtype, filters = filters, options = options, csv.params = csv.params)
  
  # RadarChart, Dot Chart
  p <- plot_ly()
  
  
  if (dtype == 'result-criteria') {
    
    if (ctype == 'radar') {
      
      p <- plot_ly(type = 'scatterpolar', fill = 'none', mode='lines+markers+text', textfont=list(size=options$fontsize))
      for (group in unique(sd$df[['Group']])) {
        for (value in options$assessmentValue) {
          ccolor <- ifelse(value == 'A - Adequado', 'green', ifelse(value == 'P - Parcialmente adequado', 'orange', 'red'))
          name <- paste0('Qtd de evidências avaliadas como ', value)
          if (options$measurement == 'pct') {
            name <- paste0('Pct de evidências avaliadas como ', value)
            value <- paste0('Pct.',value)
          }
          
          p_idx <- (sd$df[['Group']] == group)
          r <- sd$df[[value]][p_idx]
          
          theta <- paste0('Critério-',substr(sd$df$name[p_idx],1,25))
          p <- add_trace(
            p, r = r, theta = theta
            #, name = paste0(name,' in ',group)
            #, fillcolor = ccolor
            , line = list(color = ccolor, size = 12)
            , marker = list(color = ccolor, size = 12)
            , name = name
          )
        }
      }
      p <- layout(p, legend=list(orientation = 'h', font = list(size=options$fontsize))
                  , font = list(size=options$fontsize+4), autosize = T
                  , margin = list(l=10, r=10, b=75, t=75, pad=4))
    
    } else if (ctype == 'heatmap') {
      df <- sd$df
      
      cids <- as.vector(sapply(csv.params$criteria$unique, FUN = function(x) return(x)))
      if (length(filters$dimension) > 0) {
        t_ids <- csv.params$criteria$df[[csv.params$criteria$extra.fields$dimension]] %in% filters$dimension
        cids <- csv.params$criteria$df[[csv.params$criteria$id]][t_ids]
      }
      cids_wo <- setdiff(cids, unique(df$id))
      
      df[['Total']] <- rep(0, nrow(df))
      for (value in options$assessmentValue) {
        df[['Total']] <- df[['Total']] + df[[value]]
      }
      
      df <- df[,c('id','Total')]
      for (cid in cids_wo) df <- rbind(df, list(id=cid, Total=0))
      
      xy_max <- floor(sqrt(nrow(df)))+1
      for (i in 1:((xy_max*xy_max)-nrow(df))) df <- rbind(df, list(id=0, Total=0))
      df <- data.table::setorderv(df, 'Total', -1)
      
      zmatrix <- matrix(df$Total, xy_max)
      lmatrix <- matrix(paste0('C-',df$id), xy_max)
      
      lannotations <- list()
      for (x in 1:xy_max) {
        for (y in 1:xy_max) {
          lannotations[[length(lannotations)+1]] <- list(
            y = x-1, x = y-1, text = ifelse(lmatrix[x,y] == 'C-0', '', paste0(lmatrix[x,y], '\n(',zmatrix[x,y],')'))
            , xref = "x" , yref = "y", showarrow = FALSE
            , font =list(size=options$fontsize)
          )
        }
      }
      
      pcolors <- list(
        'A - Adequado' = list(r=0, g=255, b=0)
        , 'P - Parcialmente adequado' = list(r=255, g=165, b=0)
        , 'I - Inadequado' = list(r=255, g=0, b=0)
      )
      colorscale <- list(r=0,g=0,b=0)
      for (avalue in options$assessmentValue) {
        colorscale$r  <- colorscale$r + pcolors[[avalue]]$r
        colorscale$g  <- colorscale$g + pcolors[[avalue]]$g
        colorscale$b  <- colorscale$b + pcolors[[avalue]]$b
      }
      colorscale$r  <- colorscale$r / length(options$assessmentValue)
      colorscale$g  <- colorscale$g / length(options$assessmentValue)
      colorscale$b  <- colorscale$b / length(options$assessmentValue)
      colorscale <- list(c(0, "#ffffff"),list(1, paste0("rgb(",colorscale$r,",",colorscale$g,",",colorscale$b,")")))
      
      p <- plot_ly()
      p <- add_trace(p, type='heatmap', z=zmatrix, showscale=T, colorscale=colorscale)
      ax <- list(title = "", zeroline = F, showline = F, showticklabels = F,gridwidth = 2, showgrid = F)
      p <- layout(p, autosize =T, annotations=lannotations, xaxis = ax, yaxis = ax, font =list(textfont=list(size=options$fontsize)))
      
    }
    
  } else if (dtype == 'result-distribution') {
    
    if (ctype == 'pie') {
      p <- plot_ly()
      groups <- unique(unique(sd$df[['Group']]))
      
      i <- 0
      max_col <- ifelse((length(groups)%%2 == 0), 2, 1) 
      for (group in groups) {
        vals <- as.vector(sapply(options$assessmentValue, FUN = function(value) {
          if (options$formula == 'mean') {
            return(round(weighted.mean(sd$df[[value]][sd$df[['Group']] == group], sd$df[['Total']][sd$df[['Group']] == group]), 2))
          } else {
            return(sum(sd$df[[value]][sd$df[['Group']] == group]))
          }
        }))
        
        pre_g <- ifelse(length(filters[['technologyType']]) == 0 && length(filters[['technology']]) == 0
                        && length(filters[['dimension']] > 0), 'na Dim.', 'em Tec.')
        lgroup <- ifelse(group=='Ciclo 1','em Total',paste(pre_g, gsub("[^:]*:(.*)", "\\1",group)))
        
        p <- add_pie(
          p
          , labels = options$assessmentValue
          , title = list(
            text = paste0("\n\n", sum(vals), ' Avaliações de Evidências\n', lgroup, '\n(', group,')')
            , font = list(size=options$fontsize+4)
          )
          , values = vals, sort = F, hole = 0.8
          , domain = list(row = i %/% max_col, column = i %% max_col)
          , marker = list(colors=c('green', 'orange', 'red')) 
          , text = paste(vals, 'Avaliações como\n', options$assessmentValue)
          , textinfo = "percent+text"
          , textposition = "outside"
          , textfont = list(size=options$fontsize)
        )
        i <- i + 1
      }
      
      p <- layout(
        p, grid=list(rows=length(groups)/max_col, columns=max_col)
        , xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        , yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        , legend=list(orientation = 'h'), autosize = T
        , font = list(size=options$fontsize)
      )
    } else if (ctype == 'bar') {
      
      df <- data.frame(Group=unique(unique(sd$df[['Group']])))
      
      ncols <- c()
      for (value in options$assessmentValue) {
        if (options$measurement == 'pct') value <- paste0('Pct.',value)
        vals <- as.vector(sapply(df[['Group']], FUN = function(x) {
          if (options$formula == 'mean') {
            return(weighted.mean(sd$df[[value]][sd$df[['Group']] == x], sd$df[['Total']][sd$df[['Group']] == x]))
          } else {
            return(sum(sd$df[[value]][sd$df[['Group']] == x]))
          }
        }))
        df[[value]] <- vals
        ncols <- c(ncols, value)
      }
      
      df[['Total']] <- rep(0,nrow(df))
      df[['Label']] <- PTXQC::delLCS(gsub("[^:]*:(.*)", "\\1",df[['Group']]))
      for (ncoln in ncols) {
        df[['Total']] <- df[['Total']] + df[[ncoln]]
      }
      
      if (options[['typeBar']] == 'group') {
        corderby <- c()
        if ('A - Adequado' %in% options$assessmentValue) corderby <- c(corderby, 'A - Adequado')
        if ('I - Inadequado' %in% options$assessmentValue) corderby <- c(corderby, 'I - Inadequado')
        if (options$measurement == 'pct') corderby <- paste0('Pct.',corderby)
        df <- data.table::setorderv(df, corderby, rep(1,length(corderby)))
      } else {
        df <- data.table::setorderv(df, 'Total', -1)
      }
      
      #
      p <- plot_ly(type='bar', textposition='inside'
                   , orientation = ifelse(options[['typeBar']] == 'group', 'h', 'v')
                   , textfont=list(size=options$fontsize+4))
      for (value in options$assessmentValue) {
        ccolor <- ifelse(value == 'A - Adequado', 'green', ifelse(value == 'P - Parcialmente adequado', 'orange', 'red'))
        if (options$measurement == 'pct') value <- paste0('Pct.', value)
        if (options[['typeBar']] == 'group') {
          y <- df[['Label']]; x <- df[[value]];
          if (options[['showAsPct']]) x <- as.vector(x/df$Total)*100
          text <- round(x,2)
        } else {
          x <- df[['Label']]; y <- df[[value]];
          if (options[['showAsPct']]) y <- as.vector(y/df$Total)*100
          text <- round(y, 2)
        }
        p <- add_trace(p, x=x, y=y, text=text, name=ifelse(options[['showAsPct']], paste0('Pct.',value), value), marker=list(color=ccolor))
      }
      
      p <- layout(p, autosize = T
                  , barmode = ifelse(options[['typeBar']] == 'group', 'group', 'stack')
                  , legend=list(orientation = 'h', font=list(size=options$fontsize))
                  , xaxis=list(tickfont=list(size=options$fontsize+4))
                  , font = list(size=options$fontsize))
      
    }
    
  } else if (dtype == 'about-assessments') {
    
    if (ctype == 'pie') {
      
      
      dimensions <- unique(sd$df[[csv.params$dimension$name]]) 
      
      vals <- as.vector(sapply(dimensions, FUN = function(dimension) {
        return(sum(sd$df[[csv.params$dimension$id]] == dimension))
      }))
      
      
      pre_g <- ifelse(length(filters[['technologyType']]) == 1, paste('Tec.',filters[['technologyType']]), 'Total')
      
      
      p <- plot_ly()
      p <- add_pie(
        p
        , labels = dimensions
        , title = list(
          text = paste0("\n\n",sum(vals),' Avaliações\nem ',pre_g,'\n(Ciclo 1)')
          , font = list(size=(options$fontsize+4))
        )
        , values = vals
        , hole = 0.8
        , sort = F
        , marker = list(colors=c('#4472C4','#ED7D31','#A5A5A5')) 
        , text = paste(vals,'Avaliações na\nDim.',dimensions)
        , textinfo = "percent+text"
        , textposition = "outside"
        , textfont = list(size=options$fontsize)
      )
      p <- layout(
        p, grid=list(rows=1, columns=1)
        , xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        , yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        , legend=list(orientation = 'h')
        , showlegend = T
        , autosize = T
        , margin = list(l=10, r=10, b=75, t=75, pad=4)
        , font = list(size=options$fontsize)
      )
    }
    
  } else if (dtype == 'about-evidences') {
    
    if (ctype == 'pie') {
      
      dimensions <- unique(sd$df[[csv.params$dimension$name]]) 
      vals <- as.vector(sapply(dimensions, FUN = function(dimension) {
        return(length(unique(sd$df[[csv.params$evidence$id]][sd$df[[csv.params$dimension$id]] == dimension])))  
      }))
      
      pre_g <- ifelse(length(filters[['technologyType']]) == 1, paste('Tec.',filters[['technologyType']]), 'Total')
      sgroup <- paste0(unique(sd$df['Ciclo']), collapse=',')
      
      p <- plot_ly()
      p <- add_pie(
        p
        , labels = dimensions
        , title = list(
          text = paste0("\n\n",sum(vals),' Evidências\nem ',pre_g,'\n(', sgroup,')')
          , font = list(size=(options$fontsize+4))
        )
        , values = vals
        , hole = 0.8
        , sort = F
        , marker = list(colors=c('#70AD47','#5B9BD5','#FFC000')) 
        , text = paste(vals,'Evidências na\nDim.',dimensions)
        , textinfo = "percent+text"
        , textposition = "outside"
        , textfont = list(size=options$fontsize)
      )
      p <- layout(
        p, grid=list(rows=1, columns=1)
        , xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        , yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        , legend=list(orientation = 'h')
        , showlegend = T
        , autosize = T
        , margin = list(l=10, r=10, b=75, t=75, pad=4)
        , font = list(size=options$fontsize)
      )
    }
  
  }
  return(p)
}
