#install.packages(c('pivottabler','plotly'))
library(readr)
library(plotly)
library(pivottabler)

fcriterios <- read_csv("data/criterios.csv")
fevidencias <- read_csv("data/evidencias.csv")

fassessment <- read_csv('data/assessment.csv')

filter <- list(
  ciclo=c(1)
  , dimensao=c("Educacional")
  , tipo=c("Digital")
)

idx <- (df$`Cód. Ciclo` %in% filter$ciclo)
idx <- idx & (df$Dimensão %in% filter$dimensao)
idx <- idx & (df$`Tipo de Tecnologia` %in% filter$tipo)

fdata <- df[idx,]


vdata <- PivotTable$new()
vdata$addData(fdata)

vdata$addColumnDataGroups("Avaliação da Evidência")
vdata$addRowDataGroups("Cód. Critério")
vdata$defineCalculation(calculationName = "Total", summariseExpression="n()")
vdata$evaluatePivot()

assessment_types <- unique(df$`Avaliação da Evidência`)

pdata <- vdata$asDataFrame()
pdata[['id']] <- rownames(pdata)
for (at in assessment_types) {
  pdata[[paste0('Pct. ',at)]] <- round(pdata[[at]]/pdata$Total,3)
}

rdata <- pdata[(1:nrow(pdata)-1),]
rdata <- merge(
  rdata
  , fcriterios[,c('Cód. Critério','Cód. Item Edital','Critério')]
  , by.x='id', by.y='Cód. Critério')

rdata <- rdata[order(rdata$`Pct. A - Adequado`, decreasing=T, na.last=T),]
rdata <- rdata [(1:20),]

p <- plot_ly(type = 'scatterpolar', fill = 'toself')
for (at in assessment_types) {
  p <- add_trace(
    p
    , r = rdata[[ass_ev]]
    , theta = paste0(rdata$`Cód. Item Edital`,'.',rdata$Critério)
    , name = ass_ev
  )
}
p <- layout(p, legend=list(orientation = 'h'), title="")
p



p <- plot_ly()

p1 <- plot_ly(type = 'pie'
             , textinfo = 'label+value+percent'
             , marker = list(line = list(color = '#FFFFFF', width = 1))
             , sort=F)
p1 <- add_trace(
  p1 
  , labels = c('A','B','C','D')
  , values = c(2,3,5,10)
  , hole = 0.8
  , textinfo = 'text'
  , textposition='inside'
  , text = c('Aaaaaaaaaa','Bbbbbbbbb','Ccccccccccccc','Ddddddddddd')
)

p2 <- plot_ly(type = 'pie'
              , textinfo = 'label+value+percent'
              , marker = list(line = list(color = '#FFFFFF', width = 1))
              , sort=F)
p2 <- add_trace(
  p2 
  , labels = c('A','B','C','D')
  , values = c(10,3,5,2)
  , hole = 0.9
  , textposition='outside'
  , textinfo = 'label+value+percent'
  , title = 'dddddff'
)

do.call(subplot, lapply(c(p1,p2), FUN = function(x) { x }))

p <- subplot()
p

#(ptdf)
