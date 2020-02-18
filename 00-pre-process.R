library(readr)

# concat columns from a df
concat.columns <- function(df, columns, sep='') {
  to_return <- df[[columns[1]]]
  if (length(columns) > 1) {
    to_return <- paste0(
      to_return
      , do.call(
        paste0
        , lapply(columns[2:length(columns)], FUN = function(x) {
          v <- df[[x]]; v[is.na(v)] <-''; return(paste0(sep,v))
        })))
  }
  return(to_return)
}

## remove unecessary information and save using UTF-8 encoding 

dfe1 <- read_csv('data/evidencias-pre.csv', locale=locale(encoding = "latin1"))
dfe2 <- read_csv('data/evidencias-pre2.csv', locale=locale(encoding = "utf-8"))
dfe2 <- dfe2[,c(colnames(dfe1))]
dfe <- dfe1[!dfe1$`Cód. Evidência` %in% dfe2$`Cód. Evidência`,]
dfe <- rbind(dfe, dfe2)
write_csv(dfe, 'data/evidencias.csv')

dfc <- read_csv('data/criterios-pre.csv', locale=locale(encoding = "latin1"))
write_csv(dfc, 'data/criterios.csv')

dfa <- read_csv('data/assessment-log-pre.csv', locale=locale(encoding = "latin1"))
scols <- c('Cód. Critério'
           ,'Cód. Evidência'
           , 'Cód. Avaliador'
           , 'Avaliação consolidadora?'
           , 'Avaliação da Evidência')
dfa <- dfa[,scols]
dfa <- merge(dfa, dfe, by='Cód. Evidência')
dfa <- merge(dfa, dfc, by='Cód. Critério')
write_csv(dfa, 'data/assessment-log.csv')
write_csv(dfa, 'data/assessment.csv')

##
dfga <- read_csv('data/geo-appraisers-pre.csv', locale=locale(encoding = "latin1"))
write_csv(dfga, 'data/geo-appraisers.csv')
