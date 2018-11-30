check <- 
   function(
      ativo,
      valor,
      operacao,
      ...
   ) {
      status <- 
         tryCatch({
            dat <- getQuote(ativo)
            if (
               (operacao %in% c('compra', 'stop') & dat$Last <= valor) |
               (operacao == 'venda' & dat$Last >= valor)
            ) {
               status <- list(data_hora = dat$`Trade Time`, status = 'executada')
            } else {
               status <- list(data_hora = dat$`Trade Time`, status = 'aberta')
            }
         }, 
         error = function(x) return(NA)
         )
      return(status)
   }

check_planilha <- 
   function(planilha, aba) {
      gap <- gs_title(planilha)
      
      dat <- 
         gap %>%
         gs_read(
            ws = aba,
            col_types = 'iccicncnn'
         ) %>% 
         mutate(data_hora = as_datetime(data_hora))
      
      # atu <- row.names(dat)[dat$status == 'aberta'][1]
      atu <- 1
      
      consulta <- 
         dat %>% 
         filter(status == 'aberta') %>% 
         pmap_dfr(check)
      
      if (nrow(consulta) > 0) {
         dat[dat$status == 'aberta', 'data_hora'] <- consulta$data_hora
         dat[dat$status == 'aberta', 'status'] <- consulta$status
      }

      cancela_stop <- 
         dat %>% 
         split(dat$id) %>% 
         map(
            ~ filter(.x, operacao == 'stop') %>% pull(status) == 'aberta'  &
               filter(.x, operacao == 'venda') %>% pull(status) != 'aberta'
         ) %>% unlist()
      
      dat[dat$operacao == 'stop', 'status'][cancela_stop] <- 'cancelada'
      
      dat[dat$operacao == 'compra' & dat$status == 'executada', 'movimentacao'] <- 
         -1 * dat$qtd[dat$operacao == 'compra' & dat$status == 'executada'] * dat$valor[dat$operacao == 'compra' & dat$status == 'executada']
      dat[dat$operacao %in% c('venda', 'stop') & dat$status == 'executada', 'movimentacao'] <- 
         dat$qtd[dat$operacao %in% c('venda', 'stop') & dat$status == 'executada'] * dat$valor[dat$operacao %in% c('venda', 'stop') & dat$status == 'executada']
      
      dat[dat$operacao == 'compra', 'balanco_op'] <- 
         dat %>% 
         group_by(id) %>% 
         summarise(balanco_op = sum(movimentacao, na.rm = T)) %>% 
         pull(balanco_op)
      
      if (!is.na(atu)) {
         atu <- as.numeric(atu)

         gs_edit_cells(
            gap,
            ws = aba,
            input = dat$data_hora[atu:nrow(dat)],
            anchor = paste0('B', atu + 1)
         )

         gs_edit_cells(
            gap,
            ws = aba,
            input = dat$status[atu:nrow(dat)],
            anchor = paste0('G', atu + 1)
         )

         gs_edit_cells(
            gap,
            ws = aba,
            input = dat$movimentacao[atu:nrow(dat)],
            anchor = paste0('H', atu + 1)
         )

         gs_edit_cells(
            gap,
            ws = aba,
            input = dat$balanco_op[atu:nrow(dat)],
            anchor = paste0('I', atu + 1)
         )
      }
      
      return(dat)
   }

packages.list <- c('tidyverse', 'quantmod', 'lubridate', 'googlesheets')
new.packages <- packages.list[!(packages.list %in% installed.packages()[,'Package'])]
if(length(new.packages)) {
   install.packages(new.packages, repos = 'https://cran.fiocruz.br/')
}
lapply(packages.list, require, character.only = TRUE)
gs_auth(token = 'googlesheets_token.rds')
dat <- check_planilha('investsim', 'investsim')