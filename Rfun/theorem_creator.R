build_theorem <- function(section = "Teorema", 
                          content = "Let $\\int\\limits_{0}^{2} f(x) dx = 2$ then it equals $2$",
                          proof = sample(c("Trivial.","Se deja al lector.", "Ejercicio.","Esto es una simple consecuencia de teoría de conjuntos."), 1),
                          name = "", id = paste0(section,runif(1),name)){
  
  
  #Counters for excercises, theorems, definitions, propositions, etcetera
  if(!exists("teonum")){  teonum <<- 0 } 
  if(!exists("exnum")){   exnum  <<- 0 } 
  if(!exists("propnum")){ propnum<<- 0 } 
  if(!exists("cornum")){  cornum <<- 0 } 
  if(!exists("defnum")){  defnum <<- 0 } 
  if(!exists("ejnum")){   ejnum  <<- 0 } 
  if(!exists("cejnum")){  cejnum <<- 0 } 
  
  
  
  #Create alerts for remarks and warnings
  if (section %in% c("Nota","Advertencia")){
    switch(section,
           Nota        = {alert_class <- "alert-info"},
           Advertencia = {alert_class <- "alert-warning"},
           {alert_class <- "alert-danger"}
    )
    
    cat(paste('<div id = "', id, '" class="alert ',alert_class,'"><b>',section,': </b>', content,'</div>'))
    
  } else {
    
    #Switch panel class depending on section
    switch(section,
           Teorema        = {panel_class <- "panel-primary"; teonum  <<- teonum + 1;  theonum <- teonum},
           Propiedades    = {panel_class <- "panel-primary"; propnum <<- propnum + 1; theonum <- propnum},
           Corolario      = {panel_class <- "panel-primary"; cornum  <<- cornum + 1;  theonum <- cornum},
           Definición     = {panel_class <- "panel-info";    defnum  <<- defnum + 1;  theonum <- defnum},
           Ejemplo        = {panel_class <- "panel-success"; ejnum   <<- ejnum + 1;   theonum <- ejnum},
           Ejercicio      = {panel_class <- "panel-success"; exnum   <<- exnum + 1;   theonum <- exnum},
           Contraejemplo  = {panel_class <- "panel-warning"; cejnum  <<- cejnum + 1;  theonum <- cejnum},
           {
             panel_class <- "panel-default"; theonum <- "";
           }
    )
    
    
    
    
    #Create title of theorem
    if (name != ""){
      panel_title <- paste('\t<div id = ', id ,'class="panel-heading">\n\t\t<h4>', section, theonum, '[', name, ']', '</h4>\n\t</div>')  
    } else {
      panel_title <- paste('\t<div id = ', id ,' class="panel-heading">\n\t\t<h4>', section, theonum, '</h4>\n\t</div>')
    }
    
    #Create panel
    panel_open  <- paste0('\n<div class="panel ',panel_class,'">\n')
    panel_body  <- paste0('\n\t<div class="panel-body">\n\t\t<i>', content,'</i>')
    
    #Add proof subpanel
    if (gsub("[[:space:]]", "", proof) != ""){
      if (section == "Ejercicio"){
        proofname <- "<b>Solución:</b>"
      } else {
        proofname <- "<b>Demostración:</b>"
      }
      panel_proof <- paste('\n\t\t<div class="well" style = "margin-top: 10px;">\n\t\t\t',proofname, proof,"\n\t\t</div>\n")
    } else {
      panel_proof <- "" 
    }
    
    panel_close <- '\t</div>\n</div>\n'
    
    cat("<!--html_preserve-->")
    cat(paste0(panel_open,panel_title,panel_body,panel_proof,panel_close))
    cat("<!--/html_preserve-->")
  }
}