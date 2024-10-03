rm(list=ls())
# Pakete ##########

.packages <- c("sjPlot","corrplot","effects","bruceR","psych","tableone","lavaanPlot","flextable","xml2","rvest")
new.packages <- .packages[!(.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=TRUE)
lapply(.packages, require, character.only=TRUE)



# Daten einlesen ##########

daten <- read.csv2('Daten.csv', na.strings="")


#Daten überprüfen####
vars<-c("Variable1","Variable2","Variable3","Variable4","Variable5","Variable6","Variable7","Variable8","Variable9","Variable10","Variable11")
for (i in vars){
  print(i)
  print(summary(daten[[i]]))
}

#Berechnung 1-1-1 Multilevel Mediationsmodell#####
med <- c("Variable1","Variable2","Variable3","Variable4","Variable5")
cont <- c("Variable6","Variable6","Variable7")
x<-c("Variable8","Variable9","Variable10")
count<-0
med_model<-list()
for(i in x){
  for(j in med){
    if(i==j) next
    count<-count+1
    med_model[[count]]<-bruceR::PROCESS(
      daten,
      y = "jobdis",
      x = i,
      meds = j,
      covs = cont,
      clusters = c("Person"),
      hlm.re.m = "(1+Woche|Person)",
      hlm.re.y = "(1+Woche|Person)",
      hlm.type = c("1-1-1"),
      cov.path = c("both"),
      center = FALSE,
      nsim=1000,
      file=paste0("med_model_output_", count, ".html")
    )
  }
}

#Übersichtsdatei erstellen mit Parametern des jeweiligen Modells (was ist x und med?)#####
count<-0
models<-list()
for(i in x){
  for(j in med){
    if(i==j) next
    count<-count+1
    models[[count]]<-paste0("Modell ", count,"x=",i," m=",j,sep="")
  }
}
write.csv2(models,"Modelle.csv")

#html Dateien anpassen, um Koeffizienten und Sternchen einzufügen#####
for (k in seq_along(med_model)) {
  model_result <- as.data.frame(med_model[[k]]$results)
  
  text <- list()
  
  for (i in 1:nrow(model_result)) {
    coeff <- round(model_result[i, 1], 3)
    se <- round(model_result[i, 2], 3)
    p <- round(model_result[i, 7], 3)
    
    
    p[p < .05] <- "<sup>&#42;</sup></td>"
    p[p < .01] <- "<sup>&#42;&#42;</sup></td>"
    p[p < .001] <- "<sup>&#42;&#42;&#42;</sup></td>"
    p[p >= .05] <- "</td>"
    
    
    if (i == 1) {
      name <- "Indirect (ab)"
    } else if (i == 2) {
      name <- "Direct(c')"
    } else if (i == 3) {
      name <- "Total(c)"
    }
    
    
    name <- paste0("<td>", name, sep = "")
    coeff <- paste0("<td>", coeff,p, "</td>", sep = "")
    se <- paste0("<td>(", se, ")</td>", sep = "")
    
    
    text[[i]] <- t(t(c(
      "<tr>", name, coeff, "<td></td>", "<td></td>", "</tr>",
      "<tr>", "<td></td>", se, "<td></td>", "<td></td>", "</tr>"
    )))
  }
  
  
  ergebnis <- rbind(text[[1]], text[[2]], text[[3]])
  
  
  a <- paste(readLines(paste0("med_model_output_", k, ".html")))
  row <- (grep("Marginal", a)) - 1
  a <- append(a, ergebnis, row)
  writeLines(a, paste0("med_model_output_with_med", k, ".html"))
}

# Tabelle als .csv abspeichern mit Nummerierung, um sie der Datei mit Modellnamen ("Modelle.csv") zuordnen zu können####
for(i in 1:length(med_model)){
  table <- xml2::read_html(paste0("med_model_output_with_med", i, ".html"))
  
  table <-html_table(table,fill=TRUE)
  table<-table[[1]]
  write.csv2(table,paste0("med_model_", i, ".csv"))
}
