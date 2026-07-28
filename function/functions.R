cleanDate <- function(date_vector){
  #date_vector - column (vector) for date
  new_date <- as.Date(date_vector, format="%d/%m/%Y")
  for (i in 1:length(new_date)){
   if (is.na(new_date[i])){
      new_date[i]<-as.Date(as.numeric(date_vector[i]),origin="1899-12-30")
   }else{
      new_date[i] <- new_date[i]
   }
 }
return(new_date)
}