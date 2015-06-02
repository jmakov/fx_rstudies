 LaFTest<-function(){
   require(LaF)
    File<-laf_open_csv(filename="test.csv",   column_types=c("integer", "categorical",  "string", "integer", "double"), column_names=c("id", "gender", "postcode", "age", "income"))
    return(FIle)
 }
 