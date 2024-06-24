#define function to query database tables
fetch_db_table <- function(con = NULL, schema, table) {
  
  if(!DBI::dbIsValid(con)) {
    stop("No database connection provided.")
  }
  
  table <- dplyr::tbl(con, 
                      dbplyr::in_schema(dbplyr::sql(schema), 
                                        dbplyr::sql(table))) |> 
    dplyr::collect()
  
  return(table)
}
