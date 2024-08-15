#define function to query database tables
fetch_db_table <- function(con = NULL, schema, table, filter = NULL) {
  
  if(!DBI::dbIsValid(con)) {
    stop("No database connection provided.")
  }
  
  #built query
  query <- dplyr::tbl(con, dbplyr::in_schema(schema, table))
  
  #apply filters to query
  if (!is.null(filter)) {
    combined_filter <- paste(filter, collapse = " & ")
    query <- query |> dplyr::filter(!!rlang::parse_expr(combined_filter))
  }
  
  #execute query
  result <- query |> dplyr::collect()
  
  return(result)
}

# examples
# a <- fetch_db_table(con, "creel", "fishery_location_lut")
# b <- fetch_db_table(con, "creel", "fishery_location_lut", filter = "survey_type == 'Index'")
# c <- fetch_db_table(con, "creel", "fishery_location_lut", filter = c("survey_type == 'Index'", "section_num == '1'"))

# old version of function

# #define function to query database tables
# fetch_db_table <- function(con = NULL, schema, table) {
#   
#   if(!DBI::dbIsValid(con)) {
#     stop("No database connection provided.")
#   }
#   
#   table <- dplyr::tbl(con, 
#                       dbplyr::in_schema(dbplyr::sql(schema), 
#                                         dbplyr::sql(table))) |> 
#     dplyr::collect()
#   
#   return(table)
# }


