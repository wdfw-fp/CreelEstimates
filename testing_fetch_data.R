#comparing data brought into R by the two methods. 1) data.wa.gov as default, 2) direct Postgres connection
#number of columns may vary between public facing views and database tables
#however, the number of rows should the the same.

#exact match
sno_dwg <- fetch_data("Snohomish fall salmon 2023")
sno_postgres <- fetch_data("Snohomish fall salmon 2023", data_source = "internal")

#missing some rows of closure? ~20
cascade_dwg <- fetch_data("Cascade fall salmon 2022")
cascade_postgres <- fetch_data("Cascade fall salmon 2022", data_source = "direct")

#missing 1 row of closure?
puy_dwg <- fetch_data("Puyallup_Carbon salmon 2023")
puy_postgres <- fetch_data("Puyallup_Carbon salmon 2023", data_source = "direct")

#number of rows differ for effort, interview, and catch??
hoh_dwg <- fetch_data("Hoh winter steelhead 2023-24")
hoh_postgres <- fetch_data("Hoh winter steelhead 2023-24", data_source = "internal")

#exact match
willapa_dwg <- fetch_data("Willapa winter steelhead 2022")
willapa_postgres <- fetch_data("Willapa winter steelhead 2022", data_source = "internal")

#exact match
skagit_dwg <- fetch_data("Skagit spring Chinook 2022 upper")
skagit_postgres <- fetch_data("Skagit spring Chinook 2022 upper", data_source = "direct")

#exact match
quil_dwg <- fetch_data("Quillayute fall salmon 2023")
quil_postgres <- fetch_data("Quillayute fall salmon 2023", data_source = "direct")

#exact match
nooksack_dwg <- fetch_data("Nooksack winter gamefish 2022-23")
nooksack_postgres <- fetch_data("Nooksack winter gamefish 2022-23", data_source = "direct")
