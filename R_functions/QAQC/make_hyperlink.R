#function to convert generated URL to hyperlink
make_hyperlink <- function(myurl, mytext = myurl) {
  paste('<a href="', myurl, '">', mytext, '</a>')
}
#example use
#df %>% gt() %>% fmt (columns = 'fishapps_event_link', fns = make_hyperlink)