
create_template(stock_name = "bluefish", 
                output_dir = here::here("docs"),
                send_to_google_doc = TRUE,
                overwrite = TRUE)


create_template(stock_name = "scup", 
                output_dir = here::here("docs"),
                send_to_google_doc = TRUE,
                overwrite = TRUE)

# create_template(stock_name = "black-sea-bass_north", 
#                 output_dir = here::here("docs"),
#                 send_to_google_doc = TRUE,
#                 overwrite = TRUE)
# 
# create_template(stock_name = "black-sea-bass_south", 
#                 output_dir = here::here("docs"),
#                 send_to_google_doc = TRUE,
#                 overwrite = TRUE)

create_template(stock_name = "black-sea-bass", 
                output_dir = here::here("docs"),
                send_to_google_doc = TRUE,
                overwrite = TRUE)

create_template(stock_name = "atlantic-herring", 
                output_dir = here::here("docs"),
                send_to_google_doc = TRUE,
                overwrite = TRUE)

merge_to_bookdown(stock_name = "atlantic-herring",
                  output_dir = here::here("docs"), 
                  render_book = TRUE, 
                  methods_gdoc_path = "generic_methods",
                  overwrite = TRUE) 

create_template(stock_name = "butterfish", 
                output_dir = here::here("docs"),
                send_to_google_doc = TRUE,
                overwrite = TRUE)

merge_to_bookdown(stock_name = "butterfish",
                  output_dir = here::here("docs"), 
                  render_book = TRUE, 
                  methods_gdoc_path = "generic_methods",
                  overwrite = TRUE)

create_template(stock_name = "striped-bass", 
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

merge_to_bookdown(stock_name = "striped-bass",
                  output_dir = here::here("docs"), 
                  render_book = TRUE, 
                  methods_gdoc_path = "generic_methods",
                  overwrite = TRUE)


create_template(stock_name = "longfin-squid", 
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

merge_to_bookdown(stock_name = "longfin-squid",
                  output_dir = here::here("docs"), 
                  render_book = TRUE, 
                  methods_gdoc_path = "generic_methods",
                  overwrite = TRUE)

## sept 2020 assessments
create_template(stock_name = "acadian-redfish", 
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)


create_template(stock_name = "american-lobster_sne",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)


