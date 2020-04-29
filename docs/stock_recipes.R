
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

create_template(stock_name = "american-lobster_gom-gb",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "atlantic-halibut",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "atlantic-wolffish",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "ocean-pout",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "offshore-hake-and-silver-hake_sgb-ma",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "sea-scallop",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "red-hake_sgb-ma",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "red-hake_gom-ngb",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "silver-hake_gom-ngb",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "windowpane_gom-gb",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "windowpane_sne-ma",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "winter-flounder_gom",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "winter-flounder_gb",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)

create_template(stock_name = "winter-flounder_sne-ma",
                output_dir = here::here("docs"),
                send_to_google_doc = FALSE,
                overwrite = TRUE)