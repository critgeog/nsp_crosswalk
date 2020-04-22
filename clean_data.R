library(tidycensus)
library(tidyverse)
# library(purrr)
library(readxl)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = 'sf')
th_api_acs <- '45544f0d114cfaa037a5566745d18bb8d4778cfa'

# tomorrow
# figure out 383 tracts that don't match. What's up?
# clean script up

# done


# useful link for additional approaches: https://serialmentor.com/blog/2016/6/13/reading-and-combining-many-tidy-data-files-in-R

# data: https://www.huduser.gov/portal/datasets/NSP.html

data_path <- "NSP3"
files <- dir(data_path, pattern = "*.xls")

files

data <- files %>%
  # read in all the files, appending the path before the filename using read_xls()
  map(~ read_xls(file.path(data_path, .))) %>% 
  reduce(rbind) # reduce with rbind into one dataframe

data %>%
  mutate(fipsone = substr(SUM090, 1,5),
         fipstwo = substr(SUM090, 16,21)) %>%
  # select(SUM090, fipsone, fipstwo) %>% View()
  unite(geoid, fipsone, fipstwo, sep = "") %>%
  # select(SUM090,geoid) %>%
  group_by(geoid) %>%
  summarize(mort0407 = sum(HMDA),       # Home Mortgage Disclosure Act count of primary mortgages executed between 2004 and 2007. Note that this mortgage count is based on Census Tract level data that is assigned to Block Group parts based on the HU2007 variable above.
            starts0910 = sum(STARTS),   # Estimated number of foreclosure starts in the target area in the past year. Each geographic area was allocated its estimated share of foreclosure starts in the state (from Mortgage Bankers Association National Delinquency Survey State Counts of Foreclosure Starts July 2009 to June 2010) based on its estimated share of serious delinquent borrowers (calculated for each geographic area as HMDA * SDQ_RATE).
            REO0910 = sum(REO),         # Estimated number of completed foreclosures in the target area in the past year. Each geographic area was allocated its estimated share of completed foreclosures in the state (from RealtyTrac Count of REO completions July 2009 to June 2010) based on its estimated share of serious delinquent borrowers (calculated for each geographic area as HMDA * SDQ_RATE).
            hc_rt0910 = median(HC_RATE),
            hc_loans = round(sum(hc_rt0910/100*mort0407),0)) -> data2   # Home Mortgage Disclosure Act data showing the percent of primary mortgages executed between 2004 and 2007 that were high cost. This is the Census Tract level rate.

# write_csv(data, "fiftystates_alldata_NSP3.csv")
# write_csv(data2, "fiftystates_NSP3.csv")



# tidycensus

us <- unique(fips_codes$state)[1:51]

# 20013-17 census tracts for all 50 states w/ geometry
totalpop_sf <- reduce(
  map(us, function(x) {
    get_acs(geography = "tract", variables = "B01003_001",
            state = x, key = th_api_acs, geometry = TRUE)
  }),
  rbind
)




# using this one 
# misses 384 tracts
work <- left_join(totalpop_sf,d3, by = c("GEOID" = "trtid10"))
# anti_join(totalpop_sf, d3, by = c("GEOID" = "trtid10")) -> a1



###*************
# read in msas, 2018 1 year ACS
metropop <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                    variables = "B01003_001", year = 2018, survey = "acs1", cb = TRUE,
                    key = th_api_acs)

top50metros <- metropop %>%
  top_n(51, estimate) %>% # San Juan is #35. delete in next line.
  filter(!(GEOID %in% c("41980")))

metros <- core_based_statistical_areas(cb = TRUE) %>%
  filter(GEOID %in% top50metros$GEOID) %>%
  select(metro_name = NAME)

top50metros_tracts <- st_join(work, metros, join = st_within, 
                   left = FALSE) 

# map tracts in top 50 metros
library(tmap)
tmap_mode("view")

tm_shape(top50metros_tracts) +
  tm_polygons("mort", lwd=0, style = 'jenks', alpha = .5 )


# create metro level variables
top50metros_tracts %>%
  group_by(metro_name) %>%
  summarise(hc_metro = sum(hc, na.rm = TRUE),
            mort_metro = sum(mort, na.rm = TRUE),
            hc_metro_rate = hc_metro/mort_metro*100) %>%
  arrange(desc(hc_metro_rate)) -> metro_analyze

# map metropolitan areas
tm_shape(metro_analyze) +
  tm_bubbles("hc_metro", scale = 3)


# str(totalpop_sf)

# # 2005-2009 census tracts for all 50 states w/out geometry
# totalpop <- map_df(us, function(x) {
#   get_acs(geography = "tract", year = 2009, variables = "B01003_001", 
#           state = x, key = th_api_acs)
# })
# 
# str(totalpop)

# save csv of US tracts in all 50 states
# write_csv(totalpop, "totalpop2009acs_tracts2000_us.csv")

# totalpop13_sf <- reduce(
#   map(us, function(x) {
#     get_acs(geography = "tract", year = 2013, variables = "B01003_001",
#             state = x, key = th_api_acs, geometry = FALSE)
#   }),
#   rbind
# )

# ?load_variables
# load_variables(2010, "sf3") %>% View()

# totalhu10_sf <- reduce(
#   map(us, function(x) {
#     get_decennial(geography = "tract", year = 2010, sumfile = 'sf1', variables = "H001001",
#             state = x, key = th_api_acs, geometry = TRUE)
#   }),
#   rbind
# )

# work13 <- left_join(totalpop13_sf,d3, by = c("GEOID" = "trtid10"))
# anti_join(totalpop13_sf, d3, by = c("GEOID" = "trtid10")) -> a13

# work_walk <- left_join(totalpop13_sf,crosswalk, by = c("GEOID" = "trtid10"))
# anti_join(totalpop13_sf, crosswalk, by = c("GEOID" = "trtid10")) -> a_walk

# work10_walk <- left_join(totalhu10_sf,crosswalk, by = c("GEOID" = "trtid10"))
# anti_join(totalhu10_sf, crosswalk, by = c("GEOID" = "trtid10")) -> a10_walk

# 21 tracts different between 2010 and 2017 acs
# anti_join(totalpop13, totalhu10_sf, by = "GEOID") -> boundary1017