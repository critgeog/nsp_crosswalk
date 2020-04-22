library(tidyverse)

# read_csv("fiftystates_alldata_NSP3.csv")
fiftystates_NSP3 <- read_csv("fiftystates_NSP3.csv")

# crosswalk 
# source: https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx
# additional info: https://s4.ad.brown.edu/Projects/Diversity/Researcher/Bridging.htm
# https://s4.ad.brown.edu/Projects/Diversity/Researcher/BoundaryAdjustments.htm
# https://s4.ad.brown.edu/projects/diversity/Researcher/ltdb3.htm
crosswalk <- read_csv("crosswalk_2000_2010.csv")


walkdata2 <- left_join(crosswalk, fiftystates_NSP3, by = c("trtid00" = "geoid"))
# write_csv(walkdata2, "walkdata2c.csv", na = "0")

fiftystates_NSP3 %>%
  summarize(
    total = sum(mort0407, na.rm = TRUE))
# 40,413,107 is total number of mortgage loans in dataset

# walkdata2 %>%
#   mutate(mort = mort0407*weight,
#          starts = starts0910*weight,
#          reo = REO0910*weight,
#          hc = hc_loans*weight) ->d2a

# d2a %>%
#   summarize(
#     hmda_total = sum(mort, na.rm = TRUE),
#     starts_total = sum(starts, na.rm = TRUE),
#     reo_total = sum(reo, na.rm = TRUE),
#     hc_total = sum(hc, na.rm = TRUE))


walkdata2 %>%
  group_by(trtid10) %>%
  summarize(mort = sum(mort0407*weight),
         starts = sum(starts0910*weight),
         reo = sum(REO0910*weight),
         hc = sum(hc_loans*weight),
         pstarts = starts/mort*100,
         preo = reo/mort*100,
         p_hc = hc/mort*100) ->d3

d3 %>%
  summarize(
    hmda_total = sum(mort, na.rm = TRUE),
    starts_total = sum(starts, na.rm = TRUE),
    reo_total = sum(reo, na.rm = TRUE),
    hc_total = sum(hc, na.rm = TRUE))