#---------------------------------------------------------------------------------------------#
# Replication script for 'Political Underrepresentation Among Public Benefits Recipients: Evidence from Linked Administrative Data'
#---------------------------------------------------------------------------------------------#

#---------------------------
# LOAD PACKAGES

library(cowplot)
library(kableExtra)
library(purrr)
library(scales)
library(tidyverse)

#---------------------------
# SET WORKING DIRECTORY

setwd("C:/Users/K011014/Downloads/") # Put your filepath here

#---------------------------
# LOAD ALL DATA USED IN THIS SCRIPT

files <- list.files(path = "./", 
                    pattern = ".rda")

walk(files, ~ load(.x, .GlobalEnv))

#---------------------------------------------------------------------------------------------
# FIGURE 1

# Remove the individuals who were not old enough to vote in the 2020 election, and individuals who were deceased as of the date of the 2020 election (11/3/2020). They should not be included in the denominator of the calculation. 

dt <- dhs_voter_data %>%
  filter(age_2020ge >= 18 & deceased_2020ge == 0) %>%
  
# Keep only the individuals who received TANF, SSI, Medicaid, public housing, SNAP, Section 8, or CCDF benefits in 2020 prior to the election
  
  filter(tanf_2020pe + ssi_2020pe + medicaid_2020pe + public_hous_2020pe + snap_2020pe + section8_2020pe + ccdf_2020pe > 0) %>%
  
# Create a 'Total benefit recipients' grouping
  
  mutate(all_benefits_recipients = 1) %>%
  
# Create dummy vars for 1) Not registered to vote as of 2020 GE, 2) Registered but didn't vote in 2020 GE, and 3) Registered and did vote in 2020 GE
  
  mutate(not_registered_2020ge = ifelse(registered_2020ge > 0, 0, 1), 
         registered_didnt_vote_2020ge = ifelse(registered_2020ge > 0 & voted_2020ge < 1, 1, 0), 
         registered_voted_2020ge = ifelse(registered_2020ge > 0 & voted_2020ge > 0, 1, 0)) %>%
  
# Calculate the 2020 GE turnout rates by 2020 program activity
    
  select(pseudo_id, not_registered_2020ge, registered_didnt_vote_2020ge, registered_voted_2020ge, public_hous_2020pe, section8_2020pe, snap_2020pe, ssi_2020pe, tanf_2020pe, medicaid_2020pe, ccdf_2020pe, all_benefits_recipients) %>%
  gather(program, value, -pseudo_id, -not_registered_2020ge, -registered_didnt_vote_2020ge, -registered_voted_2020ge) %>%
  filter(value > 0) %>%
  group_by(program) %>%
  summarise(n = n(),
            pct_not_registered = mean(not_registered_2020ge), 
            pct_registered_didnt_vote = mean(registered_didnt_vote_2020ge),
            pct_registered_voted = mean(registered_voted_2020ge)) %>%
  ungroup()  %>%
  arrange(n) %>%
  mutate(program_name = ifelse(program == 'ccdf_2020pe', 'CCDF',
                        ifelse(program == 'tanf_2020pe', 'TANF', 
                        ifelse(program == 'ssi_2020pe', 'SSI', 
                        ifelse(program == 'medicaid_2020pe', 'Medicaid', 
                        ifelse(program == 'section8_2020pe', 'Section 8', 
                        ifelse(program == 'public_hous_2020pe', 'Public housing',
                        ifelse(program == 'snap_2020pe', 'SNAP',
                        'All benefits')))))))) %>%
  select(program_name, n, pct_not_registered, pct_registered_didnt_vote, pct_registered_voted) %>%
  
# Create the label y positions
  
  gather(variable, value, -program_name, -n) %>%
  mutate(variable_order = ifelse(variable == 'pct_not_registered', 1, 
                                 ifelse(variable == 'pct_registered_didnt_vote', 2, 
                                        3))) %>%
  group_by(program_name) %>%
  arrange(program_name, desc(variable_order)) %>%
  mutate(cumul_pct = cumsum(value),
         label_position = cumul_pct - 0.5 * value) %>%
  ungroup() %>% 
  mutate(variable = ifelse(variable == 'pct_not_registered', 'Not registered', 
                           ifelse(variable == 'pct_registered_didnt_vote', 'Registered but did not vote', 
                                  'Registered and voted')),
         n = paste('(N = ', scales::number(n, big.mark = ","), ')', sep = ""),
         x_axis_label = paste(program_name, n, sep = "_")) %>%
  mutate(variable = factor(variable, levels = c('Not registered', 'Registered but did not vote', 'Registered and voted')))

# Order the programs in the graph by their turnout rate from smallest to largest, with the 'total' bar at the end

dt1 <- dt %>%
  filter(variable == 'Registered and voted' & program_name != 'All benefits') %>%
  distinct(program_name, variable, value) %>%
  mutate(program_order = rank(value)) %>%
  select(program_name, program_order)

dt2 <- dt %>%
  select(program_name) %>%
  filter(program_name == 'All benefits') %>%
  distinct(program_name) %>%
  mutate(program_order = 9)

bind_rows(dt1, dt2) %>%
  inner_join(dt, by = c('program_name')) %>%
  
# Create the graph
  
  ggplot(aes(x = reorder(x_axis_label, program_order), y = value, fill = variable)) + 
  geom_bar(position = "stack", stat = "identity", width = 0.8) +
  geom_text(aes(label = scales::percent(value, accuracy = 1), y = label_position), size = 2) +
  scale_y_continuous(breaks = seq(0,1,.1),
                     limits = c(0,1), 
                     labels = scales::percent_format(accuracy = 1)) +  
  xlab(" ") + ylab(" ") +
  scale_x_discrete(labels = function(x){gsub("_", "\n", x)}) +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        axis.text.x = element_text(size = 7),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        plot.caption = element_text(hjust = 0)) +
  scale_fill_manual(values=c("#CCCCCC", "#999999", "#666666"))

#---------------------------------------------------------------------------------------------
# TABLE 1

# Remove the individuals who were not old enough to vote in the 2020 election, and individuals who were deceased as of the date of the 2020 election (11/3/2020). They should not be included in the denominator of the calculation. 

dt <- dhs_voter_data %>%
  filter(age_2020ge >= 18 & deceased_2020ge == 0) %>%
  
# Keep only the individuals who received TANF, SSI, Medicaid, public housing, SNAP, Section 8, or CCDF benefits in 2020 prior to the election
  
  filter(tanf_2020pe + ssi_2020pe + medicaid_2020pe + public_hous_2020pe + snap_2020pe + section8_2020pe + ccdf_2020pe > 0) %>%
  
# Create a 'total' row for the table
  
  mutate(total = 1) %>%
  
# Calculate 1) The number of program recipients as a percentage of Allegheny CVAP, 2) The number of program recipients who were registered to vote as of the 2020 GE as a percentage of total Allegheny registrants as of 2020 GE, and 3) The number of program recipients who voted in the 2020 GE as a percentage of total Allegheny voters in the 2020 GE  
  
  select(pseudo_id, registered_2020ge, voted_2020ge, public_hous_2020pe, section8_2020pe, snap_2020pe, ssi_2020pe, tanf_2020pe, medicaid_2020pe, ccdf_2020pe, total) %>%
  gather(program, value, -pseudo_id, -registered_2020ge, -voted_2020ge) %>%
  filter(value == 1) %>%
  select(-value) %>%
  group_by(program) %>%
  summarise(n = n(),
            pct_of_county_cvap = n() / 953365,
            pct_of_county_registrants = sum(registered_2020ge) / 942849, 
            pct_of_county_voters = sum(voted_2020ge) / 726720) %>%
  ungroup() %>%
  mutate(program_name = ifelse(program == 'ccdf_2020pe', 'CCDF',
                        ifelse(program == 'tanf_2020pe', 'TANF', 
                        ifelse(program == 'ssi_2020pe', 'SSI', 
                        ifelse(program == 'medicaid_2020pe', 'Medicaid', 
                        ifelse(program == 'section8_2020pe', 'Section 8', 
                        ifelse(program == 'public_hous_2020pe', 'Public housing',
                        ifelse(program == 'snap_2020pe', 'SNAP',
                        'Total benefits recipients')))))))) %>%
  select(program_name, n, pct_of_county_cvap, pct_of_county_registrants, pct_of_county_voters) 

# Arrange the programs in descending order by size, with the 'total' row at the bottom

dt1 <- dt %>%
  filter(program_name != 'Total benefits recipients') %>%
  arrange(desc(n))

dt2 <- dt %>%
  filter(program_name == 'Total benefits recipients')

bind_rows(dt1, dt2) %>%
  
# Format rates as percentages 
  
  mutate(pct_of_county_cvap = percent(pct_of_county_cvap, accuracy = 0.1), 
         pct_of_county_registrants = percent(pct_of_county_registrants, accuracy = 0.1), 
         pct_of_county_voters = percent(pct_of_county_voters, accuracy = 0.1)) %>%
  
# Create document table
  
  kbl(caption = 'Representation of age-eligible program recipients among Allegheny County electorate in 2020',    
      align = 'l', 
      booktabs = T,
      linesep = "",
      col.names = c('2020 program', 'N', 'Number as % of total Allegheny County CVAP', 'Number who were registered to vote as of 2020 GE, as % of total Allegheny County registrants as of 2020 GE', 'Number who voted in 2020 GE, as % of total Allegheny County voters in the election'), 
      format.args = list(big.mark = ',')) %>%
  column_spec(3:5, width = '10em') %>%
  row_spec(8, bold = TRUE) %>%
  kable_styling(latex_options = c('scale_down','HOLD_position'), font_size = 9) %>%
  footnote(general = 'The total Allegheny County citizen voting-age population comes from American Community Survey Table S2901 2019 1-year estimates. The total number of Allegheny County registered voters as of the 2020 GE and the total number of County voters in the election come from https://results.enr.clarityelections.com/PA/Allegheny/106267/web.264614/#/summary?category=C_1.', 
           general_title = 'Notes: ', 
           footnote_as_chunk = T, 
           title_format = c('italic'),
           threeparttable = TRUE)

#---------------------------------------------------------------------------------------------
# TABLE 2

#-------------------------------------
# CALCULATE DESCRIPTIVE STATS FOR ALLEGHENY COUNTY CVAP

# Calculate the Allegheny CVAP's mean wage and salary income in past 12 months across all Allegheny County PUMA's. Do this by weighting the mean earnings of each PUMA by its person count. 

allegheny_cvap_mean_earnings <- allegheny_cvap_earnings %>%
  mutate(weight = count / sum(count), 
         weighted_earnings = weight*earnings) %>%
  summarise(allegheny_cvap = as.character(dollar(round(sum(weighted_earnings), 0)))) %>%
  mutate(group = 'Mean earnings in past year')

# STACK ALLEGHENY COUNTY CVAP DEMOGRAPHICS AND MEAN EARNINGS

allegheny_county_cvap_breakdown <- bind_rows(allegheny_cvap_demographics, allegheny_cvap_mean_earnings)

#-------------------------------------
# BREAK DOWN THE DEMOGRAPHICS OF VOTING-AGE 2020 PUBLIC BENEFITS RECIPIENTS IN ALLEGHENY COUNTY

# Remove the individuals who were not at least 18 years old as of the 2020 election

public_benefits_cvap_demographics <- dhs_voter_data %>%
  filter(age_2020ge >= 18) %>%
  
# Keep only the individuals who received TANF, SSI, Medicaid, public housing, SNAP, Section 8, or CCDF benefits in 2020 prior to the election
  
  filter(tanf_2020pe + ssi_2020pe + medicaid_2020pe + public_hous_2020pe + snap_2020pe + section8_2020pe + ccdf_2020pe > 0) %>%

# Group the individuals into the same age groups as the Census data uses
  
  mutate(census_age_grouping_2020ge = ifelse(age_2020ge >= 18 & age_2020ge < 30, '18 to 29', 
                                      ifelse(age_2020ge >= 30 & age_2020ge < 65, '30 to 64', 
                                      ifelse(age_2020ge >= 65, '65+', 
                                      NA)))) %>%
  
# Group the individuals into the same racial categories as the Census data uses
  
  mutate(race_ethnicity = ifelse(race == 'White' & hispanic %in% c(0, NA), 'White alone', 
                          ifelse(race == 'Black' & hispanic %in% c(0, NA), 'Black alone', 
                          ifelse(!(race %in% c('White', 'Black')) & hispanic %in% c(0, NA), 'Other alone',
                          NA))),
         hispanic = ifelse(hispanic > 0, 'Hispanic', 'Not Hispanic')) %>%
  select(pseudo_id, race_ethnicity, hispanic, gender, census_age_grouping_2020ge) %>%
  gather(variable, value, -pseudo_id) %>%
  group_by(variable, value) %>%
  summarise(count = n()) %>%
  filter(!is.na(value)) %>%
  group_by(variable) %>%
  mutate(pct_of_total = percent(count / sum(count), accuracy = 0.1)) %>%
  ungroup() %>%
  select(value, pct_of_total) %>%
  rename(group = value, 
         public_benefits_cvap = pct_of_total)

#-------------------------------------
# CALCULATE THE MEAN 2019 EARNINGS AMONG VOTING-AGE 2020 PUBLIC BENEFITS RECIPIENTS IN ALLEGHENY COUNTY

public_benefits_cvap_mean_earnings <- earnings_2019 %>%
  
  summarise(public_benefits_cvap = as.character(dollar(round(mean(earnings_2019), 0)))) %>%
  mutate(group = 'Mean earnings in past year')

# Combine the demographics and earnings stats for the sample

public_benefits_cvap_breakdown <- bind_rows(public_benefits_cvap_demographics, public_benefits_cvap_mean_earnings)

#-------------------------------------
# CREATE THE DOCUMENT TABLE 

allegheny_county_cvap_breakdown %>%
  inner_join(public_benefits_cvap_breakdown, by = c('group')) %>%
  kbl(caption = 'Comparison of Allegheny County CVAP with voting-age public benefits recipients',    
      align = 'l', 
      booktabs = T,
      linesep = "",
      col.names = c(' ', 'Allegheny County CVAP', 'Voting-age public benefits recipients in 2020'), 
      format.args = list(big.mark = ',')) %>%
  pack_rows(index = c("Age composition" = 3, "Gender composition" = 1, "Race composition" = 2, " " = 1, " " = 1)) %>%
  kable_styling(latex_options = c('HOLD_position'), font_size = 9) %>%
  footnote(general = 'The Allegheny County CVAP demographics data comes from ACS Table S2901 2019 1-year estimates. The Allegheny County CVAP data on wage and salary income in the past year comes from ACS PUMS 2019 1-year estimates data aggregated across all Allegheny County PUMAs.', 
           general_title = 'Notes: ', 
           footnote_as_chunk = T, 
           title_format = c('italic'),
           threeparttable = TRUE)

#---------------------------------------------------------------------------------------------
# FIGURE A1

# Reshape data for easier graphing

month_xwalk <- data.frame(month = c('202001', '202002', '202003', '202004', '202005', '202006', '202007', '202008', '202009', '202010', '202011', '202012'), 
                          month_date = c('2020-01-01', '2020-02-01', '2020-03-01', '2020-04-01', '2020-05-01', '2020-06-01', '2020-07-01', '2020-08-01', '2020-09-01', '2020-10-01', '2020-11-01', '2020-12-01')) %>%
  mutate(month_date = as.Date(month_date, '%Y-%m-%d'))

dt <- benefit_counts_2020 %>%
  gather(variable, count, -month, -benefit) %>%
  mutate(source_of_data = ifelse(variable == 'count_acdhs', 'Allegheny County DHS matched data', 
                                 'PA DHS official totals')) %>%
  inner_join(month_xwalk, by = c('month'))

# Create the plots

plot_snap <- dt %>%
  filter(benefit == 'SNAP') %>%
  ggplot(mapping = aes(x = month_date, y = count, colour = source_of_data)) +
  geom_line() +
  geom_point() +
  theme_bw() + 
  ggtitle("SNAP") +
  xlab(" ") + ylab(" ") +
  scale_y_continuous(breaks = seq(100000,200000,20000),
                     limits = c(100000,200000),
                     labels = comma) +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        plot.title = element_text(size = 11, hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

plot_tanf <- dt %>%
  filter(benefit == 'TANF') %>%
  ggplot(mapping = aes(x = month_date, y = count, colour = source_of_data)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() + 
  ggtitle("TANF") +
  xlab(" ") + ylab(" ") +
  scale_y_continuous(breaks = seq(7000,15000,2000),
                     limits = c(7000,15000),
                     labels = comma) +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        plot.title = element_text(size = 11, hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

plot_ssi <- dt %>%
  filter(benefit == 'SSI') %>%
  ggplot(mapping = aes(x = month_date, y = count, colour = source_of_data)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() + 
  ggtitle("SSI") +
  xlab(" ") + ylab(" ") +
  scale_y_continuous(breaks = seq(30000,40000,2000),
                     limits = c(30000,40000),
                     labels = comma) +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        plot.title = element_text(size = 11, hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

plot_medicaid <- dt %>%
  filter(benefit == 'Medicaid') %>%
  ggplot(mapping = aes(x = month_date, y = count, colour = source_of_data)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() + 
  ggtitle("Medicaid") +
  xlab(" ") + ylab(" ") +
  scale_y_continuous(breaks = seq(200000,300000,20000),
                     limits = c(200000,300000),
                     labels = comma) +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        plot.title = element_text(size = 11, hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Arrange the plots in a panel

chart_panel <- plot_grid(
  plot_snap + theme(legend.position = 'none'), 
  plot_tanf + theme(legend.position = 'none'), 
  plot_ssi + theme(legend.position = 'none'), 
  plot_medicaid + theme(legend.position = 'none'),
  align = 'vh', 
  hjust = -1, 
  nrow = 2, 
  rel_heights = c(2,2))

# Create a legend

legend <- get_legend(
  plot_snap +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = 'bottom', 
          legend.text = element_text(size = 10),
          legend.title = element_blank()))

# Create the full plot panel

plot_grid(chart_panel, legend, ncol = 1, rel_heights = c(1, 0.05), rel_widths = c(4))

#---------------------------------------------------------------------------------------------
# TABLE A2

# Create footnotes for table

medicaid_footnote <- paste0("Medicaid", footnote_marker_alphabet(1, double_escape = T))
snap_footnote <- paste0("SNAP", footnote_marker_alphabet(2, double_escape = T))
tanf_footnote <- paste0("TANF", footnote_marker_alphabet(3, double_escape = T))
ssi_footnote <- paste0("SSI", footnote_marker_alphabet(4, double_escape = T))
section8_footnote <- paste0("Section 8", footnote_marker_alphabet(5, double_escape = T))
public_housing_footnote <- paste0("Public housing", footnote_marker_alphabet(6, double_escape = T))
ccdf_footnote <- paste0("CCDF", footnote_marker_alphabet(7, double_escape = T))

# Calculate the demographic breakdowns for each benefit

dt <- allegheny_versus_usa_benefit_pops %>%
  filter(variable == 'Total') %>%
  select(-variable, -grouping) %>%
  rename(total_recipients = program_recipient_count, 
         total_population = population_count)

allegheny_versus_usa_benefit_pops %>%
  left_join(dt, by = c('program', 'geography')) %>%
  mutate(program_recipient_breakdown = ifelse((program_recipient_count / total_recipients) < 0.001, '<0.1%', percent(program_recipient_count / total_recipients, accuracy = 0.1)),
         
# Calculate the per capita rates of benefit receipt for each benefit and demographic group

         program_recipient_percapita_group = ifelse((program_recipient_count / population_count) < 0.001, '<0.1%', percent(program_recipient_count / population_count, accuracy = 0.1))) %>%
  select(-population_count, -total_recipients, -total_population) %>%
  pivot_wider(names_from = geography, values_from = c('program_recipient_count', 'program_recipient_breakdown', 'program_recipient_percapita_group')) %>%
  
# Re-order the rows
  
  mutate(variable_order = ifelse(variable == 'Gender', 1, 
                          ifelse(variable == 'Race', 2, 
                          ifelse(variable == 'Age group', 3, 
                          4))), 
         program_order = ifelse(program == 'Medicaid', 1, 
                         ifelse(program == 'SNAP', 2, 
                         ifelse(program == 'TANF', 3, 
                         ifelse(program == 'SSI', 4, 
                         ifelse(program == 'Section 8', 5, 
                         ifelse(program == 'Public housing', 6, 
                         7))))))) %>%
  arrange(program_order, variable_order) %>%
  
# Re-order the columns
  
  rename(program_recipients_us = program_recipient_count_U.S., 
         program_recipients_allegheny = `program_recipient_count_Allegheny County`, 
         program_recipient_breakdown_us = program_recipient_breakdown_U.S., 
         program_recipient_breakdown_allegheny = `program_recipient_breakdown_Allegheny County`, 
         program_recipient_percapita_group_us = program_recipient_percapita_group_U.S., 
         program_recipient_percapita_group_allegheny = `program_recipient_percapita_group_Allegheny County`) %>%
  select(variable, grouping, program_recipients_allegheny, program_recipient_breakdown_allegheny, program_recipient_percapita_group_allegheny, program_recipients_us, program_recipient_breakdown_us, program_recipient_percapita_group_us) %>%
  
# Create the document table
  
  kbl(caption = 'Comparison of Allegheny County and United States public benefits populations', 
      align = 'l', 
      booktabs = T,
      linesep = "",
      longtable = T,
      col.names = c(' ', ' ', 'N', '% of total recipients', '% of population subgroup', 'N', '% of total recipients', '% of population subgroup'), 
      format.args = list(big.mark = ',')) %>%
  pack_rows(index = c(setNames(11, medicaid_footnote), setNames(11, snap_footnote), setNames(11, tanf_footnote), setNames(10, ssi_footnote), setNames(10, section8_footnote), setNames(10, public_housing_footnote), setNames(9, ccdf_footnote)), 
            escape = F) %>%
  add_header_above(header = c(" " = 2, "Allegheny County" = 3, "United States" = 3)) %>%
  row_spec(c(11, 22, 33, 43, 53, 63, 72), bold = TRUE) %>%
  column_spec(3:8, width = '5em') %>%
  column_spec(1, width = '8em') %>%
  collapse_rows(columns = 1, valign = "top") %>%
  kable_styling(latex_options = c('HOLD_position', 'repeat_header'), font_size = 9) %>%
  footnote(general = 'The Allegheny County and U.S. data presented are not exactly comparable for two reasons. First, the Allegheny County data cover 2020, while the U.S. data cover the most recent time period publicly available, generally either 2019 or 2020. Second, the Allegheny County data include everyone who received benefits at any point during the year, while the U.S. data for some programs reflect point-in-time counts or estimates based on samples from the full population. Racial categorizations vary across programs and do not sum to 100% within programs because they are not mutually exclusive.',
           general_title = 'Notes:',
           alphabet = c('U.S. Medicaid counts are based on data from ACS 2019 1-year estimates Tables S2704 and S27007', 
                        'U.S. SNAP counts are based on data from (ref:cronquist2021)',
                        'U.S. TANF counts are based on data from (ref:tanf2020)', 
                        'U.S. SSI counts are based on data from (ref:ssi2021) and the Survey of Income and Program Participation (SIPP)', 
                        'U.S. Section 8 counts are based on the U.S. Department of Housing and Urban Development Picture of Subsidized Households 2020 data set. Counts are numbers of households, not individuals.', 
                        'U.S. Section 8 counts are based on the U.S. Department of Housing and Urban Development Picture of Subsidized Households 2020 data set. Counts are numbers of households, not individuals.', 
                        'U.S. CCDF counts are based on data from (ref:ccdf2019). Disaggregations of U.S. CCDF recipients by gender are not publicly available. Recipient counts and the corresponding population denominators are limited to children ages 0 to 12, since this is the range of age eligibility for CCDF.'), 
           threeparttable = TRUE)

#---------------------------------------------------------------------------------------------
# TABLE A3

#--------------------------------------
# CALCULATE PARTY BREAKDOWN AMONG ALL COUNTY RESIDENTS WHO WERE REGISTERED TO VOTE AS OF 2020 ELECTION

dt1 <- allegheny_registered_voters_2020ge %>%
  group_by(party_2020ge) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(n = sum(count), 
         pct_of_total = percent(count / sum(count), accuracy = 0.1)) %>%
  select(-count) %>%
  pivot_wider(names_from = party_2020ge, values_from = c('pct_of_total')) %>%
  mutate(program_name = 'Allegheny County residents')

#--------------------------------------
# CALCULATE PARTY BREAKDOWN AMONG ALLEGHENY COUNTY PUBLIC BENEFITS RECIPIENTS WHO WERE REGISTERED TO VOTE AS OF 2020 ELECTION

dt2 <- dhs_voter_data %>%
  
# Keep only the individuals who were registered to vote as of the 2020 GE
  
  filter(registered_2020ge > 0) %>%
  
# Keep only the individuals who received TANF, SSI, Medicaid, public housing, SNAP, Section 8, or CCDF benefits in 2020 prior to the election
  
  filter(tanf_2020pe + ssi_2020pe + medicaid_2020pe + public_hous_2020pe + snap_2020pe + section8_2020pe + ccdf_2020pe > 0) %>%
  
# Create a 'total' row for the table
  
  mutate(total = 1) %>%
  
# Reshape the data for easier tabulating
  
  select(pseudo_id, party_2020ge, public_hous_2020pe, section8_2020pe, snap_2020pe, ssi_2020pe, tanf_2020pe, medicaid_2020pe, ccdf_2020pe, total) %>%
  gather(program, value, -pseudo_id, -party_2020ge) %>%
  filter(value > 0) %>%
  select(-value) %>%

# Group the parties into just Democrat, Republican, or Independent
  
  mutate(party_2020ge = replace_na(ifelse(party_2020ge == 'D', 'Democrat', 
                        ifelse(party_2020ge == 'R', 'Republican', 
                        'Independent')), 'Independent')) %>%
  
# Calculate the party breakdown by benefit
  
  group_by(program, party_2020ge) %>%
  summarise(count = n()) %>%
  group_by(program) %>%
  mutate(n = sum(count),
         pct_of_total = percent(count / sum(count), accuracy = 0.1)) %>%
  ungroup() %>%
  select(-count) %>%
  pivot_wider(names_from = party_2020ge, values_from = c('pct_of_total')) %>%
  mutate(program_name = ifelse(program == 'ccdf_2020pe', 'CCDF',
                        ifelse(program == 'tanf_2020pe', 'TANF', 
                        ifelse(program == 'ssi_2020pe', 'SSI', 
                        ifelse(program == 'medicaid_2020pe', 'Medicaid', 
                        ifelse(program == 'section8_2020pe', 'Section 8', 
                        ifelse(program == 'public_hous_2020pe', 'Public housing',
                        ifelse(program == 'snap_2020pe', 'SNAP',
                        'Total benefits recipients')))))))) %>%
  select(program_name, n, Democrat, Independent, Republican)

# Reorder the rows

dt2a <- dt2 %>%
  filter(program_name != 'Total benefits recipients') %>%
  arrange(desc(n))

dt2b <- dt2 %>%
  filter(program_name == 'Total benefits recipients')

dt2 <- bind_rows(dt2a, dt2b)

bind_rows(dt2, dt1) %>%
  select(program_name, n, Democrat, Republican, Independent) %>%
  
# Create the document table
  
  kbl(caption = 'Party registration as of the 2020 general election among Allegheny County public benefits recipients and all county residents', 
      align = 'l', 
      booktabs = T,
      linesep = "",
      col.names = c('2020 program', 'N', '% Democrat', '% Republican', '% Other/No Party'), 
      format.args = list(big.mark = ',')) %>%
  row_spec(8:9, bold = TRUE) %>%
  kable_styling(latex_options = c('HOLD_position'), font_size = 9) %>%
  footnote(general = 'Benefits recipients counts are limited to individuals who received benefits prior to November 2020', 
           general_title = 'Notes: ', 
           footnote_as_chunk = T, title_format = c('italic'))

#---------------------------------------------------------------------------------------------
# FIGURE A2

#-------------------------------------------
# 2020 ELECTION TURNOUT

# Remove the individuals who were not old enough to vote in the 2020 election, and individuals who were deceased as of the date of the 2020 election (11/3/2020). They should not be included in the denominator of the calculation. 

dt1 <- dhs_voter_data %>%
  filter(age_2020ge >= 18 & deceased_2020ge == 0) %>%
  
# Reshape data to make it easier to tabulate
  
  select(pseudo_id, voted_2020ge, snap_2020pe, ssi_2020pe, tanf_2020pe, medicaid_2020pe) %>%
  gather(program, value, -pseudo_id, -voted_2020ge) %>%
  filter(value > 0) %>%
  
# Calculate turnout rates by benefit group
  
  group_by(program) %>%
  summarise(turnout_rate = mean(voted_2020ge)) %>%
  ungroup()  %>%
  mutate(election = '2020', 
         program = ifelse(program == 'medicaid_2020pe', 'Medicaid', 
                   ifelse(program == 'snap_2020pe', 'SNAP', 
                   ifelse(program == 'tanf_2020pe', 'TANF', 
                   ifelse(program == 'ssi_2020pe', 'SSI', 
                   NA)))))

#-------------------------------------------
# 2018 ELECTION TURNOUT

# Remove the individuals who were not old enough to vote in the 2018 election, and individuals who were deceased as of the date of the 2018 election (11/6/2018). They should not be included in the denominator of the calculation.   
  
dt2 <- dhs_voter_data %>%
  filter(age_2018ge >= 18 & deceased_2018ge == 0) %>%
  
# Reshape data to make it easier to tabulate
  
  select(pseudo_id, voted_2018ge, snap_2018pe, ssi_2018pe, tanf_2018pe, medicaid_2018pe) %>%
  gather(program, value, -pseudo_id, -voted_2018ge) %>%
  filter(value > 0) %>%
  
# Calculate turnout rates by benefit group
  
  group_by(program) %>%
  summarise(turnout_rate = mean(voted_2018ge)) %>%
  ungroup()  %>%
  mutate(election = '2018', 
         program = ifelse(program == 'medicaid_2018pe', 'Medicaid', 
                   ifelse(program == 'snap_2018pe', 'SNAP', 
                   ifelse(program == 'tanf_2018pe', 'TANF', 
                   ifelse(program == 'ssi_2018pe', 'SSI', 
                   NA)))))

#-------------------------------------------
# 2016 ELECTION TURNOUT

# Remove the individuals who were not old enough to vote in the 2016 election, and individuals who were deceased as of the date of the 2016 election (11/8/2016). They should not be included in the denominator of the calculation.   

dt3 <- dhs_voter_data %>%
  filter(age_2016ge >= 18 & deceased_2016ge == 0) %>%
  
# Reshape data to make it easier to tabulate
  
  select(pseudo_id, voted_2016ge, snap_2016pe, ssi_2016pe, tanf_2016pe, medicaid_2016pe) %>%
  gather(program, value, -pseudo_id, -voted_2016ge) %>%
  filter(value > 0) %>%
  
# Calculate turnout rates by benefit group
  
  group_by(program) %>%
  summarise(turnout_rate = mean(voted_2016ge)) %>%
  ungroup()  %>%
  mutate(election = '2016', 
         program = ifelse(program == 'medicaid_2016pe', 'Medicaid', 
                   ifelse(program == 'snap_2016pe', 'SNAP', 
                   ifelse(program == 'tanf_2016pe', 'TANF', 
                   ifelse(program == 'ssi_2016pe', 'SSI', 
                   NA)))))

#-------------------------------------------
# CREATE THE GRAPH

bind_rows(dt1, dt2, dt3, allegheny_turnout_rates) %>%
  mutate(election = ifelse(election == '2016', 1,
                    ifelse(election == '2018', 2,
                    ifelse(election == '2020', 3,
                    NA)))) %>%
  ggplot(mapping = aes(x = election, y = turnout_rate, colour = program)) +
  geom_line() +
  geom_point(aes(shape = program, colour = program)) +
  geom_hline(yintercept = 0) +
  theme_bw() + 
  scale_x_continuous(breaks = c(1,2,3),
                     labels = c('2016 GE', '2018 GE', '2020 GE')) +
  scale_y_continuous(breaks = seq(0,0.8,0.1),
                     limits = c(0,0.8),
                     labels = scales::percent_format(accuracy = 1)) +
  xlab(" ") + ylab(" ") +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.caption = element_text(hjust = 0)) 

