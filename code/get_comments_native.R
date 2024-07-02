
source("code/setup.R")

gs4_auth(email = "devin.jl@gmail.com")

# read from google sheet (requires authorization)
native_groups_raw <- read_sheet("1crWSexVnc1979ob5ql3_xATpF3K9f5Z7XpZ4QpaTe3Y")

write_csv(native_groups_raw, file = here("data", "native_groups_raw.csv"))

native_groups_raw <- read_csv(here("data", "native_groups_raw.csv"))

# split out to one obs per moniker
native_groups <- native_groups_raw %>%
  mutate(string = str_to_lower(`Strings`) %>%
           str_split("\\|")
  ) %>%
  unnest(string) %>%
  mutate(string = str_squish(string)) %>%
  distinct() %>%
  select(string, everything())

# inspect
head(native_groups$string)

# custom clean function for these data
clean <- . %>%
  str_to_lower() %>%
  str_replace_all("-|_", "") %>%
  str_replace_all("—", "-") %>% # "Chickahominy Indian Tribe—Eastern Division" is "CHICKAHOMINY INDIANS-EASTERN DIVISION INCORPORATED" in IRS data
  str_replace_all("&", "and") %>%
  #str_replace_all("confederated|confederate", "confederate(d|)")  %>%
  str_replace_all("pueblo de" ,"pueblo of") %>%
  str_replace_all("\\\\b", "\\b") %>%
  #str_replace_all("tribes|tribe|tribal", "trib[a-z]*") %>%
  str_replace_all("inter.tribal","intertribal") %>%
  str_replace_all("'|`|ʻ|‘|’|/", ".") %>%
  str_replace_all("ā", "a") %>%
  str_replace_all("ō", "o") %>%
  str_replace_all("ñ", "n") %>%
  str_replace_all("í", "i") %>%
  str_replace_all("é", "e") %>%
  str_replace_all("ū", "u") %>%
  str_remove_all(", inc.\\b|, inc\\b| inc\\b|, llc.\\b|, llc\\b| llc\\b") %>%
  str_squish()

clean("Nā Kuleana o Kānaka ‘Ōiwi")


# apply function to strings to search for
native_groups$string %<>% clean()

# inspect
head(native_groups$string)

native_groups$string |> str_extract_all("[^a-z]") |> unlist() |> unique()

native_groups$string |> str_extract_all("[^a-z| |.|^|$].*") |> unlist() |> unique()


# look at short strings
native_groups %<>%
  mutate(characters = nchar(string) )

# drop short strings
native_groups %<>% filter(characters > 2)

# look at long strings
native_groups %>%  arrange(characters) %>% head(100) |> pull(string) #kablebox() #%>% view()

# look at long strings
native_groups %>%  arrange(-characters) %>% head(100) |> pull(string) #%>% view()

# drop duplicates
native_groups %<>% distinct() %>% drop_na(string)

# check to make sure we don't have problematic specials
native_groups |> filter(str_dct(string, "\\)|\\("))

head(native_groups$string)

native_group_strings <- paste0("\\b",
  native_groups$string) %>% #, "\b") %>%
  paste0(collapse = "|") |>
  str_replace_all("nation\\|", "nation\\\\b\\|")

native_group_strings |> str_sub(0, 5000)

#########
# load commentorg metadata from rulemaking reopo
here::here("data", "comment_metadata_orgs.rdata") |>
  str_replace("University of Michigan Dropbox/Devin Judge-Lord/native-advocacy", "rulemaking") |>
  load()

# apply same function to data to search in
orgs <- comment_metadata_orgs %>%
  mutate(organization_clean = organization %>%
           str_to_lower() %>%
           str_squish() %>%
           clean() %>%
           str_squish())

# check for problem specials
native_group_strings |> str_dct("\\)|\\(")

orgs %>% head(100)

# make a flag for commenting orgs in list of native orgs
# THIS TAKES A LONG TIME
orgs %<>% mutate(string = str_extract_all(organization_clean, native_group_strings))

native_orgs <- orgs |> unnest_longer(string, keep_empty = FALSE) |>
  filter(nchar(string) > 2)


native_orgs |> filter(organization == "huy")

native_orgs %<>% distinct(organization, organization_clean, string)

# orgs that matched multiple strings
native_orgs |> distinct(organization_clean, string) |>  add_count(organization_clean) |> filter(n>1) |>  arrange(-n) |> kablebox()



# make sure strings have a unique match
native_groups %>% add_count(string, sort = T) |> filter(n>1) |> select(Name, string, Source) |> arrange(string) |> kablebox()

native_groups |>
 filter(str_detect(string,"huy|apache nation")) |>
  mutate(string = string |> str_remove_all("\\\\b|\\$|\\^"))

## FIXME NEED TO SPLIT NATIVE_ORG STRINGS before merging on string
# join in metadata
native_orgs %<>%
  left_join(native_groups |>
              mutate(string = string |> str_remove_all("\\\\b|\\$|\\^")),
            relationship = "many-to-many")


native_orgs |> kablebox()

#########################
# COMMENTS FROM THESE ORGS
#########################

# load comment metadata
if(!exists("comment_metadata")){

  here::here("data", "comment_metadata.rdata") |>
    str_replace("University of Michigan Dropbox/Devin Judge-Lord/native-advocacy", "rulemaking") |>
    load()
}

# format for merging
comment_metadata %<>%
  mutate(organization = str_to_lower(organization) %>% str_squish()) |>
  # make smaller
  filter(is.na(organization),
         nchar(organization) < 3)

# To whom shoud we match "apache nation"?
comment_metadata |> filter(organization == "apache nation")

# join in comment metadata
native_org_comments <- native_orgs |>
  left_join(comment_metadata)

native_org_comments %<>%
  mutate(comment_url = paste0("https://www.regulations.gov/comment/", document_id)) %>%
  select(comment_url, submitter_name, title, everything())

native_org_comments %>% kablebox()


# COMMENTS
# save comment data
save(native_org_comments,
     file = here::here("data", "native_org_comments.rdata"))

write_csv(native_org_comments, file = here::here("data", "native_org_comments.csv") )

native_org_comments %>% arrange(docket_id) %>%
  sheet_write("1HqI6MSMxCeMdcmrhivFlSmzeazpD-ioTjWcoa3GH4Fk",
              sheet = "matched_comments")


# COMMENTERS
# create a list of matched orgs, with a count of comments per organization
native_org_commenters <- native_org_comments %>% add_count(Name, name = "n_comments") %>% distinct(organization, `Strings`, Name, n_comments ) %>% arrange(-n_comments)

native_org_commenters |> kablebox()

# Save list of matched orgs
save(native_org_commenters,
     file = here::here("data", "native_org_commenters.rdata"))

native_org_commenters %>% #view
  write_csv(here::here("data", "native_org_commenters.csv"))

native_org_commenters %>%
sheet_write("1HqI6MSMxCeMdcmrhivFlSmzeazpD-ioTjWcoa3GH4Fk",
            sheet = "matched_commenters")


########################
# OTHER CANDIDATE ORGS #
########################
search <- "\\btribe|\\btribal\\b|\\breservation|rancheria|band of|american indian|indians|indigenous|first nation|hawai.i|\\bnative|pueblo|apache|mowhawk|ponca trib|paiute|shoshone|\\bbarona\\b|\\bviejas\\b"

unmatched <- orgs %>% filter(str_dct(organization_clean, search),
                         !organization_clean %in% native_org_comments$organization_clean)%>%
  distinct(organization_clean)

save(unmatched,
     file = here::here("data", "unmatched_commenters.rdata"))

write_csv(unmatched, file = here::here("data", "unmatched_commenters.csv") )

sheet_write(unmatched, "1HqI6MSMxCeMdcmrhivFlSmzeazpD-ioTjWcoa3GH4Fk",
            sheet = "unmatched_commenters")


###################################
# FROM IRS 501(c) NONPROFITS DATA #
###################################
if(!exists("nonprofit_resources")){
load("~/University of Michigan Dropbox/Devin Judge-Lord/FINREGRULEMAKE2/finreg/data/nonprofit_resources_clean.Rdata")
}

names(nonprofit_resources)

# clean 501(c) name in the same way we cleaned the list of native groups
nonprofit_resources %<>%
  mutate(organization_clean = name %>%
           str_to_lower() %>%
           str_squish() %>%
           clean() %>%
           str_squish())

# subset to nonprofits that match a native group
# THIS TAKES A LONG TIME
nonprofit_resources %<>%
  mutate(string = str_extract_all(organization_clean, native_group_strings))

# subset to ones that matched
matched_ngos <- nonprofit_resources |> unnest_longer(string, keep_empty = FALSE)

# merge in our metadata
matched_ngos %<>%
  left_join(native_groups)

# collapse strings
matched_ngos %<>%
  group_by(organization_clean) %>%
  mutate(string = paste(string, sep = ";;;")) %>%
  ungroup() %>%
  select(name, organization_clean, SUBSECTION, string, Name, everything())

# names that matched multiple strings
matched_ngos |> filter(str_detect(string, "\\|")) |> kablebox()

# inspect matches
matched_ngos |> kablebox()

# save
save(matched_ngos,
     file = here::here("data", "native_matched_ngos.rdata"))

write_csv(matched_ngos, here("data", "native_matched_ngos.csv"))

# write matched ngos to google sheet
sheet_write(matched_ngos, "1HqI6MSMxCeMdcmrhivFlSmzeazpD-ioTjWcoa3GH4Fk",
            sheet = "matched_ngos")

# subset to nonprofits that DID NOT match a native group, but fit broader search pattern
unmatched_ngos <- nonprofit_resources %>%
  filter(str_dct(organization_clean, search),
         !organization_clean %in% matched_ngos$organization_clean) %>%
  distinct() %>%
  select(name, organization_clean, SUBSECTION, everything())

unmatched_ngos |> kablebox()

save(unmatched_ngos,
     file = here::here("data", "native_unmatched_ngos.rdata"))

write_csv(unmatched_ngos, here("data", "native_unmatched_ngos.csv"))

# write unmatched ngos to google sheet
sheet_write(unmatched_ngos, "1HqI6MSMxCeMdcmrhivFlSmzeazpD-ioTjWcoa3GH4Fk",
            sheet = "unmatched_ngos")

