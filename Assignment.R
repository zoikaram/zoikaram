library(tidyverse)
library(mclm)
library(here)
library(ggrepel)
library(kableExtra)
options(digits=3)

corpus_folder <- here("Politics project", "Converted sessions")

fnames_BASE <- get_fnames(corpus_folder) %>% 
  keep_re("[.]txt")
short_fnames <- short_names(fnames_BASE)
Country <- short_fnames %>%
  re_retrieve_first("([^_]+)", requested_group = 1)
Year <- short_fnames %>%
  re_retrieve_last("([^_]+)", requested_group = 1)
Decade <- substr(Year, 1, 3) %>% paste("0", sep = "") 

code <- read.csv("C:\\Users\\karamzo\\Documents\\corpus_class\\Politics project\\countries_codes.csv", header=TRUE, stringsAsFactors=FALSE)
country = as.data.frame(Country)
names(country)="Code"
Continent = left_join(x = country, y = code, by = "Code")

print(fnames_BASE, 10, hide_path = corpus_folder)

##### Keyword Analysis #####

# store names of target corpus files in fnames_target
fnames_target <- fnames_BASE %>%
  keep_re("Sessions of 70s") %>%
  print(hide_path = corpus_folder)

# store names of reference corpus files in fnames_ref
fnames_ref <- fnames_BASE %>%
  drop_re("Sessions of 70s") %>%
  print(n = 10, hide_path = corpus_folder)

## Frequency Lists
# build frequency list for target corpus
flist_target <- fnames_target %>%
  freqlist(re_token_splitter = r"--[(?xi)    \s+   ]--", # whitespace as token splitter
           re_drop_token     = r"--[(?xi)  [:\[\]] ]--", # drop tokens with :, [ or ]
           file_encoding     = "windows-1252") %>%
  print(n=100)

# build frequency list for reference corpus
flist_ref <- fnames_ref %>%
  freqlist(re_token_splitter = r"--[(?xi)  \s+   ]--",
           re_drop_token     = r"--[(?xi)  [:\[\]] ]--",
           file_encoding     = "windows-1252") %>%
  print(n=100)

flist_target %>% 
  as_tibble() %>% 
  kbl(col.names = c("Rank", "Type", "Absolute", "Relative")) %>% 
  kable_minimal(full_width = FALSE) %>% 
  add_header_above(c(" " = 2, "Frequency" = 2)) %>% 
  scroll_box(height = "400px")

## Association scores ##
# calculate scores
scores_kw <- assoc_scores(flist_target, flist_ref)

# print scores, sorted by PMI
print(scores_kw, sort_order = "PMI")
scores_kw <- scores_kw[order(scores_kw$PMI,decreasing=TRUE),]
scores_kw[,c(1,12,14,2,3,4,5,6)] %>% 
  as_tibble() %>% 
  kbl() %>% 
  kable_minimal(full_width = FALSE) %>% 
  #add_header_above(c(" " = 2, "Frequency" = 2)) %>% 
  scroll_box(height = "400px")

# print scores, sorted by G_signed
print(scores_kw, sort_order = "G_signed", n=30)

## Filtering of keywords by PMI and signed G^2 ##
top_scores_kw <- scores_kw %>% 
  filter(PMI >= 2 & G_signed >= 2)

# print top_scores_kw, sorted by PMI
top_scores_kw %>%
  print(sort_order = "PMI")


#### Correspondence Analysis ####
features <- read_types(here("Politics project", "topics.txt"))
print(features, n = 100)

d <- map(setNames(fnames_BASE, short_fnames), function(fname) {
  freqlist(fname)[features]
}) %>%
  bind_cols() %>% 
  data.frame(row.names = features) %>% 
  as.matrix() %>% 
  t() %>% 
  drop_empty_rc()

d
d_ca <- ca(d)
summary(d_ca)

texts_df <- row_pcoord(d_ca)[,c(1, 2)] %>% 
  as_tibble(rownames = "text") %>% 
  mutate(Subcorpus = Continent)
words_df <- col_pcoord(d_ca)[,c(1, 2)] %>% 
  as_tibble(rownames = "word")

dim_1 <- sprintf("Dimension 1 (%.2f%%)", summary(d_ca)$scree[1,3])
dim_2 <- sprintf("Dimension 2 (%.2f%%)", summary(d_ca)$scree[2,3])

ggplot(words_df, aes(x = V1, y = V2)) +
  geom_text(aes(label = word), color = "gray1") +
  #geom_point(data = texts_df, aes(color = Subcorpus)) +
  #scale_color_manual(values = c("#0000CD","#DC143C")) +
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_vline(xintercept = 0, color = "darkgray") +
  theme_bw(base_size = 12) +
  labs(x = dim_1, y = dim_2) +
  coord_fixed()

words_2010 <- words_df %>%
  arrange(desc(V1)) %>%
  head(20) %>% pull(word)
words_1970 <- words_df %>%
  arrange(V1) %>%
  head(20) %>% pull(word)


get_coords <- function(d_ca, sub_corp) {
  texts_df <- row_pcoord(d_ca)[,c(1, 2)] %>% 
    as_tibble(rownames = "text") %>% 
    mutate(Subcorpus = sub_corp)
  
  words_df <- col_pcoord(d_ca)[,c(1, 2)] %>% 
    as_tibble(rownames = "word")
  
  list(
    rows = texts_df,
    cols = words_df
  )
}

plot_ca <- function(ca_coords, variances) {
  dim_1 <- sprintf("Dimension 1 (%.2f %%)", variances[[1]])
  dim_2 <- sprintf("Dimension 2 (%.2f %%)", variances[[2]])
  
  ggplot(ca_coords$cols, aes(x = V1, y = V2)) +
    geom_text(aes(label = word), color = "gray60") +
    geom_point(data = ca_coords$rows, aes(color = Subcorpus)) +
    #scale_color_manual(values = c("#0000CD","#DC143C")) +
    geom_hline(yintercept = 0, color = "darkgray") +
    geom_vline(xintercept = 0, color = "darkgray") +
    theme_bw(base_size = 12) +
    labs(x = dim_1, y = dim_2) +
    coord_fixed()
}

#features <- read_types(here("Politics project", "topics.txt"))
#d <- compile_frequencies(features, fnames_BASE, short_fnames)
#d_ca <- ca(d)
ca_coords <- get_coords(d_ca, Decade)
plot_ca(ca_coords, summary(d_ca)$scree[,3])



#######################################################################3#####
# Frequency of "develop"
flist_ref %>% keep_re("develop")
flist_target %>% keep_re("develop")

# Concordance of "develop"
develop <- conc(fnames_BASE, "develop")
develop[,3]


# Co-occurrences with climate
develop_cooc <- surf_cooc(fnames_BASE, "develop", re_token_splitter = "\\s+")
develop_cooc$target_freqlist
develop_cooc$ref_freqlist

# Climate change
map(climate_cooc, keep_re, "change")


## Association scores ##
# calculate scores
scores_kw <- assoc_scores(flist_target, flist_ref)
scores_kw <- assoc_scores(climate_cooc)

# print scores, sorted by PMI
print(scores_kw, sort_order = "PMI")

# print scores, sorted by G_signed
print(scores_kw, sort_order = "G_signed", n=30)


## Filtering of keywords by PMI and signed G^2 ##
top_scores_kw <- scores_kw %>% 
  filter(PMI >= 2 & G_signed >= 2)

# print top_scores_kw, sorted by PMI
top_scores_kw %>%
  print(sort_order = "PMI")


#### Collocation Analysis ####
coocs <- fnames_BASE %>% 
  surf_cooc("(?xi)  ^ develop",
            w_left = 1,
            w_right = 1,
            re_token_splitter = r"--[(?xi)    \s+   ]--",
            re_drop_token     = r"--[(?xi)  [:\[\]] ]--",
            file_encoding     = "windows-1252")
print(coocs$target_freqlist, n=30)
print(coocs$ref_freqlist, n=50)

## Association scores ##
# calculate scores
scores_colloc <- assoc_scores(coocs)

# print scores, sorted by PMI
print(scores_colloc, sort_order = "PMI")

# print scores, sorted by G_signed
print(scores_colloc, sort_order = "G_signed")

## Filtering of Collocates by PMI and signed G^2
top_scores_colloc <- scores_colloc %>% 
  filter(PMI >= 2 & G_signed >= 2)

# print top_scores_colloc, sorted by PMI
top_scores_colloc %>%
  print(sort_order = "PMI")

# print top_scores_colloc, sorted by G_signed
top_scores_colloc %>%
  print(sort_order = "G_signed")

#### Post-processing ####
top_scores_kw %>%
  write_assoc("ahlct001_top_keywords.csv") 
# top_scores_kw <- read_assoc("ahlct001_top_keywords.csv")

top_scores_colloc %>%
  write_assoc("big_top_collocates.csv") 
# top_scores_colloc <- read_assoc("big_top_collocates.csv")

## A nice way of showing the scores in a report
top_scores_kw %>% # also valid for top_scores_colloc
  as_tibble() %>%
  select(type, a, PMI, G_signed) %>% # select 4 columns
  arrange(desc(PMI)) %>%             # sort by PMI (descending) 
  head(30) %>%                       # select top 30 rows
  kbl(col.names = c("Type", "Frequency", "PMI", r"(Signed $G^2$)")) %>% 
  kable_minimal() %>% 
  scroll_box(height = "400px")

top_scores_colloc %>% # also valid for top_scores_kw
  as_tibble() %>%
  select(type, a, PMI, G_signed) %>% # select 4 columns
  arrange(desc(G_signed)) %>%        # sort by G_signed (descending)  
  head(30) %>%                       # select top 30 rows
  kbl(col.names = c("Type", "Frequency", "PMI", r"(Signed $G^2$)")) %>% 
  kable_minimal() %>% 
  scroll_box(height = "400px")

## Plotting the association scores
top_scores_df <- as_tibble(top_scores_kw)

theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>%
  ggplot(aes(x = PMI, y = G_signed)) +
  labs(x = "PMI", y = "Signed G")

g + geom_point()
g + geom_point(aes(size = a))
g + geom_text(aes(label = type))

high_G_signed <- top_scores_df %>% 
  filter(G_signed > 100) # extract types with high G_signed

g + geom_point() +
  ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


#### Correspondence Analysis ####
features <- read_types(here("Politics project", "topics.txt"))
print(features, n = 100)

d <- map(setNames(fnames_BASE, short_fnames), function(fname) {
  freqlist(fname)[features]
}) %>%
  bind_cols() %>% 
  data.frame(row.names = features) %>% 
  as.matrix() %>% 
  t() %>% 
  drop_empty_rc()

d
d_ca <- ca(d)
summary(d_ca)

texts_df <- row_pcoord(d_ca)[,c(1, 2)] %>% 
  as_tibble(rownames = "text") %>% 
  mutate(Subcorpus = Continent)
words_df <- col_pcoord(d_ca)[,c(1, 2)] %>% 
  as_tibble(rownames = "word")

dim_1 <- sprintf("Dimension 1 (%.2f%%)", summary(d_ca)$scree[1,3])
dim_2 <- sprintf("Dimension 2 (%.2f%%)", summary(d_ca)$scree[2,3])

ggplot(words_df, aes(x = V1, y = V2)) +
  geom_text(aes(label = word), color = "gray1") +
  geom_point(data = texts_df, aes(color = Subcorpus)) +
  #scale_color_manual(values = c("#0000CD","#DC143C")) +
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_vline(xintercept = 0, color = "darkgray") +
  theme_bw(base_size = 12) +
  labs(x = dim_1, y = dim_2) +
  coord_fixed()

words_2010 <- words_df %>%
  arrange(desc(V1)) %>%
  head(20) %>% pull(word)
words_1970 <- words_df %>%
  arrange(V1) %>%
  head(20) %>% pull(word)


get_coords <- function(d_ca, sub_corp) {
  texts_df <- row_pcoord(d_ca)[,c(1, 2)] %>% 
    as_tibble(rownames = "text") %>% 
    mutate(Subcorpus = sub_corp)
  
  words_df <- col_pcoord(d_ca)[,c(1, 2)] %>% 
    as_tibble(rownames = "word")
  
  list(
    rows = texts_df,
    cols = words_df
  )
}

plot_ca <- function(ca_coords, variances) {
  dim_1 <- sprintf("Dimension 1 (%.2f %%)", variances[[1]])
  dim_2 <- sprintf("Dimension 2 (%.2f %%)", variances[[2]])
  
  ggplot(ca_coords$cols, aes(x = V1, y = V2)) +
    geom_text(aes(label = word), color = "gray60") +
    geom_point(data = ca_coords$rows, aes(color = Subcorpus)) +
    #scale_color_manual(values = c("#0000CD","#DC143C")) +
    geom_hline(yintercept = 0, color = "darkgray") +
    geom_vline(xintercept = 0, color = "darkgray") +
    theme_bw(base_size = 12) +
    labs(x = dim_1, y = dim_2) +
    coord_fixed()
}

#features <- read_types(here("Politics project", "topics.txt"))
#d <- compile_frequencies(features, fnames_BASE, short_fnames)
#d_ca <- ca(d)
ca_coords <- get_coords(d_ca, Decade)
plot_ca(ca_coords, summary(d_ca)$scree[,3])
