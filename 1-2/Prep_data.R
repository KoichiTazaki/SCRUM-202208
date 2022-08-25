library(tidyverse);
library(data.table);

data_dir <- "/home/yusuke/Data/SCRUM/1-2/";


# GI-CRC #
sample.info.CRC <- read.csv(paste(data_dir, "clinical/GI-SCREEN-CRC/SAMPLE.csv", sep = ""), skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>%
  filter(EventName %in% c("OCP", "OCA_1"));
patient.info.CRC <- read.csv(paste(data_dir, "clinical/GI-SCREEN-CRC/_PatientInfo.csv", sep = ""), skip = 1, header = T, fileEncoding = "UTF-8-BOM");

# GI-nonCRC #
sample.info.nonCRC <- read.csv(paste(data_dir, "clinical/GI-SCREEN-NonCRC/SAMPLE.csv", sep = ""), skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>%
  filter(EventName %in% c("OCP", "OCA_1"));
patient.info.nonCRC <- read.csv(paste(data_dir, "clinical/GI-SCREEN-NonCRC/_PatientInfo.csv", sep = ""), skip = 1, header = T, fileEncoding = "UTF-8-BOM");

sample.info.nonCRC.site <- sample.info.nonCRC %>%
  select(REGNO, LBSITE, LBSITE_GCPRIM, LBSITE_ESOPRIM, LBSITE_OTHPRIM, LBSITE_NEUROPRIM,LBSITE_HCPRIM,LBSITE_BTPRIM, LBSITE_PANPRIM, LBSITE_GISTPRIM,
         LBSITE_GCMETA, LBSITE_ESOMETA, LBSITE_OTHMETA, LBSITE_NEUROMETA, LBSITE_HBPMETA, LBSITE_GISTMETA) %>%
  gather(key = key, value = value, -REGNO, -LBSITE) %>% filter(value != "") %>%
  mutate(Position = case_when(str_detect(key, "PRIM") ~ "LBSITE_PRIM",
                              str_detect(key, "META") ~ "LBSITE_META"),
         Code = str_replace(key, "LBSITE_", "")) %>%
  mutate(Code = str_replace(Code, "PRIM|META", "")) %>%
  spread(key = Position, value = Code);
sample.info.nonCRC <- sample.info.nonCRC %>%
  left_join(sample.info.nonCRC.site %>% select(REGNO, LBSITE, LBSITE_PRIM, LBSITE_META));


# LC #
sample.info.LC <- read.csv(paste(data_dir, "clinical/LCSCRUM/SAMPLE.csv", sep = ""), header = T, fileEncoding = "shift-jis");
patient.info.LC <- read.csv(paste(data_dir, "clinical/LCSCRUM/PatientInfo.csv", sep = ""),  header = T, fileEncoding = "shift-jis");

all.sample.info <- rbind.data.frame(sample.info.CRC %>% select(REGNO, SiteName, EventName, LBSITE, LBSITE_PRIM, LBSITE_META) %>% mutate(Project = "CRC"),
                                    sample.info.nonCRC %>% select(REGNO, SiteName, EventName, LBSITE, LBSITE_PRIM, LBSITE_META) %>% mutate(Project = "NonCRC"),
                                    sample.info.LC %>% select(REGNO, CENTER) %>% mutate(Project = "LC", EventName = "OCA", LBSITE = "", LBSITE_PRIM = "", LBSITE_META = "") %>% rename(SiteName = CENTER));
all.patient.info <- rbind.data.frame(patient.info.CRC %>% select(REGNO, SiteName, AGE, SEX) %>% mutate(Project = "CRC"),
                                     patient.info.nonCRC %>% select(REGNO, SiteName, AGE, SEX) %>% mutate(Project = "NonCRC"),
                                     patient.info.LC %>% select(REGNO, CENTER, AGE, SEX) %>% mutate(Project = "LC") %>% rename(SiteName = CENTER));
save(all.sample.info, all.patient.info, file = "./sample.patient.RData");

# Read oncomine data #
seq.files <- c(list.files(path = paste(data_dir, "genome/GI-SCREEN-CRC", sep = ""), pattern = ".tsv", full.names = T),
               list.files(path = paste(data_dir, "genome/GI-SCREEN-NonCRC", sep = ""), pattern = ".tsv", full.names = T),
               list.files(path = paste(data_dir, "genome/LCSCRUM", sep = ""), pattern = ".tsv", full.names = T));

get_mutation <- function(genes){
  mutations <- lapply(seq.files, function(x){read.delim(x) %>%
      filter(FUNC1.gene %in% genes) %>%
      mutate(file = x)});
  #mutations <- do.call("rbind", mutations);
  #browser();
  mutations.m <- rbindlist(mutations, fill = TRUE);
  mutations.m <- mutations.m %>%
    filter(PANEL %in% c("OCP", "OCA")) %>%
    filter(call == "POS") %>%
    select(X.REGNO, PANEL, rowtype, CHROM, POS, REF, ALT, QUAL, FUNC1.function, FUNC1.location, FUNC1.exon, FUNC1.coding, FUNC1.protein, FUNC1.oncomineGeneClass, FUNC1.oncomineVariantClass, file);
  return(mutations.m);
}


