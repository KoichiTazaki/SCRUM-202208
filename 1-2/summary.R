library(tidyverse);


sample.info <- read.csv("./patient_info/GI-SCREEN-CRC/SAMPLE.csv" , skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>%
  filter(EventName %in% c("OCP", "OCA_1"));

seq.files <- list.files(path = "./OPC/GI-SCREEN-CRC/", pattern = ".tsv", full.names = T);
patient.info <- read.csv("./patient_info/GI-SCREEN-CRC/_PatientInfo.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM");

focus.genes <- c("KRAS", "NRAS", "ERBB3", "BRAF", "ERBB2");
focus.snv <- lapply(seq.files, function(x){read.delim(x) %>%
  filter(FUNC1.gene %in% focus.genes) %>%
  filter(FUNC1.location == "exonic") %>%
  filter(rowtype %in% c("snp", "del")) %>%
  filter(call == "POS") %>%
  select(X.REGNO, PANEL, POS, FUNC1.gene, FUNC1.protein, FUNC1.coding, FUNC1.function, rowtype) %>%
    mutate(file = x)});
focus.snv <- do.call("rbind", focus.snv);

focus.snv.n <- focus.snv %>%
  rename(REGNO = X.REGNO) %>%
  mutate(file = str_extract(file, pattern="[0-9]*\\-[0-9]*\\.tsv")) %>%
  unique() %>%
  group_by(REGNO, FUNC1.gene) %>%
  summarise(PANEL = paste0(PANEL, collapse = ";"),
            POS = paste0(POS, collapse = ";"),
            FUNC1.protein = paste0(FUNC1.protein, collapse = ";"),
            FUNC1.coding = paste0(FUNC1.coding, collapse = ";"),
            FUNC1.function = paste0(FUNC1.function, collapse = ";"),
            rowtype = paste0(rowtype, collapse = ";"),
            file = paste0(file, collapse = ";"));
  
HER.cnv <- lapply(seq.files, function(x){read.delim(x) %>%
    filter(FUNC1.gene %in% c("ERBB2", "ERBB3")) %>%
    filter(rowtype == "CNV") %>%
    mutate(file = x)});

HER.cnv <- rbindlist(HER.cnv, fill=TRUE, idcol=NULL);

HER.cnv.n <- HER.cnv %>%
  rename(REGNO = X.REGNO) %>%
  mutate(file = str_extract(file, pattern="[0-9]*\\-[0-9]*\\.tsv")) %>%
  unique() %>%
  select(REGNO, PANEL, FUNC1.gene, rowtype, call, FORMAT.1.CN, file) %>% 
  group_by(REGNO, FUNC1.gene) %>%
  summarise(PANEL = paste0(PANEL, collapse = ";"),
            call = paste0(call, collapse = ";"),
            max.CN = max(FORMAT.1.CN),
            FORMAT.1.CN = paste0(FORMAT.1.CN, collapse = ";"),
            rowtype = paste0(rowtype, collapse = ";"),
            file = paste0(file, collapse = ";"),
            n = n());

# summary #

summary <- patient.info %>%
  select(REGNO, SiteName, AGE, SEX) %>%
  inner_join(sample.info %>% select(REGNO, LBDAT, LBTEST, EventDate, LBSITE,LBSITE_PRIM,LBSITE_META, LBSITE_OTH, PATH_OTH, PRICX, PRIEGFR, PRIRX), by = "REGNO") %>%
  left_join(HER.cnv.n %>% filter(FUNC1.gene == "ERBB2") %>% rename_all(funs(paste0(., ".ERBB2"))), by = c("REGNO" = "REGNO.ERBB2")) %>%
  left_join(HER.cnv.n %>% filter(FUNC1.gene == "ERBB3") %>% rename_all(funs(paste0(., ".ERBB3"))), by = c("REGNO" = "REGNO.ERBB3")) %>%
  left_join(focus.snv.n %>% filter(FUNC1.gene == "KRAS") %>% rename_all(funs(paste0(., ".KRAS.snv"))), by = c("REGNO" = "REGNO.KRAS.snv")) %>%
  left_join(focus.snv.n %>% filter(FUNC1.gene == "NRAS") %>% rename_all(funs(paste0(., ".NRAS.snv"))), by = c("REGNO" = "REGNO.NRAS.snv")) %>%
  left_join(focus.snv.n %>% filter(FUNC1.gene == "BRAF") %>% rename_all(funs(paste0(., ".BRAF.snv"))), by = c("REGNO" = "REGNO.BRAF.snv")) %>%
  left_join(focus.snv.n %>% filter(FUNC1.gene == "ERBB2") %>% rename_all(funs(paste0(., ".ERBB2.snv"))), by = c("REGNO" = "REGNO.ERBB2.snv")) %>%
  left_join(focus.snv.n %>% filter(FUNC1.gene == "ERBB3") %>% rename_all(funs(paste0(., ".ERBB3.snv"))), by = c("REGNO" = "REGNO.ERBB3.snv"));

write.table(summary, file = "summary.txt", sep = "\t", row.names = F, quote = F);

  