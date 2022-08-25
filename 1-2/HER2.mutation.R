load("./sample.patient.RData");

all.patient.info %>%
  filter(REGNO %in% all.sample.info$REGNO) %>%
  group_by(Project) %>%
  summarise(n = n());

HER2.mutation <- get_mutation("ERBB2") %>%
  filter(FUNC1.location == "exonic") %>%
  filter(FUNC1.function != "synonymous") %>%
  filter(FUNC1.protein != "") %>%
  filter(rowtype %in% c("snp", "del", "ins"));

gp <- HER2.mutation %>%
  mutate(file = str_extract(file, pattern="[0-9]*\\-[0-9]*\\.tsv")) %>%
  left_join(all.sample.info, by = c("X.REGNO" = "REGNO")) %>%
  mutate(FUNC1.protein = fct_reorder(FUNC1.protein, POS)) %>%
  group_by(Project, FUNC1.exon, FUNC1.protein) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = FUNC1.protein, y = n, label = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label= n), vjust= -1, color="red", size=2.5) +
  facet_grid(Project ~ ., scales  = "free_x", space = "free_x") +
  theme_bw(base_size = 10) +
  xlab("HER2 mutation") +
  ylab("Frequency") +
  ylim(0, 110) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "none");
ggsave(gp, file = "HER2.mutation.SCRUM1.2.png", width = 11, height = 5);

HER2.mutation %>%
  mutate(file = str_extract(file, pattern="[0-9]*\\-[0-9]*\\.tsv")) %>%
  left_join(all.sample.info, by = c("X.REGNO" = "REGNO")) %>%
  mutate(FUNC1.protein = fct_reorder(FUNC1.protein, POS)) %>%
  group_by(Project, FUNC1.exon, FUNC1.protein) %>%
  summarise(n = n()) %>%
  write.table("./SCRUM1.2.HER2.mutation.freq.txt", sep = "\t", quote = F, row.names = F);

HER2.mutation.summary <- HER2.mutation %>%
  rename(REGNO = X.REGNO) %>%
  mutate(file = str_extract(file, pattern="[0-9]*\\-[0-9]*\\.tsv")) %>%
  unique() %>%
  group_by(REGNO) %>%
  summarise(PANEL = paste0(PANEL, collapse = ";"),
            POS = paste0(POS, collapse = ";"),
            FUNC1.protein = paste0(FUNC1.protein, collapse = ";"),
            FUNC1.coding = paste0(FUNC1.coding, collapse = ";"),
            FUNC1.function = paste0(FUNC1.function, collapse = ";"),
            rowtype = paste0(rowtype, collapse = ";"),
            file = paste0(file, collapse = ";"));

all.patient.info %>%
  filter(REGNO %in% all.sample.info$REGNO) %>%
  left_join(all.sample.info %>% select(-SiteName)) %>%
  left_join(HER2.mutation.summary) %>%
  rename(HER2.mut.POS = POS,
         HER2.mut.protein = FUNC1.protein,
         HER2.mut.coding = FUNC1.coding,
         HER2.mut.function = FUNC1.function) %>%
  write_excel_csv("./SCRUM1.2.HER2.mutation.csv");


HER2.mutation %>%
  rename(REGNO = X.REGNO) %>%
  left_join(all.sample.info %>% select(-SiteName)) %>%
  mutate(LBSITE_META = as.character(LBSITE_META)) %>%
  mutate(LBSITE_PRIM = as.character(LBSITE_PRIM)) %>%
  mutate(Organ = case_when(Project == "LC"  ~ "LC", 
                           Project == "CRC" ~ "CRC", 
                           Project == "NonCRC" & is.na(LBSITE_META)  ~ LBSITE_PRIM, 
                           Project == "NonCRC" & is.na(LBSITE_PRIM)  ~ LBSITE_META)) %>%
  left_join(all.patient.info) %>%
  select(FUNC1.protein, Organ, SiteName) %>%
  group_by(FUNC1.protein, Organ, SiteName) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(Organ = case_when(Organ == "BT" ~ "Biliary Tract", 
                           Organ == "CRC" ~ "CRC", 
                           Organ == "GC" ~ "Gastric", 
                           Organ == "HBP" ~ "HBP", 
                           Organ == "LC" ~ "Lung",
                           Organ == "OTH" ~ "Other", 
                           Organ == "ESO" ~ "Eso",
                           Organ == "PAN" ~ "Pancreas")) %>%
  spread(key = Organ, value = n, fill = 0) %>%
  write_excel_csv("./SCRUM1.2.HER2.mutation.site.csv");
