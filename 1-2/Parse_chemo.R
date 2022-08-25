library(tidyverse);
library(readxl);

## SCRUM1-2 ##
# LC-SCRUM #

LC.SCRUM.chemo <- read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/LCSCRUM/CHTHRPY.csv", header = T, fileEncoding = "shift-jis");
LC.SCRUM.chemo.merged <- rbind.data.frame(
  LC.SCRUM.chemo %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN1, CX_ANGYN1_V, CX_ANG1, CX_ANG1_V, CX_ANG1OTH, CX_MTD1, CX_MTD1_V, CX_MTD1CT, CX_MTD1CT_V, CX_MTD1OTH, CX_MTD1RX, CX_MTD1RX_V, CX_MTD1STDT, CX_MTD1ENDT, CX_MTD1RSP, CX_MTD1RSP_V, CX_MTD1RSP_OTH, CX_MTD1PRG, CX_MTD1PRG_V, CX_MTD1PRGDT, CX_MTD1PRGDTND, CX_MTD1PRGDTND_V) %>% rename_all(.funs = funs(sub("1", "", .))) %>% mutate(Regimen = 1),
  LC.SCRUM.chemo %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN2, CX_ANGYN2_V, CX_ANG2, CX_ANG2_V, CX_ANG2OTH, CX_MTD2, CX_MTD2_V, CX_MTD2CT, CX_MTD2CT_V, CX_MTD2OTH, CX_MTD2RX, CX_MTD2RX_V, CX_MTD2STDT, CX_MTD2ENDT, CX_MTD2RSP, CX_MTD2RSP_V, CX_MTD2RSP_OTH, CX_MTD2PRG, CX_MTD2PRG_V, CX_MTD2PRGDT, CX_MTD2PRGDTND, CX_MTD2PRGDTND_V) %>% rename_all(.funs = funs(sub("2", "", .))) %>% mutate(Regimen = 2),
  LC.SCRUM.chemo %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN3, CX_ANGYN3_V, CX_ANG3, CX_ANG3_V, CX_ANG3OTH, CX_MTD3, CX_MTD3_V, CX_MTD3CT, CX_MTD3CT_V, CX_MTD3OTH, CX_MTD3RX, CX_MTD3RX_V, CX_MTD3STDT, CX_MTD3ENDT, CX_MTD3RSP, CX_MTD3RSP_V, CX_MTD3RSP_OTH, CX_MTD3PRG, CX_MTD3PRG_V, CX_MTD3PRGDT, CX_MTD3PRGDTND, CX_MTD3PRGDTND_V) %>% rename_all(.funs = funs(sub("3", "", .))) %>% mutate(Regimen = 3),
  LC.SCRUM.chemo %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN4, CX_ANGYN4_V, CX_ANG4, CX_ANG4_V, CX_ANG4OTH, CX_MTD4, CX_MTD4_V, CX_MTD4CT, CX_MTD4CT_V, CX_MTD4OTH, CX_MTD4RX, CX_MTD4RX_V, CX_MTD4STDT, CX_MTD4ENDT, CX_MTD4RSP, CX_MTD4RSP_V, CX_MTD4RSP_OTH, CX_MTD4PRG, CX_MTD4PRG_V, CX_MTD4PRGDT, CX_MTD4PRGDTND, CX_MTD4PRGDTND_V) %>% rename_all(.funs = funs(sub("4", "", .))) %>% mutate(Regimen = 4),
  LC.SCRUM.chemo %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN5, CX_ANGYN5_V, CX_ANG5, CX_ANG5_V, CX_ANG5OTH, CX_MTD5, CX_MTD5_V, CX_MTD5CT, CX_MTD5CT_V, CX_MTD5OTH, CX_MTD5RX, CX_MTD5RX_V, CX_MTD5STDT, CX_MTD5ENDT, CX_MTD5RSP, CX_MTD5RSP_V, CX_MTD5RSP_OTH, CX_MTD5PRG, CX_MTD5PRG_V, CX_MTD5PRGDT, CX_MTD5PRGDTND, CX_MTD5PRGDTND_V) %>% rename_all(.funs = funs(sub("5", "", .))) %>% mutate(Regimen = 5),
  LC.SCRUM.chemo %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN6, CX_ANGYN6_V, CX_ANG6, CX_ANG6_V, CX_ANG6OTH, CX_MTD6, CX_MTD6_V, CX_MTD6CT, CX_MTD6CT_V, CX_MTD6OTH, CX_MTD6RX, CX_MTD6RX_V, CX_MTD6STDT, CX_MTD6ENDT, CX_MTD6RSP, CX_MTD6RSP_V, CX_MTD6RSP_OTH, CX_MTD6PRG, CX_MTD6PRG_V, CX_MTD6PRGDT, CX_MTD6PRGDTND, CX_MTD6PRGDTND_V) %>% rename_all(.funs = funs(sub("6", "", .))) %>% mutate(Regimen = 6),
  LC.SCRUM.chemo %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN7, CX_ANGYN7_V, CX_ANG7, CX_ANG7_V, CX_ANG7OTH, CX_MTD7, CX_MTD7_V, CX_MTD7CT, CX_MTD7CT_V, CX_MTD7OTH, CX_MTD7RX, CX_MTD7RX_V, CX_MTD7STDT, CX_MTD7ENDT, CX_MTD7RSP, CX_MTD7RSP_V, CX_MTD7RSP_OTH, CX_MTD7PRG, CX_MTD7PRG_V, CX_MTD7PRGDT, CX_MTD7PRGDTND, CX_MTD7PRGDTND_V) %>% rename_all(.funs = funs(sub("7", "", .))) %>% mutate(Regimen = 7),
  LC.SCRUM.chemo %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN8, CX_ANGYN8_V, CX_ANG8, CX_ANG8_V, CX_ANG8OTH, CX_MTD8, CX_MTD8_V, CX_MTD8CT, CX_MTD8CT_V, CX_MTD8OTH, CX_MTD8RX, CX_MTD8RX_V, CX_MTD8STDT, CX_MTD8ENDT, CX_MTD8RSP, CX_MTD8RSP_V, CX_MTD8RSP_OTH, CX_MTD8PRG, CX_MTD8PRG_V, CX_MTD8PRGDT, CX_MTD8PRGDTND, CX_MTD8PRGDTND_V) %>% rename_all(.funs = funs(sub("8", "", .))) %>% mutate(Regimen = 8),
  LC.SCRUM.chemo %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN9, CX_ANGYN9_V, CX_ANG9, CX_ANG9_V, CX_ANG9OTH, CX_MTD9, CX_MTD9_V, CX_MTD9CT, CX_MTD9CT_V, CX_MTD9OTH, CX_MTD9RX, CX_MTD9RX_V, CX_MTD9STDT, CX_MTD9ENDT, CX_MTD9RSP, CX_MTD9RSP_V, CX_MTD9RSP_OTH, CX_MTD9PRG, CX_MTD9PRG_V, CX_MTD9PRGDT, CX_MTD9PRGDTND, CX_MTD9PRGDTND_V) %>% rename_all(.funs = funs(sub("9", "", .))) %>% mutate(Regimen = 9),
  LC.SCRUM.chemo %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN10, CX_ANGYN10_V, CX_ANG10, CX_ANG10_V, CX_ANG10OTH, CX_MTD10, CX_MTD10_V, CX_MTD10CT, CX_MTD10CT_V, CX_MTD10OTH, CX_MTD10RX, CX_MTD10RX_V, CX_MTD10STDT, CX_MTD10ENDT, CX_MTD10RSP, CX_MTD10RSP_V, CX_MTD10RSP_OTH, CX_MTD10PRG, CX_MTD10PRG_V, CX_MTD10PRGDT, CX_MTD10PRGDTND, CX_MTD10PRGDTND_V) %>% rename_all(.funs = funs(sub("10", "", .))) %>% mutate(Regimen = 10));

# GI-SCREEN-CRC #
GI.CRC.chemo <- read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-CRC/CHTHRPY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM");

GI.CRC.chemo.merged <- rbind.data.frame(
  GI.CRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD1,CX_CRCMTD1CD, CX_CRCMTD1CT, CX_CRCMTD1CTCD, CX_CRCMTD1OTH) %>% rename_all(.funs = funs(sub("1", "", .))) %>% mutate(Regimen = 1),
  GI.CRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD2,CX_CRCMTD2CD, CX_CRCMTD2CT, CX_CRCMTD2CTCD, CX_CRCMTD2OTH) %>% rename_all(.funs = funs(sub("2", "", .))) %>% mutate(Regimen = 2),
  GI.CRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD3,CX_CRCMTD3CD, CX_CRCMTD3CT, CX_CRCMTD3CTCD, CX_CRCMTD3OTH) %>% rename_all(.funs = funs(sub("3", "", .))) %>% mutate(Regimen = 3),
  GI.CRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD4,CX_CRCMTD4CD, CX_CRCMTD4CT, CX_CRCMTD4CTCD, CX_CRCMTD4OTH) %>% rename_all(.funs = funs(sub("4", "", .))) %>% mutate(Regimen = 4),
  GI.CRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD5,CX_CRCMTD5CD, CX_CRCMTD5CT, CX_CRCMTD5CTCD, CX_CRCMTD5OTH) %>% rename_all(.funs = funs(sub("5", "", .))) %>% mutate(Regimen = 5),
  GI.CRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD6,CX_CRCMTD6CD, CX_CRCMTD6CT, CX_CRCMTD6CTCD, CX_CRCMTD6OTH) %>% rename_all(.funs = funs(sub("6", "", .))) %>% mutate(Regimen = 6),
  GI.CRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD7,CX_CRCMTD7CD, CX_CRCMTD7CT, CX_CRCMTD7CTCD, CX_CRCMTD7OTH) %>% rename_all(.funs = funs(sub("7", "", .))) %>% mutate(Regimen = 7),
  GI.CRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD8,CX_CRCMTD8CD, CX_CRCMTD8CT, CX_CRCMTD8CTCD, CX_CRCMTD8OTH) %>% rename_all(.funs = funs(sub("8", "", .))) %>% mutate(Regimen = 8),
  GI.CRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD9,CX_CRCMTD9CD, CX_CRCMTD9CT, CX_CRCMTD9CTCD, CX_CRCMTD9OTH) %>% rename_all(.funs = funs(sub("9", "", .))) %>% mutate(Regimen = 9),
  GI.CRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD10,CX_CRCMTD10CD, CX_CRCMTD10CT, CX_CRCMTD10CTCD, CX_CRCMTD10OTH) %>% rename_all(.funs = funs(sub("10", "", .))) %>% mutate(Regimen = 10));

# GI-SCREEN-NonCRC #
GI.NonCRC.chemo <- rbind.data.frame(
    read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/ESOCHTHRPY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>% rename_all(.funs = funs(sub("ESO", "", .))),
    read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/HBPCHTHRPY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>% rename_all(.funs = funs(sub("HBP", "", .))),
    read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/GCCHTHRPY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>% rename_all(.funs = funs(sub("GC", "", .))),
    read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/GISTCHTHRPY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>% rename_all(.funs = funs(sub("GIST", "", .))));

GI.NonCRC.chemo.merged <- rbind.data.frame(
  GI.NonCRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT1, CX_TREAT1CD, CX_MTD1, CX_MTD1CD, CX_MTD1CT, CX_MTD1CTCD, CX_MTD1OTH) %>% rename_all(.funs = funs(sub("1", "", .)))  %>% mutate(Regimen = 1),
  GI.NonCRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT2, CX_TREAT2CD, CX_MTD2, CX_MTD2CD, CX_MTD2CT, CX_MTD2CTCD, CX_MTD2OTH) %>% rename_all(.funs = funs(sub("2", "", .)))  %>% mutate(Regimen = 2),
  GI.NonCRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT3, CX_TREAT3CD, CX_MTD3, CX_MTD3CD, CX_MTD3CT, CX_MTD3CTCD, CX_MTD3OTH) %>% rename_all(.funs = funs(sub("3", "", .)))  %>% mutate(Regimen = 3),
  GI.NonCRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT4, CX_TREAT4CD, CX_MTD4, CX_MTD4CD, CX_MTD4CT, CX_MTD4CTCD, CX_MTD4OTH) %>% rename_all(.funs = funs(sub("4", "", .)))  %>% mutate(Regimen = 4),
  GI.NonCRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT5, CX_TREAT5CD, CX_MTD5, CX_MTD5CD, CX_MTD5CT, CX_MTD5CTCD, CX_MTD5OTH) %>% rename_all(.funs = funs(sub("5", "", .)))  %>% mutate(Regimen = 5),
  GI.NonCRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT6, CX_TREAT6CD, CX_MTD6, CX_MTD6CD, CX_MTD6CT, CX_MTD6CTCD, CX_MTD6OTH) %>% rename_all(.funs = funs(sub("6", "", .)))  %>% mutate(Regimen = 6),
  GI.NonCRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT7, CX_TREAT7CD, CX_MTD7, CX_MTD7CD, CX_MTD7CT, CX_MTD7CTCD, CX_MTD7OTH) %>% rename_all(.funs = funs(sub("7", "", .)))  %>% mutate(Regimen = 7),
  GI.NonCRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT8, CX_TREAT8CD, CX_MTD8, CX_MTD8CD, CX_MTD8CT, CX_MTD8CTCD, CX_MTD8OTH) %>% rename_all(.funs = funs(sub("8", "", .)))  %>% mutate(Regimen = 8),
  GI.NonCRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT9, CX_TREAT9CD, CX_MTD9, CX_MTD9CD, CX_MTD9CT, CX_MTD9CTCD, CX_MTD9OTH) %>% rename_all(.funs = funs(sub("9", "", .)))  %>% mutate(Regimen = 9),
  GI.NonCRC.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT10, CX_TREAT10CD, CX_MTD10, CX_MTD10CD, CX_MTD10CT, CX_MTD10CTCD, CX_MTD10OTH) %>% rename_all(.funs = funs(sub("10", "", .)))  %>% mutate(Regimen = 10));

GI.NonCRC.other.chemo <- read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/OTHCHTHRPY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>% rename_all(.funs = funs(sub("OTH", "", .)));
GI.NonCRC.other.chemo.merged <- rbind.data.frame(
  GI.NonCRC.other.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT1, CX_TREAT1CD, CX_MTD1, CX_MTD1CD, CX_MTD1RT, CX_MTD1RTCD, CX_MTD1CT, CX_MTD1CTCD ,CX_MTD1OTH) %>% rename_all(.funs = funs(sub("1", "", .))),
  GI.NonCRC.other.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT2, CX_TREAT2CD, CX_MTD2, CX_MTD2CD, CX_MTD2RT, CX_MTD2RTCD, CX_MTD2CT, CX_MTD2CTCD ,CX_MTD2OTH) %>% rename_all(.funs = funs(sub("2", "", .))),
  GI.NonCRC.other.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT3, CX_TREAT3CD, CX_MTD3, CX_MTD3CD, CX_MTD3RT, CX_MTD3RTCD, CX_MTD3CT, CX_MTD3CTCD ,CX_MTD3OTH) %>% rename_all(.funs = funs(sub("3", "", .))),
  GI.NonCRC.other.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT4, CX_TREAT4CD, CX_MTD4, CX_MTD4CD, CX_MTD4RT, CX_MTD4RTCD, CX_MTD4CT, CX_MTD4CTCD ,CX_MTD4OTH) %>% rename_all(.funs = funs(sub("4", "", .))),
  GI.NonCRC.other.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT5, CX_TREAT5CD, CX_MTD5, CX_MTD5CD, CX_MTD5RT, CX_MTD5RTCD, CX_MTD5CT, CX_MTD5CTCD ,CX_MTD5OTH) %>% rename_all(.funs = funs(sub("5", "", .))),
  GI.NonCRC.other.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT6, CX_TREAT6CD, CX_MTD6, CX_MTD6CD, CX_MTD6RT, CX_MTD6RTCD, CX_MTD6CT, CX_MTD6CTCD ,CX_MTD6OTH) %>% rename_all(.funs = funs(sub("6", "", .))),
  GI.NonCRC.other.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT7, CX_TREAT7CD, CX_MTD7, CX_MTD7CD, CX_MTD7RT, CX_MTD7RTCD, CX_MTD7CT, CX_MTD7CTCD ,CX_MTD7OTH) %>% rename_all(.funs = funs(sub("7", "", .))),
  GI.NonCRC.other.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT8, CX_TREAT8CD, CX_MTD8, CX_MTD8CD, CX_MTD8RT, CX_MTD8RTCD, CX_MTD8CT, CX_MTD8CTCD ,CX_MTD8OTH) %>% rename_all(.funs = funs(sub("8", "", .))),
  GI.NonCRC.other.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT9, CX_TREAT9CD, CX_MTD9, CX_MTD9CD, CX_MTD9RT, CX_MTD9RTCD, CX_MTD9CT, CX_MTD9CTCD ,CX_MTD9OTH) %>% rename_all(.funs = funs(sub("9", "", .))),
  GI.NonCRC.other.chemo %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT10, CX_TREAT10CD, CX_MTD10, CX_MTD10CD, CX_MTD10RT, CX_MTD10RTCD, CX_MTD10CT, CX_MTD10CTCD ,CX_MTD10OTH) %>% rename_all(.funs = funs(sub("10", "", .)))
)  

LC.SCRUM.chemo.merged %>%
  write_excel_csv("./LC.SCRUM.chemotherapy.csv");

GI.CRC.chemo.merged %>%
  write_excel_csv("./GI.CRC.chemotherapy.csv");

GI.NonCRC.chemo.merged %>%
  write_excel_csv("./GI.NonCRC.chemotherapy.csv");

GI.NonCRC.other.chemo.merged %>%
  write_excel_csv("./GI.NonCRC.other.chemotherapy.csv");

# DT info #

DTinfo.LC <- read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/LCSCRUM/DTINFO.csv", header=T, fileEncoding = "shift-jis");
DTinfo.CRC <- read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-CRC/DTINFO.csv", header=T, skip = 1, fileEncoding = "UTF-8-BOM") %>%
  group_by(REGNO) %>% slice(which.max(ActivityId));
DTinfo.NonCRC <- read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/DTINFO.csv", header=T, skip = 1, fileEncoding = "UTF-8-BOM") %>%
  group_by(REGNO) %>% slice(which.max(ActivityId));

LC.SCRUM.chemo.merged %>%
  left_join(DTinfo.LC %>% select(REGNO, VISIT, SURVIVE,  SURVIVE, FINALDT, DEATHDT, D_CAUSEL), by = "REGNO") %>%
  filter(SURVIVE=="death") %>% 
  filter(D_CAUSEL != "death of original disease");
GI.CRC.chemo.merged %>%
  left_join(DTinfo.CRC %>% select(REGNO, ActivityName, SURVIVE, FINALDT, DEATHDT, D_CAUSEL),, by = "REGNO") %>%
  filter(SURVIVE=="death") %>% 
  filter(D_CAUSEL != "death of original disease");
GI.NonCRC.chemo.merged %>%
  left_join(DTinfo.NonCRC %>% select(REGNO, ActivityName, SURVIVE, FINALDT, DEATHDT, D_CAUSEL), by = "REGNO") %>%
  filter(SURVIVE=="death") %>% 
  filter(D_CAUSEL != "death of original disease");
GI.NonCRC.other.chemo.merged %>%
  left_join(DTinfo.NonCRC %>% select(REGNO, ActivityName, SURVIVE, FINALDT, DEATHDT, D_CAUSEL), by = "REGNO") %>%
  filter(SURVIVE=="death") %>% 
  filter(D_CAUSEL != "death of original disease");


# Filter for specific patients #

Specific.patients <- rbind.data.frame(read_excel("./トポテシン.xlsx") %>%
  gather(key = Data, value = REGNO) %>%
  mutate(Drug = "Topotecin"),
  read_excel("T-mab.xlsx") %>%
  gather(key = Data, value = REGNO) %>%
  mutate(Drug = "T-mab"),
  read_excel("./Bev.xlsx") %>%
  gather(key = Data, value = REGNO) %>%
  mutate(Drug = "Bev"));

# LC-SCRUM #
read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/LCSCRUM/PatientInfo.csv", header = T, fileEncoding = "shift-jis") %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/LC.SCRUM.PatientInfo.20201009.csv");

read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/LCSCRUM/CLIN_INFO.csv", header = T, fileEncoding = "shift-jis") %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/LC.SCRUM.CLINInfo.20201009.csv");
  #pull(Drug) %>%
  #table();
read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/LCSCRUM/DTINFO.csv", header = T, fileEncoding = "shift-jis") %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/LC.SCRUM.DTInfo.20201009.csv");
read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/LCSCRUM/SUMMARY.csv", header = T, fileEncoding = "cp932") %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/LC.SCRUM.Summary.20201009.csv");
LC.SCRUM.chemo.merged %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/LC.SCRUM.CHTHRPY.20201009.csv");

# CRC #  
read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-CRC/_PatientInfo.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/GI.SCREEN.CRC.PatientInfo.20201009.csv");
  #pull(Drug) %>%
  #table();
read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-CRC/DTINFO.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/GI.SCREEN.CRC.DTInfo.20201009.csv");
read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-CRC/SUMMARY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/GI.SCREEN.CRC.Summary.20201009.csv");
GI.CRC.chemo.merged %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/GI.SCREEN.CRC.CHTHRPY.20201009.csv");

# Non-CRC #

read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/_PatientInfo.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/GI.SCREEN.NonCRC.PatientInfo.20201009.csv");
  #pull(Drug) %>%
  #table();
read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/DTINFO.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/GI.SCREEN.NonCRC.DTInfo.20201009.csv");
read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/SUMMARY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/GI.SCREEN.NonCRC.Summary.20201009.csv");
GI.NonCRC.chemo.merged %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/GI.SCREEN.NonCRC.CHTHRPY.20201009.csv");
GI.NonCRC.other.chemo.merged %>%
  inner_join(Specific.patients) %>%
  write_excel_csv("./for_CSPV/GI.SCREEN.NonCRC.other.HTHRPY.20201009.csv");

# Data downloaded on August, 2020 #
# LC-SCRUM #
LC.SCRUM.chemo.202008 <- read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/clinical/LCSCRUM/CHTHRPY.csv", header = T, fileEncoding = "shift-jis");
LC.SCRUM.chemo.202008.merged <- rbind.data.frame(
  LC.SCRUM.chemo.202008 %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN1, CX_ANGYN1_V, CX_ANG1, CX_ANG1_V, CX_ANG1OTH, CX_MTD1, CX_MTD1_V, CX_MTD1CT, CX_MTD1CT_V, CX_MTD1OTH, CX_MTD1RX, CX_MTD1RX_V, CX_MTD1STDT, CX_MTD1ENDT, CX_MTD1RSP, CX_MTD1RSP_V, CX_MTD1RSP_OTH, CX_MTD1PRG, CX_MTD1PRG_V, CX_MTD1PRGDT, CX_MTD1PRGDTND, CX_MTD1PRGDTND_V) %>% rename_all(.funs = funs(sub("1", "", .))) %>% mutate(Regimen = 1),
  LC.SCRUM.chemo.202008 %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN2, CX_ANGYN2_V, CX_ANG2, CX_ANG2_V, CX_ANG2OTH, CX_MTD2, CX_MTD2_V, CX_MTD2CT, CX_MTD2CT_V, CX_MTD2OTH, CX_MTD2RX, CX_MTD2RX_V, CX_MTD2STDT, CX_MTD2ENDT, CX_MTD2RSP, CX_MTD2RSP_V, CX_MTD2RSP_OTH, CX_MTD2PRG, CX_MTD2PRG_V, CX_MTD2PRGDT, CX_MTD2PRGDTND, CX_MTD2PRGDTND_V) %>% rename_all(.funs = funs(sub("2", "", .))) %>% mutate(Regimen = 2),
  LC.SCRUM.chemo.202008 %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN3, CX_ANGYN3_V, CX_ANG3, CX_ANG3_V, CX_ANG3OTH, CX_MTD3, CX_MTD3_V, CX_MTD3CT, CX_MTD3CT_V, CX_MTD3OTH, CX_MTD3RX, CX_MTD3RX_V, CX_MTD3STDT, CX_MTD3ENDT, CX_MTD3RSP, CX_MTD3RSP_V, CX_MTD3RSP_OTH, CX_MTD3PRG, CX_MTD3PRG_V, CX_MTD3PRGDT, CX_MTD3PRGDTND, CX_MTD3PRGDTND_V) %>% rename_all(.funs = funs(sub("3", "", .))) %>% mutate(Regimen = 3),
  LC.SCRUM.chemo.202008 %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN4, CX_ANGYN4_V, CX_ANG4, CX_ANG4_V, CX_ANG4OTH, CX_MTD4, CX_MTD4_V, CX_MTD4CT, CX_MTD4CT_V, CX_MTD4OTH, CX_MTD4RX, CX_MTD4RX_V, CX_MTD4STDT, CX_MTD4ENDT, CX_MTD4RSP, CX_MTD4RSP_V, CX_MTD4RSP_OTH, CX_MTD4PRG, CX_MTD4PRG_V, CX_MTD4PRGDT, CX_MTD4PRGDTND, CX_MTD4PRGDTND_V) %>% rename_all(.funs = funs(sub("4", "", .))) %>% mutate(Regimen = 4),
  LC.SCRUM.chemo.202008 %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN5, CX_ANGYN5_V, CX_ANG5, CX_ANG5_V, CX_ANG5OTH, CX_MTD5, CX_MTD5_V, CX_MTD5CT, CX_MTD5CT_V, CX_MTD5OTH, CX_MTD5RX, CX_MTD5RX_V, CX_MTD5STDT, CX_MTD5ENDT, CX_MTD5RSP, CX_MTD5RSP_V, CX_MTD5RSP_OTH, CX_MTD5PRG, CX_MTD5PRG_V, CX_MTD5PRGDT, CX_MTD5PRGDTND, CX_MTD5PRGDTND_V) %>% rename_all(.funs = funs(sub("5", "", .))) %>% mutate(Regimen = 5),
  LC.SCRUM.chemo.202008 %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN6, CX_ANGYN6_V, CX_ANG6, CX_ANG6_V, CX_ANG6OTH, CX_MTD6, CX_MTD6_V, CX_MTD6CT, CX_MTD6CT_V, CX_MTD6OTH, CX_MTD6RX, CX_MTD6RX_V, CX_MTD6STDT, CX_MTD6ENDT, CX_MTD6RSP, CX_MTD6RSP_V, CX_MTD6RSP_OTH, CX_MTD6PRG, CX_MTD6PRG_V, CX_MTD6PRGDT, CX_MTD6PRGDTND, CX_MTD6PRGDTND_V) %>% rename_all(.funs = funs(sub("6", "", .))) %>% mutate(Regimen = 6),
  LC.SCRUM.chemo.202008 %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN7, CX_ANGYN7_V, CX_ANG7, CX_ANG7_V, CX_ANG7OTH, CX_MTD7, CX_MTD7_V, CX_MTD7CT, CX_MTD7CT_V, CX_MTD7OTH, CX_MTD7RX, CX_MTD7RX_V, CX_MTD7STDT, CX_MTD7ENDT, CX_MTD7RSP, CX_MTD7RSP_V, CX_MTD7RSP_OTH, CX_MTD7PRG, CX_MTD7PRG_V, CX_MTD7PRGDT, CX_MTD7PRGDTND, CX_MTD7PRGDTND_V) %>% rename_all(.funs = funs(sub("7", "", .))) %>% mutate(Regimen = 7),
  LC.SCRUM.chemo.202008 %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN8, CX_ANGYN8_V, CX_ANG8, CX_ANG8_V, CX_ANG8OTH, CX_MTD8, CX_MTD8_V, CX_MTD8CT, CX_MTD8CT_V, CX_MTD8OTH, CX_MTD8RX, CX_MTD8RX_V, CX_MTD8STDT, CX_MTD8ENDT, CX_MTD8RSP, CX_MTD8RSP_V, CX_MTD8RSP_OTH, CX_MTD8PRG, CX_MTD8PRG_V, CX_MTD8PRGDT, CX_MTD8PRGDTND, CX_MTD8PRGDTND_V) %>% rename_all(.funs = funs(sub("8", "", .))) %>% mutate(Regimen = 8),
  LC.SCRUM.chemo.202008 %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN9, CX_ANGYN9_V, CX_ANG9, CX_ANG9_V, CX_ANG9OTH, CX_MTD9, CX_MTD9_V, CX_MTD9CT, CX_MTD9CT_V, CX_MTD9OTH, CX_MTD9RX, CX_MTD9RX_V, CX_MTD9STDT, CX_MTD9ENDT, CX_MTD9RSP, CX_MTD9RSP_V, CX_MTD9RSP_OTH, CX_MTD9PRG, CX_MTD9PRG_V, CX_MTD9PRGDT, CX_MTD9PRGDTND, CX_MTD9PRGDTND_V) %>% rename_all(.funs = funs(sub("9", "", .))) %>% mutate(Regimen = 9),
  LC.SCRUM.chemo.202008 %>% select(REGNO, CENTER, VISIT, CX_ADDINFO, CX_ADDINFO_V, CX_ANGYN10, CX_ANGYN10_V, CX_ANG10, CX_ANG10_V, CX_ANG10OTH, CX_MTD10, CX_MTD10_V, CX_MTD10CT, CX_MTD10CT_V, CX_MTD10OTH, CX_MTD10RX, CX_MTD10RX_V, CX_MTD10STDT, CX_MTD10ENDT, CX_MTD10RSP, CX_MTD10RSP_V, CX_MTD10RSP_OTH, CX_MTD10PRG, CX_MTD10PRG_V, CX_MTD10PRGDT, CX_MTD10PRGDTND, CX_MTD10PRGDTND_V) %>% rename_all(.funs = funs(sub("10", "", .))) %>% mutate(Regimen = 10));

# GI-SCREEN #
GI.CRC.chemo.202008 <- read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/clinical//GI-SCREEN-CRC/CHTHRPY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM");

GI.CRC.chemo.202008.merged <- rbind.data.frame(
  GI.CRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD1,CX_CRCMTD1CD, CX_CRCMTD1CT, CX_CRCMTD1CTCD, CX_CRCMTD1OTH) %>% rename_all(.funs = funs(sub("1", "", .))) %>% mutate(Regimen = 1),
  GI.CRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD2,CX_CRCMTD2CD, CX_CRCMTD2CT, CX_CRCMTD2CTCD, CX_CRCMTD2OTH) %>% rename_all(.funs = funs(sub("2", "", .))) %>% mutate(Regimen = 2),
  GI.CRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD3,CX_CRCMTD3CD, CX_CRCMTD3CT, CX_CRCMTD3CTCD, CX_CRCMTD3OTH) %>% rename_all(.funs = funs(sub("3", "", .))) %>% mutate(Regimen = 3),
  GI.CRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD4,CX_CRCMTD4CD, CX_CRCMTD4CT, CX_CRCMTD4CTCD, CX_CRCMTD4OTH) %>% rename_all(.funs = funs(sub("4", "", .))) %>% mutate(Regimen = 4),
  GI.CRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD5,CX_CRCMTD5CD, CX_CRCMTD5CT, CX_CRCMTD5CTCD, CX_CRCMTD5OTH) %>% rename_all(.funs = funs(sub("5", "", .))) %>% mutate(Regimen = 5),
  GI.CRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD6,CX_CRCMTD6CD, CX_CRCMTD6CT, CX_CRCMTD6CTCD, CX_CRCMTD6OTH) %>% rename_all(.funs = funs(sub("6", "", .))) %>% mutate(Regimen = 6),
  GI.CRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD7,CX_CRCMTD7CD, CX_CRCMTD7CT, CX_CRCMTD7CTCD, CX_CRCMTD7OTH) %>% rename_all(.funs = funs(sub("7", "", .))) %>% mutate(Regimen = 7),
  GI.CRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD8,CX_CRCMTD8CD, CX_CRCMTD8CT, CX_CRCMTD8CTCD, CX_CRCMTD8OTH) %>% rename_all(.funs = funs(sub("8", "", .))) %>% mutate(Regimen = 8),
  GI.CRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD9,CX_CRCMTD9CD, CX_CRCMTD9CT, CX_CRCMTD9CTCD, CX_CRCMTD9OTH) %>% rename_all(.funs = funs(sub("9", "", .))) %>% mutate(Regimen = 9),
  GI.CRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_CRCSTDT, CX_CRCMTD10,CX_CRCMTD10CD, CX_CRCMTD10CT, CX_CRCMTD10CTCD, CX_CRCMTD10OTH) %>% rename_all(.funs = funs(sub("10", "", .))) %>% mutate(Regimen = 10));

# GI-SCREEN-NonCRC #
GI.NonCRC.chemo.202008 <- rbind.data.frame(
  read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/ESOCHTHRPY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>% rename_all(.funs = funs(sub("ESO", "", .))),
  read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/HBPCHTHRPY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>% rename_all(.funs = funs(sub("HBP", "", .))),
  read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/GCCHTHRPY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>% rename_all(.funs = funs(sub("GC", "", .))),
  read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/20201005/20201005_SCRUM_1_2/GI-SCREEN-NonCRC/GISTCHTHRPY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>% rename_all(.funs = funs(sub("GIST", "", .))));

GI.NonCRC.chemo.202008.merged <- rbind.data.frame(
  GI.NonCRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT1, CX_TREAT1CD, CX_MTD1, CX_MTD1CD, CX_MTD1CT, CX_MTD1CTCD, CX_MTD1OTH) %>% rename_all(.funs = funs(sub("1", "", .)))  %>% mutate(Regimen = 1),
  GI.NonCRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT2, CX_TREAT2CD, CX_MTD2, CX_MTD2CD, CX_MTD2CT, CX_MTD2CTCD, CX_MTD2OTH) %>% rename_all(.funs = funs(sub("2", "", .)))  %>% mutate(Regimen = 2),
  GI.NonCRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT3, CX_TREAT3CD, CX_MTD3, CX_MTD3CD, CX_MTD3CT, CX_MTD3CTCD, CX_MTD3OTH) %>% rename_all(.funs = funs(sub("3", "", .)))  %>% mutate(Regimen = 3),
  GI.NonCRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT4, CX_TREAT4CD, CX_MTD4, CX_MTD4CD, CX_MTD4CT, CX_MTD4CTCD, CX_MTD4OTH) %>% rename_all(.funs = funs(sub("4", "", .)))  %>% mutate(Regimen = 4),
  GI.NonCRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT5, CX_TREAT5CD, CX_MTD5, CX_MTD5CD, CX_MTD5CT, CX_MTD5CTCD, CX_MTD5OTH) %>% rename_all(.funs = funs(sub("5", "", .)))  %>% mutate(Regimen = 5),
  GI.NonCRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT6, CX_TREAT6CD, CX_MTD6, CX_MTD6CD, CX_MTD6CT, CX_MTD6CTCD, CX_MTD6OTH) %>% rename_all(.funs = funs(sub("6", "", .)))  %>% mutate(Regimen = 6),
  GI.NonCRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT7, CX_TREAT7CD, CX_MTD7, CX_MTD7CD, CX_MTD7CT, CX_MTD7CTCD, CX_MTD7OTH) %>% rename_all(.funs = funs(sub("7", "", .)))  %>% mutate(Regimen = 7),
  GI.NonCRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT8, CX_TREAT8CD, CX_MTD8, CX_MTD8CD, CX_MTD8CT, CX_MTD8CTCD, CX_MTD8OTH) %>% rename_all(.funs = funs(sub("8", "", .)))  %>% mutate(Regimen = 8),
  GI.NonCRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT9, CX_TREAT9CD, CX_MTD9, CX_MTD9CD, CX_MTD9CT, CX_MTD9CTCD, CX_MTD9OTH) %>% rename_all(.funs = funs(sub("9", "", .)))  %>% mutate(Regimen = 9),
  GI.NonCRC.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT10, CX_TREAT10CD, CX_MTD10, CX_MTD10CD, CX_MTD10CT, CX_MTD10CTCD, CX_MTD10OTH) %>% rename_all(.funs = funs(sub("10", "", .)))  %>% mutate(Regimen = 10));


GI.NonCRC.other.chemo.202008 <- read.csv(file =  "/home/yusuke/Data/SCRUM/1-2/clinical/GI-SCREEN-NonCRC/OTHCHTHRPY.csv", skip = 1, header = T, fileEncoding = "UTF-8-BOM") %>% rename_all(.funs = funs(sub("OTH", "", .)));
GI.NonCRC.other.chemo.202008.merged <- rbind.data.frame(
  GI.NonCRC.other.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT1, CX_TREAT1CD, CX_MTD1, CX_MTD1CD, CX_MTD1RT, CX_MTD1RTCD, CX_MTD1CT, CX_MTD1CTCD ,CX_MTD1OTH) %>% rename_all(.funs = funs(sub("1", "", .))),
  GI.NonCRC.other.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT2, CX_TREAT2CD, CX_MTD2, CX_MTD2CD, CX_MTD2RT, CX_MTD2RTCD, CX_MTD2CT, CX_MTD2CTCD ,CX_MTD2OTH) %>% rename_all(.funs = funs(sub("2", "", .))),
  GI.NonCRC.other.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT3, CX_TREAT3CD, CX_MTD3, CX_MTD3CD, CX_MTD3RT, CX_MTD3RTCD, CX_MTD3CT, CX_MTD3CTCD ,CX_MTD3OTH) %>% rename_all(.funs = funs(sub("3", "", .))),
  GI.NonCRC.other.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT4, CX_TREAT4CD, CX_MTD4, CX_MTD4CD, CX_MTD4RT, CX_MTD4RTCD, CX_MTD4CT, CX_MTD4CTCD ,CX_MTD4OTH) %>% rename_all(.funs = funs(sub("4", "", .))),
  GI.NonCRC.other.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT5, CX_TREAT5CD, CX_MTD5, CX_MTD5CD, CX_MTD5RT, CX_MTD5RTCD, CX_MTD5CT, CX_MTD5CTCD ,CX_MTD5OTH) %>% rename_all(.funs = funs(sub("5", "", .))),
  GI.NonCRC.other.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT6, CX_TREAT6CD, CX_MTD6, CX_MTD6CD, CX_MTD6RT, CX_MTD6RTCD, CX_MTD6CT, CX_MTD6CTCD ,CX_MTD6OTH) %>% rename_all(.funs = funs(sub("6", "", .))),
  GI.NonCRC.other.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT7, CX_TREAT7CD, CX_MTD7, CX_MTD7CD, CX_MTD7RT, CX_MTD7RTCD, CX_MTD7CT, CX_MTD7CTCD ,CX_MTD7OTH) %>% rename_all(.funs = funs(sub("7", "", .))),
  GI.NonCRC.other.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT8, CX_TREAT8CD, CX_MTD8, CX_MTD8CD, CX_MTD8RT, CX_MTD8RTCD, CX_MTD8CT, CX_MTD8CTCD ,CX_MTD8OTH) %>% rename_all(.funs = funs(sub("8", "", .))),
  GI.NonCRC.other.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT9, CX_TREAT9CD, CX_MTD9, CX_MTD9CD, CX_MTD9RT, CX_MTD9RTCD, CX_MTD9CT, CX_MTD9CTCD ,CX_MTD9OTH) %>% rename_all(.funs = funs(sub("9", "", .))),
  GI.NonCRC.other.chemo.202008 %>% select(REGNO, SiteName, SiteCode, EventId, EventName, EventDate, ActivityId, ActivityName, CX_ADDINFO, CX_ADDINFOCD, CX_TREAT10, CX_TREAT10CD, CX_MTD10, CX_MTD10CD, CX_MTD10RT, CX_MTD10RTCD, CX_MTD10CT, CX_MTD10CTCD ,CX_MTD10OTH) %>% rename_all(.funs = funs(sub("10", "", .)))
)  


# Unique patients #

Unique.patients.SCRUM <- rbind.data.frame(data.frame(REGNO = c(unique(as.vector(LC.SCRUM.chemo.merged$REGNO)),unique(as.vector(LC.SCRUM.chemo.202008.merged$REGNO))), data = "LC-SCRUM"),
                 data.frame(REGNO = c(unique(as.vector(GI.CRC.chemo.merged$REGNO)),unique(as.vector(GI.CRC.chemo.202008.merged$REGNO))), data = "GI-SCREEN-CRC"),
                 data.frame(REGNO = c(unique(as.vector(GI.NonCRC.chemo.merged$REGNO)),unique(as.vector(GI.NonCRC.chemo.202008.merged$REGNO))), data = "GI-SCREEN-NonCRC"),
                 data.frame(REGNO = c(unique(as.vector(GI.NonCRC.other.chemo.merged$REGNO)),unique(as.vector(GI.NonCRC.other.chemo.202008.merged$REGNO))), data = "GI-SCREEN-NonCRC")) %>%
  mutate(Oct.2020 = case_when(REGNO %in% c(unique(as.vector(LC.SCRUM.chemo.merged$REGNO)), unique(as.vector(GI.CRC.chemo.merged$REGNO)), unique(as.vector(GI.NonCRC.chemo.merged$REGNO)),unique(as.vector(GI.NonCRC.other.chemo.merged$REGNO))) ~ "Y", TRUE ~ "N")) %>%
  mutate(Aug.2020 = case_when(REGNO %in% c(unique(as.vector(LC.SCRUM.chemo.202008.merged$REGNO)), unique(as.vector(GI.CRC.chemo.202008.merged$REGNO)), unique(as.vector(GI.NonCRC.chemo.202008.merged$REGNO)),unique(as.vector(GI.NonCRC.other.chemo.202008.merged$REGNO))) ~ "Y", TRUE ~ "N"));
write.table(Unique.patients.SCRUM, file = "./unique.patients.txt", sep = "\t", row.names = F, quote = F);

Unique.patients.SCRUM %>%
  filter(REGNO %in% Specific.patients$REGNO) %>%
write.table(file = "./for_CSPV/unique.patients.txt", sep = "\t", row.names = F, quote = F);
