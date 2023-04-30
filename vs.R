# install.packages("readxl")
# install.packages("openxlsx")
# install.packages("dplyr")
# install.packages("lubridate")
library(lubridate)
library(readxl)
library(openxlsx)
library(dplyr)
library(data.table)

raw_vs <- read_xlsx("raw_vs.xlsx")
sds_dm <- read_xlsx("sds_dm.xlsx")
sds_ds <- read_xlsx("sds_ds.xlsx")
sds_suppdm <- read_xlsx("sds_suppdm.xlsx")
sds_sv <- read_xlsx("sds_sv.xlsx")

#Where not missing vstestcd
raw_vs <- raw_vs[complete.cases(raw_vs$VSTESTCD), ]

#Keep only usubjid and rfstdtc column
sds_dm1<-sds_dm
sds_dm <- select(sds_dm, USUBJID, RFSTDTC)

# Merge the datasets based on the "USUBJID" variable
vs_der <- merge(raw_vs, sds_dm, by = "USUBJID")

#Generate necessary columns 
vs_der <- vs_der %>%
  mutate(RFSTDT = if_else(nchar(RFSTDTC) >= 10, as.Date(substr(RFSTDTC, 1, 10), format = "%Y-%m-%d"), NA),
         VSDT = if_else(nchar(VSDTC) >= 10, as.Date(substr(VSDTC, 1, 10), format = "%Y-%m-%d"), NA),
         VSDY = if_else(is.na(RFSTDT), NA_integer_, VSDY),
         VSORRESU = if_else(VSTESTCD == "WSTAVG" & !is.na(VSORRES), "cm", VSORRESU),
         VSSTRESU = if_else(VSTESTCD == "WSTAVG" & !is.na(VSORRES), "cm", VSSTRESU)) 

#Getting Re-screening date
intsc <- sds_suppdm %>%
  filter(QNAM == "PREVSCRN")

#Sort sds_ds dataset
intsc1 <- sds_ds %>%
  filter(DSCAT == "PROTOCOL MILESTONE" & DSDECOD == "INFORMED CONSENT OBTAINED") %>%
  arrange(USUBJID, DSDTC)

# Merge instc & instcdatasets
scrn <- inner_join(intsc, intsc1, by = "USUBJID")


scrn1 <- scrn %>%
  arrange(USUBJID, DSDTC) %>%
  group_by(USUBJID) %>%
  slice_tail(n = 1) %>%
  mutate(rscrn_dt = if_else(nchar(DSSTDTC) >= 10, as.Date(substr(DSSTDTC, 1, 10), format = "%Y-%m-%d"), NA)) %>%
  filter(!is.na(rscrn_dt))%>%
  select(USUBJID, rscrn_dt)


# Getting Early discontinued subjects  from SV
sv <- sds_sv %>%
  mutate(svstdt = ifelse(nchar(SVSTDTC) >= 10, as.Date(substr(SVSTDTC, 1, 10), format = "%Y-%m-%d"), NA),
         svendt = ifelse(nchar(SVENDTC) >= 10, as.Date(substr(SVENDTC, 1, 10), format = "%Y-%m-%d"), NA)) %>%
  select(USUBJID, SVSTDTC, SVENDTC, svstdt, svendt, VISITNUM)

sv_erly_disc_ <- sv %>%
  filter(!is.na(svstdt) & VISITNUM %in% c(11, 12)) %>%
  arrange(desc(svstdt)) %>%
  select(USUBJID, svstdt, svendt)

sv_erly_disc <- sv_erly_disc_ %>%
  mutate(sv_erly_disc = pmax(svstdt, svendt))

sv_erly_disc <- sv_erly_disc %>%
  arrange(USUBJID, svstdt, svendt)

sv_erly_disc <- sv_erly_disc %>%
  arrange(USUBJID, svstdt, svendt) %>%
  group_by(USUBJID) %>%
  slice_tail(n = 1)

# Getting Informed consent date, Study drug start and end date from DM
dm <- sds_dm1 %>%
  arrange(USUBJID) %>%
  select(USUBJID, RFICDTC, RFSTDTC, RFENDTC)

# Getting Latest DSSTDTC from DS
disp <- sds_ds %>%
  filter(EPOCH == 'STUDY-CORE', DSSTDTC != '') %>%
  arrange(USUBJID, desc(DSSTDTC))

# Rename the epoch column to epoch_ in the disp data frame
disp <- disp %>% rename(epoch_ = EPOCH)

# Keep only the first row for each usubjid, and convert dsstdtc to a Date object
final_ds <- disp %>%
  group_by(USUBJID) %>%
  slice_head(n = 1) %>%
  mutate(final_dt = as.Date(substring(DSSTDTC, 1, 10), format = '%Y-%m-%d')) %>%
  select(USUBJID, DSCAT, DSSCAT, DSTERM, DSDECOD, epoch_, DSSTDTC, final_dt)

# Merge the four data frames together by usubjid
temp <- merge(dm, scrn1, by = "USUBJID", all = TRUE)
temp <- merge(temp, sv_erly_disc, by = "USUBJID", all = TRUE)
temp <- merge(temp, final_ds, by = "USUBJID", all = TRUE)


disp1 <- sds_ds %>% 
  filter(DSDECOD %in% 'INFORMED CONSENT OBTAINED' & DSSEQ == 2) %>% 
  arrange(USUBJID, DSDECOD, DSSEQ) %>% 
  select(DSSTDTC, USUBJID) %>% 
  rename(dsstdtc1 = DSSTDTC)


# Epoch Derivation
vs_der1 <- right_join(temp, vs_der, by = "USUBJID")
vs_der1  <- left_join(vs_der1,disp1,by="USUBJID")

vs_der1$rficdt <- ymd(substr(vs_der1$RFICDTC, 1, 10))
vs_der1$rfstdt <- ymd(substr(vs_der1$RFSTDTC.x, 1, 10))
vs_der1$rfendt <- ymd(substr(vs_der1$RFENDTC, 1, 10))
vs_der1$ref_dt <- ymd(substr(vs_der1$VSDTC, 1, 10))
vs_der1$rficdt1 <- ymd(substr(vs_der1$dsstdtc1, 1, 10))

# vs_der1 <- vs_der1 %>%
#   mutate(epoch = ifelse(!is.na(ref_dt),
#                         ifelse(NA < rficdt & rficdt <= ref_dt & ref_dt < rscrn_dt, "INITIAL SCREENING",
#                                ifelse(NA < rficdt & rficdt <= ref_dt & ref_dt < rfstdt, "INITIAL SCREENING",
#                                       ifelse(is.na(rfstdt) & NA < rficdt & rficdt <= ref_dt & ref_dt < rficdt1 & !is.na(rficdt1), "INITIAL SCREENING",
#                                              ifelse(is.na(rfstdt) & NA < rficdt & rficdt <= ref_dt, "INITIAL SCREENING", NA_character_)
#                                       )
#                                )
#                         ),
#                         NA_character_
#   ))



vs_der1 <- vs_der1 %>%
  mutate(epoch = ifelse(!is.na(ref_dt), 
                        ifelse((is.na(rscrn_dt) | (rficdt <= rscrn_dt)) & (rficdt <= ref_dt) & (ref_dt < rfstdt | is.na(rfstdt)), "INITIAL SCREENING",
                               ifelse((rficdt < rscrn_dt) & (rscrn_dt <= ref_dt) & (ref_dt <= rfstdt | is.na(rfstdt)), "RE-SCREENING",
                                      ifelse(!is.na(rfendt) & (rfstdt <= ref_dt) & (ref_dt <= rfendt), "TREATMENT",
                                             ifelse(is.na(rfendt) & !is.na(final_dt) & (rfstdt <= ref_dt) & (ref_dt <= final_dt), "TREATMENT",
                                                    ifelse(is.na(rfendt) & is.na(final_dt) & (rfstdt <= ref_dt), "TREATMENT",
                                                           ifelse((!is.na(rfendt) & ref_dt > rfendt) | (!is.na(sv_erly_disc) & ref_dt > sv_erly_disc) | (!is.na(final_dt) & ref_dt > final_dt), "FOLLOW-UP", NA_character_)
                                                    )
                                             )
                                      )
                               )
                        ),
                        NA_character_
  ),
  .keep = "unused")



# Baseline Flag derivation
vs_base0 <- vs_der1 %>%
  filter(0 < VSDT & VSDT <= RFSTDT & !is.na(VSSTRESC)) %>%
  arrange(USUBJID, VSCAT, VSTESTCD, VSDT, VSSPID)


vs_base1 <- vs_base0 %>%
  arrange(VSTESTCD,USUBJID, VSCAT, VSDT, VSSPID) %>%
  group_by(VSTESTCD) %>%
  mutate(vsBLFL = 'Y') %>%
  slice_tail(n = 1) %>% 
  select(USUBJID, VSSEQ, VSCAT, VSTESTCD, VSGRPID, VSSPID, VSDT, vsBLFL)

vs_der1 <- vs_der1 %>%
  arrange(USUBJID, VSSEQ, VSCAT, VSTESTCD, VSGRPID, VSSPID, VSDT)

vs <- left_join(vs_der1, vs_base1, by = c("USUBJID", "VSSEQ", "VSCAT", "VSTESTCD", "VSGRPID", "VSSPID", "VSDT"))
vs$vsorresu[is.na(vs$VSORRES)] <- ""

vs <- vs[order(vs$USUBJID, vs$VSSEQ), ]
vs <- vs[, !names(vs) %in% "epoch_", drop = FALSE]
colnames(vs)[colnames(vs) == "RFSTDTC.x"] <- "RFSTDTC"

