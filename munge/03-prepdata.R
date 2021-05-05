

# Create vars PRIOR to imputation (used in imp model) ------

pdata <- pdata %>%
  mutate(
    d_efgroup = factor(case_when(
      num_dcEf < 40 ~ "rEF",
      num_dcEf <= 49 ~ "mrEF",
      num_dcEf >= 50 ~ "pEF"
    ),
    levels = c("pEF", "mrEF", "rEF")
    ),

    d_age_cat = case_when(
      num_age < 65 ~ "<65",
      num_age >= 65 ~ ">=65"
    ),
    d_dmBmi_cat = case_when(
      is.na(num_dmBmi) ~ NA_character_,
      num_dmBmi < 25 ~ "1.<25",
      num_dmBmi >= 25 ~ "2.>=25"
    ),

    d_dmBpm_cat = factor(case_when(
      num_dmBpm <= 80 ~ 1,
      num_dmBpm > 80 ~ 2
    ),
    levels = 1:2,
    labels = c("<=80", ">80")
    ),
    
    d_dmBp1_cat = factor(case_when(
      num_dmBp1 <= 140 ~ 1,
      num_dmBp1 > 140 ~ 2
    ),
    levels = 1:2,
    labels = c("<=140", ">140")
    ),
    
    d_dcBp1_cat = factor(case_when(
      num_dcBp1 < 110 ~ 2,
      num_dcBp1 >= 110 ~ 1
    ),
    levels = 1:2,
    labels = c(">=110", "<110")
    ),

    d_dcSod_cat = factor(case_when(
      num_dcSod < 135 ~ 2,
      num_dcSod >= 135 ~ 1
    ),
    levels = 1:2,
    labels = c(">=135", "<135")
    ),

    d_change_Sod = num_dcSod - num_hsSod,
    d_changepercent_Sod = (num_dcSod - num_hsSod) / num_hsSod * 100,
    d_change_Pot = num_dcPot - num_hsPot,
    d_changepercent_Pot = (num_dcPot - num_hsPot) / num_hsPot * 100,
    d_change_Cre = num_dcCre - num_hsCre,
    d_changepercent_Cre = (num_dcCre - num_hsCre) / num_hsCre * 100,

    # eGFR according to CKD-EPI
    sex = recode(num_dmgender, "Male" = 1, "Female" = 0),
    ethnicity = case_when(
      is.na(num_dmEthnic) ~ NA_real_,
      num_dmEthnic == "Black" ~ 1,
      TRUE ~ 0
    ),
    d_hsCKDEPI = nephro::CKDEpi.creat(num_hsCre, sex, num_age, ethnicity),
    d_dcCKDEPI = nephro::CKDEpi.creat(num_dcCre, sex, num_age, ethnicity),

    d_change_CKDEPI = d_dcCKDEPI - d_hsCKDEPI,
    d_changepercent_CKDEPI = (d_dcCKDEPI - d_hsCKDEPI) / d_hsCKDEPI * 100,

    d_dcCKDEPI_cat = factor(case_when(
      d_dcCKDEPI < 60 ~ 2,
      d_dcCKDEPI >= 60 ~ 1
    ),
    levels = 1:2,
    labels = c(">=60", "<60")
    ),

    d_dcHb_cat = factor(case_when(
      is.na(num_dcHb) | is.na(num_dmgender) ~ NA_real_,
      num_dcHb < 12 & num_dmgender == "Female" | num_dcHb < 13 & num_dmgender == "Male" ~ 2,
      TRUE ~ 1
    ),
    levels = 1:2,
    labels = c(">=12/13(women/men)", "<12/13(women/men)")
    ),

    d_change_Hb = num_dcHb - num_hsHb,
    d_changepercent_Hb = (num_dcHb - num_hsHb) / num_hsHb * 100,

    d_dcBpm_cat = factor(case_when(
      is.na(num_dcBpm) | is.na(num_dcRyth) ~ NA_real_,
      num_dcBpm >= 70 & num_dcRyth %in% c("Sinus", "Other") |
        num_dcBpm >= 80 & num_dcRyth == "Atrial Fibrillation/Flutter" ~ 2,
      TRUE ~ 1
    ),
    levels = 1:2,
    labels = c("<70/80(sinus/af)", ">=70/80(sinus/af)")
    ),

    d_dmHF_history = case_when(
      is.na(num_dmHF) ~ NA_character_,
      num_dmHF %in% c("Yes with previous hospitalisation", "Yes without previous hospitalisation") ~ "Yes",
      TRUE ~ "No"
    ),
    d_dmHF_cat = case_when(
      is.na(num_dmHF) ~ NA_character_,
      num_dmHF == "Yes with previous hospitalisation" ~ "Previous HF hosp",
      TRUE ~ "No previous HF hosp"
    ),
    d_HFdiagnosis = case_when(
      num_dmHF == "No" ~ "<12mo",
      num_dmMonth %in% c("< 6 months", "6 - 12 months") ~ "<12mo",
      num_dmMonth %in% c("> 12 months") ~ ">12mo"
    ),

    num_dmEtio_c1 = relevel(num_dmEtio_c1, ref = "Non-ischemic heart disease"),

    num_dmEtio = factor(case_when(
      num_dmEtio == "Ischemic heart disease documented by coronary angiography" ~ "IHD doc by ca",
      num_dmEtio == "Ischemic heart disease not documented by coronary angiography" ~ "IHD not documented by ca",
      TRUE ~ as.character(num_dmEtio)
    )),

    d_dmDev_cat = case_when(
      is.na(num_dmDev) ~ NA_character_,
      num_dmDev %in% c("No") ~ "1.No",
      num_dmDev %in% c("PM") ~ "2.PM",
      num_dmDev %in% c("CRT-P", "CRT-D", "ICD") ~ "3.CRT/ICD"
    ),

    d_dmSmoking1 = case_when(
      is.na(num_dmSmoking) ~ NA_character_,
      num_dmSmoking == "Current" ~ "2.Current",
      TRUE ~ "1.Never/Former"
    ),
    d_dmSmoking2 = case_when(
      is.na(num_dmSmoking) ~ NA_character_,
      num_dmSmoking %in% c("Current", "Former") ~ "2.Current/Former",
      TRUE ~ "1.Never"
    ),

    d_dmAfib2 = case_when(
      is.na(num_dmAfib) ~ NA_character_,
      num_dmAfib == "No" ~ "No",
      TRUE ~ "Yes"
    ),
    d_dmAfib3 = case_when(
      is.na(num_dmAfib) ~ NA_character_,
      num_dmAfib == "No" ~ "No",
      num_dmAfib == "Permanent" ~ "Permanent",
      TRUE ~ "Persistent/Paroxysmal"
    ),

    # no of non-cardiac comorbs
    d_dmDiab = case_when(
      num_dmDiab == "Newly diagnosed" ~ "Yes",
      TRUE ~ as.character(num_dmDiab)
    ),
    d_dmThy = case_when(
      num_dmThy == "No" ~ "No",
      num_dmThy %in% c("Hypothyroidism", "Hyperthyroidism") ~ "Yes"
    ),
    d_anemia = case_when(
      is.na(num_hsHb) | is.na(num_dmgender) ~ NA_character_,
      num_hsHb < 13 & num_dmgender == "Male" ~ "Yes",
      num_hsHb < 12 & num_dmgender == "Female" ~ "Yes",
      TRUE ~ "No"
    ),

    d_hsHosPresCli = recode_factor(num_hsHosPresCli,
      `Pulmonary edema` = "Pulm edema/Card schock",
      `Cardiogenic schock` = "Pulm edema/Card schock",
      .default = "Other"
    ),
    d_hsHosPresCli = relevel(d_hsHosPresCli, ref = "Other"),

    d_dcRyth = recode(num_dcRyth,
      `Atrial Fibrillation/Flutter` = "Atrial Fibrillation",
      .default = "Other"
    ),

    d_dcCardiov_AF = case_when(
      num_dcCardiov == "Not performed" ~ "No",
      num_dcCardiov == "Performed" & num_dcElca == "No" ~ "No",
      num_dcCardiov == "Performed" & num_dcElca == "Yes" ~ "Yes"
    ),

    d_X_pulmc_alvoedema = case_when(
      num_dcXrn == "Yes" ~ "No",
      is.na(num_dcXpu) | is.na(num_dcXal) ~ NA_character_,
      num_dcXpu == "No" & num_dcXal == "No" ~ "No",
      num_dcXpu == "Yes" | num_dcXal == "Yes" ~ "Yes",
      TRUE ~ NA_character_
    ),

    d_dcNyha_cat = case_when(
      num_dcNyha %in% c("NYHA I", "NYHA II") ~ "I-II",
      num_dcNyha %in% c("NYHA III", "NYHA IV") ~ "III-IV"
    ),

    tmp_dcnyha = case_when(num_dcNyha == "NYHA I" ~ 1,
                           num_dcNyha == "NYHA II" ~ 2,
                           num_dcNyha == "NYHA III" ~ 3,
                           num_dcNyha == "NYHA IV" ~ 4), 
    tmp_hsnyha = case_when(num_hsNyha == "NYHA I" ~ 1,
                           num_hsNyha == "NYHA II" ~ 2,
                           num_hsNyha == "NYHA III" ~ 3,
                           num_hsNyha == "NYHA IV" ~ 4), 
    
    improvment1class_dcNyha = if_else(tmp_dcnyha < tmp_hsnyha, "Yes", "No"),
    
    d_qtcfridericia = ifelse(!num_dcRyth_c1 %in% c("Paced", "Other"), num_dcQt / ((60 / num_dcHr2)^0.33), NA),

    d_bsa = sqrt(num_dmHeight * num_dmWeight / 3600),
    d_LAVI = num_dcLavol / d_bsa,

    d_change_weight = num_dcWeight - num_dmWeight,
    d_changepercent_weight = (num_dcWeight - num_dmWeight) / num_dmWeight * 100,

    d_days_in_hosp = as.numeric(num_dcDischdt - num_dmVisitdt),
    d_days_in_hosp_cat = factor(case_when(
      d_days_in_hosp <= 7 ~ 1,
      d_days_in_hosp > 7 ~ 2
    ),
    levels = 1:2,
    labels = c("<= 7 days", ">7 days")
    ),

    d_loopDiurd = case_when(
      is.na(num_mdDiurd_c2) ~ NA_character_,
      num_mdDiurd_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") |
        num_mdDiur2d_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") ~ "Yes",
      TRUE ~ "No"
    ),

    tmp_dosed1 = case_when(
      num_mdDiurd_c2 == "Flurosemide" ~ num_mdDiurddo / 40 * 40,
      num_mdDiurd_c2 == "Torasemide" ~ num_mdDiurddo / 10 * 40,
      num_mdDiurd_c2 == "Bumetanide" ~ num_mdDiurddo / 1 * 40
    ),

    tmp_dosed2 = case_when(
      num_mdDiur2d_c2 == "Flurosemide" ~ num_mdDiur2ddo / 40 * 40,
      num_mdDiur2d_c2 == "Torasemide" ~ num_mdDiur2ddo / 10 * 40,
      num_mdDiur2d_c2 == "Bumetanide" ~ num_mdDiur2ddo / 1 * 40
    ),

    d_loopDiurddose_eqFurosemide = case_when(
      num_mdDiurd_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") &
        num_mdDiur2d_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") ~ tmp_dosed1 + tmp_dosed2,
      num_mdDiurd_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") ~ tmp_dosed1,
      num_mdDiur2d_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") ~ tmp_dosed2
    ),


    d_arb_or_ace_or_arni = case_when(
      is.na(num_mdACEd) | is.na(num_mdATd) ~ NA_character_,
      num_mdACEd == "Yes" | num_mdATd == "Yes" | num_mdARNId == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    d_ACEdose_eqEnapril = case_when(
      num_mdACEd_c2 == "Ramipril" ~ num_mdACEddo / 10 * 40,
      num_mdACEd_c2 == "Enalapril" ~ num_mdACEddo / 40 * 40,
      # num_mdACEd_c2 == "Perindopril" ~ num_mdACEddo / 4,
      num_mdACEd_c2 == "Captopril" ~ num_mdACEddo / 150 * 40,
      num_mdACEd_c2 == "Lisinopril" ~ num_mdACEddo / 35 * 40,
      # num_mdACEd_c2 == "Fosinopril" ~ num_mdACEddo / 20
    ),

    d_ACEdosetarget = case_when(
      d_ACEdose_eqEnapril < 40 / 2 ~ "1.<50%",
      d_ACEdose_eqEnapril < 40 ~ "2.50-<100%",
      d_ACEdose_eqEnapril >= 40 ~ "3.100%"
    ),

    d_ATdose_eqValsartan = case_when(
      num_mdATd_c2 == "Candesartan" ~ num_mdATddo / 32 * 320,
      num_mdATd_c2 == "Losartan" ~ num_mdATddo / 150 * 320,
      num_mdATd_c2 == "Valsartan" ~ num_mdATddo / 320 * 320
    ),

    d_ATdosetarget = case_when(
      d_ATdose_eqValsartan < 320 / 2 ~ "1.<50%",
      d_ATdose_eqValsartan < 320 ~ "2.50-<100%",
      d_ATdose_eqValsartan >= 320 ~ "3.100%"
    ),

    d_BBdose_eqBisoprolol = case_when(
      num_mdBBd_c2 == "Bisoprolol" ~ num_mdBBddo / 10 * 10,
      num_mdBBd_c2 == "Metoprolol" ~ num_mdBBddo / 200 * 10,
      num_mdBBd_c2 == "Nebivolol" ~ num_mdBBddo / 10 * 10,
      num_mdBBd_c2 == "Carvedilol" ~ num_mdBBddo / 50 * 10
    ),

    d_BBdosetarget = case_when(
      d_BBdose_eqBisoprolol < 10 / 2 ~ "1.<50%",
      d_BBdose_eqBisoprolol < 10 ~ "2.50-<100%",
      d_BBdose_eqBisoprolol >= 10 ~ "3.100%"
    ),

    # Meds at follow-up

    d_f1Diurh = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1Diurh)
    ),

    d_f1loopDiurh = case_when(
      num_f1MedAny == "No" ~ "No",
      is.na(num_f1Diurh_c2) ~ NA_character_,
      num_f1Diurh_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") |
        num_f1Diur2h_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") ~ "Yes",
      TRUE ~ "No"
    ),

    d_f1f1ALh = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1ALh)
    ),

    d_f1ACEh = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1ACEh)
    ),

    d_f1ACEhdose_eqEnapril = case_when(
      num_f1ACEh_c2 == "Ramipril" ~ num_f1ACEhdo / 10 * 40,
      num_f1ACEh_c2 == "Enalapril" ~ num_f1ACEhdo / 40 * 40,
      num_f1ACEh_c2 == "Captopril" ~ num_f1ACEhdo / 150 * 40,
      num_f1ACEh_c2 == "Lisinopril" ~ num_f1ACEhdo / 35 * 40,
    ),

    d_f1ACEdosetarget = case_when(
      d_f1ACEhdose_eqEnapril < 40 / 2 ~ "1.<50%",
      d_f1ACEhdose_eqEnapril < 40 ~ "2.50-<100%",
      d_f1ACEhdose_eqEnapril >= 40 ~ "3.100%"
    ),

    d_f1ATh = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1ATh)
    ),

    d_f1ATdose_eqValsartan = case_when(
      num_f1ATh_c2 == "Candesartan" ~ num_f1AThdo / 32 * 320,
      num_f1ATh_c2 == "Losartan" ~ num_f1AThdo / 150 * 320,
      num_f1ATh_c2 == "Valsartan" ~ num_f1AThdo / 320 * 320
    ),

    d_f1ATdosetarget = case_when(
      d_f1ATdose_eqValsartan < 320 / 2 ~ "1.<50%",
      d_f1ATdose_eqValsartan < 320 ~ "2.50-<100%",
      d_f1ATdose_eqValsartan >= 320 ~ "3.100%"
    ),

    d_f1ARNI = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1ARNI)
    ),
    d_f1_arb_or_ace_or_arni = case_when(
      is.na(d_f1ACEh) | is.na(d_f1ATh) ~ NA_character_,
      d_f1ACEh == "Yes" | d_f1ATh == "Yes" | d_f1ARNI == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    d_f1BBh = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1BBh)
    ),

    d_f1BBdose_eqBisoprolol = case_when(
      num_f1BBh_c2 == "Bisoprolol" ~ num_f1BBhdo / 10 * 10,
      num_f1BBh_c2 == "Metoprolol" ~ num_f1BBhdo / 200 * 10,
      num_f1BBh_c2 == "Nebivolol" ~ num_f1BBhdo / 10 * 10,
      num_f1BBh_c2 == "Carvedilol" ~ num_f1BBhdo / 50 * 10
    ),

    d_f1BBdosetarget = case_when(
      d_f1BBdose_eqBisoprolol < 10 / 2 ~ "1.<50%",
      d_f1BBdose_eqBisoprolol < 10 ~ "2.50-<100%",
      d_f1BBdose_eqBisoprolol >= 10 ~ "3.100%"
    ),

    d_f1Ccbh = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1Ccbh)
    ),
    d_f1Nith = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1Nith)
    ),
    d_f1Ivabh = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1Ivabh)
    ),
    d_f1Digoh = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1Digoh)
    ),
    d_f1Amih = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1Amih)
    ),

    d_change_f1Nt = num_f1Nt - num_hsNt,
    d_changepercent_f1Nt = (num_f1Nt - num_hsNt) / num_hsNt * 100,
    d_change_f1Bnp = num_f1Bnp - num_hsBnp,
    d_changepercent_f1Bnp = (num_f1Bnp - num_hsBnp) / num_hsBnp * 100,

    d_change_dcNt = num_dcNt - num_hsNt,
    d_changepercent_dcNt = (num_dcNt - num_hsNt) / num_hsNt * 100,
    d_change_dcBnp = num_dcBnp - num_hsBnp,
    d_changepercent_dcBnp = (num_dcBnp - num_hsBnp) / num_hsBnp * 100,

    d_either_hsNtBnp = coalesce(num_hsNt, num_hsBnp),
    d_either_dcNtBnp = coalesce(num_dcNt, num_dcBnp),
    
    d_change_either_dcNtBnp = d_either_dcNtBnp - d_either_hsNtBnp,
    d_changepercent_either_dcNtBnp = (d_either_dcNtBnp - d_either_hsNtBnp) / d_either_hsNtBnp * 100,
    
    # Outcomes
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = coalesce(num_dcDischdt, num_dmVisitdt),

    outtime_death = as.numeric(enddtm - startdtm),

    out_death = case_when(
      num_f1vital == "Alive" ~ 0,
      num_f1vital == "Dead" ~ 1
    ),

    out_deathcv = case_when(
      is.na(out_death) ~ NA_real_,
      num_f1DeathCs %in% c("Cardiac", "Vascular") ~ 1,
      TRUE ~ 0
    ), # pats with missing info are NOT included in CV

    out_deathcv_exclhf = case_when(
      is.na(out_death) ~ NA_real_,
      num_f1DeathCs %in% c("Cardiac", "Vascular") &
        (num_f1DthCa != "Heart Failure" | is.na(num_f1DthCa)) ~ 1,
      TRUE ~ 0
    ),

    out_deathnoncv = case_when(
      is.na(out_death) ~ NA_real_,
      num_f1DeathCs == c("Non cardiovascular") ~ 1,
      TRUE ~ 0
    ), # pats with missing info are NOT included in nonCV

    out_deathhf = case_when(
      is.na(out_death) ~ NA_real_,
      num_f1DthCa == "Heart Failure" ~ 1,
      TRUE ~ 0
    ),

    out_deathscd = case_when(
      is.na(out_death) ~ NA_real_,
      num_f1DeathCs == "Cardiac" & num_f1DthCaMd == "Sudden" ~ 1,
      TRUE ~ 0
    ),

    out_deathunknown = case_when(
      is.na(out_death) ~ NA_real_,
      is.na(num_f1DeathCs) & num_f1vital == "Dead" ~ 1,
      TRUE ~ 0
    ),

    # All-cause hosp
    out_hosp = case_when(
      num_f1lost != "No" ~ NA_real_,
      num_f1hosp1 == "Yes" |
        num_f1hosp2 == "Yes" |
        num_f1hosp3 == "Yes" |
        num_f1hosp4 == "Yes" |
        num_f1hosp5 == "Yes" ~ 1,
      TRUE ~ 0
    ),
    out_hospdtm = coalesce(
      num_f1hosp1dt, num_f1hosp2dt, num_f1hosp3dt,
      num_f1hosp4dt, num_f1hosp5dt
    ),
    outtime_hosp = as.numeric(out_hospdtm - startdtm),
    outtime_hospmissing = case_when(
      out_hosp == 1 & is.na(outtime_hosp) ~ 1,
      out_hosp == 1 ~ 0
    ),
    outtime_hosp = ifelse(out_hosp == 1 & is.na(outtime_hosp), outtime_death / 2, outtime_hosp),
    outtime_hosp = pmin(outtime_hosp, outtime_death, na.rm = TRUE),

    # CV
    out_hospcv = case_when(
      num_f1lost != "No" ~ NA_real_,
      num_f1hosp1cs %in% c("Cardiac, non HF", "HF", "Vascular") |
        num_f1hosp2cs %in% c("Cardiac, non HF", "HF", "Vascular") |
        num_f1hosp3cs %in% c("Cardiac, non HF", "HF", "Vascular") |
        num_f1hosp4cs %in% c("Cardiac, non HF", "HF", "Vascular") |
        num_f1hosp5cs %in% c("Cardiac, non HF", "HF", "Vascular") ~ 1,
      TRUE ~ 0
    ),
    out_hospcvdtm = case_when(
      num_f1hosp1cs %in% c("Cardiac, non HF", "HF", "Vascular") ~ num_f1hosp1dt,
      num_f1hosp2cs %in% c("Cardiac, non HF", "HF", "Vascular") ~ num_f1hosp2dt,
      num_f1hosp3cs %in% c("Cardiac, non HF", "HF", "Vascular") ~ num_f1hosp3dt,
      num_f1hosp4cs %in% c("Cardiac, non HF", "HF", "Vascular") ~ num_f1hosp4dt,
      num_f1hosp5cs %in% c("Cardiac, non HF", "HF", "Vascular") ~ num_f1hosp5dt
    ),
    outtime_hospcv = as.numeric(out_hospcvdtm - startdtm),
    outtime_hospcv = ifelse(out_hospcv == 1 & is.na(outtime_hospcv), outtime_death / 2, outtime_hospcv),
    outtime_hospcv = pmin(outtime_hospcv, outtime_death, na.rm = TRUE),

    # CV excl HF
    out_hospcv_exclhf = case_when(
      num_f1lost != "No" ~ NA_real_,
      num_f1hosp1cs %in% c("Cardiac, non HF", "Vascular") |
        num_f1hosp2cs %in% c("Cardiac, non HF", "Vascular") |
        num_f1hosp3cs %in% c("Cardiac, non HF", "Vascular") |
        num_f1hosp4cs %in% c("Cardiac, non HF", "Vascular") |
        num_f1hosp5cs %in% c("Cardiac, non HF", "Vascular") ~ 1,
      TRUE ~ 0
    ),
    out_hospcv_exclhfdtm = case_when(
      num_f1hosp1cs %in% c("Cardiac, non HF", "Vascular") ~ num_f1hosp1dt,
      num_f1hosp2cs %in% c("Cardiac, non HF", "Vascular") ~ num_f1hosp2dt,
      num_f1hosp3cs %in% c("Cardiac, non HF", "Vascular") ~ num_f1hosp3dt,
      num_f1hosp4cs %in% c("Cardiac, non HF", "Vascular") ~ num_f1hosp4dt,
      num_f1hosp5cs %in% c("Cardiac, non HF", "Vascular") ~ num_f1hosp5dt
    ),
    outtime_hospcv_exclhf = as.numeric(out_hospcv_exclhfdtm - startdtm),
    outtime_hospcv_exclhf = ifelse(out_hospcv_exclhf == 1 & is.na(outtime_hospcv_exclhf), outtime_death / 2, outtime_hospcv_exclhf),
    outtime_hospcv_exclhf = pmin(outtime_hospcv_exclhf, outtime_death, na.rm = TRUE),

    # HF hosp
    out_hosphf = case_when(
      num_f1lost != "No" ~ NA_real_,
      num_f1hosp1cs == "HF" |
        num_f1hosp2cs == "HF" |
        num_f1hosp3cs == "HF" |
        num_f1hosp4cs == "HF" |
        num_f1hosp5cs == "HF" ~ 1,
      TRUE ~ 0
    ),
    out_hosphfdtm = case_when(
      num_f1hosp1cs == "HF" ~ num_f1hosp1dt,
      num_f1hosp2cs == "HF" ~ num_f1hosp2dt,
      num_f1hosp3cs == "HF" ~ num_f1hosp3dt,
      num_f1hosp4cs == "HF" ~ num_f1hosp4dt,
      num_f1hosp5cs == "HF" ~ num_f1hosp5dt
    ),
    outtime_hosphf = as.numeric(out_hosphfdtm - startdtm),
    outtime_hosphf = ifelse(out_hosphf == 1 & is.na(outtime_hosphf), outtime_death / 2, outtime_hosphf),
    outtime_hosphf = pmin(outtime_hosphf, outtime_death, na.rm = TRUE),

    # Non-CV
    out_hospnoncv = case_when(
      num_f1lost != "No" ~ NA_real_,
      num_f1hosp1cs %in% c("Non CV", "Renal dysfunction") |
        num_f1hosp2cs %in% c("Non CV", "Renal dysfunction") |
        num_f1hosp3cs %in% c("Non CV", "Renal dysfunction") |
        num_f1hosp4cs %in% c("Non CV", "Renal dysfunction") |
        num_f1hosp5cs %in% c("Non CV", "Renal dysfunction") ~ 1,
      TRUE ~ 0
    ),
    out_hospnoncvdtm = case_when(
      num_f1hosp1cs %in% c("Non CV", "Renal dysfunction") ~ num_f1hosp1dt,
      num_f1hosp2cs %in% c("Non CV", "Renal dysfunction") ~ num_f1hosp2dt,
      num_f1hosp3cs %in% c("Non CV", "Renal dysfunction") ~ num_f1hosp3dt,
      num_f1hosp4cs %in% c("Non CV", "Renal dysfunction") ~ num_f1hosp4dt,
      num_f1hosp5cs %in% c("Non CV", "Renal dysfunction") ~ num_f1hosp5dt
    ),
    outtime_hospnoncv = as.numeric(out_hospnoncvdtm - startdtm),
    outtime_hospnoncv = ifelse(out_hospnoncv == 1 & is.na(outtime_hospnoncv), outtime_death / 2, outtime_hospnoncv),
    outtime_hospnoncv = pmin(outtime_hospnoncv, outtime_death, na.rm = TRUE),

    # all-cause death or hf hosp
    out_deathhosphf = ifelse(out_hosphf == 1, 1, out_death),
    # cv death or hf hosp
    out_deathcvhosphf = ifelse(out_hosphf == 1, 1, out_deathcv)
  ) %>%
  mutate(d_no_noncardiac_comorbs = rowSums(select(
    ., num_dmStroke, num_dmPvd, num_dmVte, d_dmDiab,
    num_dmHyChol, num_dmCopd, num_dmApn,
    num_dmHepa, d_dmThy,
    num_dmDis, num_dmDepr,
    num_dmPark, num_dmRheu,
    d_anemia
  ) == "Yes")) %>%
  mutate_if(is.character, as.factor) %>%
  select(-starts_with("tmp_"))
