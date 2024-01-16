option compress=yes;
/*options symbolgen mlogic mprint;*/
/*options nosymbolgen nomlogic nomprint;*/

%let path_work=E:\DataAnalysis\DiscSod;
/*%let path_work=\\Client\E$\DataAnalysis\DiscSod;*/

libname cprd "E:\Data\CPRD_GOLD_202307";
libname a "&path_work.\Input"; *Input;
libname b "E:\Data\Resources"; *Dictionary, Sources;
libname x "&path_work.\Temp"; *Temporary Data;
libname y "&path_work.\Save"; *Data Set for Further Analysis;
libname z "&path_work.\Output"; *Save Results for Export;

/********************************************/
/* Run the macro code (SAS_MACRO.sas) first */
/********************************************/

/***************************/
/* Disease, drug code list */
/***************************/

/* Dictionary */
*GOLD;
%IMPORT_TXT (
path=E:\Data\Resources\Dictionary\CPRD_CodeBrowser_202212_GOLD
, filename=product
, lib=work
, data_name=gold
, var_name_list=prodcode	dmdcode	gemscriptcode	therapyevents	productname	ingredient	strength	formulation	routeofadministration	bnfchapter	bnftext	
, var_format_list=$8. $32. $32. best32. $148. $148. $22. $23. $49. $17. $50. $13. $32.
, delimiter='09'x
);
data b.dict_rx_gold; set gold; run;

data dict_1;
set gold;
prod=lowcase(productname);
ingr=lowcase(ingredient);
keep prodcode productname ingredient prod ingr;
run;
data dict_2_1 (keep=prodcode prod) dict_2_2 (keep=prodcode ingr);
set dict_1;
run;
data dict_3;
set dict_2_1 (rename=(prod=term)) dict_2_2(rename=(ingr=term));
run;
data b.dict_rx_comb_gold; set dict_3; run;

data split; set b.dict_rx_comb_gold; run;
%split (indata=split, outdata=split, symbol=" ", col_name=term, keep_var_list=prodcode);
%split (indata=split, outdata=split, symbol="(", col_name=term, keep_var_list=prodcode);
%split (indata=split, outdata=split, symbol=")", col_name=term, keep_var_list=prodcode);
%split (indata=split, outdata=split, symbol="/", col_name=term, keep_var_list=prodcode);
%split (indata=split, outdata=split, symbol=",", col_name=term, keep_var_list=prodcode);
%split (indata=split, outdata=split, symbol=".", col_name=term, keep_var_list=prodcode);
data b.dict_rx_split_gold; set split; run;

%IMPORT_TXT (
path=E:\Data\Resources\Dictionary\CPRD_CodeBrowser_202212_GOLD
, filename=medical_name
, lib=work
, data_name=gold_dx
, var_name_list=medcode	readcode	ClinicalEvents	ReferralEvents	TestEvents	ImmunisationEvents	ReadTerm	DatabaseBuild
, var_format_list=$20. $7. best32. best32. best32. best32. $128. $32.
, delimiter='09'x
);
data b.dict_dx_gold; set gold_dx; run;

%IMPORT_TXT (
path=E:\Data\Resources\GOLD_2307_Lookups
, filename=common_dosages
, lib=work
, data_name=dosage
, var_name_list=dosageid	dosage_text	daily_dose	dose_number	dose_unit	dose_frequency	dose_interval	choice_of_dose	dose_max_average	change_dose	dose_duration	
, var_format_list=$64. $25. 8. 8. $1. 8. 8. 8. 8. 8. 8.
, delimiter='09'x
);
data b.lookup_rxdosage_gold; set dosage; run;

*Aurum;
%IMPORT_TXT (
path=E:\Data\Resources\Dictionary\CPRD_CodeBrowser_202211_Aurum
, filename=CPRDAurumProduct
, lib=work
, data_name=aurum
, var_name_list=ProdCodeId	dmdid	TermfromEMIS	ProductName	Formulation	RouteOfAdministration	DrugSubstanceName	SubstanceStrength	BNFChapter	DrugIssues
, var_format_list=$18. $19. $67. $47. $22. $11. $126. $159. best32. best32.
, delimiter='09'x
);
data b.dict_rx_aurum; set aurum; run;

data dict_0;
set aurum;
format PRODCODEID 17.;
bnfchapter2=left(bnfchapter);
if length(trim(bnfchapter2))^=8 then do;
if length(trim(bnfchapter2))=7 then bnfchapter3=cat("0",trim(bnfchapter2));
else if length(trim(bnfchapter2))=6 then bnfchapter3=cat("00",trim(bnfchapter2));
else if length(trim(bnfchapter2))=5 then bnfchapter3=cat("000",trim(bnfchapter2));
else if length(trim(bnfchapter2))=4 then bnfchapter3=cat("0000",trim(bnfchapter2)); end;
else bnfchapter3=bnfchapter2;
drop bnfchapter bnfchapter2;
rename bnfchapter3=bnfchapter;
run;
data dict_1;
set dict_0;
term1=lowcase(termfromemis);
term2=lowcase(productname);
term3=lowcase(drugsubstancename);
run;
data dict_2_1 (keep=prodcodeid term1) 
	dict_2_2 (keep=prodcodeid term2)
	dict_2_3 (keep=prodcodeid term3);
set dict_1;
if term1^='' then output dict_2_1;
if term2^='' then output dict_2_2;
if term3^='' then output dict_2_3;
run;
data dict_3;
length term $148;
set dict_2_1(rename=(term1=term)) 
	dict_2_2(rename=(term2=term))
	dict_2_3(rename=(term3=term));
run;
data b.dict_rx_comb_aurum; set dict_3; run;

data split; set b.dict_rx_comb_aurum; run;
%split (indata=split, outdata=split, symbol=" ", col_name=term, keep_var_list=prodcodeid);
%split (indata=split, outdata=split, symbol="(", col_name=term, keep_var_list=prodcodeid);
%split (indata=split, outdata=split, symbol=")", col_name=term, keep_var_list=prodcodeid);
%split (indata=split, outdata=split, symbol="/", col_name=term, keep_var_list=prodcodeid);
%split (indata=split, outdata=split, symbol=",", col_name=term, keep_var_list=prodcodeid);
%split (indata=split, outdata=split, symbol=".", col_name=term, keep_var_list=prodcodeid);
data b.dict_rx_split_aurum; set split; run;

%IMPORT_TXT (
path=E:\Data\Resources\Dictionary\CPRD_CodeBrowser_202211_Aurum
, filename=CPRDAurumMedical
, lib=work
, data_name=aurum_dx
, var_name_list=MedCodeId	Observations	OriginalReadCode	CleansedReadCode	Term	SnomedCTConceptId	SnomedCTDescriptionId	Release	EmisCodeCategoryId
, var_format_list=$18. $32. $32. $32. $128. $32. $32. $32. $32.
, delimiter='09'x
);
data b.dict_dx_aurum; set aurum_dx; run;

/* Disease codes: Read, SNOMED, ICD10 */
*Import: hypertension (gold);
%IMPORT_TXT (
path=&path_work.\Input 
, filename=GOLD_HTN
, lib=x
, data_name=list_dx_gold_ext
, var_name_list=code code_attributes 
, var_format_list=$5. $256.
, delimiter='09'x
);
*Import: hypertension (aurum);
%IMPORT_TXT (
path=&path_work.\Input
, filename=Aurum_HTN
, lib=x
, data_name=list_dx_aurum_ext
, var_name_list=medcodeid term hypertens
, var_format_list=$20. $256. $1.
, delimiter=','
);
*Import: others;
%IMPORT_XLSX (
path=&path_work.\Input
, filename=Index
, lib=work
, data_name=temp
, sheet=DX
);

*Gold;
data dx_gd_1;
set temp;
if cd_sys="Read code";
if length(trim(var_cd))^=7 then do;
	if length(trim(var_cd))=4 then readcode=cat(trim(var_cd),".00");
	else if length(trim(var_cd))=5 then readcode=cat(trim(var_cd),".0");
end;
else readcode=trim(var_cd);
drop id var var_cd cd_sys;
if readcode^="";
run;

proc sql;
create table dx_gd_2 as
select distinct a.type_id, a.info, b.medcode as code, b.readterm as term, "gold" as code_sys
from dx_gd_1 as a
left join b.dict_dx_gold as b on a.readcode=b.readcode
where a.type_id^="PRE_INCL_2";
quit;
proc sql;
create table dx_gd_3 as 
select distinct "PRE_INCL_2" as type_id, "Hypertension" as info, a.code, b.readterm as term, "gold" as code_sys
from x.list_dx_gold_ext as a
left join b.dict_dx_gold as b on a.code=b.medcode;
quit;
data dx_gd_4; set dx_gd_2 dx_gd_3; run;

data x.list_dx_gold; set dx_gd_4; run;
/*proc freq data=x.list_dx_gold; table type_id; run;*/

* Aurum by Chengsheng;
data temp;
set x.list_dx_aurum_cheng;
keep type_id info medcodeid code_sys;
rename medcodeid=code;
run;
data x.list_dx_aurum; set temp; run;

* HES;
data dx_hes_1;
set temp;
if cd_sys="ICD10";
if var_cd^="";
code=trim(var_cd);
drop id var var_cd cd_sys;
code_sys="hes";
run;
data x.list_dx_hes; set dx_hes_1; run;

* Put together (Gold/Aurum/HES);
data y.list_dx; set x.list_dx_aurum x.list_dx_gold (drop=term) x.list_dx_hes ; run;

/* Drug (product) codes: GOLD, AURUM */
*Import: Sodium-containing drugs (Gold);
%IMPORT_TXT (
path=&path_work.\Input
, filename=GOLD_SodDrug
, lib=x
, data_name=list_rx_gold_ext 
, var_name_list=prodcode dmdcode gmescriptcode therapyevents productname ingredient strength formulation routeofadministration bnfchapter VAR12 VAR13
, var_format_list=$5. $32. $16. $16. $256. $256. $32. $32. $32. $10. $253. $16. $8.
, delimiter='09'x
);
*Import: Sodium-containing drugs (Aurum);
%IMPORT_TXT (
path=&path_work.\Input
, filename=Aurum_SodDrug 
, lib=x
, data_name=list_rx_aurum_ext 
, var_name_list=ProdCodeId	dmdid	Term_from_EMIS	ProductName	Formulation	RouteOfAdministration	DrugSubstanceName	SubstanceStrength	BNFChapter	DrugIssues
, var_format_list=$18. $19. $255. $999. $999. $999. $999. $999. $999. $100.
, delimiter='09'x
);
*Import: others;
%IMPORT_XLSX (
path=&path_work.\Input
, filename=Index
, lib=work
, data_name=temp
, sheet=RX
);

data rx_sod_0 rx_oth_0;
set temp;
if e^="drop";
drop id e f order;
ingr=trim(active_ingredient);
if substr(type_id,1,5)="SOD_0" then output rx_sod_0;
if substr(type_id,1,3)^="SOD" then output rx_oth_0;
run;

*Gold;
**Sodium-cotaining drugs and standard formulations;
proc sql;
create table rx_sod_gd_1 as
select distinct a.type_id, a.info, a.ingr, b.prodcode as code, b.term as term, "gold" as code_sys
from rx_sod_0 as a
left join b.dict_rx_split_gold as b on a.ingr=b.term
where ingr not in ("vitamin c", "phosphate", "calcium");
quit;
/*proc freq data=rx_sod_gd_1; table ingr; run; *vitamin C, phosphate, calcium;*/

data temp_1;
set b.dict_rx_comb_gold;
if find(term, "vitamin c")>0;
run;
proc sql;
create table rx_sod_gd_2 as
select distinct a.type_id, a.info, a.ingr, b.prodcode as code, b.term, "gold" as code_sys
from rx_sod_0 as a
left join temp_1 as b on a.ingr="vitamin c";
quit;

data temp_2;
set b.dict_rx_comb_gold;
if (find(term, "phosphate")>0 and find(term, "hydrogen")>0);
run;
proc sql;
create table rx_sod_gd_3 as
select distinct a.type_id, a.info, a.ingr, b.prodcode as code, b.term, "gold" as code_sys
from rx_sod_0 as a
left join temp_2 as b on a.ingr="phosphate";
quit;

data temp_3;
set b.dict_rx_comb_gold;
if (find(term, "calcium carbonate")>0 or find(term, "calcium gluconate")>0 or find(term, "calcium lactate")>0);
run;
proc sql;
create table rx_sod_gd_4 as
select distinct a.type_id, a.info, a.ingr, b.prodcode as code, b.term, "gold" as code_sys
from rx_sod_0 as a
left join temp_3 as b on a.ingr="calcium";
quit;
data rx_sod_gd_5; 
set rx_sod_gd_1 rx_sod_gd_2 rx_sod_gd_3 rx_sod_gd_4; 
if code="" then delete;
run;

proc sql;
create table rx_sod_gd_6 as
select distinct 
	(case when code in (select prodcode from x.list_rx_gold_ext) then tranwrd(type_id,"SOD_0","SOD_1")
		else type_id end) as type_id
	, info, ingr, term, code, code_sys
from rx_sod_gd_5
order by type_id;
quit;
/*proc freq data=rx_sod_gd_6; table type_id; run;*/

**other drugs;
proc sql;
create table rx_oth_gd_1 as
select distinct a.type_id, a.info, a.ingr, b.prodcode as code, b.term as term, "gold" as code_sys
from rx_oth_0 as a
left join b.dict_rx_split_gold as b on a.ingr=b.term;
quit;
/*proc freq data=rx_oth_gd_1; table ingr; run;*/

data x.list_rx_gold; set rx_sod_gd_6 rx_oth_gd_1; run;

* Aurum;
**Sodium-cotaining drugs and standard formulations;
proc sql;
create table rx_sod_ar_1 as
select distinct a.type_id, a.info, a.ingr, b.prodcodeid as code, b.term as term, "aurum" as code_sys
from rx_sod_0 as a
left join b.dict_rx_split_aurum as b on a.ingr=b.term
where ingr not in ("vitamin c", "phosphate", "calcium")
;quit;
/*proc freq data=rx_sod_ar_1; table ingr; run; *vitamin C, phosphate, calcium;*/

data temp_1;
set b.dict_rx_comb_aurum;
if find(term, "vitamin c")>0;
run;
proc sql;
create table rx_sod_ar_2 as
select distinct a.type_id, a.info, a.ingr, b.prodcodeid as code, b.term, "aurum" as code_sys
from rx_sod_0 as a
left join temp_1 as b on a.ingr="vitamin c";
quit;

data temp_2;
set b.dict_rx_comb_aurum;
if (find(term, "phosphate")>0 and find(term, "hydrogen")>0);
run;
proc sql;
create table rx_sod_ar_3 as
select distinct a.type_id, a.info, a.ingr, b.prodcodeid as code, b.term, "aurum" as code_sys
from rx_sod_0 as a
left join temp_2 as b on a.ingr="phosphate";
quit;

data temp_3;
set b.dict_rx_comb_aurum;
if (find(term, "calcium carbonate")>0 or find(term, "calcium gluconate")>0 or find(term, "calcium lactate")>0);
run;
proc sql;
create table rx_sod_ar_4 as
select distinct a.type_id, a.info, a.ingr, b.prodcodeid as code, b.term, "aurum" as code_sys
from rx_sod_0 as a
left join temp_3 as b on a.ingr="calcium";
quit;

data rx_sod_ar_5; 
set rx_sod_ar_1 rx_sod_ar_2 rx_sod_ar_3 rx_sod_ar_4; 
if code="" then delete;
run;

proc sql;
create table rx_sod_ar_6 as
select distinct 
	(case when code in (select prodcodeid from x.list_rx_aurum_ext) then tranwrd(type_id,"SOD_0","SOD_1")
		else type_id end) as type_id
	, info, ingr, term, code, code_sys
from rx_sod_ar_5
order by type_id;
quit;
/*proc freq data=rx_sod_ar_6; table type_id; run;*/

**other drugs;
proc sql;
create table rx_oth_ar_1 as
select distinct a.type_id, a.info, a.ingr, b.prodcodeid as code, b.term, "aurum" as code_sys
from rx_oth_0 as a
left join b.dict_rx_split_aurum as b on a.ingr=b.term;
quit;
/*proc freq data=rx_oth_ar_1; table ingr; run;*/

data x.list_rx_aurum; 
length term $148;
set rx_sod_ar_6 rx_oth_ar_1; 
run;

** Put together (Gold/Aurum);
data y.list_rx; set x.list_rx_aurum x.list_rx_gold; run;

*Disease codes for frailty index;
%IMPORT_XLSX (
path=&path_work.\Input
, filename=Index
, lib=work
, data_name=temp
, sheet=Frailty_Read
);
data fi_1;
set temp;
if readcode^="";
if length(trim(readcode))^=7 then do;
if length(trim(readcode))=4 then code=cat(trim(readcode),".00");
	else if length(trim(readcode))=5 then code=cat(trim(readcode),".0");
end;
else code=trim(readcode);
code_sys="gold";
drop readcode;
run;
proc sql;
create table fi_2 as
select distinct a.*, b.medcode
from fi_1 as a
left join b.dict_dx_gold as b on a.code=b.readcode
having medcode^=""
;quit;
data x.list_dx_fi_gold; set fi_2; run;

data fi_3;
set x.efi_aurum_cheng;
rename snomedctconceptid=code medcodeid=medcode;
keep deficit snomedctconceptid code_sys medcodeid;
code_sys="aurum";
run;
data x.list_dx_fi_aurum; set fi_3; run;

data y.list_dx_fi; 
set  x.list_dx_fi_aurum (drop=code) x.list_dx_fi_gold (drop=ctv3_description code); 
run;

*Product codes for frailty index;
data list_rx_fi_gold;
set b.dict_rx_gold;
if ingredient^="";
ingr=lowcase(ingredient);
code_sys="gold";
rename prodcode=code;
keep prodcode ingredient ingr code_sys;
run; *87388 > 39787 obs;

data list_rx_fi_aurum;
set b.dict_rx_aurum;
if drugsubstancename^="";
ingr=lowcase(drugsubstancename);
code_sys="aurum";
rename drugsubstancename=ingredient prodcodeid=code;
keep prodcodeid drugsubstancename ingr code_sys;
run; *71856 > 15948 obs;

data y.list_rx_fi; 
length ingredient $148 ingr $148;
set list_rx_fi_aurum list_rx_fi_gold; 
run;



/***********************************************/
/* Data conversion: GOLD, Aurum, HES, ONS, IMD */
/***********************************************/

/*** GOLD ***/
%let path_in=E:\Data\DiscSod\LinkedData\GOLD_linked;

*Gold: linked data;
%IMPORT_TXT(
path=&path_in.
, filename=23_003198_linkage_eligibility_gold
, lib=a, data_name=gd_pat_link
, var_name_list=patid	pracid	linkdate	hes_apc_e	ons_death_e	lsoa_e	sgss_e	chess_e	hes_op_e	hes_ae_e	hes_did_e	cr_e	sact_e	rtds_e	mhds_e	icnarc_e
, var_format_list=$20. $5. ddmmyy10. $1. $1. $1. $1. $1. $1. $1. $1. $1. $1. $1. $1. $1.
, delimiter='09'x
);
data x.incl_pre_gold_0; set a.gd_pat_link; if hes_apc_e="1" and ons_death_e="1"; run;

%IMPORT_TXT (
path=&path_in., filename=practice_imd_23_003198
, lib=a, data_name=gd_prac_imd
, var_name_list=pracid	country	e2019_imd_10
, var_format_list=$5. $10. 2.
, delimiter='09'x
);
%IMPORT_TXT (
path=&path_in., filename=death_patient_23_003198
, lib=a, data_name=gd_death
, var_name_list=patid	pracid	gen_death_id	n_patid_death	match_rank	dor	dod	dod_partial	nhs_indicator	pod_category	cause	cause1	cause2	cause3	cause4	cause5	cause6	cause7	cause8	cause9	cause10	cause11	cause12	cause13	cause14	cause15	cause_neonatal1	cause_neonatal2	cause_neonatal3	cause_neonatal4	cause_neonatal5	cause_neonatal6	cause_neonatal7	cause_neonatal8
, var_format_list=$20. $5. $20. 3. 1. ddmmyy10. ddmmyy10. $12. $1. $255. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6. $6.
, delimiter='09'x
);
%IMPORT_TXT (
path=&path_in., filename=hes_patient_23_003198
, lib=a, data_name=gd_hes_pat
, var_name_list=patid	pracid	gen_hesid	n_patid_hes	gen_ethnicity	match_rank
, var_format_list=$20. $5. 20. 3. $10. $1.
, delimiter='09'x
);
%IMPORT_TXT (
path=&path_in., filename=hes_hospital_23_003198
, lib=a, data_name=gd_hes_hosp
, var_name_list=patid	spno	admidate	discharged	admimeth	admisorc	disdest	dismeth	duration	elecdate	elecdur
, var_format_list=$20. 20. ddmmyy10. ddmmyy10. $5. 3. 3. 1. 5. ddmmyy10. 5.
, delimiter='09'x
);
%IMPORT_TXT (
path=&path_in., filename=hes_primary_diag_hosp_23_003198
, lib=a, data_name=gd_hes_dx1st_hosp
, var_name_list=patid	spno	admidate	discharged	ICD_PRIMARY	ICDx
, var_format_list=$20. 20. ddmmyy10. ddmmyy10. $5. $5.
, delimiter='09'x
);
%IMPORT_TXT (
path=&path_in., filename=hes_diagnosis_epi_23_003198
, lib=a, data_name=gd_hes_dx_epi
, var_name_list=patid	spno epikey epistart epiend	ICD	ICDx d_order
, var_format_list=$20. 20. 20. ddmmyy10. ddmmyy10. $5. $5. 2.
, delimiter='09'x
);
%IMPORT_TXT (
path=&path_in., filename=hes_diagnosis_hosp_23_003198
, lib=a, data_name=gd_hes_dx_hosp
, var_name_list=patid	spno	admidate	discharged	ICD	ICDx
, var_format_list=$20. 20. ddmmyy10. ddmmyy10. $5. $5.
, delimiter='09'x
);


*Gold: primary;
**Extract from CPRD GOLD: patient, practice, clinical, additional, test, therapy;
%MACRO data_extraction (dataset_list=);
%do dataset_i=1 %to %sysfunc(countw(&dataset_list., %str( ),q));
%let dataset=%scan(&dataset_list., &dataset_i., %str( ),q);
%EXTRACT (
data_in=cprd.&dataset.,
data_out=a.gd_&dataset.,
data_list=y.extpat_gold,
remove_migrators=Y
);
%end;
%MEND;
%data_extraction (dataset_list=additional clinical patient test therapy consultation);
proc sql;
create table a.gd_practice as
select *
from cprd.practice
where pracid in (select (substr(patid, length(patid)-4, 5)) from y.extpat_gold)
	and pracid not in (select gold_pracid from b.prac_migrt)
;quit;

proc sql;
create table temp_htn as
select distinct a.patid, a.eventdate, a.medcode, b.type_id, b.info
from a.gd_clinical as a
inner join y.list_dx as b on a.medcode=b.code 
where b.type_id="PRE_INCL_2" and b.code_sys="gold"
;quit;

proc sql;
create table pat_1 as
select distinct a.patid, a.gender, a.yob, a.frd, a.tod, a.toreason, a.accept, min(b.eventdate) as htn_1st_dt format yymmdd10.
from a.gd_patient as a
left join temp_htn as b on a.patid=b.patid
group by a.patid 
;quit;

data x.incl_pre_gold_1; set pat_1; if accept="1"; run; *94757 obs;
data x.incl_pre_gold_2; set pat_1; if 2004 <= year(htn_1st_dt) and year(htn_1st_dt) <= 2021; run; *30026 obs;
data x.incl_pre_gold_3; set pat_1; if htn_1st_dt - frd >= 365; run; *64702 obs;

* Individuals who have linkage data ;
proc sql;
create table pat_2 as
select a.*
from x.incl_pre_gold_1 as a
inner join x.incl_pre_gold_2 as b on a.patid=b.patid
inner join x.incl_pre_gold_3 as c on a.patid=c.patid
inner join x.incl_pre_gold_0 as e on a.patid=e.patid
;quit; *24579 obs;
data y.all_pat_gold; set pat_2; run;

* death, imd data;
proc sql;
create table temp as 
select distinct a.*
	, b.dod as death_dt, cause, cause1, cause2, cause3, cause4, cause5, cause6, cause7, cause8, cause9, cause10, cause11, cause12, cause13, cause14, cause15
	, c.e2019_imd_10 as imd, d.lcd
from y.all_pat_gold as a
left join a.gd_death as b on a.patid=b.patid
left join a.gd_prac_imd as c on substr(a.patid,length(a.patid)-5+1,5)=c.pracid
left join a.gd_practice as d on substr(a.patid,length(a.patid)-5+1,5)=d.pracid
;quit; *24579 obs;
data y.all_info_gold; set temp; run;

* dx, rx, lab data;
proc sql;
create table dx as
select distinct patid, eventdate, medcode as code, "gold" as code_sys
from a.gd_clinical
where patid in (select patid from y.all_pat_gold)
;quit;

proc sql;
create table temp_1 as
select distinct a.patid
	, b.spno as visit_key, b.admidate as eventdate, b.discharged as hosp_dc_dt
	, b.ICD as code
	, c.ICD_Primary
	, d.d_order as dx_order
	, "hes" as code_sys
from a.gd_hes_pat as a
left join a.gd_hes_dx_hosp as b on a.patid=b.patid
left join a.gd_hes_dx1st_hosp as c on a.patid=c.patid and b.spno=c.spno
left join a.gd_hes_dx_epi as d on a.patid=d.patid and b.spno=d.spno and b.ICD=d.ICD
where a.patid in (select patid from y.all_pat_gold)
;quit;

data temp_2;
set temp_1;
format dx1st 1.;
if code=ICD_primary then dx1st=1;
else dx1st=0;
drop ICD_primary;
run;

proc sql;
create table temp_3 as
select distinct *
from temp_2
order by patid, eventdate, dx1st desc;
quit;

data temp_4; set dx temp_3; run;
data y.all_dx_gold; set temp_4; run;

proc sql;
create table rx as
select distinct patid, eventdate, consid, prodcode, dosageid, qty, numdays, numpacks, packtype
from a.gd_therapy
where patid in (select patid from y.all_pat_gold)
;quit;
data y.all_rx_gold; set rx; run;

proc sql;
create table lab1 as
select distinct a.patid, a.eventdate, a.enttype, "clinical" as source, b.* 
from a.gd_clinical as a
left join a.gd_additional as b on a.patid=b.patid and a.adid=b.adid
where a.enttype in (1, 4, 5, 13)
	and a.patid in (select patid from y.all_pat_gold)
;quit;
proc sql;
create table lab2 as
select distinct patid, eventdate, enttype, "test" as source, data1, data2, data3, data4, data5, data6, data7
from a.gd_test
where enttype in (163, 175, 177)
	and patid in (select patid from y.all_pat_gold)
;quit;
data y.all_lab_gold; set lab1 lab2; run;



/*** Aurum ***/
*Aurum: linked data;
*Aurum: primary;
* Individuals who have linkage data ;
* death, imd data;
* dx, rx, lab data;



/****************************************************************/
/* Study population based on first hyptertension diagnosis date */
/****************************************************************/

data x.incl_gold_0; 
set y.all_info_gold;
if death_dt=. or htn_1st_dt < death_dt;
if (tod=. or htn_1st_dt < tod);
if (lcd=. or htn_1st_dt < lcd);
run; *24512;

proc sql;
create table temp_sod as
select distinct a.patid, a.eventdate, a.prodcode, b.type_id, b.info, b.ingr
from a.gd_therapy as a
inner join y.list_rx as b on a.prodcode=b.code 
where substr(b.type_id,1,5)="SOD_1" and b.code_sys="gold"
;quit;
proc sql;
create table temp_1 as
select distinct a.patid, a.yob, a.htn_1st_dt,  b.*
from y.all_pat_gold as a
inner join temp_sod as b on a.patid=b.patid 
		and (a.htn_1st_dt - 180 <= b.eventdate and b.eventdate < a.htn_1st_dt)
group by b.patid, b.type_id
having count(distinct b.eventdate)>1
;quit; *16818 > 16828 (calcium gluconate and lactate) obs;
proc sql;
create table temp_2 as
select distinct patid, htn_1st_dt, type_id as index_tp
from temp_1
order by patid, index_tp
;quit; *1894 > 1897 (calcium gluconate and lactate);
data x.incl_gold_1; set temp_2; run;

data x.incl_gold_2; 
set y.all_info_gold; 
if year(htn_1st_dt) - yob > 60; 
run; *15276 obs;

proc sql;
create table temp as
select distinct a.*, c.index_tp label=""
from y.all_pat_gold as a
inner join x.incl_gold_0 as b on a.patid=b.patid
inner join x.incl_gold_1 as c on a.patid=c.patid
inner join x.incl_gold_2 as d on a.patid=d.patid
;quit; *1383>1385 (calcium gluconate and lactate) obs;
data y.cht_pat_gold; set temp; run;

/*proc sql;*/
/*select count(distinct patid)*/
/*from y.cht_pat_gold*/
/*;quit; *1366 individuals;*/

%Flow (stt_data=a.gd_patient
, out_data=test
, elig_data_list=x.incl_pre_gold_0 x.incl_pre_gold_1 x.incl_pre_gold_2 x.incl_pre_gold_3 x.incl_gold_0 x.incl_gold_1 x.incl_gold_2
, id=patid
); *1366;


/*********************************************************************/
/* Dataset on treatment, outcome, eligibility, covariates, sequence */
/*********************************************************************/

*treatment;
proc sql;
create table temp_1 as
select distinct a.*, b.type_id, b.info, b.ingr
from y.all_rx_gold as a
inner join y.list_rx as b on a.prodcode=b.code 
where substr(b.type_id,1,3)="SOD" and b.code_sys="gold"
;quit;
proc sql;
create table temp_2 as
select distinct a.patid, a.htn_1st_dt, b.eventdate, b.type_id, b.dosageid, b.qty
from y.cht_pat_gold as a 
left join temp_1 as b on a.patid=b.patid 
		and a.htn_1st_dt <= b.eventdate
;quit; *121754>121855(added calcium gluconate and calcium lactate) obs;
data y.cht_trt_gold; set temp_2; run;

*outcome; 
**MI, stroke, HF;
proc sql;
create table out_1 as
select distinct a.patid, a.eventdate, a.code, a.code_sys, b.type_id
from y.all_dx_gold as a
inner join y.list_dx as b on a.code=b.code and a.code_sys=b.code_sys 
where substr(b.type_id,1,3)="OUT" and b.code_sys="gold"
;quit;
proc sql;
create table out_2 as
select distinct a.patid, a.eventdate, a.code, a.code_sys, b.type_id
from y.all_dx_gold as a
inner join y.list_dx as b on (substr(a.code,1,3)=b.code or substr(a.code,1,5)=b.code) and a.code_sys=b.code_sys 
where substr(b.type_id,1,3)="OUT" and b.code_sys="hes" and a.dx_order<=3
;quit; *Only first 3 diagnoses for HES;
data out_3; set out_1 out_2; run;
proc sql;
create table out_4 as
select distinct a.patid, a.htn_1st_dt, b.eventdate, b.type_id
from y.cht_pat_gold as a
inner join out_3 as b on a.patid=b.patid and a.htn_1st_dt < b.eventdate
;quit;

**all-cause death;
data out_5;
set y.all_info_gold;
if htn_1st_dt < death_dt;
type_id="OUT_6";
rename death_dt=eventdate;
keep patid htn_1st_dt death_dt type_id;
run;

**cardiovascular death - primary cause of death;
data out_6;
set y.all_info_gold;
if htn_1st_dt < death_dt;
if substr(cause1,1,1)="I";
type_id="OUT_4";
rename death_dt=eventdate;
keep patid htn_1st_dt death_dt type_id;
run;

data data y.cht_out_gold; set out_4 out_5 out_6; run;

*eligibility;
proc sql;
create table temp_1 as
select distinct a.patid, a.eventdate, b.type_id
from y.all_dx_gold as a
inner join y.list_dx as b on a.code=b.code 
where substr(b.type_id,1,4)="EXCL" and b.code_sys="gold" and a.eventdate^=.
;quit;
proc sql;
create table temp_2 as
select distinct a.patid, a.eventdate, b.type_id
from y.all_dx_gold as a
inner join y.list_dx as b on substr(a.code,1,3)=b.code or substr(a.code,1,5)=b.code
where substr(b.type_id,1,4)="EXCL" and b.code_sys="hes" and a.eventdate^=.
;quit;
data y.cht_elg_gold; set temp_1 temp_2; run;
/*proc freq data=y.cht_elg_gold; table type_id; run;*/

*covariates;
**demographic: age, sex, IMD quintiles - all_info_gold;
data y.cht_cov_demo_gold;
set y.all_info_gold;
keep patid gender yob imd;
run;

**bmi, drinking, smoking, SBP/DBP, TC, HDL, LDL;
data temp_1 (drop=data1 data2 rename=(data3=data))
	temp_2 (drop=data2 data3 rename=(data1=data))
	temp_3 (drop=data2 data3 rename=(data1=data))
	temp_4 (drop=data1 data3 rename=(data2=data))
	temp_5 (drop=data2 data3 rename=(data1=data))
	temp_6 (drop=data1 data3 rename=(data2=data))
	temp_7 (drop=data1 data3 rename=(data2=data))
	temp_8 (drop=data1 data3 rename=(data2=data));
set y.all_lab_gold (keep=patid eventdate enttype data1 data2 data3);
if data1="" and data2="" and data3="" then delete;
if enttype=13 then do; data_tp="BMI"; output temp_1; end;
if enttype=5 then do; data_tp="ALC"; output temp_2; end;
if enttype=4 then do; data_tp="SMK"; output temp_3; end;
if enttype=1 then do; data_tp="SBP"; output temp_4; end;
if enttype=1 then do; data_tp="DBP"; output temp_5; end;
if enttype=163 then do; data_tp="TC"; output temp_6; end;
if enttype=175 then do; data_tp="HDL"; output temp_7; end;
if enttype=177 then do; data_tp="LDL"; output temp_8; end;
run;

data temp_9; 
set temp_1 - temp_8; 
if data="" then delete; 
drop enttype;
run;
/*proc freq data=temp_9; table data_tp; run;*/

data y.cht_cov_lab_gold; set temp_9; run;

**dx, dx (hes), rx;
proc sql;
create table temp_1 as
select distinct a.patid, a.eventdate, b.type_id
from y.all_dx_gold as a
inner join y.list_dx as b on a.code=b.code 
where substr(b.type_id,1,6)="COV_DX" and b.code_sys="gold" and a.eventdate^=.
;quit;

proc sql;
create table temp_2 as
select distinct a.patid, a.eventdate, b.type_id
from y.all_dx_gold as a
inner join y.list_dx as b on substr(a.code,1,3)=b.code or substr(a.code,1,5)=b.code
where substr(b.type_id,1,6)="COV_DX" and b.code_sys="hes" and a.eventdate^=.
;quit;

proc sql;
create table temp_3 as 
select distinct a.patid, a.eventdate, b.type_id
from y.all_rx_gold as a
inner join y.list_rx as b on a.prodcode=b.code
where substr(b.type_id,1,6)="COV_RX" and b.code_sys="gold" and a.eventdate^=.
;quit;

data y.cht_cov_dxrx_gold; set temp_1 - temp_3; run;

*sequence: enrolment of each trial;
proc sql;
create table temp_0 as
select distinct a.patid, a.htn_1st_dt, a.index_tp, b.eventdate
from y.cht_pat_gold as a
left join y.cht_trt_gold as b on a.patid=b.patid and a.index_tp=b.type_id
;quit;
data temp_1;
set temp_0;
array date{*} enrol_dt1 - enrol_dt20 ;
array period{*} enrol_prd1 - enrol_prd19;
enrol_dt1=htn_1st_dt;
do i=1 to dim(date)-1;
	date{i+1}=date{i}+(365.25/12)*3;
	if date{i} <= eventdate and eventdate < date{i+1} then period{i}=1; else period{i}=0;
end;
drop i;
format enrol_dt1 - enrol_dt20 yymmdd10.;
run;
%MACRO XXX;
proc sql;
create table temp_2 as
select distinct patid, htn_1st_dt, index_tp
	%do i=1 %to 19;
		, enrol_dt&i. , max(enrol_prd&i.) as enrol_prd&i.
	%end;
		, enrol_dt20
from temp_1
group by patid, index_tp
;quit;
%MEND; %XXX;
%MACRO XXX;
data temp_3;
set temp_2;
trial1=1;
%do i=1 %to 19;
%let b=%eval(&i.+1);
	if min(of enrol_prd1 - enrol_prd&i.)=0 then trial&b.=0; else trial&b.=1;
%end;
run;
%MEND; %XXX; *1383>1385 (calcium carbonate and lactate) obs;
%MACRO XXX;
proc sql;
create table temp_4 as
select distinct patid, htn_1st_dt, index_tp
	%do i=1 %to 20;
		, enrol_dt&i., min(trial&i.) as trial&i.
	%end;
from temp_3
group by patid
;quit;
%MEND; %XXX;
%MACRO XXX;
%do i=1 %to 20;
data temp_5_&i.;
set temp_4 (keep=patid htn_1st_dt trial&i. enrol_dt&i. index_tp);
if trial&i.=1;
format enrol_dt yymmdd10.;
trial_id=&i.;
enrol_dt=enrol_dt&i.;
drop trial&i. enrol_dt&i.;
run;
%end;
%MEND; %XXX;
data temp_6; set temp_5_1 - temp_5_20; run;
/*proc freq data=temp_6; table trial_id; run;*/

data y.seq_pat_gold; 
set temp_6; 
patid2=_n_;
run; *9833 > 9836 (calcium carbonate and lactate) obs;


/*********************************************/
/* Sequence of Trials: enrolment, assignment */
/*********************************************/

*enrolment-assignment;
proc sql;
create table x.excl_gold_0 as
select distinct a.*, b.death_dt, b.tod
from y.seq_pat_gold as a
inner join y.all_info_gold as b on a.patid=b.patid 
	and ((b.death_dt < a.enrol_dt and b.death_dt^=.) or (b.tod < a.enrol_dt and b.tod^=.) or (b.lcd < a.enrol_dt and b.lcd^=.))
;quit;

%Eligible (in_data=y.seq_pat_gold, out_data=x.excl_gold_1, info_data=y.cht_elg_gold
, assmnt_period1=-36500, assmnt_period2=0, assmnt_id=EXCL_1);
%Eligible (in_data=y.seq_pat_gold, out_data=x.excl_gold_4_1, info_data=y.cht_elg_gold
, assmnt_period1=-36500, assmnt_period2=0, assmnt_id=EXCL_4_1);
%Eligible (in_data=y.seq_pat_gold, out_data=x.excl_gold_4_2, info_data=y.cht_elg_gold
, assmnt_period1=-36500, assmnt_period2=0, assmnt_id=EXCL_4_2);
%Eligible (in_data=y.seq_pat_gold, out_data=x.excl_gold_4_3, info_data=y.cht_elg_gold
, assmnt_period1=-36500, assmnt_period2=0, assmnt_id=EXCL_4_3);
%Eligible (in_data=y.seq_pat_gold, out_data=x.excl_gold_5, info_data=y.cht_elg_gold
, assmnt_period1=-36500, assmnt_period2=0, assmnt_id=EXCL_5);
%Eligible (in_data=y.seq_pat_gold, out_data=x.excl_gold_6_1, info_data=y.cht_elg_gold
, assmnt_period1=-60, assmnt_period2=0, assmnt_id=EXCL_6_1);
%Eligible (in_data=y.seq_pat_gold, out_data=x.excl_gold_6_2, info_data=y.cht_elg_gold
, assmnt_period1=-60, assmnt_period2=0, assmnt_id=EXCL_6_2);
data x.excl_gold_4; set x.excl_gold_4_1 - x.excl_gold_4_3; run;
data x.excl_gold_6; set x.excl_gold_6_1 - x.excl_gold_6_2; run;

data x.excl_gold; set x.excl_gold_0 x.excl_gold_1 x.excl_gold_4 - x.excl_gold_6; run;

proc sql;
create table temp_0 as
select distinct *
from y.seq_pat_gold
where patid2 not in (select patid2 from x.excl_gold)
order by patid, trial_id
;quit; *7921>7833 (calcium and lcd) obs;
proc sql;
create table temp_1 as
select distinct a.* 
		, max(case when substr(b.type_id,5,1)="1" then 1 else 0 end) as sod_rx
		, max(case when substr(b.type_id,5,1)="0" then 1 else 0 end) as std_rx
from temp_0 as a
left join y.cht_trt_gold as b on a.patid=b.patid 
		and a.enrol_dt <= b.eventdate < a.enrol_dt + (365.25/12)*3
		and substr(a.index_tp,7,1)=substr(b.type_id,7,1)
group by a.patid, a.trial_id, a.index_tp
;quit; *7871>7833 (calcium and lcd) obs;
/*proc sql;*/
/*create table test as*/
/*select distinct **/
/*from temp_1*/
/*group by patid, trial_id*/
/*having count(distinct index_tp)>1 and (count(distinct sod_rx)>1 or count(distinct std_rx)>1)*/
/*order by patid, trial_id*/
/*;quit;*/
proc sql;
create table temp_2 as
select distinct patid, trial_id, enrol_dt, min(sod_rx) as sod_rx, min(std_rx) as std_rx
from temp_1
group by patid, trial_id
;quit;
data temp_3;
set temp_2;
if sod_rx=1 then trt=0; *continue;
if sod_rx=0 and std_rx=1 then trt=1; *switch to standard form;
if sod_rx=0 and std_rx=0 then trt=9; *exclude;
run;
/*proc sql;*/
/*create table test as*/
/*select **/
/*from temp_3*/
/*group by patid, trial_id*/
/*having count(distinct trt)>1*/
/*;quit;*/
/*proc freq data=temp_3; table trial_id*trt/nocol nopercent; run;*/
proc sql;
create table temp_4 as
select distinct a.patid, a.enrol_dt, a.trial_id, trt
	, (
	case when trt=0 then min(c.eventdate)
	else case when trt=1 then min(d.eventdate) end end
	) as rx_1st_dt format yymmdd10.
	, min(b.eventdate) as outcome_dt format yymmdd10.
from temp_3 as a
left join y.cht_out_gold as b on a.patid=b.patid 
		and a.enrol_dt <= b.eventdate
left join y.cht_trt_gold as c on a.patid=c.patid 
	and a.enrol_dt <= c.eventdate and substr(c.type_id,1,5)="SOD_1"
left join y.cht_trt_gold as d on a.patid=d.patid 
	and a.enrol_dt <= d.eventdate and substr(d.type_id,1,5)="SOD_0"
group by a.patid, a.trial_id
;quit; *7871>7783 (calcium and lcd) obs;
data x.excl_gold_7 x.excl_gold_8;
set temp_4;
if trt=9 then output x.excl_gold_7;
if enrol_dt <= outcome_dt and outcome_dt <=rx_1st_dt then output x.excl_gold_8;
/*drop outcome_dt rx_1st_dt;*/
run;
data temp_5;
set temp_4;
if trt=9 then delete;
if enrol_dt <= outcome_dt and outcome_dt <=rx_1st_dt then delete;
/*drop outcome_dt rx_1st_dt;*/
run; *7304>7297 (calcium and lcd) obs;
data temp_6;
set temp_5;
by patid trial_id;
pre_trial=lag(trial_id);
if first.patid then do; pre_trial=.; flag_1st=1; end;
else if trial_id=pre_trial+1 then flag_1st=0;
else flag_1st=1;
retain incl 0;
incl=incl+flag_1st;
run;
/*proc sql;*/
/*create table test as*/
/*select **/
/*from temp_5*/
/*group by patid*/
/*having count(distinct incl)>1*/
/*order by patid, trial_id*/
/*;quit;*/
proc sql;
create table temp_7 as
select distinct patid, enrol_dt, trial_id, trt
from temp_6
group by patid
having min(trial_id)=1 and incl=min(incl)
;quit; *6797>6793 (calcium and lcd) obs;
data y.seq_asgmt_gold; set temp_7; run;
proc freq data=y.seq_asgmt_gold; table trial_id*trt/norow nocol nopercent; run;


/*********************************/
/* Follow-up: outcome, censoring */
/*********************************/

*outcome;
proc sql;
create table out_1 as
select distinct a.*, b.eventdate, b.type_id
from y.seq_asgmt_gold as a
left join y.cht_out_gold as b on a.patid=b.patid and a.enrol_dt < b.eventdate
;quit;
%MACRO XXX;
proc sql;
create table out_2 as
select distinct patid, trial_id, enrol_dt
	%do i=2 %to 6; 
	, (case when type_id="OUT_&i." then eventdate else . end) as out&i._dt format yymmdd10.
	%end;
from out_1
;quit;
proc sql;
create table out_3 as
select distinct patid, trial_id, enrol_dt
	%do i=2 %to 6; 
	, min(out&i._dt) as out&i._dt format yymmdd10.
	%end;
from out_2
group by patid, trial_id
order by patid, trial_id
;quit;
%MEND; %XXX;
data out_4;
set out_3;
format out1_dt yymmdd10.;
out1_dt=min(out2_dt,out3_dt,out4_dt);
run;
data x.seq_fu0_gold; set out_4; run;

*censoring;
**death, loss to follow-up (transfer out), three years after assignment, or the end of the study period;
proc sql;
create table cens_1 as
select distinct a.*, b.death_dt as cens1_dt format yymmdd10.
		, b.tod as cens2_dt
		, b.lcd as cens3_dt
		, (a.enrol_dt+365.25*3) as cens4_dt format yymmdd10.
		, mdy(12,31,2022) as cens5_dt format yymmdd10.
from y.seq_asgmt_gold as a 
left join y.all_info_gold as b on a.patid=b.patid
;quit;
data x.seq_fu1_gold; set cens_1; run;

**deviate from assigned treatment strategy;
**discontinuation arm (switching): 1. reiniation, 2. discontinuation of index class (standard form);
proc sql;
create table pp_1 as
select distinct a.*, b.index_tp, max(case when b.index_tp=c.type_id then 1 else 0 end) as oth_index_tp format 1. 
from y.seq_asgmt_gold as a
left join y.cht_pat_gold as b on a.patid=b.patid
left join y.cht_trt_gold as c on a.patid=c.patid and a.enrol_dt <= c.eventdate < a.enrol_dt + 365.25/12*3
where trt=1
group by a.patid, a.trial_id, b.index_tp
;quit;
data pp_2(drop=oth_index_tp rename=(index_tp=int_index_tp)) 
	pp_3(drop=oth_index_tp rename=(index_tp=oth_index_tp));
set pp_1;
if oth_index_tp=0 then output pp_2;
if oth_index_tp=1 then output pp_3;
run;
proc sql;
create table pp_4 as
select distinct a.*, b.*, c.*
from y.seq_asgmt_gold as a
left join pp_2 as b on a.patid=b.patid and a.trial_id=b.trial_id 
left join pp_3 as c on a.patid=c.patid and a.trial_id=c.trial_id 
where a.trt=1
;quit; 
/*proc sql;*/
/*create table test as*/
/*select distinct **/
/*from pp_4*/
/*group by patid, trial_id*/
/*having count(distinct int_index_tp)>1*/
/*;quit;*/
proc sql;
create table pp_5 as
select distinct a.patid, a.trial_id, min(b.eventdate) as cens6_dt1 format yymmdd10.
from pp_4 as a
left join y.cht_trt_gold as b on a.patid=b.patid and a.enrol_dt <= b.eventdate 
	and a.oth_index_tp^=b.type_id
	and substr(b.type_id,1,5)="SOD_1"
group by a.patid, a.trial_id
;quit;
data x.seq_fu2_gold; set pp_5; run;  *reinitiation;

proc sql;
create table stnd_1 as
select distinct a.*, b.*, c.dosage_text, c.daily_dose
from pp_4 as a
left join y.cht_trt_gold as b on a.patid=b.patid and a.enrol_dt <= b.eventdate
	and substr(a.int_index_tp,7,1)=substr(b.type_id,7,1) and substr(b.type_id,1,5)="SOD_0"
left join b.lookup_rxdosage_gold as c on b.dosageid=c.dosageid
order by a.patid, a.trial_id, a.int_index_tp, b.eventdate
;quit;
/*proc freq data=stnd_1; table qty daily_dose; run;*/
data stnd_2;
set stnd_1;
if daily_dose in (. 0)  or qty in (.) then rx_dur=28;
else rx_dur=qty/daily_dose;
run;
/*proc freq data=stnd_2; table rx_dur; run;*/
proc sql;
create table stnd_3 as
select distinct patid, trial_id, enrol_dt, trt, int_index_tp, eventdate, max(rx_dur) as trt_dur
from stnd_2
group by patid, trial_id, int_index_tp, eventdate
order by patid, trial_id, int_index_tp, eventdate
;quit;
proc sql;
create table stnd_4 as
select distinct *, min(eventdate) as rx_1st_dt format yymmdd10.
from stnd_3
group by patid, trial_id, int_index_tp
order by patid, trial_id, int_index_tp, eventdate
;quit;
data stnd_5;
set stnd_4;
rx_end_dt=eventdate + trt_dur -1;
lag_dt=lag(rx_end_dt)+90;
if eventdate=rx_1st_dt then do; lag_dt=.; flag_1st=1; end;
else if eventdate <= lag_dt then flag_1st=0;
else flag_1st=1;
retain episode 0;
episode=episode+flag_1st;
format lag_dt rx_end_dt yymmdd10.;
run;
/*proc sql;*/
/*create table test as*/
/*select **/
/*from stnd_5*/
/*group by patid, trial_id*/
/*having count(distinct int_index_tp)>1*/
/*order by patid, trial_id, int_index_tp, eventdate*/
/*;quit;*/
proc sql;
create table stnd_6 as
select distinct patid, trial_id, trt, enrol_dt, int_index_tp, episode
		, max(rx_end_dt)+90 as trt_end_dt format yymmdd10.
from stnd_5
group by patid, trial_id, int_index_tp, episode
;quit;
proc sql;
create table stnd_7 as
select distinct *
from stnd_6
group by patid, trial_id, int_index_tp
having min(episode)=episode
;quit;
proc sql;
create table stnd_8 as
select distinct patid, trial_id, min(trt_end_dt) as cens6_dt2 format yymmdd10.
from stnd_7
group by patid, trial_id
;quit; 
data x.seq_fu3_gold; set stnd_8; run; *discontinue at least one standard form of index class drug;


**continuation arm: discontinuation of at least one sodium-containing drug class;
proc sql;
create table cont_1 as
select distinct a.*, b.index_tp, c.*, d.dosage_text, d.daily_dose 
from y.seq_asgmt_gold as a
left join y.cht_pat_gold as b on a.patid=b.patid
left join y.cht_trt_gold as c on a.patid=c.patid and a.enrol_dt <= c.eventdate 
	and a.patid=c.patid and b.index_tp=c.type_id
left join b.lookup_rxdosage_gold as d on c.dosageid=d.dosageid
where trt=0
order by a.patid, a.trial_id, b.index_tp, c.eventdate
;quit;
data cont_2;
set cont_1;
if daily_dose in (. 0)  or qty in (.) then rx_dur=28;
else rx_dur=qty/daily_dose;
run;
proc sql;
create table cont_3 as
select distinct patid, trial_id, enrol_dt, trt, index_tp, eventdate, max(rx_dur) as trt_dur
from cont_2
group by patid, trial_id, index_tp, eventdate
order by patid, trial_id, index_tp, eventdate
;quit;
proc sql;
create table cont_4 as
select distinct *, min(eventdate) as rx_1st_dt format yymmdd10.
from cont_3
group by patid, trial_id, index_tp
order by patid, trial_id, index_tp, eventdate
;quit;
data cont_5;
set cont_4;
rx_end_dt=eventdate + trt_dur -1;
lag_dt=lag(rx_end_dt)+90;
if eventdate=rx_1st_dt then do; lag_dt=.; flag_1st=1; end;
else if eventdate <= lag_dt then flag_1st=0;
else flag_1st=1;
retain episode 0;
episode=episode+flag_1st;
format lag_dt rx_end_dt yymmdd10.;
run;
/*proc sql;*/
/*create table test as*/
/*select **/
/*from cont_5*/
/*group by patid, trial_id*/
/*having count(distinct index_tp)>1*/
/*order by patid, trial_id, index_tp, eventdate*/
/*;quit;*/
proc sql;
create table cont_6 as
select distinct patid, trial_id, trt, enrol_dt, index_tp, episode
		, max(rx_end_dt)+90 as trt_end_dt format yymmdd10.
from cont_5
group by patid, trial_id, index_tp, episode
;quit;
proc sql;
create table cont_7 as
select distinct *
from cont_6
group by patid, trial_id, index_tp
having min(episode)=episode
;quit;
proc sql;
create table cont_8 as
select distinct patid, trial_id, min(trt_end_dt) as cens6_dt2 format yymmdd10.
from cont_7
group by patid, trial_id
;quit; 
data x.seq_fu4_gold; set cont_8; run; *discontinue at least one sodium-containing drug;

proc sql;
create table temp as
select distinct a.*, b.*, c.*, d.*, e.*, f.*
from y.seq_asgmt_gold as a
left join x.seq_fu0_gold as b on a.patid=b.patid and a.trial_id=b.trial_id
left join x.seq_fu1_gold as c on a.patid=c.patid and a.trial_id=c.trial_id
left join x.seq_fu2_gold as d on a.patid=d.patid and a.trial_id=d.trial_id
left join x.seq_fu3_gold as e on a.patid=e.patid and a.trial_id=e.trial_id
left join x.seq_fu4_gold as f on a.patid=f.patid and a.trial_id=f.trial_id
;quit;
data temp2;
set temp;
if enrol_dt > min(cens1_dt, cens2_dt, cens3_dt, cens4_dt, cens5_dt) then delete;
format cens6_dt yymmdd10.;
cens6_dt=min(cens6_dt1, cens6_dt2);
run;
data y.seq_fu_gold; set temp2; run;



/***********************************************/
/* Covariates: baseline, time-varing variables */
/***********************************************/
data temp_1;
set y.seq_asgmt_gold;
array date{*} index_dt1 - index_dt12;
index_dt1=enrol_dt;
do i=1 to dim(date)-1;
	date{i+1}=date{i}+(365.25/12)*3;
end;
drop i;
format index_dt1 - index_dt12 yymmdd10.;
run;
%MACRO XXX;
%do i=1 %to 12;
data temp_2_&i.;
set temp_1 (keep=patid trial_id index_dt&i.);
format index_dt yymmdd10.;
time_id=&i.;
index_dt=index_dt&i.;
drop index_dt&i.;
run;
%end;
%MEND; %XXX;
data temp_3; set temp_2_1 - temp_2_12; run;
proc sql;
create table temp_4 as
select distinct a.*, b.*
from temp_3 as a
left join y.seq_fu_gold as b on a.patid=b.patid and a.trial_id=b.trial_id
order by patid, trial_id, time_id
;quit;
data temp_5;
set temp_4;
if index_dt > min(cens1_dt, cens2_dt, cens3_dt, cens4_dt) then delete;
run;*81564 > 75424 obs;
data x.seq_indextv_gold; set temp_5; keep patid trial_id index_dt time_id enrol_dt trt; run;

**age, sex, IMD quintiles;
proc sql;
create table demo_1 as
select distinct a.patid, a.trial_id, a.enrol_dt, b.*
from x.seq_indextv_gold as a
left join y.cht_cov_demo_gold as b on a.patid=b.patid
;quit;
data demo_2;
set demo_1;
rename gender=COV_DEMO_2 age=COV_DEMO_1;
if imd in (1 2) then COV_DEMO_3=1;
else if imd in (3 4) then COV_DEMO_3=2;
else if imd in (5 6) then COV_DEMO_3=3;
else if imd in (7 8) then COV_DEMO_3=4;
else if imd in (9 10) then COV_DEMO_3=5;
age=year(enrol_dt)-yob;
keep patid trial_id age gender COV_DEMO_3;
run;
data x.seq_cov1_gold; set demo_2; run;
/*proc freq data=demo_2; table cov_demo_1 - cov_demo_3/norow; run;*/

**most recent BMI, drinking, smoking, TC, HDL, LDL;
***Measure;
proc sql;
create table temp_1 as
select distinct a.*, b.eventdate, b.data, b.data_tp
from x.seq_indextv_gold as a
left join y.cht_cov_lab_gold as b on a.patid=b.patid 
		and b.eventdate <= a.index_dt and b.data_tp in ("BMI","TC","HDL","LDL")
group by a.patid, a.trial_id, a.index_dt, b.data_tp
having max(b.eventdate)=b.eventdate
;quit;
/*proc freq data=temp_1; table data*data_tp; run;*/
/*data test; set temp_1; if data>=50; run;*/
data temp_2; 
set temp_1; 
if data=0 then data1=.;
else if data>=100 then data1=.;
else data1=data;
drop data;
rename data1=data;
run;
/*proc freq data=temp_2; table data*data_tp; run;*/
/*proc summary data=temp_2; class data_tp; var data; output out=test; run;*/
proc sql;
create table temp_3 as 
select distinct patid, index_dt, enrol_dt, trial_id, time_id, trt, eventdate
		, mean(data) as data, data_tp
from temp_2
group by patid, trial_id, time_id,  eventdate, data_tp
;quit;
/*proc sql;*/
/*create table test as*/
/*select distinct **/
/*from temp_3*/
/*group by patid, trial_id, time_id, data_tp*/
/*having count(data)>1*/
/*;quit;*/
proc sql;
create table temp_4 as
select distinct a.patid, a.trial_id, a.time_id, b.data as BMI
		, c.data as TC, d.data as LDL, e.data as HDL 
from temp_3 as a
left join temp_3 as b on a.patid=b.patid and a.trial_id=b.trial_id and a.time_id=b.time_id and b.data_tp="BMI"
left join temp_3 as c on a.patid=c.patid and a.trial_id=c.trial_id and a.time_id=c.time_id and c.data_tp="TC"
left join temp_3 as d on a.patid=d.patid and a.trial_id=d.trial_id and a.time_id=d.time_id and d.data_tp="LDL"
left join temp_3 as e on a.patid=e.patid and a.trial_id=e.trial_id and a.time_id=e.time_id and e.data_tp="HDL"
;quit;
data temp_5;
set temp_4;
if 0 < BMI < 18.5 then COV_DEMO_4="1";
else if 18.5 <= BMI and BMI < 25.0 then COV_DEMO_4="2";
else if 25.0 <= BMI and BMI < 30.0 then COV_DEMO_4="3";
else if 30.0 <= BMI then COV_DEMO_4="4";
else cov_demo_4="9";

if TC > 3.8 or LDL > 2.6 then COV_DX_7_lab=1; 
else COV_DX_7_lab=0;
run; 
/*proc freq data=temp_5; table cov_demo_4 cov_dx_7_lab; run;*/
/*data test;*/
/*set temp_5;*/
/*if cov_dx_7_lab^=1;*/
/*run;*/
/*data test;*/
/*set temp_5;*/
/*if cov_demo_4="9";*/
/*run;*/
data x.seq_cov2_gold; set temp_5; run;

***Answer;
proc sql;
create table as_1 as
select distinct a.*, b.eventdate, b.data, b.data_tp
from x.seq_indextv_gold as a
left join y.cht_cov_lab_gold as b on a.patid=b.patid 
		and b.eventdate <= a.index_dt and b.data_tp in ("ALC","SMK")
group by a.patid, a.trial_id, a.time_id, b.data_tp
having max(b.eventdate)=b.eventdate
;quit;
/*proc freq data=as_1; table data*data_tp/norow nocol nopercent; run; *1,2,3;*/
/*proc sql;*/
/*create table test as*/
/*select distinct **/
/*from as_1*/
/*group by patid, trial_id, time_id, data_tp*/
/*having count(data)>1*/
/*;quit; */
proc sql;
create table as_2 as
select distinct patid, trial_id, enrol_dt, time_id, index_dt, trt, eventdate
		, (case when min(input(data,1.))=1 then "1" 
			else case when max(input(data,1.))=3 then "3" 
			else "2" end end) as data
		, data_tp
from as_1
group by patid, trial_id, time_id, data_tp
;quit; *priority: Yes (1) > Past (3) > No (2);
/*proc sql;*/
/*create table test2 as*/
/*select distinct a.*, b.data as data_check*/
/*from test as a*/
/*left join as_2 as b on a.patid=b.patid and a.trial_id=b.trial_id and a.time_id=b.time_id and a.data_tp=b.data_tp */
/*;quit;*/
proc sql;
create table as_3 as
select distinct a.*, b.data as data_past
from as_2 as a
left join y.cht_cov_lab_gold as b on a.patid=b.patid and a.data_tp=b.data_tp 
		and b.eventdate < a.index_dt and a.data^=b.data
;quit;*history;
/*proc freq data=as_3; table data*data_past/norow nocol nopercent; run;*/
proc sql;
create table as_4 as
select distinct patid, trial_id, enrol_dt, time_id, index_dt, trt, eventdate, data, data_tp
		, (case when data_past="" then ""
			else case when min(input(data_past,1.))=1 then "1" 
			else case when max(input(data_past,1.))=3 then "3" 
			else "2" end end end) as data_past
from as_3
group by patid, trial_id, time_id, data_tp
;quit; *priority: Yes (1) > Past (3) > No (2);
data as_5;
set as_4;
if data_past="" then data_new=data;
if data=1 then data_new="1";
if data=3 then data_new="3";
if data=2 then do; 
if data_past=0 then data_new="2";
if data_past in ("1" "3") then data_new="3";
end;
run;
/*proc freq data=as_5; table data_new; run;*/
proc sql;
create table as_6 as
select distinct patid, trial_id, time_id, data_new as data, data_tp
from as_5
;quit;
/*proc sql;*/
/*create table test as*/
/*select distinct **/
/*from as_6*/
/*group by patid, trial_id, time_id, data_tp*/
/*having count(data)>1*/
/*;quit;*/
proc sql;
create table as_7 as
select distinct a.patid, a.trial_id, a.time_id, b.data as ALC, c.data as SMK
from as_6 as a
left join as_6 as b on a.patid=b.patid and a.trial_id=b.trial_id and a.time_id=b.time_id and b.data_tp="ALC"
left join as_6 as c on a.patid=c.patid and a.trial_id=c.trial_id and a.time_id=c.time_id and c.data_tp="SMK"
;quit;
data as_8;
set as_7;
if ALC="" then COV_DEMO_5="9";
else COV_DEMO_5=ALC;
if SMK="" then COV_DEMO_6="9";
else COV_DEMO_6=SMK;
run;
/*proc freq data=as_8; table cov_demo_5 cov_demo_6/norow; run;*/
data x.seq_cov3_gold; set as_8; run;

**COV_BP:recent BP (within 180d);
proc sql;
create table bp_1 as
select distinct a.*, b.eventdate, b.data, b.data_tp
from x.seq_indextv_gold as a
left join y.cht_cov_lab_gold as b on a.patid=b.patid 
		and a.index_dt-180 < b.eventdate and b.eventdate <= a.index_dt and b.data_tp in ("SBP", "DBP")
group by a.patid, a.trial_id, a.time_id
having max(b.eventdate)=b.eventdate
;quit;
/*proc freq data=bp_1; table data*data_tp/norow nocol nopercent; run;*/
data bp_2; 
set bp_1; 
if data=0 then data1=.;
else data1=data;
drop data;
rename data1=data;
run;
/*proc freq data=bp_2; table data*data_tp/norow nocol nopercent; run;*/
proc sql;
create table bp_3 as 
select distinct patid, trial_id, enrol_dt, time_id, index_dt, trt, eventdate, mean(data) as data, data_tp
from bp_2
group by patid, trial_id, time_id, eventdate, data_tp
;quit;
/*proc sql;*/
/*create table test as*/
/*select **/
/*from bp_3 */
/*group by patid, trial_id, eventdate*/
/*having count(data)>2*/
/*;quit;*/
proc sql;
create table bp_4 as 
select distinct patid, trial_id, time_id
		, max(case when data_tp="SBP" then data else . end) as SBP
		, max(case when data_tp="DBP" then data else . end) as DBP
from bp_3
group by patid, trial_id, time_id
;quit;
data bp_5; set bp_4;
if SBP < 120 and DBP < 80 then COV_BP=1; *normal;
if (120 <= SBP and SBP < 130) and DBP < 80 then COV_BP=2; *elevated;
	if (130 <= SBP and SBP < 140) or (80 <= DBP and DBP < 90) then COV_BP=3;*high BP stage 1;
if (140 <= SBP) or (90 <= DBP) then COV_BP=4; *high BP stage 2;
if SBP=. or DBP=. then COV_BP=9; *missing;
run;
/*proc freq data=bp_5; table cov_lab_1; run;*/
data x.seq_cov4_gold; set bp_5; run;

**frailty index;
proc sql;
create table fi_1 as
select distinct a.patid, a.trial_id, a.enrol_dt, a.time_id, a.index_dt
	, (case when count(distinct c.ingr)>=5 then 1 else 0 end) as polypharmacy
from x.seq_indextv_gold as a
left join y.all_rx_gold as b on a.patid=b.patid and a.index_dt - 180 < b.eventdate and b.eventdate <= a.index_dt
left join y.list_rx_fi as c on b.prodcode=c.code and c.code_sys="gold"
group by a.patid, a.trial_id
;quit;
proc sql;
create table fi_2 as
select distinct a.*, count(distinct c.deficit) as n_deficit
from fi_1 as a
left join y.all_dx_gold as b on a.patid=b.patid and b.eventdate <= a.index_dt and b.code_sys="gold"
left join y.list_dx_fi as c on b.code=c.medcode and c.code_sys="gold"
group by a.patid, a.trial_id, a.time_id
;quit;
data fi_3;
set fi_2;
eFI = (n_deficit + polypharmacy)/36;
if 0 <= eFI AND eFI <= 0.12 then COV_DEMO_7=1; *Fit;
if 0.12 < eFI AND eFI <= 0.24 then COV_DEMO_7=2; *Mild frailty;
if 0.24 < eFI AND eFI <= 0.36 then COV_DEMO_7=3; *Moderate frailty;
if 0.36 < eFI then COV_DEMO_7=4; *Severe frailty;
drop polypharmacy n_deficit; 
run;
/*proc freq data=fi_3; table COV_DEMO_7/norow; run;*/
data x.seq_cov5_gold; set fi_3; run;


**number of general practice visits within 180;
proc sql;
create table visit_1 as
select distinct a.patid, a.trial_id, a.enrol_dt, a.time_id, a.index_dt, b.eventdate
from x.seq_indextv_gold as a
left join a.gd_consultation as b on a.patid=b.patid and a.index_dt - 180 < b.eventdate and b.eventdate <= a.index_dt
;quit;
proc sql;
create table visit_2 as
select distinct patid, trial_id, time_id, count(distinct eventdate) as COV_DEMO_8
from visit_1
group by patid, trial_id, time_id
;quit;
data x.seq_cov6_gold; set visit_2; run;

**history of dx, rx (*dyslipidaemia would be defined by dx and lab data);
proc sql;
create table history_1 as
select distinct a.patid, a.trial_id, a.enrol_dt, a.time_id, a.index_dt, b.*
from x.seq_indextv_gold as a
inner join y.cht_cov_dxrx_gold as b on a.patid=b.patid and 0 < b.eventdate and b.eventdate <= a.index_dt
where substr(type_id,1,6)="COV_DX"
;quit;
data history_2;
set history_1;
if type_id="COV_DX_13" and eventdate <= index_dt-180 then delete;
run;*Infection but not recent;
proc sql;
create table history_3 as
select distinct a.patid, a.trial_id, a.enrol_dt, a.time_id, a.index_dt, b.*
from x.seq_indextv_gold as a
inner join y.cht_cov_dxrx_gold as b on a.patid=b.patid and (a.index_dt-180 < b.eventdate and b.eventdate <= a.index_dt) 
where substr(type_id,1,6)="COV_RX"
;quit;
data history_4; set history_2 history_3; run;
%MACRO XXX;
proc sql;
create table history_5 as
select distinct patid, trial_id, enrol_dt, time_id, index_dt
	%do a=1 %to 24; , (case when type_id="COV_DX_&a." then 1 else 0 end) as COV_DX_&a. %end;
	%do b=1 %to 22; , (case when type_id="COV_RX_&b." then 1 else 0 end) as COV_RX_&b. %end;
from history_4
;quit;
proc sql;
create table history_6 as
select distinct patid, trial_id, enrol_dt, time_id, index_dt
	%do a=1 %to 24; , max(COV_DX_&a.) as COV_DX_&a. %end;
	%do b=1 %to 22; , max(COV_RX_&b.) as COV_RX_&b. %end;
from history_5
group by patid, trial_id, time_id
;quit;
%MEND; %XXX;
proc sql;
create table history_7 as
select distinct a.patid, a.trial_id, a.time_id, b.*, c.cov_dx_7_lab
from x.seq_indextv_gold as a
left join history_6 as b on a.patid=b.patid and a.trial_id=b.trial_id and a.time_id=b.time_id
left join x.seq_cov2_gold as c on a.patid=c.patid and a.trial_id=c.trial_id and a.time_id=c.time_id
;quit;
data history_8;
set history_7;
V=max(cov_dx_7, cov_dx_7_lab);
drop cov_dx_7 cov_dx_7_lab;
rename v=COV_DX_7;
Run;
proc stdize data=history_8 out=history_9 reponly missing=0; run;
/*proc tabulate data=history_9;*/
/*class COV_DX_1 - COV_DX_24 COV_RX_1 - COV_RX_22;*/
/*table (all COV_DX_1 - COV_DX_24 COV_RX_1 - COV_RX_22),(N ColPctn);*/
/*run;*/
data x.seq_cov7_gold; set history_9; run;

proc sql;
create table temp as
select distinct a.*, b.*, c.*, d.*, e.*, f.*, g.*, h.*
from x.seq_indextv_gold as a
left join x.seq_cov1_gold as b on a.patid=b.patid and a.trial_id=b.trial_id
left join x.seq_cov2_gold as c on a.patid=c.patid and a.trial_id=c.trial_id and a.time_id=c.time_id
left join x.seq_cov3_gold as d on a.patid=d.patid and a.trial_id=d.trial_id and a.time_id=d.time_id
left join x.seq_cov4_gold as e on a.patid=e.patid and a.trial_id=e.trial_id and a.time_id=e.time_id
left join x.seq_cov5_gold as f on a.patid=f.patid and a.trial_id=f.trial_id and a.time_id=f.time_id
left join x.seq_cov6_gold as g on a.patid=g.patid and a.trial_id=g.trial_id and a.time_id=g.time_id
left join x.seq_cov7_gold as h on a.patid=h.patid and a.trial_id=h.trial_id and a.time_id=h.time_id
;quit;
data y.seq_cov_gold; set temp; run;

/*****************/
/* Final dataset */
/*****************/

%MACRO XXX;
proc sql;
create table temp as 
select distinct a.*, d.htn_1st_dt
	%do a=1 %to 6;
	, b.out&a._dt %end;
	%do b=1 %to 6;
	, b.cens&b._dt %end;
	, b.cens6_dt1, b.cens6_dt2
	%do c=1 %to 8; 
	, c.cov_demo_&c. as cov_base_demo_&c. %end;
	, c.cov_bp as cov_base_bp
	%do d=1 %to 24;
	, c.cov_dx_&d. as cov_base_dx_&d. %end;
	%do e=1 %to 22;
	, c.cov_rx_&e. as cov_base_rx_&e. %end;
from y.seq_asgmt_gold as a
inner join y.seq_fu_gold as b on a.patid=b.patid and a.trial_id=b.trial_id
left join y.seq_cov_gold as c on a.patid=c.patid and a.trial_id=c.trial_id and c.time_id=1
left join y.seq_pat_gold as d on a.patid=d.patid
;quit;
%MEND; %XXX;
data y.fin_wide_gold; set temp; run; *6788;

%MACRO XXX;
proc sql;
create table temp_1 as 
select distinct a.*, b.time_id, b.index_dt
	%do a=4 %to 8; 
	, b.cov_demo_&a. as cov_tv_demo_&a. %end;
	, b.cov_bp as cov_tv_bp
	%do b=1 %to 24;
	, b.cov_dx_&b. as cov_tv_dx_&b. %end;
	%do c=1 %to 22;
	, b.cov_rx_&c. as cov_tv_rx_&c. %end;
from y.fin_wide_gold as a 
left join y.seq_cov_gold as b on a.patid=b.patid and a.trial_id=b.trial_id
;quit;
%MEND; %XXX; *75424>68220 (calcium and lcd);
data y.fin_long_gold; set temp_1; run;

/**********************/
/* Merge GOLD & Aurum */
/**********************/


/************************/
/* Statistical Analysis */
/************************/

*Flow chart;
%Flow (stt_data=a.gd_patient
, out_data=temp_flow
, elig_data_list=x.incl_pre_gold_0 x.incl_pre_gold_1 x.incl_pre_gold_2 x.incl_pre_gold_3 x.incl_gold_0 x.incl_gold_1 x.incl_gold_2
, id=patid
);
data flow_gold_0;
set temp_flow;
trial_id=0;
run;

proc freq data=y.fin_wide_gold; table trial_id*trt/norow nocol nopercent out=seq_pat; run;
data z.seq_pat_n;
set seq_pat;
rename count=n_rest;
if trt=1 then criteria="ENROLMENT - Discontinuation";
if trt=0 then criteria="ENROLMENT - Continuation";
drop percent;
run;

%MACRO XXX;
%do trial_no=1 %to 20;

%let data_list=x.excl_gold_0 x.excl_gold_1 x.excl_gold_4 x.excl_gold_5 x.excl_gold_6 x.excl_gold_7 x.excl_gold_8;
%do data_n=1 %to %sysfunc(countw(&data_list., %str( ),q));
%let data_nm=%scan(&data_list., &data_n., %str( ),q);

%let nn=%index(&data_nm.,.);
%let data_nm_length=%eval(%length(&data_nm.) - &nn.);
%let new_data=%substr(&data_nm., %eval(%length(&data_nm.) - &data_nm_length.)+1, &data_nm_length.);

data &new_data.;
set &data_nm.;
if trial_id=&trial_no.;
run;
%end;

%if &trial_no.=1 %then %do;
%Flow (stt_data=y.cht_pat_gold
, out_data=temp_flow
, elig_data_list=excl_gold_0 excl_gold_1 excl_gold_4 excl_gold_5 excl_gold_6 excl_gold_7 excl_gold_8
, id=patid
); 
%end;

%else %do;
data seq_pat_gold;
set y.fin_wide_gold;
if trial_id=%eval(&trial_no.-1) and trt=0;
run; 

%Flow (stt_data=seq_pat_gold
, out_data=temp_flow
, elig_data_list=excl_gold_0 excl_gold_1 excl_gold_4 excl_gold_5 excl_gold_6 excl_gold_7 excl_gold_8
, id=patid
); %end;

data temp_flow_1;
set temp_flow;
trial_id=&trial_no.;
run;
data temp_flow_2;
set z.seq_pat_n (drop=trt);
if trial_id=&trial_no.;
run;
data temp_flow_3; set temp_flow_1 temp_flow_2; run;
data flow_gold_&trial_no.; set temp_flow_3; run;

proc delete data=temp_flow_1 temp_flow_2 seq_pat_gold excl_gold_0 excl_gold_1 excl_gold_4 excl_gold_5 excl_gold_6 excl_gold_7 excl_gold_8; run;
%end;
%MEND; %XXX;

data z.flow_seq; set flow_gold_0 - flow_gold_20; run;

*Weights: IPTW, IPCW;
**probability of discontinuation;
proc logistic data=y.fin_wide_gold descending;
class trt cov_base_demo_2 - cov_base_demo_7 cov_base_bp cov_base_dx_1 -  cov_base_dx_7 cov_base_dx_9 - cov_base_dx_14 cov_base_dx_16 cov_base_dx_18 - cov_base_dx_21 cov_base_dx_24 cov_base_rx_1 - cov_base_rx_22;
model trt(event='1')=cov_base_demo_1 cov_base_demo_8 cov_base_demo_2 - cov_base_demo_7 cov_base_bp cov_base_dx_1 -  cov_base_dx_7 cov_base_dx_9 - cov_base_dx_14 cov_base_dx_16 cov_base_dx_18 - cov_base_dx_21 cov_base_dx_24 cov_base_rx_1 - cov_base_rx_22;
output out=model_1 prob=prob_1;
run; *Remove due to small size: cov_dx_8 (type 1 diabetes), _15 (liver disease), _17 (gout), _22 (Schizophrenia), _23 (Parkinson);
data temp_1 (drop=_level_ prob_1); 
set model_1; 
if trt=1 then iptw=1/prob_1;
else if trt=0 then iptw=1/(1-prob_1);
run;
data y.fin_wide_wt; set temp_1; run;

**probabaility of uncensoring;
data temp;
set y.fin_long_gold;
format censor_dt yymmdd10.;
censor_dt=min(cens1_dt, cens2_dt, cens3_dt, cens4_dt, cens5_dt, cens6_dt);
if index_dt < censor_dt and censor_dt <= index_dt + (365.25/12)*3 then censor=1;
else if censor_dt <= index_dt then delete;
else censor=0;
run;
proc logistic data=temp;
class censor cov_base_demo_2 - cov_base_demo_7 cov_base_bp cov_base_dx_1 - cov_base_dx_7 cov_base_dx_9 - cov_base_dx_14 cov_base_dx_16 cov_base_dx_18 - cov_base_dx_21 cov_base_dx_24 cov_base_rx_1 - cov_base_rx_22 
	  cov_tv_demo_4 - cov_tv_demo_7 cov_tv_bp cov_tv_dx_1 -  cov_tv_dx_7 cov_tv_dx_9 - cov_tv_dx_14 cov_tv_dx_16 cov_tv_dx_18 - cov_tv_dx_21 cov_tv_dx_24 cov_tv_rx_1 - cov_tv_rx_22;
model censor(event='0')=trt time_id time_id*time_id cov_base_demo_1 - cov_base_demo_8 cov_base_bp cov_base_dx_1 - cov_base_dx_7 cov_base_dx_9 - cov_base_dx_14 cov_base_dx_16 cov_base_dx_18 - cov_base_dx_21 cov_base_dx_24 cov_base_rx_1 - cov_base_rx_22 
cov_tv_demo_4 - cov_tv_demo_7 cov_tv_bp cov_tv_dx_1 -  cov_tv_dx_7 cov_tv_dx_9 - cov_tv_dx_14 cov_tv_dx_16 cov_tv_dx_18 - cov_tv_dx_21 cov_tv_dx_24 cov_tv_rx_1 - cov_tv_rx_22;
output out=model_2 prob=prob_2;
run;
proc sql;
create table temp_2 as
select distinct a.*, b.prob_1
from model_2 as a
left join model_1 as b on a.patid=b.patid and a.trial_id=b.trial_id
order by patid, trial_id, time_id
;quit;
data temp_3 (drop=_level_);
set temp_2;
by patid trial_id time_id;
if first.trial_id then do; prob_2_cp=1; end;
retain prob_2_cp;
prob_2_cp=prob_2_cp*prob_2;
ipcw=1/prob_2_cp;
if trt=1 then iptw=1/prob_1;
else if trt=0 then iptw=1/(1-prob_1);
run;
data temp_4 (drop=prob_1 prob_2 prob_2_cp);
set temp_3;
w1=iptw;
w2=iptw*ipcw;
run;
data y.fin_long_wt; set temp_4; run;

*Baseline characteristics;
%Table1 (in_for_table1=y.fin_wide_wt
, treatment_var=trt
, categorical_var_list=cov_base_demo_2 cov_base_demo_3 cov_base_demo_4 cov_base_demo_5 cov_base_demo_6 cov_base_demo_7 cov_base_bp cov_base_dx_1 cov_base_dx_2 cov_base_dx_3 cov_base_dx_4 cov_base_dx_5 cov_base_dx_6 cov_base_dx_7 cov_base_dx_8 cov_base_dx_9 cov_base_dx_10 cov_base_dx_11 cov_base_dx_12 cov_base_dx_13 cov_base_dx_14 cov_base_dx_15 cov_base_dx_16 cov_base_dx_17 cov_base_dx_18 cov_base_dx_19 cov_base_dx_20 cov_base_dx_21 cov_base_dx_22 cov_base_dx_23 cov_base_dx_24 cov_base_rx_1 cov_base_rx_2 cov_base_rx_3 cov_base_rx_4 cov_base_rx_5 cov_base_rx_6 cov_base_rx_7 cov_base_rx_8 cov_base_rx_9 cov_base_rx_10 cov_base_rx_11 cov_base_rx_12 cov_base_rx_13 cov_base_rx_14 cov_base_rx_15 cov_base_rx_16 cov_base_rx_17 cov_base_rx_18 cov_base_rx_19 cov_base_rx_20 cov_base_rx_21 cov_base_rx_22
, continuous_var_list=cov_base_demo_1 cov_base_demo_8
, weight=dummy_weight
, out_table1=z.char_crude);

*Risk analysis;

data temp_crude;
set y.fin_long_wt;
rename &outcome.=outcome &group.=exposure;
run;
ff
