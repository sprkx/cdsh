%MACRO IMPORT_TXT (path=, filename=, lib=, data_name=, var_name_list=, var_format_list=, delimiter=);
data &lib..&data_name.;
%let _EFIERR_ = 0; /* set the ERROR detection macro variable */
infile "&path.\&filename..txt"
delimiter=&delimiter. MISSOVER DSD lrecl=32767 firstobs=2 ;

%let input_list=;
%do var_n=1 %to %sysfunc(countw(&var_name_list., %str( ),q));
	%let var_name=%scan(&var_name_list., &var_n., %str( ),q);
	%let var_format=%scan(&var_format_list., &var_n., %str( ),q);
	informat &var_name. &var_format.;
	format &var_name. &var_format.;
	%if %substr(&var_format.,1,1)=$ %then %let input_list=&input_list. &var_name. $;
	%else %let input_list=&input_list. &var_name.;
%end;
input &input_list.;
if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
run;
%MEND;
%MACRO IMPORT_XLSX (path=, filename=, lib=, data_name=, sheet=);
proc import
out=&lib..&data_name.
datafile= "&path.\&filename..xlsx"
dbms=xlsx replace;
sheet="&sheet.";
run;
%MEND;
%MACRO SPLIT (indata=, outdata=, symbol=, col_name=, keep_var_list=);
proc sql noprint;
select max(count(&col_name.,&symbol.))+1 into :max_cnt
from &indata.;
quit;
data temp_1;
set &indata.;
length &col_name.1 $100;
array name (&max_cnt) $100;
do i=1 to &max_cnt;
name(i)=scan(&col_name.,i,&symbol.);
&col_name.1=name(i);
if &col_name.1^="" then output;
end;
keep &keep_var_list. &col_name. &col_name.1;
run;
data &outdata;
set temp_1;
drop &col_name.;
rename &col_name.1=&col_name.;
run;
proc delete data=temp_1; run;
%MEND;
%MACRO EXTRACT (data_in=, data_out=, data_list=, remove_migrators=);
proc sql;
create table &data_out. as
select *
from &data_in. 
where patid in (select patid from &data_list.)
%IF &remove_migrators.=Y %then %do;
	and (substr(patid, length(patid)-4, 5)) not in (select gold_pracid from b.prac_migrt)
%end;
%else;
;
quit;
%MEND;
%MACRO Flow (stt_data=, out_data=, elig_data_list=, id=);
data temp_0; set &stt_data.; run;

proc sql;
create table rest_0 as
select count(distinct &id.) as n_rest
from temp_0;
quit;

%let elig_data_list1=&elig_data_list.;
%let i=1;
%do order=1 %to %sysfunc(countw(&elig_data_list1.,%str( ),q));
%let elig_data=%scan(&elig_data_list1., &order., %str( ),q);

%let lib_n=%index(&elig_data.,.);
%let crtr_length=%eval(%length(&elig_data.) - &lib_n.);
%let criteria=%substr(&elig_data.,%eval(%length(&elig_data.) - &crtr_length.)+1,&crtr_length.);

data temp_1; set temp_0; run;

proc sql;
create table excl_&i. as
select count(distinct &id.) as n_excl, "&criteria." as criteria length=30
from temp_1
%if %index(&elig_data.,incl)>0 %then %do; 
where &id. not in (select &id. from &elig_data.)
%end;
%if %index(&elig_data.,excl)>0 %then %do; 
	where &id. in (select &id. from &elig_data.)
%end;
;quit;

proc sql;
create table temp_2 as
select *
from temp_1
%if %index(&elig_data.,incl)>0 %then %do; 
where &id. in (select &id. from &elig_data.)
%end;
%if %index(&elig_data.,excl)>0 %then %do; 
where &id. not in (select &id. from &elig_data.)
%end;
;quit;

proc sql;
create table rest_&i. as
select count(distinct &id.) as n_rest
from temp_2;
quit;

data temp_0; set temp_2; run;
proc delete data=temp_1 temp_2; run;
%let i=%eval(&i.+1);
%end;
%let i=%eval(&i.-1);
data rest; set rest_1 - rest_&i.; run;
data excl; set excl_1 - excl_&i.; run;

data temp_3; merge rest excl; run;
data &out_data.; set rest_0 temp_3; run; 
%MEND;
%MACRO Eligible (in_data=, out_data=, info_data=, assmnt_period1=, assmnt_period2=, assmnt_id=);
proc sql;
create table &out_data. as
select distinct a.*, b.eventdate, b.type_id
from &in_data. as a
inner join &info_data. as b on a.patid=b.patid and a.enrol_dt + &assmnt_period1. <= b.eventdate <= a.enrol_dt + &assmnt_period2.
where b.type_id="&assmnt_id."
;quit;
%MEND;
