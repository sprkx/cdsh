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
%MACRO FLOW (stt_data=, out_data=, elig_data_list=, id=);
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
%MACRO TABLE1 (in_for_table1= , treatment_var= , categorical_var_list= , continuous_var_list= , weight=dummy_weight, out_table1= ); 
* we dont need to see each 2 by 2 table or proc means output, so suppressing it. this information is saved as sas datasets 
and provided in excel format later; 

ods graphics off;            /* or use the %ODSOff macro */
ods exclude all;             /* suspend all open destinations */

data s1; set &in_for_table1; dummy_weight=1; *create dummy weights ;
trt=&treatment_var; * rename treatment variable to a simpler form for use within this macro; 
run;

************************** Crude table- categorical covariates *****************************************;


data fortable; retain &categorical_var_list; set s1;
keep &categorical_var_list; 
run;

proc contents data = fortable 
out = vars(keep = name varnum)
noprint;
run; 

proc sort data=vars; by varnum; run; 

proc sql noprint; select max(varnum) into: n from vars; quit;

%do i= 1 %to &n;

proc sql noprint; select NAME into: variable from vars where varnum=&i; quit;

ods output CrossTabFreqs=tabs_&i; * see in reults explorer-> results tab of interest-> properties-> name;

proc freq data=s1;
tables (&variable)*trt/nopct norow chisq;
weight &weight; 
run;

ods output clear;
run;

proc sort data=tabs_&i;
by trt;
run;

data tabs_&i;
set tabs_&i; where trt ne . ; *delete totals;
run;

data tabs_&i; set tabs_&i; rename &variable=cat; run;
data tabs_&i; set tabs_&i; category=put(cat, 10.); drop cat _LABEL_; run;

   proc append base= freqs data = tabs_&i; 
   run;

%end;

data cat_freqs; set freqs; where ColPercent ne .; 
trt_cat=catx('_', 'trt_cat', trt);
run;

*The following statements delete the base version and rename the youngest historical version to the base version. This is done because otherwise proc append keeps 
adding output rows to your output dataset across different runs, which is often times not desirable;

proc datasets NOLIST;
 delete freqs (gennum=0);
quit;

data temp (index = (trt_cat));
  set cat_freqs;
  obs=_n_;
run;

data _null_ ;
  dcl hash hh   (             ) ;
  hh.definekey  ('k'          ) ;
  hh.definedata ('table', 'trt', 'category','frequency', 'colpercent', 'obs') ;
  hh.definedone () ;
  do k = 1 by 1 until ( last.trt_cat ) ;
    set temp;
    by trt_cat ;
    hh.add () ;
  end ;
  hh.output (dataset: trt_cat) ;
run ;

proc sort data= trt_cat_0; by table category; run; 
proc sort data= trt_cat_1; by table category; run; 

data t1a; merge trt_cat_0 (rename= (frequency= Ref_freq colpercent=Ref_percent)) 
trt_cat_1 (rename= (frequency= Treated_freq colpercent=Treated_percent)); 
by table category; 
drop trt;
run;

proc datasets NOLIST;
   modify t1a; 
     attrib _all_ label=' '; *removing labels to preserve ordering; 
run;

proc sort data=t1a; by obs ; run;

data t1a; set t1a; drop obs; run;

************************** Crude table- continuous covariates *****************************************;

data formeans; retain &continuous_var_list; set s1;
keep &continuous_var_list; 
run;

proc contents data = formeans
out = contvars(keep = name varnum)
noprint;
run; 

proc sql noprint; select max(varnum) into: n from contvars; quit;

%do i= 1 %to &n;

proc sql noprint; select NAME into: variable from contvars where varnum=&i; quit;

proc means data=s1 noprint;
var &variable;
class trt;
weight &weight; 
OUTPUT OUT=c1;
run;

data c1; set c1; where trt ne . and _stat_ in ('MEAN', 'STD'); run;

proc transpose data=c1 out=means_&i; 
by trt; *subject id;
      id _stat_; *prescription id: Goes to the heading of the column;
		var &variable; *variable that goes in the column;
      run;

data means_&i; set means_&i; category=put(_name_, 50.); drop _name_ _LABEL_; run;

   proc append base= means data = means_&i; 
   run;

%end;

data cont_means; set means;  
n_percent= catx('/', MEAN, STD);
trt_cat=catx('_', 'trt_cat', trt);
rename mean=frequency std=colpercent ;
run;

*The following statements delete the base version and rename the youngest historical version to the base version. This is done because otherwise proc append keeps 
adding output rows to your output dataset across different runs, which is often times not desirable;

proc datasets NOLIST;
 delete means (gennum=0);
quit;

data temp1 (index = (trt_cat));
  set cont_means;
run;

data _null_ ;
  dcl hash hh   (             ) ;
  hh.definekey  ('k'          ) ;
  hh.definedata ('trt', 'category','frequency', 'colpercent', 'n_percent') ;
  hh.definedone () ;
  do k = 1 by 1 until ( last.trt_cat ) ;
    set temp1;
    by trt_cat ;
    hh.add () ;
  end ;
  hh.output (dataset: trt_cat) ;
run ;

data t1b; merge trt_cat_0 (rename= (frequency= Ref_freq colpercent=Ref_percent n_percent= Reference_n_percent)) 
trt_cat_1 (rename= (frequency= Treated_freq colpercent=Treated_percent n_percent= Treated_n_percent)); 
drop trt;
run;

proc datasets NOLIST;
   modify t1b; 
     attrib _all_ label=' '; 
run;

data t1_combined; set t1b (in=r) t1a (in=s); length variable_type $25.;
if r=1 then variable_type='Continuous [mean(sd)]'; 
if s=1 then variable_type='Categorical [n(%)]';
run; 

** concatenate categorical and continuous covariate tables; 

data t1_combined1; set t1_combined;
pre=' (';
post=')';
rounded_ref_freq= round(ref_freq, 1); if table= '' then rounded_ref_freq=round(ref_freq, 0.1);
rounded_treated_freq= round(treated_freq, 1); if table= '' then rounded_treated_freq=round(treated_freq, 0.1);
rounded_ref_percent= round(ref_percent, 0.1);
rounded_treated_percent= round(treated_percent, 0.1);
** quantities needed for std diff calculation; 
pt=(treated_percent/100); pc= (ref_percent/100); ** proportion of treated and reference for categorial variables;
xt=(treated_freq); xc= (Ref_freq); *means in treated and reference for continuous variables; 
st2= treated_percent*treated_percent; sc2=ref_percent*ref_percent; 
*variances in treated and reference for continuous variables; 
run;

** calculate the crude and standardized difference and prepare columns for output; 

data t1_combined2; length table $50;
set t1_combined1;
length ref_column $25 Treatment_column $25;
crude_diff= round((treated_percent-ref_percent), 0.1); ** percent differences for categorical covariates; 
if table= '' then crude_diff= round((treated_freq-ref_freq), 0.1); ** mean differences for continuous covariates; 
std_diff= round((100*(pt-pc))/ (sqrt((pt*(1-pt)+ pc*(1-pc))/2)), 0.1); ** categorical covariates; 
if table= '' then std_diff= round((100*(xt-xc))/sqrt((st2+sc2)/2), 0.1); ** continuous covariates; 
Ref_column= cat(rounded_ref_freq, pre, rounded_ref_percent, post);
Treatment_column= cat(rounded_treated_freq, pre, rounded_treated_percent, post);
run;

data t1_combined2; set t1_combined2; 
cat=strip(category); 
v=tranwrd(table, "Table",'');
v=tranwrd(v,"* trt",'');
run; 

data t1_combined3; set t1_combined2; where cat ne "0"; run;

data t1_combined3; set t1_combined3; length variable $50.;
variable= cats (v, "=", cat);
if v= '' then variable=cat; 
run;

** total row; 

proc freq data=s1; table &treatment_var/out=total_row1; 
weight &weight; 
run; 

proc sql noprint; create table total_treat as select COUNT as Treatment_column1 from total_row1 where &treatment_var=1 ; quit; 

proc sql noprint; create table total_ref as select COUNT as ref_column1 from total_row1 where &treatment_var=0 ; quit; 

data total_row; merge total_treat total_ref; run; 

data total_row; set total_row; 
treatment_column  = put(treatment_column1, 25. -L);
ref_column  = put(ref_column1, 25. -L);
run;

data t1_combined4; set total_row t1_combined3;
drop treatment_column1 ref_column1;
run; 

** final table;

data &out_table1; set t1_combined4; 
keep variable variable_type Treated_freq Treated_percent Ref_freq Ref_percent Treatment_column Ref_column crude_diff std_diff;
if variable= '' then variable='Total';
run; 

ods exclude none; 

proc datasets lib=work nolist;
 delete tabs: means:;
quit;
run;
%MEND;
%MACRO PLOT (type=, in_data=, out_surv_data=, rst_test=
, outcome_var=, group_var=, weight=
, fu_var=, fu_max=, fu_inc=, y_max=
, line_thickness= , file_name=, format=, note=);

data temp_1;
set &in_data.;
dummy_weight=1;
run;

%if &type.=ci %then %do;
%let y_axis=_1_SURVIVAL_;
%let y_label=Cumulative Incidence;
proc lifetest data=temp_1 plots=survival(f)
outsurv=survival_data timelist=(0 to &fu_max. by &fu_inc.) reduceout;
strata &group_var.;
time &fu_var.*&outcome_var.(0);
weight &weight.;
ods output failureplot=temp_2 HomTests=temp_test;
run;
%end;

%else %if &type.=km %then %do;
%let y_axis=SURVIVAL;
%let y_label=Survival Probability;
proc lifetest data=temp_1 plots=survival
outsurv=survival_data timelist=(0 to &fu_max. by &fu_inc.) reduceout;
strata &group_var.;
time &fu_var.*&outcome_var.(0);
weight &weight.;
ods output survivalplot=temp_2 HomTests=temp_test;
run;
%end;

ods graphics / reset imagename="&file_name." imagefmt=&format.;
proc sgplot data=temp_2;
step x=time y=&y_axis./group=stratum lineattrs=(pattern=1 thickness=&line_thickness.);
xaxis values=(0 to &fu_max. by &fu_inc.) valueshint label="Time to event";
yaxis offsetmin=0.02 min=0 offsetmax=0.1 max=&y_max. label="&y_label.";
run;

data &out_surv_data.;
set temp_2;
note="&note.";
run;
data &rst_test.;
set temp_test; 
note="&note.";
run;
proc delete data=temp_1 temp_2 temp_test; run;
%MEND;

