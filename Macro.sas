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
%MACRO Eligible (in_data=, out_data=, info_data=, assmnt_period1=, assmnt_period2=, assmnt_id=);
proc sql;
create table &out_data. as
select distinct a.*, b.eventdate, b.type_id
from &in_data. as a
inner join &info_data. as b on a.patid=b.patid and a.enrol_dt + &assmnt_period1. <= b.eventdate <= a.enrol_dt + &assmnt_period2.
where b.type_id="&assmnt_id."
;quit;
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
%MACRO IPTW (in_data=, group_var=, varlist_cont=, varlist_cate=
, truncate_max=, truncate_min=, weight_var=
, out_data_all=, out_data_trunc=);

proc logistic data=&in_data. desc;
class &group_var. &varlist_cate.;
model &group_var.=&varlist_cont. &varlist_cate.;
output out=ps_1 prob=prob;
run;*Remove due to small size: cov_dx_8 (type 1 diabetes), _15 (liver disease), _17 (gout), _23 (Parkinson);
data ps_2;
set ps_1;
if &group_var.=1 then &weight_var.=1/prob;
else if &group_var.=0 then &weight_var.=1/(1-prob);
run;
data &out_data_all.; set ps_2;
proc rank data=ps_2 out=ps_3 groups=1000;
var &weight_var.;
ranks rank;
run;
data ps_4;
set ps_3;
if &truncate_min.*1000 < rank+1 and rank+1 < &truncate_max.*1000;
run;
/*proc sort data=ps_4; by descending rank; run;*/
data &out_data_trunc.; set ps_4; run;
proc delete data=ps_1 - ps_4; run;
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
%MACRO fu_data (in_data=, out_data=) /minoperator;
data temp_fu;
set &in_data.;
format fu_end_dt yymmdd10. roc1 - roc6 $8.;
fu_end_dt=min(cens1_dt, cens2_dt, cens3_dt, cens4_dt);
%do out_i=1 %to 6;
%if (&out_i.=1 or &out_i.=5 or &out_i.=6) %then %do;
t&out_i.= min(out&out_i._dt, fu_end_dt)-enrol_dt;
if out&out_i._dt^="." and out&out_i._dt= min(out&out_i._dt, fu_end_dt) then out&out_i.=1; 
else do; 
	out&out_i.=0;
	if fu_end_dt=cens1_dt then roc&out_i.="censor1";
	else if fu_end_dt=cens2_dt then roc&out_i.="censor2";
	else if fu_end_dt=cens3_dt then roc&out_i.="censor3";
	else if fu_end_dt=cens4_dt then roc&out_i.="censor4";
end;
%end;

%if (&out_i.=2 or &out_i.=3 or &out_i.=4) %then %do;
t&out_i.= min(out&out_i._dt, fu_end_dt, out1_dt) - enrol_dt;
if out&out_i._dt^="." and out&out_i._dt= min(out&out_i._dt, fu_end_dt, out1_dt) then out&out_i.=1; 
else do; 
	out&out_i.=0;
	if out1_dt^=. and out1_dt <= fu_end_dt then roc&out_i.="MACE";
	else if fu_end_dt=cens1_dt then roc&out_i.="censor1";
	else if fu_end_dt=cens2_dt then roc&out_i.="censor2";
	else if fu_end_dt=cens3_dt then roc&out_i.="censor3";
	else if fu_end_dt=cens4_dt then roc&out_i.="censor4";
end;
%end;
%end;
rename trt=exposure;
run;

data &out_data.;
set temp_fu;
keep exposure patid trial_id fu_end_dt t1 out1 roc1 t2 out2 roc2 t3 out3 roc3 t4 out4 roc4 t5 out5 roc5 t6 out6 roc6 iptw;
run;
proc delete data=temp_fu; run;
%MEND;
%MACRO RiskAnalysis (in_data_crude=, in_data_adjust=
, outcome=, group=, fu_time=, weight=
, adj_varlist_cate= , adj_varlist_cont=
, out_data=, output_trt0=, output_trt1=
, whycensor=, reason_censor=, out_data_whycensor=
, note=
);

data temp_crude;
set &in_data_crude.;
rename &outcome.=outcome &group.=exposure &fu_time.=fu_day;
%if &whycensor.=Y %then %do; rename &reason_censor.=reason; %end;
run;
data temp_adjust;
set &in_data_adjust.;
rename &outcome.=outcome &group.=exposure &fu_time.=fu_day;
%if &whycensor.=Y %then %do; rename &reason_censor.=reason; %end;
dummy_weight=1;
run;

**Number of patients and events;
proc freq data=temp_crude;
tables outcome * exposure / norow nocol nopercent;
ods output CrossTabFreqs=crude_freq;
run;
proc freq data=temp_adjust;
tables outcome * exposure / norow nocol nopercent;
weight &weight.;
ods output CrossTabFreqs=adjust_freq;
run;

**Follow-up;
proc tabulate data=temp_crude out=crude_fu;
var fu_day;
class exposure;
tables (fu_day)*(sum mean std median q1 q3 min max), (exposure);
run;
proc tabulate data=temp_adjust out=adjust_fu;
var fu_day /weight=&weight.;
class exposure;
tables (fu_day)*(sum mean std median q1 q3 min max), (exposure);
run;

**Reason of censoring;
%if &whycensor.=Y %then %do;
proc tabulate data=temp_crude out=censor_1;
class exposure reason;
tables (all reason)*(N), (exposure);
run;
proc tabulate data=temp_adjust out=censor_2;
class exposure reason;
var &weight.;
tables (all reason)*(sum*&weight.), (exposure);
proc sql;
create table &out_data_whycensor. as
select distinct a.exposure, "&note." as Note length=30, a.reason, a.N as N_crude, b.&weight._sum as N_adjust format 8.1
from censor_1 as a
left join censor_2 as b on a.exposure=b.exposure and a.reason=b.reason
;quit; 
run;
%end;

**Pooled logistic regression model;
proc genmod data=temp_crude descending;
class exposure (ref='0');
model outcome=exposure/link=logit dist=bin;
lsmeans exposure / ilink exp oddsratio diff cl;
ods output Diffs=crude_risk;
run;
proc genmod data=temp_adjust descending;
class exposure (ref='0') &adj_varlist_cate.;
model outcome=exposure &adj_varlist_cate./link=logit dist=bin;
lsmeans exposure/ ilink exp oddsratio diff cl;
weight &weight.;
ods output Diffs=adjust_risk;
run;

proc sql;
create table temp_rst as
select distinct 
	a.exposure as trt format 1.
	, (case when a.exposure=0 then "&output_trt0." else "&output_trt1." end) as Treatment
	, b.frequency as Patients_c label=""
	, c.frequency as Events_c label=""
	, d.fu_day_mean as MeanFU_c label="" format 8.1
	, e.OddsRatio as cOR label=""
	, e.LowerOR as cOR_Lower label=""
	, e.UpperOR as cOR_Upper label=""
	, f.frequency as Patients_a label="" format 8.1
	, g.frequency as Events_a label="" format 8.1
	, h.fu_day_mean as MeanFU_a label="" format 8.1
	, i.OddsRatio as aOR label=""
	, e.LowerOR as aOR_Lower label=""
	, e.UpperOR as aOR_Upper label=""
	, "&note." as Note length=30
from crude_fu as a
left join crude_freq as b on a.exposure=b.exposure and b.outcome not in (0,1)
left join crude_freq as c on a.exposure=c.exposure and c.outcome=1
left join crude_fu as d on a.exposure=d.exposure
left join crude_risk as e on a.exposure=1
left join adjust_freq as f on a.exposure=f.exposure and f.outcome not in (0,1)
left join adjust_freq as g on a.exposure=g.exposure and g.outcome=1
left join adjust_fu as h on a.exposure=h.exposure
left join adjust_risk as i on a.exposure=1
;quit;
proc sort data=temp_rst; by descending trt; run;
data &out_data.; set temp_rst; run;

proc delete data=temp_rst censor_1 censor_2 adjust_freq adjust_fu adjust_risk crude_freq crude_fu crude_risk temp_adjust temp_crude; run;
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
