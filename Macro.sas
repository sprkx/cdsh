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
;quit;
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
%MACRO flow_trials(flow_stt_data=, excl_data_list=, wide_data=, seq_patn_data=);
%do trial_no=1 %to 20;

%let data_list=&excl_data_list.;
%let new_data_list=;

%do data_n=1 %to %sysfunc(countw(&data_list., %str( ),q));
%let data_nm=%scan(&data_list., &data_n., %str( ),q);
%let nn=%index(&data_nm.,.);
%let data_nm_length=%eval(%length(&data_nm.) - &nn.);
%let new_data=%substr(&data_nm., %eval(%length(&data_nm.) - &data_nm_length.)+1, &data_nm_length.);

data &new_data.;
set &data_nm.;
if trial_id=&trial_no.;
run;
%let new_data_list=&new_data_list. &new_data.;
%end;

%if &trial_no.=1 %then %do;
%Flow (stt_data=&flow_stt_data.
, out_data=temp_flow
, elig_data_list=&new_data_list.
, id=patid
); 
%end;

%else %do;
data seq_pat_gold;
set &wide_data.;
if trial_id=%eval(&trial_no.-1) and trt=0;
run; 

%Flow (stt_data=seq_pat_gold
, out_data=temp_flow
, elig_data_list=&new_data_list.
, id=patid
); %end;

data temp_flow_1;
set temp_flow;
trial_id=&trial_no.;
run;
data temp_flow_2;
set &seq_patn_data. (drop=trt);
if trial_id=&trial_no.;
run;
data temp_flow_3; set temp_flow_1 temp_flow_2; run;
data flow_gold_&trial_no.; set temp_flow_3; run;

proc delete data=temp_flow_1 temp_flow_2 seq_pat_gold excl_gold_0 excl_gold_1 excl_gold_4 excl_gold_5 excl_gold_6 excl_gold_7 excl_gold_8; run;
%end;
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
 delete tabs: means: trt_: total_: t1: c1 s1 cat_freqs cont: for: vars: temp temp1;
quit;
run;
%MEND;
%MACRO IPTW(in_data=, out_data=, group_var=, var_cate=, var_cont=);
proc logistic data=&in_data. descending;
class &group_var. &var_cate.;
model &group_var.(event='1')=&var_cate. &var_cont.;
output out=model_1 prob=prob_1;
run; 
proc sql;
select sum(case when &group_var.=1 then 1 else 0 end)
	, sum(case when &group_var.=0 then 1 else 0 end)
	into: n_trt1, :n_trt0
from model_1
;quit;
data temp_1 (drop=_level_ prob_1); 
set model_1; 
p=&n_trt1./(&n_trt1.+&n_trt0.);
if  &group_var.=1 then do; iptw=1/prob_1; stabiptw=p/prob_1; end;
else if  &group_var.=0 then do; iptw=1/(1-prob_1); stabiptw=(1-p)/(1-prob_1); end;
run;
data &out_data.; set temp_1; run;
proc delete data=model_1 temp_1; run;
%MEND;
%MACRO IPCW (in_data=, out_data=, group_var=, censor_dt= , futv_stt_dt=, futv_dur=, var_cate=, var_cont=, stab_var_cate=, stab_var_cont=);
data temp_1;
set &in_data.;
if &futv_stt_dt. < &censor_dt. and &censor_dt. <= &futv_stt_dt. + &futv_dur. then crossover=1;
else if &censor_dt.^=. and &censor_dt. <= &futv_stt_dt. then delete;
else crossover=0;
run;
proc logistic data=temp_1 descending;
class crossover &group_var. &var_cate.;
model crossover(event='0')= &group_var. time_id time_id*time_id &var_cate. &var_cont.;
output out=model_2 prob=prob_2;
proc logistic data=temp_1 descending;
class crossover &group_var. &stab_var_cate.;
model crossover(event='0')= &group_var. time_id time_id*time_id &stab_var_cate. &stab_var_cont.;
output out=model_3 prob=prob_3;
run;
proc sql;
create table temp_2 as
select distinct a.*, b.prob_3
from model_2 as a
left join model_3 as b on a.patid=b.patid and a.trial_id=b.trial_id and a.time_id=b.time_id
;quit;
proc sort data=temp_2 out=temp_3; by patid trial_id time_id; run;
data temp_4 (drop=_level_ prob_2 prob_2cp prob_3 prob_3cp); 
set temp_3; 
by patid trial_id time_id;
if first.trial_id then do; prob_2cp=1; prob_3cp=1; end;
retain prob_2cp prob_3cp;
prob_2cp=prob_2cp*prob_2;
prob_3cp=prob_3cp*prob_3;
ipcw=1/prob_2cp;
stabipcw=prob_3cp/prob_2cp;
run;
data &out_data.; set temp_4; run;
proc delete data=model_2 model_3 temp_1 - temp_4; run;
%MEND;
%MACRO TRUNCATION (in_data=, out_data=, weight=, min=, max=);
proc rank data=&in_data. out=temp_tr_1 groups=1000;
var &weight.;
ranks rank;
run;
proc sql noprint;
select max(case when rank+1=&max.*10 then &weight. end) 
	, min(case when rank+1=&min.*10 then &weight. end) 
into: w_max, :w_min
from temp_tr_1
;quit;
data temp_tr_2;
set temp_tr_1;
if &max.*10 < rank+1 then w_new=&w_max.;
else if rank+1 < &min.*10 then w_new=&w_min.;
else w_new=&weight.;
run;
data temp_tr_3;
set temp_tr_2;
drop &weight.;
rename w_new=&weight.;
run;
data &out_data.; set temp_tr_3; run;
proc delete data=temp_tr_1 - temp_tr_3; run;
%MEND;
%MACRO OutcomeDataset (in_data_wide=, out_data_wide=, in_data_long=, out_data_long=, fu_stt_dt=, fu_end_dt=, outcome=, output_fu_var=, futv_stt_dt=, futv_dur=);
data &out_data_wide.;
set &in_data_wide.;
format fu_end_dt yymmdd10.;
fu_end_dt=&fu_end_dt.;
&output_fu_var.= min(&outcome._dt, fu_end_dt)-&fu_stt_dt.;
if &outcome._dt^="." and &outcome._dt= min(&outcome._dt, fu_end_dt) then outcome=1; 
else outcome=0;
run;
data &out_data_long.;
set &in_data_long.;
format fu_end_dt yymmdd10.;
fu_end_dt=&fu_end_dt.;
if min(&outcome._dt, fu_end_dt)^=. and min(&outcome._dt, fu_end_dt) <= &futv_stt_dt. then delete;
else if &futv_stt_dt. < &outcome._dt and &outcome._dt <= &futv_stt_dt. + &futv_dur. then outcome=1;
else outcome=0;
run;
%MEND;
%MACRO RiskEstimate(in_data_wide=, in_data_long=, out_data=, group_var=, fu_var=, weight_var=, adj_var_cate=, adj_var_cont=, label_trt1=, label_trt0=, note=);
proc freq data=&in_data_wide.;
tables outcome * &group_var. / norow nocol nopercent;
ods output CrossTabFreqs=freq;
run;
proc tabulate data=&in_data_wide. out=fu;
var &fu_var.;
class &group_var.;
tables (&fu_var.)*(sum mean std median q1 q3 min max), (&group_var.);
run;
proc genmod data=&in_data_long. descending;
class patid &group_var. (ref='0');
model outcome=&group_var. trial_id time_id time_id*time_id/link=logit dist=bin;
lsmeans &group_var./ ilink exp oddsratio diff cl;
repeated subject=patid/ type=ind;
ods output Diffs=risk;
run;
proc genmod data=&in_data_long. descending;
class patid &group_var. (ref='0') &adj_var_cate. ;
model outcome=&group_var. trial_id time_id time_id*time_id &adj_var_cate. &adj_var_cont. /link=logit dist=bin;
lsmeans &group_var./ ilink exp oddsratio diff cl;
repeated subject=patid/ type=ind;
weight &weight_var.;
ods output Diffs=risk_wt;
run;
proc sql;
create table temp_rst as
select distinct 
	a.trt as trt format 1.
	, (case when a.&group_var.=0 then "&label_trt0." else "&label_trt1." end) as Treatment
	, b.frequency as Patients label=""
	, c.frequency as Events label=""
	, d.fu_day_mean as MeanFU label="" format 8.1
	, e.OddsRatio as cOR label=""
	, e.LowerOR as cOR_Lower label=""
	, e.UpperOR as cOR_Upper label=""
	, f.OddsRatio as wOR label=""
	, f.LowerOR as wOR_Lower label=""
	, f.UpperOR as wOR_Upper label=""
	, "&note." as Note length=30
from fu as a
left join freq as b on a.&group_var.=b.&group_var. and b.outcome not in (0,1)
left join freq as c on a.&group_var.=c.&group_var. and c.outcome=1
left join fu as d on a.&group_var.=d.&group_var.
left join risk as e on a.&group_var.=1
left join risk_wt as f on a.&group_var.=1
;quit;
proc sort data=temp_rst; by descending &group_var.; run;
data &out_data.; set temp_rst; run;
proc delete data=temp_rst fu freq risk risk_wt; run;
%MEND;
%MACRO PLOT_LONG (type=, in_data=, out_data=, note=
, line_thick=, label_trt0=, label_trt1=
, file_name=, format=);	
data temp; set &in_data.; t=time_id-1; run;
proc sort data=temp; by patid trial_id t; run;

proc logistic data=temp outmodel=model_4; /*NEED ADJUST FOR WEIGHTS AND BASELINE FACTORS*/
class trt (ref="0") trial_id cov_base_demo_2 - cov_base_demo_7 cov_base_bp cov_base_dx_1 -  cov_base_dx_7 cov_base_dx_9 - cov_base_dx_14 cov_base_dx_16 cov_base_dx_18 - cov_base_dx_21 cov_base_dx_24 cov_base_rx_1 - cov_base_rx_22;
model outcome = trt trial_id t t*t trt*t trt*t*t cov_base_demo_1 cov_base_demo_8 cov_base_demo_2 - cov_base_demo_7 cov_base_bp cov_base_dx_1 -  cov_base_dx_7 cov_base_dx_9 - cov_base_dx_14 cov_base_dx_16 cov_base_dx_18 - cov_base_dx_21 cov_base_dx_24 cov_base_rx_1 - cov_base_rx_22;
weight weight;
run;
/* creation of dataset with all time points under each treatment level */
data temp_1;
set temp (where = (t=0));
do t = 0 to 11;
trt = 1; output; trt = 0; output;
end;
run;
/* assignment of estimated (1-hazard) to each person-month */
proc logistic inmodel=model_4;
score data=temp_1 out=pred (keep=patid trial_id t trt P_0 rename = (P_0=p_noevent)) ;
run;
proc sort data=pred; by patid trial_id trt t; run;
/* computation of survival for each person-month */
data pred_1;
set pred;
by  patid trial_id trt;
retain surv;
if first.trt then surv=1 ;
surv=surv*p_noevent;
run;
/* some data management to plot estimated survival curves */
proc means data=pred_1 noprint;
class trt t;
var surv;
types trt*t;
output out=pred_2 (keep=trt t surv) mean=surv;
run;

proc sql;
create table pred_3 as
select distinct "&note." as note, a.t, a.surv as surv0, (1-a.surv) as ci0
				, b.surv as surv1, (1-b.surv) as ci1
				, (b.surv-a.surv) as surv_diff, (a.surv-b.surv) as ci_diff
from pred_2 as a
left join pred_2 as b on a.t=b.t and b.trt=1
where a.trt=0
;quit;

ods graphics on/ reset imagename="&file_name." imagefmt=&format. ANTIALIASMAX=3133700;
proc sgplot data = pred_3 ;
%if &type.=surv %then %do;
   series x = t y = surv0 /legendlabel="&label_trt0." lineattrs=(pattern=solid color=blue thickness=&line_thick.);
   series x = t y = surv1 /legendlabel="&label_trt1." lineattrs=(pattern=solid color=red thickness=&line_thick.);
   title 'Survival probability';
   yaxis label = 'Survival'  values = (0.0 to 1 by 0.1) ;
%end;
%if &type.=ci %then %do;
   series x = t y = ci0 /legendlabel="&label_trt0." lineattrs=(pattern=solid color=blue thickness=&line_thick.);
   series x = t y = ci1 /legendlabel="&label_trt1." lineattrs=(pattern=solid color=red thickness=&line_thick.);
   title 'Cumulative Incidence (%)';
   yaxis label = 'Cumulative Incidence'  values = (0.0 to 1 by 0.1) ;
%end;
   xaxis label = 'Three Months since treatment initiation'  values = (0 to 11 by 1) ;
   keylegend/noborder title='Treatment: ';
run;
ods graphics off;
data &out_data.; set pred_3; run;
proc delete data=temp temp_1 pred pred_1 - pred_3 model_4; run;
%MEND;

