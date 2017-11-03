libname project "Y:\Capstone Project";
run;

proc import datafile='Y:\New folder\telecomfinal.csv'
out=project.telecom
dbms=csv replace;
GETNAMES=yes;
run;

/* Missing Value treatment */
proc means data=project.telecom nmiss;
run;

data telecom1;
set project.telecom;
if missing (mou_Mean) then delete;
if age1='NA' then delete;
run;

proc means data=telecom1 nmiss;
run;

proc freq data=telecom1;
table ACTVSUBS AGE1 AGE2 AREA ;
run;

proc freq data=telecom1;
table ASL_FLAG CAR_BUY CARTYPE CHILDREN CHURN
CRCLSCOD ;
run;

proc freq data=telecom1;
table DIV_TYPE DWLLSIZE DWLLTYPE ;
run;

proc freq data=telecom1;
table ETHNIC FORGNTVL HND_PRICE HND_WEBCAP INCOME;
run;

proc freq data=telecom1;
table MAILORDR MAILRESP MARITAL MODELS MTRCYCLE NUMBCARS OCCU1;
run;

proc freq data=telecom1;
table  PRIZM_SOCIAL_ONE PROPTYPE
REFURB_NEW  SOLFLAG  TRUCK UNIQSUBS WRKWOMAN;
run;

/* Data Exploration */

data project.telecom2;
set telecom1;

/* Account spending limit dummy variable */

if asl_flag='Y' then asl_flag_dummy=1;
else asl_flag_dummy=0;

/* Children Dummy */

if children='N' or children='NA' then children_dummy=0;
else children_dummy=1;

/* Division Dummy */

if div_type= 'BTH' then div_bht_dummy=1;
else  div_bht_dummy=0; 
if div_type='LDD' then div_ldd_dummy=1;
else  div_ldd_dummy=0;
if div_type='LTD' then div_ltd_dummy=1;
else div_ltd_dummy=0;

/* Hand set web capability */

if hnd_webcap='WCMB' then WCMB_dummy=1;
else WCMB_dummy=0;

if hnd_webcap='WC' then WC_dummy=1;
else WC_dummy=0;

/* age1 */
if age1 le 20 then age1_dummy20=1;
else age1_dummy20=0;

if 21 le age1 le 35 then age1_dummy35=1;
else age1_dummy35=0;

if 36 le age1 le 50 then age1_dummy50=1;
else age1_dummy50=0;

if  51 le age1 le 90 then age1_dummy70=1;
else age1_dummy70=0;



/* age2 */
if age2 le 20 then age2_dummy20=1;
else age2_dummy20=0;

if 21 le age2 le 35 then age2_dummy35=1;
else age2_dummy35=0;

if 36 le age2 le 50 then age2_dummy50=1;
else age2_dummy50=0;

if 51 le age2 le 90 then age2_dummy70=1;
else age2_dummy70=0;

/*data telecom3;
set telecom1;
if income= NA then income_new=0;
else income_new=income;
run; */

/* Retention Calls */
if retdays= 'NA' then ret_call=0;
else ret_call=1; 

/* prizm Dummy */

if prizm_social_one='C' then city_dummy=1;
else city_dummy=0;
if prizm_social_one='R' then rural_dummy=1;
else rural_dummy=0;
if prizm_social_one='S' then suburban_dummy=1;
else suburban_dummy=0;
if prizm_social_one='T' then town_dummy=1;
else town_dummy=0;
if prizm_social_one='U' then urban_dummy=1;
else urban_dummy=0;

/* Refub or new */

if refurb_new='N' then hnd_new=1;
else hnd_new=0;

/* Optimal or non optimal plan */

if perovrrev < 1 then nonoptimal=0;
else nonoptimal=1;

run; 

/* Separation into training and validation set */

proc surveyselect data =project.telecom2
method=SRS out=telecom3
samprate=0.5 outall;
run;

data project.train project.validate;
set telecom3;
if selected=0 then output project.train;
else if selected=1 then output project.validate;
run;

/* Logistic Regression Model */

proc logistic data=project.train decending;
Model  CHURN= mou_mean--totcalls eqpdays--avgqty asl_flag_dummy--wc_dummy age1_dummy35--age1_dummy70 age2_dummy35--town_dummy hnd_new avgrev adjrev totrev adjmou drop_vce_mean
datovr_mean da_mean actvsubs--FORGNTVL/ctable lackfit;
run;

/* Droping insignificent variables */

proc logistic data=project.train decending;
Model  CHURN= totmrc_mean mou_range change_mou months eqpdays iwylis_vce_mean rev_mean comp_vce_mean
avg3mou avgmou asl_flag_dummy children_dummy wcmb_dummy age1_dummy50 age1_dummy70 hnd_new rural_dummy
adjmou drop_vce_mean datovr_mean avgrev
actvsubs uniqsubs/ctable lackfit;
run;

proc logistic data=project.train decending;
Model  CHURN= totmrc_mean mou_mean change_mou months eqpdays rev_mean comp_vce_mean
avgrev avgmou asl_flag_dummy children_dummy wcmb_dummy age1_dummy50 age1_dummy70 hnd_new rural_dummy
drop_blk_mean
actvsubs uniqsubs/ctable lackfit;
run;

/* Final Regrassion Model */

/* Checking for multicolinierity */

proc reg data=project.train;
Model  CHURN=   totmrc_mean months eqpdays rev_mean 
 avgmou asl_flag_dummy  wcmb_dummy  hnd_new ret_call comp_vce_mean
drop_blk_mean
actvsubs uniqsubs 
owylis_vce_Range age1_dummy20 age1_dummy35 nonoptimal town_dummy rural_dummy/vif;
run;

/* Regression Model Final */

proc logistic data=project.train decending outest=traintest outmodel=train1;
Model  CHURN=   totmrc_mean  months eqpdays rev_mean  comp_vce_mean
 avgmou asl_flag_dummy  wcmb_dummy  ret_call  hnd_new 
drop_blk_mean
actvsubs uniqsubs 
 age1_dummy20 age1_dummy35  nonoptimal rural_dummy town_dummy children_dummy avg3mou avgmou /selection= forward ctable lackfit;
 score out=train2;
run;



data test1;
set train2;
if f_churn=0 and i_churn=0 then out= "True Negative";
if f_churn=1 and i_churn=1 then out= "True Positive";
if f_churn=0 and i_churn=1 then out= " False Positive";
if f_churn=1 and i_churn=0 then out= "False Negative";

proc freq;
tables out;
run;

/* Validation of the model */

proc logistic data=project.validate decending outmodel=validate1;
Model  CHURN=   totmrc_mean  months eqpdays rev_mean  comp_vce_mean
 avgmou asl_flag_dummy  wcmb_dummy  ret_call  hnd_new 
drop_blk_mean
actvsubs uniqsubs 
 age1_dummy20 age1_dummy35  nonoptimal rural_dummy town_dummy children_dummy avg3mou avgmou /selection= forward ctable lackfit;
 score out=train2;
run;

proc logistic inmodel=train1;
score data= project.validate out=validationtest;
run;

data test;
set validationtest;
if f_churn=0 and i_churn=0 then out= "True Negative";
if f_churn=1 and i_churn=1 then out= "True Positive";
if f_churn=0 and i_churn=1 then out= " False Positive";
if f_churn=1 and i_churn=0 then out= "False Negative";

proc freq;
tables out;
run;

proc rank data=train2 out=decile groups=10
ties=mean; /* if probablities are same then takes average*/ 
var p_1;
ranks decile;
run;

proc sort data=decile;
by decending p_1;
run;

proc export data=decile outfile='y:\project1.csv'
dbms=csv replace;
run;

proc freq data=telecom2;
table perovrrev*churn;
run;
/* Extra 
proc logistic data=train;
Model  CHURN= totmrc_mean mou_mean mou_range  months eqpdays 
rev_mean comp_vce_mean asl_flag_dummy children_dummy
div_dummy hnd_new drop_vce_mean actvsubs uniqsubs age1  avgmou hnd_price avgrev
mtrcycle  ovrmou_mean age2 adjmou wcmb_dummy callwait_range drop_blk_mean/ctable lackfit;
run;


proc logistic data=train;
Model  CHURN= totmrc_mean mou_mean mou_range  months eqpdays 
rev_mean  asl_flag_dummy 
hnd_new  actvsubs uniqsubs age1  avgmou hnd_price avgrev
children_dummy comp_vce_mean div_dummy drop_vce_mean/ctable lackfit;
run;


proc logistic data=train;
Model  CHURN=  totmrc_mean mou_mean mou_range  months eqpdays 
rev_mean  asl_flag_dummy 
hnd_new  actvsubs uniqsubs age1  avgmou hnd_price 
/ctable lackfit;
run;

proc logistic data=train;
Model  CHURN= mou_mean drop_blk_Mean months eqpdays custcare_mean callwait_mean ovrrev_mean
adjqty ovrrev_mean rev_mean ovrmou_mean avgmou avgqty age1 age2 actvsubs uniqsubs roam_Mean recv_sms_Mean 
blck_dat_Mean da_Mean datovr_Mean drop_dat_Mean adjmou totrev adjrev
avgrev children_dummy asl_flag_dummy hnd_new
 /ctable lackfit;
run;



proc logistic data=train;
Model  CHURN= mou_mean drop_blk_Mean months eqpdays  ovrrev_mean
 ovrrev_mean  avgmou age1 actvsubs uniqsubs roam_Mean  asl_flag_dummy hnd_new
 /ctable lackfit;
run;

proc logistic data=train;
Model  CHURN=  drop_blk_Mean custcare_mean callwait_mean ovrmou_mean blck_dat_Mean drop_vce_mean 
da_Mean datovr_Mean drop_dat_Mean /ctable lackfit;
run;


proc logistic data=train;
Model  CHURN=   custcare_mean callwait_mean ovrmou_mean datovr_Mean /ctable lackfit;
run;

proc logistic data=train;
Model  CHURN=  totmrc_mean mou_mean mou_range  months eqpdays 
rev_mean  asl_flag_dummy 
hnd_new  actvsubs uniqsubs age1  avgmou hnd_price 
custcare_mean callwait_mean ovrmou_mean datovr_Mean
/ctable lackfit;
run;

proc logistic data=train;
Model  CHURN=   custcare_mean callwait_mean  ovrmou_mean  /ctable lackfit;
run;

proc logistic data=train;
Model  CHURN= custcare_mean callwait_mean  ovrmou_mean  datovr_Mean asl_flag_dummy ovrmou_mean 
hnd_new  totmrc_mean mou_mean   /ctable lackfit;
run;

proc logistic data=train;
Model  CHURN= avgmou  avgrev  /ctable lackfit;
run;

*/

proc logistic data=train;
Model  CHURN=   totmrc_mean  mou_mean    
rev_mean  asl_flag_dummy 
hnd_new  uniqsubs    
actvsubs/ctable lackfit;
run;

proc logistic data=train decending;
Model  CHURN=  totmrc_mean mou_mean mou_range  
rev_mean  asl_flag_dummy 
hnd_new  actvsubs uniqsubs age1  avgmou 
ovrmou_mean drop_blk_Mean
/ctable lackfit;
run;

data telecom10;
set project.telecom2;
if avgrev>225 then delete;
run;

proc standard data=telecom10 mean=0 std=1 out=clust;
var avgrev  drop_blk_mean ovrrev_mean mou_mean churn callwait_Mean comp_vce_Mean drop_vce_Mean;
run;

data clust; 
set clust;
avgrev3=avgrev*3;
run;

proc fastclus data=clust maxclusters=4 converge=0 maxiter=100 out=clust1 outstat=clust2;
var avgrev3 drop_blk_mean  comp_vce_Mean;
run;

/* Profiling */
data clust1;
set clust1;
keep Customer_ID cluster;
run;
Proc sort data=clust1;
by Customer_ID;
proc sort data=telecom10;
by Customer_ID;
run;

data clust3;
merge clust1(in=a) telecom10(in=b);
by Customer_ID;
if (a AND b);
run;

proc sort data=clust3;
by cluster;
proc means data=clust3 mean;
var avgrev  drop_blk_mean  comp_vce_Mean;
by cluster;
run;

proc means data=clust3 mean std;
var avgrev drop_blk_mean comp_vce_Mean;
run;

proc freq data=clust3;
table cluster*churn;
run;