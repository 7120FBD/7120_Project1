********************************************************************************;
* Title:		Project 01 - Contributors to Fibrocystic Breast Disease
* Program Name:	project01.sas
* Course:		FP MD 7120
* Author:		Sean Hennessey
* Date Started: 11Sep2016
* Date Due:		20Sep2016
*
* Partner:		Meghan Broadbent (she is working in R)
* Description:	A complete program for my portion of the Project01 Analysis.
*				We are looking at Fibrocystic Breast Disease (FBD) and 
*				investigating possible causes or contributions to the onset
*				of FBD. We suspect that (our hypothese is that) age at menarche
*				and number of children birthed are going to be the most
*				significant variables contributing to FBD, however we do not
*				really know this and, heck, thats what the purpose of an
*				investigational study is, we are excited to learn through the
*				the application of logistic analysis methods learned in class
*				what the true causes or major conributors to FBD are.
********************************************************************************;

*Set up the libname to be used on my local machine;
libname proj1 "C:\Users\SeanP\OneDrive\School\2016\Fall 2016\Logistic\project1";

**write a couple of macros to assist in generating our interaction significance table;
*this strips out the interaction term only, from ODS output generated from a logistic regression that
*includes an interaction term (the presence of  '*');
%macro intstrip(num);
data int&num.;
attrib variable length = $20;
set int&num.;
if index(variable, '*')>0;
run;
%mend intstrip;

*this combines all of the individual interaction terms into one single dataset;
%macro intcombine(howmany);
%do i=2 %to &howmany.;
	proc append base=int1 data=int&i. force;
	run;
%end;
%mend intcombine;

*Write my formats, this is used for aesthetics only;
PROC FORMAT;
	VALUE obsf
	1 = "CASE"
	2-4 = "CONTROL"
	;

	VALUE diagf
	0 = "NONE"
	1 = "FBD"
	;

	VALUE degreef
	0 = "No Degree"
	1 = "High School"
	2 = "Jr. College"
	3 = "College"
	4 = "Masters"
	5 = "Doctoral"
	;

	VALUE y1n2f
	1 = "YES"
	2 = "NO"
	;

	VALUE marriedf
	1 = "Married"
	2 = "Divorced"
	3 = "Separated"
	4 = "Widwoed"
	5 = "Never Married"
	;

run;



*import the raw data, this comes from the UMASS library of available datasets;
PROC IMPORT OUT=rawdata
	DATAFILE="C:\Users\SeanP\OneDrive\School\2016\Fall 2016\Logistic\project1\benign"
	DBMS=xls REPLACE;
	SHEET="BENIGN";
	GETNAMES=yes;
run;

*Start with a little maintenance, my EXCEL import contains three observations with all null values;
data rawdata;
set rawdata;
if agmt=. and fndx=. then delete;
run;

*Double-check my data against my partner (referred to as Meghan), this is a quick verification that
*we are using identical data;
proc means data=rawdata;
var agmt higd agmn;
run;
*On the above variables, we have identical means. This is an indication that we have each properly
*brought our data in correctly, and we are using the same data. We each have 200 observations;

*Clean the data, rename and label variables;
/*DATA mydata;    USE THIS TO MATCH MEGHAN ON VARIABLE NAMES
SET rawdata (rename=(str=Stratum obs=Observation agp1=AgePregnant agmt=Age agmn=AgeMenarche
					nlv=Miscarriages liv=Births wt=Weight aglp=AgeMenopause mst=MaritalStatus chk=MedicalCheckups
					deg=Degree higd=Grade fndx=Diagnosed));
	ATTRIB Stratum label="Age-Matched Set";
	ATTRIB observation label="Case or Control (Within Age-Matched Set)" format=obsf.;
	ATTRIB AgePregnant label="Age at Pregnancy";
	ATTRIB AgeMenarche label="Age at Menarche";
	ATTRIB Age label="Age at Interview";
	ATTRIB Miscarriages label="Number of Stillbirths or Miscarriages";
	ATTRIB Births label="Number of Live Births";
	ATTRIB Weight label="Weight (in lbs.)";
	ATTRIB AgeMenopause label="Age at Menopause";
	ATTRIB MaritalStatus label="Marital Status" format=marriedf.;
	ATTRIB MedicalCheckups label="Regular Medical Checkups" format=y1n2f.;
	ATTRIB Degree label="Highest Degree Earned" format=degreef.;
	ATTRIB Grade label="Highest Grade in School Completed";
	ATTRIB Diagnosed label="Diagnosis" format=diagf.;
run;
*/
DATA mydata;
SET rawdata; 
	ATTRIB str label="Age-Matched Set";
	ATTRIB obs label="Case or Control (Within Age-Matched Set)" format=obsf.;
	ATTRIB agp1 label="Age at Pregnancy";
	ATTRIB agmn label="Age at Menarche";
	ATTRIB agmt label="Age at Interview";
	ATTRIB nlv label="Number of Stillbirths or Miscarriages";
	ATTRIB liv label="Number of Live Births";
	ATTRIB wt label="Weight (in lbs.)";
	ATTRIB aglp label="Age at Last Menstrual Period";
	ATTRIB mst label="Marital Status" format=marriedf.;
	ATTRIB chk label="Regular Medical Checkups" format=y1n2f.;
	ATTRIB deg label="Highest Degree Earned" format=degreef.;
	ATTRIB higd label="Highest Grade in School Completed";
	ATTRIB fndx label="Diagnosis" format=diagf.;
run;

**Create dummy variables for marital status;
data mydata;
set mydata;

*mst1=0;
*mst2=0;
*mst3=0;
*mst4=0;
*mst5=0;

evermarried=1;
if mst=5 then evermarried=0;

deg0=0;
deg1=0;
deg2=0;
deg3=0;
deg4=0;
*deg5=0;

*if mst=1 then mst1=1;*marital status married;
*if mst=2 then mst2=1;*marital status divorced;
*if mst=3 then mst3=1;*marital status separated;
*if mst=4 then mst4=1;*marital status windows;
*if mst=5 then mst5=1;*marital status never married;



if deg=0 then deg0=1;*no high school degree;
if deg=1 then deg1=1;*high school degree;
if deg=2 then deg2=1;*jr. college degree;
if deg=3 then deg3=1;*college degree;
if (deg=4 OR deg=5) then deg4=1;*graduate degree;
*if deg=5 then deg5=1;*doctoral degree;

*regular medical checkups is origianlly coded for 1 yes 2 no, so Im going to change this to 0 no 1 yes;
if chk=2 then chk=0;

attrib evermarried label="Ever Married?";
attrib deg0 label="No Degree";
attrib deg1 label="High School Degree";
attrib deg2 label="Jr College Degree";
attrib deg3 label="College Degree";
attrib deg4 label="Graduate Degree";

run;

**Used for model diagnostics later, to show that we have a higher AIC before fitting our reduced model;
proc logistic data=mydata descending;
model fndx=chk agp1 agmn nlv liv wt aglp evermarried deg1 deg2 deg3 deg4 higd;
strata str;
run;

******************;
* Hosmers 7 steps ;
******************;

*********;
* Step 1 ;
*********;

**Run a univariate model for each covariate, individually;

PROC LOGISTIC data=mydata descending;
strata str;
model fndx=agmt;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=higd;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=deg0;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=deg1;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=deg2;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=deg3;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=deg4;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=chk;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=agp1;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=agmn;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=nlv;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=LIV;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=wt;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=aglp;
run;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=evermarried;
run;


*********;
* Step 2 ;
*********;

ods graphics on;
**Run a multivariate model with all the variables from step1;
PROC LOGISTIC data=mydata descending;
strata str;
model fndx=higd deg4 chk agp1 agmn wt aglp evermarried;
ods output ParameterEstimates=parmfull;
run;
ods graphics on;
**Now, eliminate covariates where p>0.05;
PROC LOGISTIC data=mydata descending plots=all;
strata str;
model fndx=chk agmn wt evermarried;
ods output ParameterEstimates=parmreduced;
run;


*********;
* Step 3 ;
*********;

**check change in Beta coef, (for confounding);
data parmfull;
set parmfull;
if variable = "HIGD" then delete;
if variable = "deg4" then delete;
if variable = "AGP1" then delete;
if variable = "AGLP" then delete;
run;

proc transpose data=parmfull out=fullest suffix=_full;
id variable;
var estimate StdErr WaldChiSq ProbChiSq;
run;

proc transpose data=parmreduced out=redest suffix=_red;
id variable;
var estimate StdErr WaldChiSq ProbChiSq;
run;

proc sort data=fullest;
by _NAME_;
run;

proc sort data=redest;
by _NAME_;
run;

data estmerge;
merge fullest redest;
by _NAME_;
run;
proc print data=estmerge;
run;
/*
data deltabeta;
set estmerge;
deltaCHK = chk_full / chk_red;
deltaAGMN = agmn_full / agmn_red;
deltaWT = wt_full / wt_red;
deltaEVERMARRIED = evermarried_full / evermarried_red;
if _NAME_="ProbChiSq" then delete;
if _NAME_="StdErr" then delete;
if _NAME_="WaldChiSq" then delete;
run;
*/
data deltabeta;
set estmerge;
deltaCHK = chk_red / chk_full;
deltaAGMN = agmn_red / agmn_full;
deltaWT = wt_red / wt_full;
deltaEVERMARRIED = evermarried_red / evermarried_full;
if _NAME_="ProbChiSq" then delete;
if _NAME_="StdErr" then delete;
if _NAME_="WaldChiSq" then delete;
run;

proc print data=deltabeta;
run;

*********;
* Step 4 ;
*********;

**Try adding in variables that were initially excluded from step 1;
*aglp liv nlv agmt;

PROC LOGISTIC data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried aglp;
ods output ParameterEstimates=parmreduced;
run;

PROC LOGISTIC data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried liv;
ods output ParameterEstimates=parmreduced;
run;

PROC LOGISTIC data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried nlv;
ods output ParameterEstimates=parmreduced;
run;

**looking to run dfbetas analysis for str 12;
PROC LOGISTIC data=mydata descending;
strata str;
by str;
model fndx=chk agmn wt evermarried/influence iplots;
run;





**Not include this below for agmt, which is basically the persons age, which is going to be 
**highly correlated with the stratum;
*PROC LOGISTIC data=mydata descending;
*strata str;
*model fndx=chk agmn wt evermarried agmt;
*ods output ParameterEstimates=parmreduced;
*run;

*Proc loess data=mydata;
*model fndx=chk agmn wt evermarried;
*run;


*proc loess data=mydata;
*model fndx=wt;
*by str;
*run;


*********;
* Step 5 ;
*********;

**Check the fit of our model and our data;

**obtain the quartile values, create bins for our continuous variables;
proc means data=mydata min p25 p50 p75 max;
var wt;
run;
*121.5 137.5 157;

data mydata;
set mydata;
w1=0;
w2=0;
w3=0;
a1=0;
a2=0;
a3=0;

if (wt>=121.5 & wt<137.5) then w1=1;
if (wt>=137.5 & wt<157) then w2=1;
if wt>=157 then w3=1;

if (agmn>=12 & agmn <13) then a1=1;
if (agmn>=13 & agmn <14) then a2=1;
if (agmn>=14) then a3=1;
attrib w1 label="Second Quartile of weight";
attrib w2 label="Third Quartile of weight";
attrib w3 label="Fourth Quartile of weight";
run;

*verify that we have done this separation properly, we should have 50, 50, and 50, in all groups;
proc freq data=mydata;
table w1 w2 w3;
run;
*indeed, we have "binned" the weight variable properly.;

**Now, re-run our model but replace our continuous variables with our now-categorical variables;
PROC LOGISTIC data=mydata descending plots=all;
strata str;
model fndx=chk agmn w1 w2 w3 evermarried;
run;

PROC LOGISTIC data=mydata descending plots=all;
strata str;
model fndx=chk wt agmn evermarried;
run;


**try weight squared;
data mydata;
set mydata;
wtsqr = wt * wt;
run;
proc means data=mydata min p25 p50 p75 max;
var wtsqr;
run;
data mydata;
set mydata;
ws1=0;
ws2=0;
ws3=0;
if (wtsqr >=14764.5 & wtsqr<18906.5) then ws1=1;
if (wtsqr >=18906.5 & wtsqr<24650) then ws2=1;
if (wtsqr >=24650) then ws3=1;
attrib ws1 label="Second Quartile Weight Squared";
attrib ws2 label="Third Quartile Weight Squared";
attrib ws3 label="Fourth Quartile Weight Squared";
run;

proc freq data=mydata;
table ws1 ws2 ws3;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn ws1 ws2 ws3 evermarried;
run;


*number 5, additionally to the plot from R;
PROC LOGISTIC data=mydata descending;
model fndx=chk agmn wt evermarried;
output out=data2 p=prob xbeta=logitvalue;
run;

proc sort data=data2;
by logitvalue;
run;

proc print data=data2;
run;

proc gplot data=data2;
plot logitvalue*wt=FNDX;
run;


*The below, we dont actually use, because there are so few ages (12-16) that it acts more like
*a categorical variable than it does a continuous variable. Output didn't make sense, so we dont use;
*proc means data=mydata min p25 p50 p75 max;
*var agmn;
*run;

data mydata9;
set mydata;
if str=9 then delete;
run;
data mydata10;
set mydata;
if str=10 then delete;
run;
data mydata11;
set mydata;
if str=11 then delete;
run;
data mydata12;
set mydata;
if str=12 then delete;
run;
proc print data=mydata1;
run;
**DIAGNOSTIC PLOTS;************************************;

PROC LOGISTIC data=mydata9 plots=all descending;
*strata str;
model fndx=chk agmn wt evermarried;
run;
PROC LOGISTIC data=mydata10 plots=all descending;
*strata str;
model fndx=chk agmn wt evermarried;
run;
PROC LOGISTIC data=mydata11 plots=all descending;
*strata str;
model fndx=chk agmn wt evermarried;
run;
PROC LOGISTIC data=mydata12 plots=all descending;
*strata str;
model fndx=chk agmn wt evermarried;
run;


*********;
* Step 6 ;
*********;

**Run interactions between our main effects variables, as well as additional variables through theory;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried chk*agmn;
ods output parameterestimates=int1;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried chk*wt;
ods output parameterestimates=int2;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried chk*evermarried;
ods output parameterestimates=int3;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried agmn*wt;
ods output parameterestimates=int4;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried agmn*evermarried;
ods output parameterestimates=int5;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried wt*evermarried;
ods output parameterestimates=int6;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried aglp*agmn;
ods output parameterestimates=int7;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried aglp*wt;
ods output parameterestimates=int8;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried aglp*evermarried;
ods output parameterestimates=int9;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried agmt*chk;
ods output parameterestimates=int10;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried agmt*agmn;
ods output parameterestimates=int11;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried agmt*wt;
ods output parameterestimates=int12;
run;

proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried agmt*evermarried;
ods output parameterestimates=int13;
run;

%intstrip(1);
%intstrip(2);
%intstrip(3);
%intstrip(4);
%intstrip(5);
%intstrip(6);
%intstrip(7);
%intstrip(8);
%intstrip(9);
%intstrip(10);
%intstrip(11);
%intstrip(12);
%intstrip(13);

%intcombine(13);

*clean up the final dataset of interaction term significance;
data interactions;
set int1 (rename=(ProbChiSq=Pvalue));
drop _ESTTYPE_ df;
run;

proc print data=interactions;
run;

**Now, for the last part in Hosmer's Step 6, we are going to add in our interaction term,;
**and make adjustments if necessary;
proc logistic data=mydata descending;
strata str;
model fndx=chk agmn wt evermarried;
run;

data myfinaldata;
set mydata;
if str=12 then delete;
run;

proc logistic data=myfinaldata descending;
strata str;
model fndx=chk wt agmn evermarried aglp*agmn;
run;


data mystrata2;
set mydata;
if obs=1 then output;
if obs=2 then output;
run;

data mystrata3;
set mydata;
if obs=1 then output;
if obs=3 then output;
run;

data mystrata4;
set mydata;
if obs=1 then output;
if obs=4 then output;
run;
proc logistic data=mystrata2 descending plots=all;
model fndx=chk wt agmn evermarried;
run;
proc logistic data=mystrata2 descending;
strata str;
model fndx=chk wt agmn evermarried;
run;
proc logistic data=mystrata3 descending;
strata str;
model fndx=chk wt agmn evermarried;
run;
proc logistic data=mystrata4 descending;
strata str;
model fndx=chk wt agmn evermarried;
run;
data mystrata24;
set mydata;
if obs=1 then output;
if obs=2 then output;
if obs=4 then output;
run;
proc logistic data=mystrata24 descending plots=all;
*strata str;
model fndx=chk wt agmn evermarried;
run;

data mystrata24red;
set mystrata24;
if str=12 then delete;
run;
proc logistic data=mystrata24red descending plots=all;
strata str;
model fndx=chk wt agmn evermarried agmn*aglp /influence iplots;
run;

ods graphics on;
title"With interaction term, before stratum 12";
proc logistic data=mydata descending plots=all;
strata str;
model fndx=chk wt agmn evermarried agmn*aglp;
run;

title"With interaction term, after stratum 12";
proc logistic data=myfinaldata descending plots=all;
strata str;
model fndx=chk wt agmn evermarried agmn*aglp;
run;

title"With interaction term, before stratum 12";
proc logistic data=mydata descending plots=all;
model fndx=chk wt agmn evermarried agmn*aglp;
run;

title"With interaction term, after stratum 12";
proc logistic data=myfinaldata descending plots=all;
model fndx=chk wt agmn evermarried agmn*aglp;
run;

**roc 1:2;
proc logistic data=mystrata24 descending plots=all;
model fndx=chk wt agmn evermarried agmn*aglp;
run;

proc logistic data=mystrata24red descending plots=all;
model fndx=chk wt agmn evermarried agmn*aglp;
run;

proc logistic data=mydata descending plots=all;
strata str;
model fndx=chk wt agmn evermarried;
run;

proc logistic data=mydata descending plots=all;
model fndx=chk wt agmn evermarried;
run;
proc logistic data=mystrata24 descending plots=all;
model fndx=chk wt agmn evermarried;
run;

proc logistic data=mydata descending plots=all;
strata str;
model fndx=chk wt agmn evermarried;
run;


proc logistic data=mystrata24red descending plots=all;
strata str;
model fndx=chk wt agmn evermarried agmn*aglp;
run;

proc logistic data=mydata descending plots=all;
strata str;
model fndx=chk wt agmn evermarried;
run;
