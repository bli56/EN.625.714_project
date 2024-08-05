
DM LOG 'CLEAR';
DM LST 'CLEAR';


%let path1 = C:\Users\LiBaini\Documents\2024_Summer\EN.625.714.81.SU24 Introductory Stochastic Differential Equations with Applications\714 project\Data ;

proc import out=data
	datafile= "&path1.\data.xlsx"
	dbms=xlsx replace;
	getnames=YES;
quit;

/*** Freq of subjects ***/
%macro freq_chk(name=, var=);
	title1 "Check subjects";
	proc freq data=&name;
		table &var;
	quit;
%mend;

%freq_chk(name=data, var=ARMFLN);
%freq_chk(name=data, var=RILUZOLEFLN);
%freq_chk(name=data, var=DTHFLN);



/*** Freq of subjects by variable ***/
%macro freq_chk2(name=, var1=, var2=);
	title1 "Check subjects";
	proc freq data=&name;
		table &var1 * &var2;
	quit;
%mend;

%freq_chk2(name=data, var1=DTHFLN, var2=ARMFLN);



/*** Stats about continuous variables ***/
proc means data=data n min max mean STD median stackodsoutput;
	var AGE  ALBUMIN  ALKALINE  ALSFRS_DELTA  BASOPHILS  CALCIUM  CREATININE  DTHDAY  HEMOGLOBIN  ONSET_DELTA  POTASSIUM  RBC  RILUZOLE_USE_DELTA  SLOPE_ALSFRS  SODIUM  WBC;
	ods output summary = stat_conti;
quit;






/*** Stats about continuous DM, LB variables by arm ***/
proc means data=data n nmiss min max P5 P95 mean STD median stackodsoutput;
	class study_Arm;
	var Age  Onset_Delta  ALBUMIN  ALKALINE  BASOPHILS  CALCIUM  CREATININE  HEMOGLOBIN  POTASSIUM  RBC  SODIUM  WBC
		chg_ALSFRS  pchg_ALSFRS  slope_ALSFRS  chg_ALSFRS_wk48  pchg_ALSFRS_wk48  slope_ALSFRS_wk48;
	ods output summary = stat_conti_arm;
quit;


/*** Stats about Riluzole use ***/
data Riluzole_err;  /* Identify the problematic obs */
	keep subject_id ALSFRS_Delta study_Arm Subject_used_Riluzole	Riluzole_use_Delta	RiluzoleFL	RiluzoleFLN;
	set data;
	if (RiluzoleFL = 'N' and Riluzole_use_Delta ^=.) or (RiluzoleFL = 'Y' and Riluzole_use_Delta =.);
run;


proc freq data=data;
	table RiluzoleFL;
quit;

proc freq data=data;
	table study_Arm*RiluzoleFL;
quit;


proc means data=data n nmiss min max P5 P95 mean STD median stackodsoutput;
	var Riluzole_use_Delta;
	ods output summary = stat_Riluz;
quit;

proc means data=data n nmiss min max P5 P95 mean STD median stackodsoutput;
	class Study_Arm;
	var Riluzole_use_Delta;
	ods output summary = stat_Riluz_arm;
quit;


/*** Stats about deaths in time window (right censored if DTHFL = 'Y') ***/

proc freq data=data;
	table DTHFL;
quit;

proc freq data=data;
	table study_Arm*DTHFL;
quit;


proc means data=data n nmiss min max P5 P95 mean STD median stackodsoutput;
	var DTHDAY;
	ods output summary = stat_DTH;
quit;

proc means data=data n nmiss min max P5 P95 mean STD median stackodsoutput;
	class Study_Arm;
	var DTHDAY;
	ods output summary = stat_DTH_arm;
quit;
















































































































































































































































































































































































































































































































































































































