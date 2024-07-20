
DM LOG 'CLEAR';
DM LST 'CLEAR';

%let path1 = C:\Users\LiBaini\Documents\2024_Summer\EN.625.714.81.SU24 Introductory Stochastic Differential Equations with Applications\714 project\Data\PRO-ACT database ;
%let path2 = C:\Users\LiBaini\Documents\2024_Summer\EN.625.714.81.SU24 Introductory Stochastic Differential Equations with Applications\714 project\Data\ ;

/*** Data preparation ***/
/* Find subjects with definite El Escorial Diagnosis */
proc import out=proact_ELESC
	datafile= "&path1.\PROACT_ELESCORIAL.csv"
	dbms=csv
	replace;
	getnames=YES;
quit;

data proact_ELESC;
	keep subject_id;
	set proact_ELESC;
	if el_escorial = 'Definite';
proc sort;
	by subject_id;
quit;


/* Find subjects with non-missing age values */
proc import out=proact_DM
	datafile= "&path1.\PROACT_DEMOGRAPHICS.csv"
	dbms=csv
	replace;
	getnames=YES;
quit;

data proact_DM;
	keep subject_id Age;
	set proact_DM;
	if Age ^=.;
proc sort;
	by subject_id;
quit;


/* Retrieve alocation info */
proc import out=proact_treat
	datafile= "&path1.\PROACT_TREATMENT.csv"
	dbms=csv
	replace;
	getnames=YES;
quit;

data ls;
	keep subject_id Study_Arm;
	set proact_treat;
run;


/* Retrieve ALSFRS-R data  */
proc import out=proact_alsfrs
	datafile= "&path1.\PROACT_ALSFRS.csv"
	dbms=csv
	replace;
	getnames=YES;
quit;

data proact_alsfrs;
	keep subject_id ALSFRS_R_Total ALSFRS_Delta;
	set proact_alsfrs;
proc sort;
	by subject_id ALSFRS_Delta;
quit;


/* Retrieve ALS history info */
proc import out=PROACT_ALSH
	datafile= "&path1.\PROACT_ALSHISTORY.csv"
	dbms=csv
	replace;
	getnames=YES;
quit;

data PROACT_ALSH;
	keep subject_id Onset_Bulbar Site_of_Onset Onset_Delta;
	set PROACT_ALSH;
	if Site_of_Onset = '' then Onset_Bulbar =.;  /* This derived info is no used for now */
		else if Site_of_Onset ^= '' then do;
			if Site_of_Onset = 'Onset: Bulbar' then Onset_Bulbar = 1;
			else Onset_Bulbar = 0;
		end;
	if Onset_Delta ^=.;
proc sort;
	by subject_id Onset_Delta;
quit;

data Proact_alsh2;
	set Proact_alsh;
	by subject_id Onset_Delta;
	if first.subject_id;
run;

/* Combine */
data alsfrs0;
	merge proact_alsfrs(in=a) ls(in=b);
	by subject_id;
	if a and b;
run;


data alsfrs1;
	merge alsfrs0(in=a) proact_ELESC(in=b);
	by subject_id;
	if a and b;
run; 

data alsfrs2;
	merge alsfrs1(in=a) proact_DM(in=b);
	by subject_id;
	if a and b;
run; 

data alsfrs3;
	merge alsfrs2(in=a) PROACT_ALSH(in=b);
	by subject_id;
	if a and b;
run;


data alsfrs4;
	set alsfrs3;
	if ALSFRS_R_Total ^=. and Study_Arm ^= '' and ALSFRS_Delta ^=. and Onset_Delta ^=. and age ^=.;
	* if abs(Onset_Delta)>. and abs(Onset_Delta) < 540;  /* Subjects who are < 540 days from symptom onset  */
	if ALSFRS_Delta >=0 and ALSFRS_Delta =< 334;
	if ALSFRS_Delta = 0 then do; week = 0; month = 0; weekflag = 0; end;  /* baseline. NOTE: In protocol the baseline is day 1  */
		else if ALSFRS_Delta >= 4  and ALSFRS_Delta =< 10  then do; week = 2; month = 0.5; weekflag = 2; end;
		else if ALSFRS_Delta >= 16  and ALSFRS_Delta =< 26  then do; week = 4; month = 1; weekflag = 2; end;
		else if ALSFRS_Delta >= 44  and ALSFRS_Delta =< 54  then do; week = 8; month = 2; weekflag = 2; end;
		else if ALSFRS_Delta >= 72  and ALSFRS_Delta =< 82  then do; week = 12; month = 3; weekflag = 2; end;
		else if ALSFRS_Delta >= 100  and ALSFRS_Delta =< 110  then do; week = 16; month = 4; weekflag = 2; end;
		else if ALSFRS_Delta >= 128  and ALSFRS_Delta =< 138  then do; week = 20; month = 5; weekflag = 2; end;
		else if ALSFRS_Delta >= 156  and ALSFRS_Delta =< 166  then do; week = 24; month = 6; weekflag = 2; end;
		else if ALSFRS_Delta >= 184  and ALSFRS_Delta =< 194  then do; week = 28; month = 7; weekflag = 2; end;
		else if ALSFRS_Delta >= 212  and ALSFRS_Delta =< 222  then do; week = 32; month = 8; weekflag = 2; end;
		else if ALSFRS_Delta >= 240  and ALSFRS_Delta =< 250  then do; week = 36; month = 9; weekflag = 2; end;
		else if ALSFRS_Delta >= 268  and ALSFRS_Delta =< 278  then do; week = 40; month = 10; weekflag = 2; end;
		else if ALSFRS_Delta >= 296  and ALSFRS_Delta =< 306  then do; week = 44; month = 11; weekflag = 2; end;
		else if ALSFRS_Delta >= 324  and ALSFRS_Delta =< 334  then do; week = 48; month = 12; weekflag = 2; end;
	if week ^=.;
proc sort;
	by subject_id ALSFRS_Delta;
quit;

/* Delete the subjects that having only one record */
proc sql;
	create table eff_data1 as select * from alsfrs4
	group by subject_id
	having count(subject_id)>1;
quit;

/* Calculate del-FS */
data del_FS;
	keep subject_id del_FS;
	set eff_data1;
	del_FS = ALSFRS_R_Total/abs(Onset_Delta);
	if ALSFRS_Delta = 0;
run;

data eff_data2;
	merge eff_data1(in=a) del_FS(in=b);
	by subject_id;
	if a and b;
proc sort;
	by subject_id ALSFRS_Delta;
quit;


/***** Calculate chg_ALSFRS_R_T, pchg_ALSFRS_R_T, slope_ALSFRS_R_T *****/
data base_ALSFRS;
	keep subject_id ALSFRS_R_Total;
	set eff_data2;
	if ALSFRS_Delta = 0;
	rename ALSFRS_R_Total = bl_ALSFRS;
run;

data eff_data4;
	merge eff_data2(in=a) base_ALSFRS(in=b);
	by subject_id;
	if a and b;
proc sort;
	by subject_id ALSFRS_Delta;
quit;

data eff_data4;
	set eff_data4;
	chg_ALSFRS_R_T = ALSFRS_R_Total - bl_ALSFRS;
	pchg_ALSFRS_R_T = chg_ALSFRS_R_T/bl_ALSFRS;
	slope_ALSFRS_R_T = chg_ALSFRS_R_T/ALSFRS_Delta;
proc sort;
	by subject_id ALSFRS_Delta;
quit;


/* Only keep the last available observation for each subject */
data eff_data5;
	drop ALSFRS_R_Total Site_of_Onset Onset_Bulbar del_FS bl_ALSFRS;
    set eff_data4;
    by subject_id ALSFRS_Delta;
    if last.subject_id;
run;


/*** Retrieve lab data  ***/
proc import out=PROACT_LABS
	datafile= "&path1.\PROACT_LABS.csv"
	dbms=csv
	replace;
	getnames=YES;
quit;


data lb1;
	set PROACT_LABS;
	Test_Name = upcase(Test_Name);
	if Laboratory_Delta = 0 and Test_Result not in ('', '-') and Test_Unit ^= '' and
	   Test_Name in ('CALCIUM','HEMOGLOBIN', 'BASOPHILS', 'ALBUMIN', 'CREATININE',
					 'ALKALINE PHOSPHATASE', 'RED BLOOD CELLS (RBC)', 'WHITE BLOOD CELL (WBC)', 'SODIUM', 'POTASSIUM');
	no+1;
run;


/* Create LB variables */
data lb2;
	set lb1;
	format base best.; 
	if Test_Name = 'ALKALINE PHOSPHATASE' then Test_Name = 'ALKALINE';
		else if Test_Name = 'RED BLOOD CELLS (RBC)' then Test_Name = 'RBC';
		else if Test_Name = 'WHITE BLOOD CELL (WBC)' then Test_Name = 'WBC';
	base = Test_Result;
proc sort;
	by subject_id Test_Name no;
quit;


data lb3;
	set lb2;
    by subject_id Test_Name no;
    if last.Test_Name;
	drop Test_Result Test_Unit Laboratory_Delta no;
run;

proc transpose data=lb3  name=subject_id  out=lb3_t;
	by subject_id;
	id Test_Name;
	var base;
quit;

proc sort data=lb3_t;
	by subject_id;
quit;


data eff_data6;
	merge eff_data5(in=a) lb3_t(in=b);
	by subject_id;
	if a;
proc sort;
	by subject_id ALSFRS_Delta;
quit;


/*** Retrieve Riluzole data  ***/
proc import out=PROACT_Riluzole
	datafile= "&path1.\PROACT_RILUZOLE.csv"
	dbms=csv
	replace;
	getnames=YES;
quit;

proc sort data = PROACT_Riluzole;
	by subject_id;
quit;


data eff_data7;
	merge eff_data6(in=a) PROACT_Riluzole(in=b);
	by subject_id;
	if a;
proc sort;
	by subject_id ALSFRS_Delta;
quit;


/*** Extract info on death ***/

proc import out=proact_die
	datafile= "&path1.\PROACT_DEATHDATA.csv"
	dbms=csv
	replace;
	getnames=YES;
quit;

proc sort data=proact_die;
	by subject_id;
quit;

data eff_data8;
	merge eff_data7(in=a) Proact_die(in=b);
	by subject_id;
	if a;
run;


data eff_data9;
	set eff_data8;
	if Subject_Died = 'Yes' and Death_Days =< 334 then do;  /* apply the censoring time window: 0~(335-1) */
		DTHFL = 'Y';
		DTHFLN = 1;
		DTHDAY = Death_Days;
	end;
	else do;
		DTHFL = 'N';
		DTHFLN = 0;
		DTHDAY = 334;
	end;

	if UPCASE(Subject_used_Riluzole) in ('YES') then do;
		RiluzoleFL = 'Y';
		RiluzoleFLN = 1;
	end;
	else if UPCASE(Subject_used_Riluzole) in ('NO', '') then do;
		RiluzoleFL = 'N';
		RiluzoleFLN = 0;
	end;
proc sort;
	by subject_id ALSFRS_Delta;
quit;

/* Create ALSFRS-R values for week 48 */
data wk48;
	keep subject_id  chg_ALSFRS_R_T  pchg_ALSFRS_R_T  slope_ALSFRS_R_T;
	set eff_data9;
	if week = 48;
	rename  chg_ALSFRS_R_T = chg_ALSFRS_wk48 
			pchg_ALSFRS_R_T = pchg_ALSFRS_wk48
			slope_ALSFRS_R_T = slope_ALSFRS_wk48;
proc sort;
	by subject_id;
quit;


data eff_data10;
	merge eff_data9(in=a) wk48(in=b);
	by subject_id;
	if a;
	rename chg_ALSFRS_R_T = chg_ALSFRS
			pchg_ALSFRS_R_T = pchg_ALSFRS
			slope_ALSFRS_R_T = slope_ALSFRS;
proc sort;
	by subject_id ALSFRS_Delta;
quit;


/*** delete useless datasets ***/
%macro deletdat(library=, name=);
	proc delete data = &library..&name;
	quit;
%mend;

%deletdat(library=work, name=Alsfrs0);
%deletdat(library=work, name=Alsfrs1);
%deletdat(library=work, name=Alsfrs2);
%deletdat(library=work, name=Alsfrs3);
%deletdat(library=work, name=Alsfrs4);

%deletdat(library=work, name=Base_alsfrs);
%deletdat(library=work, name=Del_fs);

%deletdat(library=work, name=Eff_data1);
%deletdat(library=work, name=Eff_data2);
%deletdat(library=work, name=Eff_data3);
%deletdat(library=work, name=Eff_data4);
%deletdat(library=work, name=Eff_data5);
%deletdat(library=work, name=Eff_data6);
%deletdat(library=work, name=Eff_data7);
%deletdat(library=work, name=Eff_data8);
%deletdat(library=work, name=Eff_data9);

%deletdat(library=work, name=Lb1);
%deletdat(library=work, name=Lb2);
%deletdat(library=work, name=Lb3);
%deletdat(library=work, name=Lb3_t);
%deletdat(library=work, name=Ls);

%deletdat(library=work, name=Proact_alsfrs);
%deletdat(library=work, name=Proact_alsh);
%deletdat(library=work, name=Proact_alsh2);
%deletdat(library=work, name=Proact_die);
%deletdat(library=work, name=Proact_dm);
%deletdat(library=work, name=Proact_elesc);
%deletdat(library=work, name=Proact_labs);
%deletdat(library=work, name=Proact_riluzole);
%deletdat(library=work, name=Proact_treat);
%deletdat(library=work, name=wk48);


/************************/
/*** Data Exploration ***/
/************************/

/* Number of subjects by arm */
%macro freq_chk(name=);
	proc sort data=&name out=chk_subject;
		by subject_id ALSFRS_Delta;
	quit;
	title1 "Check subjects";
	proc freq data=chk_subject;
		table study_Arm;
	quit;
	proc delete data = chk_subject;
	quit;
%mend;

%freq_chk(name=eff_data10);

/*** Stats about continuous DM, LB variables ***/
proc means data=Eff_data10 n nmiss min max P5 P95 mean STD median stackodsoutput;
	var Age  Onset_Delta  ALBUMIN  ALKALINE  BASOPHILS  CALCIUM  CREATININE  HEMOGLOBIN  POTASSIUM  RBC  SODIUM  WBC
		chg_ALSFRS  pchg_ALSFRS  slope_ALSFRS  chg_ALSFRS_wk48  pchg_ALSFRS_wk48  slope_ALSFRS_wk48;
	ods output summary = stat_conti;
quit;


/*** Stats about continuous DM, LB variables by arm ***/
proc means data=Eff_data10 n nmiss min max P5 P95 mean STD median stackodsoutput;
	class study_Arm;
	var Age  Onset_Delta  ALBUMIN  ALKALINE  BASOPHILS  CALCIUM  CREATININE  HEMOGLOBIN  POTASSIUM  RBC  SODIUM  WBC
		chg_ALSFRS  pchg_ALSFRS  slope_ALSFRS  chg_ALSFRS_wk48  pchg_ALSFRS_wk48  slope_ALSFRS_wk48;
	ods output summary = stat_conti_arm;
quit;


/*** Stats about Riluzole use ***/
data Riluzole_err;  /* Identify the problematic obs */
	keep subject_id ALSFRS_Delta study_Arm Subject_used_Riluzole	Riluzole_use_Delta	RiluzoleFL	RiluzoleFLN;
	set Eff_data10;
	if (RiluzoleFL = 'N' and Riluzole_use_Delta ^=.) or (RiluzoleFL = 'Y' and Riluzole_use_Delta =.);
run;


proc freq data=Eff_data10;
	table RiluzoleFL;
quit;

proc freq data=Eff_data10;
	table study_Arm*RiluzoleFL;
quit;


proc means data=Eff_data10 n nmiss min max P5 P95 mean STD median stackodsoutput;
	var Riluzole_use_Delta;
	ods output summary = stat_Riluz;
quit;

proc means data=Eff_data10 n nmiss min max P5 P95 mean STD median stackodsoutput;
	class Study_Arm;
	var Riluzole_use_Delta;
	ods output summary = stat_Riluz_arm;
quit;


/*** Stats about deaths in time window (right censored if DTHFL = 'Y') ***/

proc freq data=Eff_data10;
	table DTHFL;
quit;

proc freq data=Eff_data10;
	table study_Arm*DTHFL;
quit;


proc means data=Eff_data10 n nmiss min max P5 P95 mean STD median stackodsoutput;
	var DTHDAY;
	ods output summary = stat_DTH;
quit;

proc means data=Eff_data10 n nmiss min max P5 P95 mean STD median stackodsoutput;
	class Study_Arm;
	var DTHDAY;
	ods output summary = stat_DTH_arm;
quit;


/*** Export clean dataset for diffusion mapps ***/
proc export data=Eff_data10
			outfile= "&path2.proact_data"
			dbms=xlsx
			replace;
			sheet="sheet1";
quit;















































































































































































































































































































































































































































































































































































































