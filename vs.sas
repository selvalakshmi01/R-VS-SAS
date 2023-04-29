/*SDS- SDTM DATASET*/


proc import datafile="raw_vs.xlsx" out=raw_vs dbms=xlsx replace;run;
proc import datafile="sds_dm.xlsx" out=sds_dm dbms=xlsx replace;run;
proc import datafile="sds_suppdm.xlsx" out=sds_suppdm dbms=xlsx replace;run;
proc import datafile="sds_ds.xlsx" out=sds_ds dbms=xlsx replace;run;
proc import datafile="sds_sv.xlsx" out=sds_sv dbms=xlsx replace;run;


proc sort data=raw_vs;
    by usubjid visitnum vsdtc vstestcd vsspid;
    where not missing(vstestcd);
run;

proc sort data=sds_dm out=dm(keep=usubjid rfstdtc);
    by usubjid;
run;



* Deriving the Study day variables ;

data  vs_der;
    merge raw_vs(in=a) dm(in=b);
    by usubjid;
    if a;

    if length(rfstdtc) ge 10 then rfstdt=input(compress(substr(rfstdtc,1,10)),yymmdd10.);
    if length(vsdtc) ge 10 then vsdt=input(compress(substr(vsdtc,1,10)),yymmdd10.);

	if rfstdt eq . then vsdy=.;
	if vstestcd = "WSTAVG" and not missing(vsorres) then do; 
		vsorresu = "cm";
		vsstresu = "cm";
	end;

run;

/*Epoch*/
%macro epoch(indset = , stdtc = , outdset = );

*------------Getting Re-screening date ------------;
data intsc;
	set sds_suppdm;
	where qnam = 'PREVSCRN'; 
run;

proc sort data= sds_ds out= intsc1;
	by usubjid dsdtc;
	where dscat = 'PROTOCOL MILESTONE' and dsdecod = 'INFORMED CONSENT OBTAINED'; 
run;


data scrn;
	merge intsc(in=a) intsc1;
	by usubjid;
	if a;
/*	drop visit;*/
run;

data scrn1;
	set scrn;
	by usubjid dsdtc;
	if last.usubjid then rscrn_dt = input(compress(substr(dsstdtc,1,10)),yymmdd10.);
	if missing(rscrn_dt) then delete;
	keep usubjid  rscrn_dt;
	format rscrn_dt yymmdd10.;
run;

*------------Getting Early discontinued subjects  from SV------------;
	data sv;
		set sds_sv;
		if length(svstdtc) >= 10 then svstdt=input(compress(substr(svstdtc,1,10)),yymmdd10.);
	    if length(svendtc) >= 10 then svendt=input(compress(substr(svendtc,1,10)),yymmdd10.);
		keep usubjid svstdtc  svendtc svstdt svendt visitnum;
	run;

	proc sort data = sv out = sv_erly_disc_ (keep = usubjid svstdt svendt);* rename  = (svstdt = sv_erly_disc));
			by usubjid descending svstdt;
			where not missing(svstdt) and visitnum in (11,12);
	run;

	data sv_erly_disc;
		set sv_erly_disc_;
		sv_erly_disc = max(svstdt,svendt);
	run;

	proc sort data= sv_erly_disc; by usubjid SVSTDT SVENDT; run;
 
	data sv_erly_disc;
		set sv_erly_disc;
		by usubjid SVSTDT SVENDT;
		if last.usubjid;
		format sv_erly_disc yymmdd10.;
	run;

*------------Getting Informed consent date, Study drug start and end date from DM------------;

	proc sort data = sds_dm out = dm(keep = usubjid RFICDTC rfstdtc rfendtc);
		by usubjid;
	run;

*------------Getting Latest DSSTDTC from DS------------;

proc sort data=sds_ds out=disp;
	by usubjid descending dsstdtc; 
	where epoch in('STUDY-CORE') and dsstdtc ne '';
run;

data final_ds;
set disp(rename =(epoch=epoch_));
by usubjid;
if first.usubjid;
final_dt=input(compress(substr(dsstdtc,1,10)),yymmdd10.);
keep usubjid dscat dsscat dsterm dsdecod epoch_ dsstdtc final_dt;
run;

data temp;
	length usubjid $30.;
		merge dm(in = a) scrn1(in=b) sv_erly_disc(in = c) final_ds(in = d);
		by usubjid;
run;


proc sort data=sds_ds out=disp1(rename=(dsstdtc=dsstdtc1) );
	by usubjid dsdecod  dsseq; 
	where DSDECOD in('INFORMED CONSENT OBTAINED') and dsseq eq 2;
run;

data disp1;
	set disp1;
	keep dsstdtc1 usubjid;
run;

*---------------------Epoch Derivation-----------------------------;
	
	data &outdset;
		length epoch $200.;
		merge temp(in = a) &indset(in = b) disp1(in=c);
		by usubjid;
		if b;

	    if length(RFICDTC) >= 10 then RFICDT=input(compress(substr(RFICDTC,1,10)),yymmdd10.);
		if length(rfstdtc) >= 10 then rfstdt=input(compress(substr(rfstdtc,1,10)),yymmdd10.);
		if length(rfendtc) >= 10 then rfendt=input(compress(substr(rfendtc,1,10)),yymmdd10.);
		if length(&stdtc)>=10 then ref_dt =  input(compress(substr(&stdtc,1,10)),yymmdd10.);
		if length(DSSTDTC1) >= 10 then RFICDT1=input(compress(substr(DSSTDTC1,1,10)),yymmdd10.);


 


		if not missing(ref_dt) then do;
			if .< rficdt <= ref_dt < rscrn_dt then epoch = "INITIAL SCREENING"; 
			else if (. < rficdt <= ref_dt < rfstdt ) then epoch='INITIAL SCREENING';
			else if missing(rfstdt) and . < rficdt <= ref_dt  < rficdt1 and rficdt1 ne . then epoch='INITIAL SCREENING';
			else if missing(rfstdt) and . < rficdt <= ref_dt  then epoch='INITIAL SCREENING';

			if .< rscrn_dt <= ref_dt < rfstdt and rfstdt ne . then epoch = "RE-SCREENING";
			else if .< rscrn_dt <= ref_dt <= rficdt1  and rfstdt eq . then epoch = "RE-SCREENING";
			else if missing(rfstdt) and . < rficdt <rscrn_dt <= ref_dt and .<rficdt1<=ref_dt then epoch='RE-SCREENING';

			if .< rfstdt <= ref_dt <= rfendt then epoch = "TREATMENT";*"STUDY PHASE";
			if missing(rfendt) and not missing(final_dt) then do;
	 			if .< rfstdt <= ref_dt<=final_dt then epoch = "TREATMENT";
			end;
			else if missing(rfendt) and missing(final_dt) then do;
				if .< rfstdt <= ref_dt then epoch = "TREATMENT";
			end;			
			if (ref_dt > rfendt > .) or ( ref_dt >  sv_erly_disc > . ) or ( ref_dt >  final_dt > . ) then epoch = "FOLLOW-UP";*"END OF STUDY";
		end;
	run;
%mend epoch;

%epoch(indset=vs_der, stdtc=vsdtc, outdset=vs_der1);

data vs_der2;
    set vs_der1;
    if missing(vsorres) and missing(vsstresc) then 
        put "Note: Result variables are missing for the usubjid=" usubjid "VSTEST=" vstestcd "VSSEQ = " vsseq;
run;

* Baseline Flag derivation ;
proc sort data = vs_der2 out = vs_base0;
    by usubjid vscat vstestcd vsdt vsspid  ;
    where . lt vsdt le rfstdt and vsstresc ne '';
run;

data vs_base1;
    set vs_base0(drop=vsblfl);
    by usubjid vscat vstestcd vsdt vsspid  ;
    if last.vstestcd;
    vsBLFL = 'Y';

    keep usubjid vsseq vscat vstestcd vsgrpid vsspid vsdt vsblfl;
run;

proc sort data=vs_der2(drop=vsblfl);
    by usubjid vsseq vscat vstestcd vsgrpid vsspid vsdt ;
run;

data vs;
    merge vs_der2(in=a) vs_base1(in=b);
    by usubjid vsseq vscat vstestcd vsgrpid vsspid vsdt ;
    if a;

	if missing(vsorres) then vsorresu = "";
	if missing(vsstresc) then vsstresu = "";
run;

/*Final DATASET*/
proc sort data = vs(drop=epoch_);
    by usubjid vsseq ; 
run;

