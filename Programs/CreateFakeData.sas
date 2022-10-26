libname yc "C:\Users\yche465\Desktop\CS570\Project";

data prep;
set yc.prep;
run;

proc sort data=prep nodupkey out=prep_id;
by pid;
run;

data prep_id_;
set prep_id;
ID=_N_;
drop Rx_start Rx_end;
run;

proc sql outobs=13000;
create table prep_id_13k as 
select *,intnx('day', '1Jan23'd, round(ranuni(12)*365)) as Start format=mmddyy10.
from prep_id_
order by ranuni(1234);
quit;

proc sql;
create table prep_random_13k as 
select b.ID as ID,a.Rx_start as Rx_start, a.Rx_end as Rx_end, b.Start as Start, b.Start+365.25*2 as End format=mmddyy10.
from prep as a
right join prep_id_13k as b
on a.pid=b.pid;
quit; 

%macro RandBetween(min, max);
   (&min + floor((1+&max-&min)*rand("uniform")))
%mend;


proc sort data=prep_random_13k;
by ID Rx_start;
run;

data test;
set prep_random_13k;
by ID;
*vary PrEP use interval length and ensure the first interval is at least 60 days;
call streaminit(1234);
dur=(Rx_end-Rx_start);
r=%RandBetween(-(dur/2),70);
dur2=round(dur+r);
if first.ID and dur2<60	then dur2=%RandBetween(60,90);
if first.ID then do;
	Rx_start2=Start;
	Rx_end2=Start+dur2;
	end;
format Rx_start2 mmddyy10.;
format Rx_end2 mmddyy10.;
run;


*vary the duration between the next PrEP use interval;
%macro mod;
proc sql noprint;
select max(count) 
into: maxit
from ( 
select ID, count(*) as count
from test
group by ID);
quit;

%do i=1 %to &maxit;
	data test;
	set test;
	by ID;
	call streaminit(i);
	lag_Rx_end2=lag(Rx_end2);
	if first.ID then lag_Rx_end2=.; 
	format lag_Rx_end2 mmddyy10.;
	if Rx_start2=. then do;
		Rx_start2=lag_Rx_end2+%RandBetween(15,200);
		Rx_end2=Rx_start2+dur2;
		end;
	run;
%end;
%mend mod;

%mod

*Output fake dataset;

libname yc2 "C:\Users\yche465\Desktop\AIM 1\Codes\PrEP-Traj-Clustering\Data";

data yc2.syndata_13k;
set test;
if Rx_start2>End then delete;
if Rx_end2>End then Rx_end2=End;
keep ID Rx_start2 Rx_end2;
rename Rx_start2=PrEP_Start Rx_end2=PrEP_End;
run; 

PROC EXPORT DATA= YC2.SYNDATA_13k 
            OUTFILE= "C:\Users\yche465\Desktop\AIM 1\Codes\PrEP-Traj-Clustering\Data\syndata_13k.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


proc contents data=yc2.syndata_13k;
run;
