
* Import the sero-protection time series matrix;
proc import datafile="C:\Users\yche465\Desktop\AIM 1\Codes\PrEP-Traj-Clustering\Export\SeroProtect.txt" 
out=SeroProtect
dbms=tab
replace;
getnames=no;
delimiter=',';
run;

*Modify variable names, create week index variable;
data SeroProtect2;
set SeroProtect;
array a{102} Var2-Var103;
array b{102} Protect1-Protect102;
array c{102} T1-T102;
do i=1 to 102;
	b{i}=a{i};
	c{i}=i;
	end;
rename Var1=ID;
keep Var1 Protect1-Protect102 T1-T102;
run;

**************************************************************
Test run of GBTM - logit regression
	- K = 4
	- 2nd polynomial order with the time variable
**************************************************************;
PROC TRAJ DATA=SeroProtect2 OUTPLOT=OP OUTSTAT=OS OUT=OF OUTEST=OE ITDETAIL;
ID ID; VAR Protect1-Protect102; INDEP T1-T102;
MODEL LOGIT; NGROUPS 4; ORDER 2 2 2 2;
RUN;

*Graph sero-protection trajectories by identified groups;
%include "C:\Users\yche465\Desktop\AIM 1\Codes\PrEP-Traj-Clustering\Programs\trajplotnew.sas";
%trajplotnew(OP,OS,"Sero-Protection vs. Time","Logit Model","Sero-protection","Time")


*Export the k=4 model predictions and weighted averages by follow-up time to a csv file;
proc export data=OP
    outfile="C:\Users\yche465\Desktop\CS570\Project\AvgPred.csv"
    dbms=csv;
run;

*Export the k=4 model parameters' estimates to a csv file;
proc export data=OS
    outfile="C:\Users\yche465\Desktop\CS570\Project\Est_Pct.csv"
    dbms=csv;
run;


**************************************************************************
Implement GBTM modeling for K=2~K9
*************************************************************************;

*Create a macro function for implementing the GBTM procedures (under binomial distribution) at a pre-specified k value;
%macro Traj(dataset,k,order);
	*create macro variable for the order parameter;
	data seq;
	do i=1 to &k;
	order=&order;
	output;
	end;
	run;

	proc sql noprint;
	select order
	into :orderlist separated by ' '
	from seq; 
	quit;
	
	*Run the finite mixture trajectory model under binomial distribution;
	PROC TRAJ DATA=&dataset OUTPLOT=OP OUTSTAT=OS OUT=OF OUTEST=OE ITDETAIL;
		ID ID; VAR Protect1-Protect102; INDEP T1-T102;
		MODEL LOGIT; NGROUPS &k; ORDER &orderlist;
	RUN;
	*output model fitness metrics;
	data modelfit&k;
	retain K _LOGLIK_ _BIC1_ _AIC_;
	set oe;
	K=&k;
	where _TYPE_="PARMS";
	rename _LOGLIK_=LN_LIK _BIC1_=BIC _AIC_=AIC;  
	keep K _LOGLIK_ _BIC1_ _AIC_; 
	run;
%mend;

*Create a macro function to iteratively implement GBTM fitting across various k values and output fit statistics;
%macro rep_traj(data,maxk,order);
%DO I = 2 %TO &maxk;
 %Traj(&data,&I,&order);
%END;

data modfit;
do i=2 to &maxk;
datname=cat("modelfit",i);
output;
end;
run;

proc sql noprint;
select datname
into :datname separated by ' '
from modfit; 
quit;

data fit;
set &datname;
run;
%mend rep_traj;


*Conduct GBTM fitting (2nd polynomial with time variables) for k=2~10; 
%rep_traj(SeroProtect2,3,2)

*Examine model fit statistics (log-likelihood ,AIC, BIC);
proc print data=fit;
run;


