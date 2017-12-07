dm output 'clear'; dm log 'clear';

ods html close;
ods html;

/* Read in data */
data supervisor;
 infile 'C:\Users\10543890\Documents\UVU\MATH 3710\spring 2016\in class examples\chapter 4\supervisors\Supervisor.txt' firstobs=2;
 input Ratings Complaints Priviliges Learn Raises Critical Advance;
run;

/* Scatterplot. You can also use proc corr */
proc sgscatter data=supervisor;
	title "Scatterplot Matrix of Supervisor data";
	matrix Ratings Complaints Priviliges Learn Raises Critical Advance;
run;
title; /* Removes the title*/

proc corr data=supervisor;
var Ratings Complaints Priviliges Learn Raises Critical Advance;
run;

proc reg data = supervisor;
	model Ratings = Complaints Priviliges Learn Raises Critical Advance / clb clm cli;
	output out=diagnostic p=fits r=residual student=stdresid h=hatvals;
	plot student.*nqq.;
	title 'QQ Plot'; run;
	plot student.*p.;
	title 'residual Plot - fitted'; run;
	plot student.*Complaints;
	title 'residual Plot - complaints'; run;
	plot student.*Priviliges;
	title 'residual Plot - Priviliges'; run;
	plot student.*Learn;
	title 'residual Plot - Learn'; run;
	plot student.*Raises;
	title 'residual Plot - Raises'; run;
	plot student.*Critical;
	title 'residual Plot - Critical'; run;
	plot student.*Advance;
	title 'residual Plot - Advance'; run;
run;

proc print data = diagnostic;

proc univariate data=diagnostic noprint;
	var stdresid;
	histogram stdresid/normal(mu=est sigma=est color=red noprint)
						midpoints = -3 -2 -1 0 1 2 3;
	title 'histogram of residuals'; run;

%let k=10000;
/* cross - validation analysis - simulation in SAS */
proc surveyselect data=supervisor
   method=srs n=10 out=fulldata reps=&k outall noprint seed = 123345;
run;
 
/* where selected = 0 is training data set */
proc reg data = fulldata(where=(selected=0)) outest=RegOut noprint;
pred_Ratings: model Ratings = Complaints Priviliges Learn Raises Critical Advance;
by replicate;
title 'Training Model'; run;
quit;
/* where selected = 1 is test data set */
proc score data = fulldata(where=(selected=1)) score = RegOut out = newpred type=parms;
   var Complaints Priviliges Learn Raises Critical Advance;
   by replicate;
   title 'Testing Training Model'; run;
    
data final ;
   set newpred ;
   residual = Ratings - pred_Ratings;
   residual_square = residual**2;
run;    
 
proc means data = final mean noprint;
   var residual residual_square;
   output out = biasdata mean(residual residual_square) = predicted_bias mean_square_bias;
run;
 
data biasdata;
   set biasdata;
   rpmse = mean_square_bias**0.5;
run;
 
proc print data = biasdata;
run;

run;
quit;


