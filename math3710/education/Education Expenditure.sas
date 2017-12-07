dm output 'clear'; dm log 'clear';

ods html close;
ods html;


proc import datafile = 'C:\Users\10543890\Documents\UVU\MATH 3710\spring 2016\in class examples\chapter 8\Education Expenditure Data.xlsx'
OUT = education DBMS = xlsx replace;
getnames=yes;

* proc print data = education;

data education_no_Alaska;
set education;
if state = 'AK' then delete;
root_wgt = wgt**.5;
y_wt = y*root_wgt;
x1_wt = x1*root_wgt;
x2_wt = x2*root_wgt;
x3_wt = x3*root_wgt;

* proc print data = education_no_Alaska;


proc reg data=education;
model y = x1 x2 x3; title 'OLS - includes Alaska'; run;
   output out=diagnostic p=fits r=residual student=stdresid h=hatvals cookd = cooksd dffits = dffits;
	plot student.*p.;
	title 'residual Plot vs. fitted'; run;
	plot student.*x1;
	title 'residual Plot - by x1'; run;
	plot student.*x2;
	title 'residual Plot - by x2'; run;
	plot student.*x3;
	title 'residual Plot - by x3'; run;
run;

proc print data = diagnostic;

proc plot data = diagnostic;
plot stdresid*region='o';


proc reg data=education_no_Alaska;
model y = x1 x2 x3; title 'OLS - Alaska removed'; run;
   output out=diagnostic p=fits r=residual student=stdresid h=hatvals cookd = cooksd dffits = dffits;
	plot student.*p.;
	title 'residual Plot vs. fitted'; run;
	plot student.*x1;
	title 'residual Plot - by x1'; run;
	plot student.*x2;
	title 'residual Plot - by x2'; run;
	plot student.*x3;
	title 'residual Plot - by x3'; run;
run;

proc reg data = education_no_Alaska;
	model y = x1 x2 x3 / alpha = .05 clb clm cli i; 
	weight wgt; title 'WLS model'; run;
	output out=diagnostic p=fits r=residual student=stdresid h=hatvals;
	plot student.*p.;
	title 'residuals vs. fitted - WLS model';
run;

proc plot data = diagnostic;
plot stdresid*region='o';


proc reg data=education_no_Alaska;
model y_wt = root_wgt x1_wt x2_wt x3_wt / noint; title 'OLS - transformed model'; run;
   

run;
quit;
