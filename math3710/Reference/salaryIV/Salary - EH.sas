dm output 'clear'; dm log 'clear';

ods html close;
ods html;

/* read in data from excel file */
proc import datafile = 'C:\Users\10543890\Documents\UVU\MATH 3710\spring 2016\in class examples\Qualitative covariates\Salary Data\Salary.xlsx'
OUT = salary DBMS = xlsx replace;
getnames=yes;

data salary;
set salary;
bachelors = 0;
graduate_degree = 0;
if education = "BS" then bachelors = 1;
if education = "BS+" then graduate_degree = 1;
Mgr = 0;
if manager = "Yes" then Mgr = 1;
E1M = bachelors*Mgr;
E2M = graduate_degree*Mgr;

proc print data = salary;

/* Scatterplot */
proc sgscatter data=salary ;
	title "Salary by Experience";
	compare y=Salary x=experience / group=education 
	markerattrs=(symbol=circlefilled size=15);
run;

proc sgscatter data=salary ;
	title "Salary by Manager";
	compare y=Salary x=experience / group=Mgr  
	markerattrs=(symbol=circlefilled size=15);
run;

proc sgplot data=salary;
   vbox salary / category=education;
   xaxis label="education";

proc sgplot data=salary;
   vbox salary / category=manager;
   xaxis label="manager";

proc reg data=salary;
model Salary= Experience bachelors graduate_degree Mgr E1M E2M / clb clm;
   output out=diagnostic p=fits r=residual student=stdresid h=hatvals;
	plot student.*nqq.;
	title 'QQ Plot'; run;
	plot student.*Experience;
	title 'residual Plot - by experience'; run;
	plot student.*category;
	title 'residual Plot - by education / management combination'; run;
run;

proc plot data = diagnostic;
plot stdresid*category='o';

run;
quit;
