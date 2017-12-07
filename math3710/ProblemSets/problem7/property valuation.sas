dm output 'clear'; dm log 'clear';

ods html close;
ods html;


proc import datafile = 'C:\Users\10543890\Documents\UVU\MATH 3710\spring 2016\problem set 7\building sales.xlsx'
OUT = BuildingSales DBMS = xlsx replace;
getnames=yes;

proc print data = BuildingSales;

proc sgscatter data=BuildingSales;
	title "Scatterplot Matrix of Building Sales";
	matrix y x1 x3 x4 ;
run;

proc reg data = BuildingSales ;
	model y = x1 x2 x3 x4 x5 x6 x7 x8 x9 / vif;

proc reg data = BuildingSales ;
	model y = x1 x2 x3 x4 x5 x7 x8 x9 / vif;

proc reg data = BuildingSales ;
	model y = x1 x2 x3 x4 x5 x7 x8 x9 / selection = forward sle = .10;

proc reg data = BuildingSales ;
	model y = x1 x2 x3 x4 x5 x7 x8 x9 / selection = backward sls = .10;

proc reg data = BuildingSales ;
	model y = x1 x2 x3 x4 x5 x7 x8 x9 / selection = stepwise sle = 0.10 sls = .10;

proc reg data = BuildingSales ;
	model y = x1 x2 x3 x4 x5 x7 x8 x9 / selection = adjrsq rmse rsquare aic bic  best = 30;

proc reg data=BuildingSales noprint outest=information;
  x1x2: model y = x1 x2/rmse rsquare adjrsq cp aic bic;
  x1x2x5: model y = x1 x2 x5/rmse rsquare adjrsq cp aic bic;
  x1x2x7: model y = x1 x2 x7/rmse rsquare adjrsq cp aic bic;
  x1x2x9: model y = x1 x2 x9/rmse rsquare adjrsq cp aic bic;
  run;

proc print data=information;
run;

proc reg data = BuildingSales;
	model y = x1 x2 / vif adjrsq rmse rsquare aic bic;
	plot student.*nqq.;
	title 'QQ Plot'; run;
	plot student.*p.;
	title 'residual Plot - by fitted'; run;



run;
quit;
