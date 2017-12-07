
dm output 'clear'; dm log 'clear';

ods html close;
ods html;

data env;
 infile "C:\Users\10543890\Documents\UVU\MATH 3710\spring 2016\in class examples\chapter 7\EnvironmentalImpacts.txt" firstobs=2; 
 input AnnPrecip MeanJanTemp MeanJulyTemp PctGT65 PopPerHouse School PctSound PopPerSqMile 
		PctNonWhite PctWhiteCollar PctU20000 Hydrocarbons Nitrogen SO2 RelHumid AAMort;
 loghydro = log(Hydrocarbons);
 lognitro = log(Nitrogen);
 logso2 = log(SO2);
run;

proc sgscatter data=env;
	title "Scatterplot Matrix of Environmental data";
	matrix AAMort AnnPrecip MeanJanTemp MeanJulyTemp PctNonWhite PctWhiteCollar PctU20000 Hydrocarbons Nitrogen SO2 RelHumid ;
run;

proc sgscatter data=env;
	title "Scatterplot Matrix of Environmental data - log transforms";
	matrix AAMort AnnPrecip MeanJanTemp MeanJulyTemp PctNonWhite PctWhiteCollar PctU20000 loghydro lognitro logSO2 RelHumid ;
run;

proc corr data=env;
var AnnPrecip MeanJanTemp MeanJulyTemp PctGT65 PopPerHouse School PctSound PopPerSqMile 
		PctNonWhite PctWhiteCollar PctU20000 Hydrocarbons Nitrogen SO2 RelHumid AAMort;
run;

proc reg data = env;
	model AAMort = AnnPrecip MeanJanTemp MeanJulyTemp PctGT65 PopPerHouse School PctSound PopPerSqMile 
		PctNonWhite PctWhiteCollar PctU20000 lognitro logSO2 RelHumid / vif;
	title 'Full Model - without loghydro'; run;
	output out=diagnostic p=fits r=residual student=stdresid h=hatvals;
	plot student.*nqq.;
	title 'QQ Plot'; run;
	plot student.*p.;
	title 'residual Plot - fitted'; run;

proc reg data = env;
	model AAMort = AnnPrecip MeanJanTemp MeanJulyTemp PctGT65 PopPerHouse School PctSound PopPerSqMile 
		PctNonWhite PctWhiteCollar PctU20000 loghydro logSO2 RelHumid / vif;
	title 'Full Model - without logSO2'; run;
	output out=diagnostic p=fits r=residual student=stdresid h=hatvals;
	plot student.*nqq.;
	title 'QQ Plot'; run;
	plot student.*p.;
	title 'residual Plot - fitted'; run;
	

proc univariate data=diagnostic noprint;
	var stdresid;
	histogram stdresid/normal(mu=est sigma=est color=red noprint)
						midpoints = -3 -2 -1 0 1 2 3;
	title 'histogram of residuals'; run;

proc reg data = env;
	model AAMort = AnnPrecip MeanJanTemp MeanJulyTemp PctGT65 PopPerHouse School PctSound PopPerSqMile 
		PctNonWhite PctWhiteCollar PctU20000 lognitro logSO2 RelHumid / selection = cp aic bic rmse rsquare best = 10;
	title 'Full Model - without loghydro - best subset method'; run;

proc reg data = env;
	model AAMort = AnnPrecip MeanJanTemp MeanJulyTemp PctGT65 PopPerHouse School PctSound PopPerSqMile 
		PctNonWhite PctWhiteCollar PctU20000 lognitro logSO2 RelHumid / selection = stepwise sle = .10 sls = .10;
	title 'Full Model - without loghydro - stepwise selection'; run;	

proc reg data = env;
	model AAMort = AnnPrecip MeanJanTemp MeanJulyTemp PctGT65 PopPerHouse School PctSound PopPerSqMile 
		PctNonWhite PctWhiteCollar PctU20000 lognitro logSO2 RelHumid / selection = forward sle = .10;
	title 'Full Model - without loghydro - forward selection'; run;

proc reg data = env;
	model AAMort = AnnPrecip MeanJanTemp MeanJulyTemp PctGT65 PopPerHouse School PctSound PopPerSqMile 
		PctNonWhite PctWhiteCollar PctU20000 lognitro logSO2 RelHumid / selection = backward sls = .10;
	title 'Full Model - without loghydro - backward selection'; run;

proc reg data = env;
	model AAMort = AnnPrecip MeanJanTemp School PctNonWhite lognitro / clb vif;
	title 'Final Model'; run;
	output out=diagnostic p=fits r=residual student=stdresid h=hatvals;
	plot student.*nqq.;
	title 'QQ Plot'; run;
	plot student.*p.;
	title 'residual Plot - fitted'; run;

proc reg data=env noprint outest=information;
  model1: model AAMort = AnnPrecip MeanJanTemp MeanJulyTemp PctGT65 PopPerHouse School PctSound PopPerSqMile 
		PctNonWhite PctWhiteCollar PctU20000 lognitro logSO2 RelHumid /rmse rsquare adjrsq cp aic bic;
  model2: model AAMort = AnnPrecip MeanJanTemp School PctNonWhite lognitro/rmse rsquare adjrsq cp aic bic;
  run;

proc print data=information;
run;

proc univariate data=diagnostic noprint;
	var stdresid;
	histogram stdresid/normal(mu=est sigma=est color=red noprint)
						midpoints = -3 -2 -1 0 1 2 3;
	title 'histogram of residuals'; run;


proc sgplot data=env;
   vbox lognitro ;
   xaxis label="log(nitrogen level)";

proc sgplot data=env;
   vbox nitrogen ;
   xaxis label="nitrogen level";

proc sgplot data=env;
   vbox AAMort ;
   xaxis label="mortality rates";

run;
quit;
