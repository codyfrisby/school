dm output 'clear'; dm log 'clear';

ods html close;
ods html;

data supervised_workers;
	input workers supervisors;
	y_transform = supervisors/workers;
	x_transform = 1/workers;
	wgt = 1/workers**2;
	cards;
294	30
247	32
267	37
358	44
423	47
311	49
450	56
534	62
438	68
697	78
688	80
630	84
709	88
627	97
615	100
999	109
1022 114
1015 117
700	106
850	128
980	130
1025 160
1021 97
1200 180
1250 112
1500 210
1650 135
500	.   
1000 .  
;



proc reg data = supervised_workers;
model supervisors = workers/ alpha = .05 clb clm cli i; title 'SLR model'; run; 
output out=diagnostic p=fits r=residual student=stdresid h=hatvals;
plot supervisors*workers;
title 'Scatter Plot - Overlay'; run;
plot student.*workers;
title 'residuals vs. workers';
ods graphics off;



proc reg data = supervised_workers;
model y_transform = x_transform / alpha = .05 clb clm cli; 
output out=diagnostic_transformation p=fits r=residual student=stdresid h=hatvals;
plot y_transform * x_transform;
title 'Scatter Plot - OLS - transformed data'; run;
plot student.*x_transform;
title 'residuals vs. 1/x - OLS transformed data';
plot student.*p.;
title 'residuals vs. fitted - OLS transformed data';
ods graphics off;

proc reg data = supervised_workers;
	model supervisors = workers / alpha = .05 clb clm cli i; title 'WLS model'; run;
	weight wgt;
	output out=diagnostic p=fits r=residual student=stdresid h=hatvals;
	plot student.*p.;
	title 'residuals vs. fitted - WLS model';
run;


quit;
