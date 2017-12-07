dm log 'clear'; dm output 'clear';

ods html close;
ods html;

data shampoo;
	input z;
	time = _n_;
	cards;
339
319
352
330
378
392
390
395
386
383
396
396
412
387
382
423
386
420
417
474
450
444
456
449
428
444
389
447
395
417
;

proc print data = shampoo;
	title 'Shampoo original series';

proc arima data = shampoo;
	identify var = z outcov = SAC_SPAC;
	title 'Shampoo original series';
	
proc print data = SAC_SPAC;
	var lag corr partcorr;
	title 'Shampoo original series';

proc arima data = shampoo;
	identify var = z(1) outcov = SAC_SPAC2;
	title 'Shampoo 1st differences series';
	
proc print data = SAC_SPAC2;
	var lag corr partcorr;
	title 'Shampoo 1st differences series';	
run;
quit;

