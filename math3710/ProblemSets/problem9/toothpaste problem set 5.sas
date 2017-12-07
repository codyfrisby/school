dm log 'clear'; dm output 'clear';

ods html close;
ods html;

data toothpaste;
	input z;
	time = _n_;
	cards;
  235
239
244.09
252.731
264.377
277.934
286.687
295.629
310.444
325.112
336.291
344.459
355.399
367.691
384.003
398.042
412.969
422.901
434.96
445.853
455.929
465.584
477.894
491.408
507.712
517.237
524.349
532.104
538.097
544.948
551.925
557.929
564.285
572.164
582.926
595.295
607.028
617.541
622.941
633.436
647.371
658.23
670.777
685.457
690.992
693.557
700.675
712.71
726.513
736.429
743.203
751.227
764.265
777.852
791.07
805.844
815.122
822.905
830.663
839.6
846.962
853.83
860.84
871.075
877.792
881.143
884.226
890.208
894.966
901.288
913.138
922.511
930.786
941.306
950.305
952.373
960.042
968.1
972.477
977.408
977.602
979.505
982.934
985.833
991.35
996.291
1003.1
1010.32
1018.42
1029.48
;

proc arima data = toothpaste;
	identify var = z nlag = 14 outcov = SAC_SPAC_original; 
	title 'toothpaste original time series';
	
proc print data = SAC_SPAC_original;
	var lag corr partcorr;
	
proc arima data = toothpaste;
	identify var = z(1) nlag = 14 outcov = SAC_SPAC_differences; 
	estimate p = (1) printall plot; /* Specifies an AR(1) model which includes a constant term */
	title 'toothpaste 1st differences';

proc print data = SAC_SPAC_differences;
	var lag corr partcorr;
	

run;
quit;
