dm output 'clear'; dm log 'clear';

ods html close;
ods html;

options ls=78;
title "Donner Party Data - Logistic Regression";


data donner;
  infile "C:\Users\10543890\Documents\UVU\MATH 3710\spring 2016\in class examples\logistic regression\donner.txt";
  input age sex survive;
  run;

proc freq;
  tables sex*survive / chisq expected;
  run;

proc logistic data = donner descending;
  model survive = age sex age*sex / ctable outroc=ROCData;
  output out=a p=pi;
  run;

symbol1 v=dot i=join; 
proc gplot data=ROCData; 
 plot _sensit_*_1mspec_; ;

proc format;
  value sexfmt 1= 'male' 0='female';
  run;

proc sgscatter data=a ;
  plot pi*age / group= sex pbspline;
  format sex sexfmt.;
run;

run;
quit;
