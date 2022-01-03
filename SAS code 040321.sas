



*4/1/21 code for Meta-analysis project;

/**********************************************************************
 *   PRODUCT:   SAS
 *   VERSION:   9.4
 ***********************************************************************/
    data WORK.RAWDATA    ;
    infile 'C:\Users\mbreu\OneDrive - Michigan State University\Meta-analysis\final data work\RawOutput_6trt.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
       informat VAR1 best32. ;
       informat site $10. ;
       informat PI $1. ;
       informat timing $10. ;
       informat block best32. ;
       informat year best32. ;
       informat variety $15. ;
       informat yield best32. ;
       informat project $20. ;
       informat StudyCode $50. ;
       informat ID best32. ;
       format VAR1 best12. ;
       format site $10. ;
       format PI $1. ;
       format timing $10. ;
       format block best12. ;
       format year best12. ;
       format variety $15. ;
       format yield best12. ;
       format project $20. ;
       format StudyCode $50. ;
       format ID best12. ;
    input
                VAR1
                site  $
                PI  $
                timing  $
                block
                year
                variety  $
                yield
                project  $
                StudyCode  $
                ID
    ;
	run;
   

/*this was original proc import statement, used this then F4 to generate the above, so I could change specificatiosn and make variables longer
proc import datafile="C:\Users\mbreu\OneDrive - Michigan State University\Meta-analysis\final data work\RawOutput.csv"
        out=RawData
			dbms=csv
			REPLACE;
		getnames=yes;
run;
*/


*Chose the 6 treatments of interst from studies, put into new excel sheet.
Code below is  calculating lsmeans for each trt in each study to be used as new response variable, with kenward Roger df;

proc sort data=RawData; by ID;run; *need proc sort or won't run by id correctly;
*if put interval option with the ods excel opening, will put results from each study in one sheet together, ods select ask for just output interested in;
ods excel file= "C:\Users\mbreu\OneDrive - Michigan State University\Meta-analysis\data work 032321\Only6Trt.csv" options(sheet_interval="now");
ods select CovParms LSMeans ;

Proc glimmix data=RawData nobound; by ID;
class block timing;
model yield=timing/ddfm=kr;
random block;
lsmeans timing;
run;
ods excel close;


/*******************************************************************************************************************************
This section begins the actual meta-analysis now usings means generated above, then manipulated in R to do effect size calculations and 
match up estimates from above with the original study meta-data, and calculated number of replications from original study 
 ******************************************************************************************************************************/

   ***********************************************************************/;


    data WORK.EffectData    ;
    %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
    infile 'C:\Users\mbreu\OneDrive - Michigan State University\Meta-analysis\final data work\EffectCalc_6trt.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
       informat VAR1 $4. ;
       informat timing $10. ;
       informat estimate best32. ;
       informat SE best32. ;
       informat DF best32. ;
       informat tvalue best32. ;
       informat ID $10. ;
       informat res_variance best32. ;
       informat res_SE best32. ;
       informat StudyCode $50. ;
       informat n best32. ;
       informat UTC_estimate best32. ;
	  
       informat UTC_n best32. ;
       informat NomDiff best32. ;
       informat PerDiff best32. ;
       format VAR1 $4. ;
       format timing $10. ;
       format estimate best12. ;
       format SE best12. ;
       format DF best12. ;
       format tvalue best12. ;
       format ID $10. ;
       format res_variance best12. ;
       format res_SE best12. ;
       format StudyCode $50. ;
       format n best12. ;
       format UTC_estimate best12. ;
       format UTC_n best12. ;
       format NomDiff best12. ;
       format PerDiff best12. ;
    input
                VAR1  $
                timing  $
                estimate
                SE
                DF
                tvalue
                ID$
                res_variance
                res_SE
                StudyCode  $
                n
                UTC_estimate
			
                UTC_n
                NomDiff
                PerDiff
    ;
    if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
    run;

proc print data=EffectData;
run;

data EffectData; set EffectData;	
varyld = res_variance * ((1/n)+(1/UTC_n)); *<---calculate sampling variance (standard error of difference, equation from Madden workshop materials);
wtyld = 1/varyld; *this will become the weight for each study;
run;

proc sort; by timing;
proc boxplot data=EffectData;
plot NomDiff*timing/outhistory=summary;
run;
proc print data=summary;
run;


*---RANDOM EFFECT---Model R2: Random Study Effect, Heterogeneous Btw-Study Variance---;
*six treatments, so 8 variance parameters to calculate, code from Madden 2016 review;

proc glimmix data=EffectData;
class ID timing;
weight wtyld;
model NomDiff= timing/chisq covb corrb; 
random timing/subject=ID type=CSH g gcorr;
parms 
(1)(1)(1)(1)(1)(1)
(0.5)(1)/hold = 8;
lsmeans timing/lines cl;
run;

*Macro for calculating probability of certain bushel response;
*----Macro from Madden's code (experimental macro) at end of program;
*--------sigma2 ==among study variance;


*******T1******;
title "T1"
%Probnewstudy(meanES=4.0148
,minT=0,maxT=25,SIGMA2=8.6495);
*******T1_T3******;
title "T1_T3"
%Probnewstudy(meanES=10.5336
,minT=0,maxT=25,SIGMA2=60.7624);
*******T2******;
title "T2"
%Probnewstudy(meanES=6.9306
,minT=0,maxT=25,SIGMA2=37.1623);

*******T2_T3******;
title "T2_T3"
%Probnewstudy(meanES=9.4826
,minT=0,maxT=25,SIGMA2=64.144
);

*******T3******;
title "T3"
%Probnewstudy(meanES=7.4141
,minT=0,maxT=25,SIGMA2=40.7907
);

*******T3_L******;
title "T3__L"
%Probnewstudy(meanES=6.6543
,minT=0,maxT=25,SIGMA2=31.7211
);


*****moderator exploration***;

    data WORK.STUDYDATA    ;
    %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
    infile 'C:\Users\mbreu\OneDrive - Michigan State University\Meta-analysis\final data work\EffectCalc_6trt_META.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
       informat VAR1 $4. ;
       informat timing $10. ;
       informat estimate best32. ;
       informat SE best32. ;
       informat DF best32. ;
       informat tvalue best32. ;
       informat ID $5. ;
       informat res_variance best32. ;
       informat res_SE best32. ;
       informat StudyCode_x $45. ;
       informat n $2. ;
       informat UTC_estimate best32. ;
	   informat YieldMod $5.;
       informat UTC_n $2. ;
       informat NomDiff best32. ;
       informat PerDiff best32. ;
       informat site $8. ;
       informat PI $3. ;
       informat year $5. ;
       informat variety $13. ;
       informat project $50. ;
       informat StudyCode_y $45. ;
       format VAR1 $4. ;
       format timing $10. ;
       format estimate best12. ;
       format SE best12. ;
       format DF best12. ;
       format tvalue best12. ;
       format ID $5. ;
       format res_variance best12. ;
       format res_SE best12. ;
       format StudyCode_x $45. ;
       format n $2. ;
       format UTC_estimate best12. ;
	   format YieldMod $5. ;
       format UTC_n $2. ;
       format NomDiff best12. ;
       format PerDiff best12. ;
       format site $8. ;
       format PI $3. ;
       format year $5. ;
       format variety $13. ;
       format project $50. ;
       format StudyCode_y $45. ;
    input
                VAR1  $
                timing  $
                estimate
                SE
                DF
                tvalue
                ID $
                res_variance
                res_SE
                StudyCode_x  $
                n  $
                UTC_estimate
				YieldMod $
                UTC_n  $
                NomDiff
                PerDiff
                site  $
                PI  $
                year $
                variety  $
                project  $
                StudyCode_y  $
    ;
    if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
    run;
	*above made sure year and studyID were categroical $;

data EffectData; set StudyData;	
varyld = res_variance * ((1/n)+(1/UTC_n)); *<---calculate sampling variance (standard error of difference, equation from Madden workshop materials);
wtyld = 1/varyld; *this will become the weight for each study;
run;

proc glimmix data=EffectData;
class ID timing site;
weight wtyld;
model NomDiff= timing timing*site/chisq covb corrb ddfm=kr; 
random timing/subject=ID type=CSH g gcorr;
parms 
(1)(1)(1)(1)(1)(1)
(0.5)(1)/hold = 8;
lsmeans timing*site/lines cl;
run;

proc sort; by timing;
proc boxplot data=EffectData;
plot NomDiff*timing/outhistory=summary;
run;
proc print data=EffectData;
run;
***MADDEN MACROS*******;

%macro Probnewstudy(meanES=,SIGMA2=,numb=20,minT=,maxT=);
*---Experimental macro, provided as-is (that is, with no warranty).;

*---Determine probability that a randomly selected study has an Effect Size greater (less) than
	the constant _T, based on the meanES and between-study variance from a random-effects
	meta-analysis. Method described in Madden and Paul (Phytopathology 101:16-30 [2011]).
	Key references given in that paper. Macro written by L. Madden. ;
*---Macro overwrites title2 and higher.;

/*
meanES		Mean (expected) Effect Size. Use meanES estimated from a random-effects meta-analysis
SIGMA2		Among-study variance (from a random-effects meta-analysis)
minT		Minimum value of Effect-Size Constant considered 
maxT		Maximum value of Effect-Size Constant considered
numb		Number of constant Effect Sizes between minT and maxT (default is 20)
			(one actually gets one more than the value of numb)

Results are put in the data file _gener, which can be accessed after running the macro,
if desired.
*/

title2 'Probability that Effect Size is greater (or less) than specified constants';
title3 '(Investigator must use context to know which one of two probabilities is needed)';
data _gener; 
_meanES = &meanES;
_SIG2 = &SIGMA2;
label  _1mpz='Pr(ES > const)' _meanES='mean ES' _SIG2='among-study var' 
_pz='Pr(ES < const)' 
_t='const ES';

_SIGMA=sqrt(&SIGMA2);
_incr = (&maxT - &minT)/&numb;

do 	_t =  &minT to &maxT  by _incr;
	_z = (_t - &meanES)/_SIGMA;
	_pz = cdf('normal',_z);
	_1mpz = 1 - _pz;
	output;
label _t='Constant' _z='Z statistic' _pz='Prob(less than)' _1mpz='Prob(greater than)';
end;
put "data file is _gener";
run;
proc print data=_gener label; var _meanES _SIG2 _t _z _pz _1mpz; 
run;
*proc datasets nolist;*run;
%mend Probnewstudy;
run;

*---The following is for the univariate analysis of yld_diff.;

%Probnewstudy(meanES=5.29,minT=-5,maxT=25,SIGMA2=71.908);

proc sgplot data=_gener;
series y=_pz x=_t / lineattrs=(thickness=2 pattern=2 color=blue);
series y=_1mpz x=_t / lineattrs=(thickness=2 pattern=1 color=blue);
refline 0 / axis=x lineattrs=(color=black pattern=1 thickness=1.5);
run;


%macro funnel(dfile=,es=,wgt=,study=,minprec=2,maxprec=80,numb=25);
*---Experimental macro, provided as-is (that is, provided with no warranty);

*---Macro produces a funnel graph and a radial plot for assessing whether or
	not a random study effect is needed. These diagnostics, especially the
	funnel graph, are very common in some disciplines (but not yet in plant 
	pathology). Macro written by L. Madden.;

*---Funnel graph: plot of "precision" vs. effect size, with a vertical line
	for FIXED-effect common effect size, and curves for the confidence intervals
	around the common effect size. If "too many" points are outside the curves,
	this is evidence for a random effects analysis.
	In this application, precision is the inverse of the within-study SE;
*---Ideally, the funnel graph should be symmetrical. Gaps (say, on the lower
	left or right), can indicate PUBLICATION BIAS, where nonsignificant results
	are not published or made available. However, the gaps could be due to
	other factors, so this is just a guide. These graphs originiated with, or
	were advocated by, Light and Pillemer, Sterne et al., and Egger et al.;

*---Radial plots, also known as Galbraith plots, show the standardized effect
	size (_es_/SE) versus precision (1/SE) for each study. The slope of the line
	through these points is the FIXED-effects common effect size. If many points
	fall outside the confidence bands, there is evidence for a random effects
	analysis. Gaps are indicative of PUBLICATION BIAS.;
	
/*
dfile		name of the sas data file with data
es			name of variable in dfile with the effect size response variable
wgt			name of the weight variable in dfile. This needs to be the
			inverse of the "sampling variance" for each study
study		variable name for the labeled study (trial) in dfile
minprec		Minimum precision value used in the graph (set by trial and error, so
			that the full range of data is encompassed by the curves
maxprec		Maximum precision value used in the graph (set by trial and error, so
			that the full range of data is encompassed by the curves
numb		Number of precision values to make from minprect to maxprec (default25)

Macro creates various variables and data files starting with the underline symbol (_). 
As a precaution, it is best if you do not have any variables or file names that
start with this symbol. Macro also overwrites titles starting at title2. 
One can ignore tabular output here (just use the plots). Graphs are made with 
the new SGPLOT procedure, which requires sas 9.2 or later.

After running the macro, the data files _pred1s and _funnel are available for 
further use, if desired.
*/

title2 'Funnel and radial plots';
*ods listing close;
*ods html;
*ods graphics on  /* / width=6in */ ;

data _ESdata; set &dfile;
keep &es  _precdata _StES;
_precdata = sqrt(&wgt);
_StES = &es*_precdata;
run;
*proc print data=_ESdata;run;
ods exclude all;
proc mixed data=_ESdata;	*---a fixed-effect (common-effect) analysis.;
title3 '(PROC MIXED used to get fixed-effect values, go right to the plots)';
ods output solutionf=_sf; 
model _StES = _precdata / s noint outp=_pred1;
parms (1) / hold=1;
run;
ods exclude none;
title3;
data _null_; set _sf;
call symput('ref',estimate);

run;


data _prec; 

_incr = (&maxPrec - &minPrec)/&numb;
do _prec = &minPrec to &maxPrec by _incr;

	_ES1 = &ref + 2*(1/_prec);
	_ES2 = &ref - 2*(1/_prec);
	output;
end;
run;
*proc print data=_prec;run;




data _funnel; merge _prec _ESdata;
run;
*proc print data=_funnel;run;

data _pred1; set _pred1;
low = pred - 2;
high = pred + 2;

proc sort data=_pred1 out=_pred1s;
by _precdata;run;
*proc print data=_pred1s;run;



data _pred1s; set _pred1s;
label _precdata = 1/SE _StES=Standardized Effect Size;
data _funnel; set _funnel;
label &es=Effect size _precdata=1/SE;
run;

data _null_;
put "Radial plot data in file _pred1s";
put "funnel data in file _funnel";
run;

run;


proc sgplot data=_funnel noautolegend;
series x=_ES1 y=_prec / lineattrs=(color=red pattern=2 thickness=2);
series x=_ES2 y=_prec / lineattrs=(color=red pattern=2 thickness=2);
scatter x=&es y=_precdata / markerattrs=(color=blue symbol=squarefilled size=7pt);
refline &ref / axis=x  lineattrs=(color=blue pattern=1 thickness=2);
*refline 0 / axis=x lineattrs=(color=gray pattern=3 thickness=1);
run;

proc sgplot data=_pred1s noautolegend;
scatter  x=_precdata y=_StES / markerattrs=(color=red symbol=squarefilled size=7pt);
series x=_precdata y=pred / lineattrs=(color=red pattern=1 thickness=2);
series x=_precdata y=low / lineattrs=(color=blue pattern=2 thickness=1);
series x=_precdata y=high / lineattrs=(color=blue pattern=2 thickness=1);
run;   

*ods graphics off;
*ods html close;
*ods listing;
run;

title2;
ods results off;
proc datasets nolist ; delete _esdata  _prec _pred1  _sf; run; ods results;run;
%mend funnel;
run;


