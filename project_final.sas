PROC IMPORT OUT= WORK.hatecrime 
            DATAFILE= "C:\Users\jim3082\Desktop\HateCrimes_revised.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="Data$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;


proc print; title 'Hatecrime';


/*Data exploration*/

/*Data exploration*/

/*detect outliers,influential data,leverage point,multicollinearity, normality,linearity*/
proc reg data=Hatecrime;
model hatecrime = income	unemploy	metro	highschool	noncitizen	whitepov	gini	nonwhite	unaffiliated	registered	incarcerate	abolish	officers	uninsured	sunny
 / r influence vif p;
          plot npp.*r.;  output out=goodness r=Res;
 /*check correlation between variables*/
proc princomp data=Hatecrime(drop=hatecrime); 

/*test heteroscedasticity*/
proc rank data=Hatecrime out=ranking;
  var registered; ranks regiRank;
proc print;
data small large; set ranking;
  if regiRank < 22 then output small;
  if regiRank > 31 then output large;
proc reg data=small; model hatecrime = income	unemploy	metro	highschool	noncitizen	whitepov	gini	nonwhite	unaffiliated	registered	incarcerate	abolish	officers	uninsured	sunny;
proc reg data=large; model hatecrime = income	unemploy	metro	highschool	noncitizen	whitepov	gini	nonwhite	unaffiliated	registered	incarcerate	abolish	officers	uninsured	sunny;


/*check whether we need to drop oberservation 9 because or not, ob9 is an influentail outlier and  leverage point to see the F-test value*/

data subset;
set work.Hatecrime;
if state= 'District of Columbia' then delete;
run;
proc print; title 'Omiting ob9';
proc reg data=subset;
model hatecrime = income	unemploy	metro	highschool	noncitizen	whitepov	gini	nonwhite	unaffiliated	registered	incarcerate	abolish	officers	uninsured	sunny
;

/*keep ob9, because delete observation 9 makes F-test p-value bigger*/




/*variable selection*/
ods pdf file="C:\Users\jim3082\Desktop\HateCrimes6.pdf";



/*devide dataset into trianing and testing samples using random numbers - 70/30 */
data traindata testdata;
set Work.Hatecrime;
if ranuni(7)<=.7 then output traindata;else output testdata;
run;

/*stepwise selection*/
proc glmselect data=traindata testdata=testdata plots=(CoefficientPanel(unpack) asePlot Criteria);
class abolish;
model hatecrime = income	unemploy	metro	highschool	noncitizen	whitepov	gini	nonwhite	unaffiliated	registered	incarcerate	abolish	officers	uninsured	sunny
  / selection=stepwise(select=sl choose=aic);
 

/*forward selection*/
proc glmselect data=traindata testdata=testdata plots=(CoefficientPanel(unpack) asePlot Criteria);
class abolish;
model hatecrime = income	unemploy	metro	highschool	noncitizen	whitepov	gini	nonwhite	unaffiliated	registered	incarcerate	abolish	officers	uninsured	sunny
  / selection=forward(select=sl choose=aic);


/*backward selection*/
proc glmselect data=traindata testdata=testdata plots=(CoefficientPanel(unpack) asePlot Criteria);
class abolish;
model hatecrime = income	unemploy	metro	highschool	noncitizen	whitepov	gini	nonwhite	unaffiliated	registered	incarcerate	abolish	officers	uninsured	sunny
  / selection=backward(select=sl choose=aic);




   /*lasso selection*/
proc glmselect data=Hatecrime plot=all seed=123456; partition fraction(validate=0.3);
model hatecrime = income	unemploy	metro	highschool	noncitizen	whitepov	gini	nonwhite	unaffiliated	registered	incarcerate	abolish	officers	uninsured	sunny
   / selection=lasso(stop=none choose=validate);


/*Predict Hwaii hatecrime and Test assumptions of the backward selection regression model (the best one based on ASE(test)and ad R square) */

ods pdf file="C:\Users\jim3082\Desktop\HateCrimes2.pdf";
/*Test assumptions of the backward selection regression model (the best one based on ASE(test)and ad R square) */
proc reg data=traindata alpha=0.1;
model hatecrime = highschool	gini	
 / r influence vif p clb clm cli ;
          plot npp.*r.;  output out=goodness r=Res;
		  OUTPUT OUT=WANTED P=YPREDICT LCL=YLOWERIN LCLM=YLOWERMEAN UCL=YUPPERIN UCLM=YUPPERMEAN;

/*test last assumption heteroscedasticity*/
proc rank data=traindata out=ranking;
  var highschool; ranks hcRank;
proc print;
data small large; set ranking;
  if hcRank < 14 then output small;
  if hcRank > 18 then output large;
proc reg data=small; model hatecrime = 	highschool		gini		;
proc reg data=large; model hatecrime = highschool		gini		;

proc reg ; 
model  = Long Lat Temp Speed / clb clm cli r influence;
OUTPUT OUT=WANTED P=YPREDICT LCL=YLOWERIN LCLM=YLOWERMEAN UCL=YUPPERIN UCLM=YUPPERMEAN;
ods pdf close;
 

proc print;
run;
quit;








