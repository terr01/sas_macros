

data sim;
	%let n   = 20000;
	%let n1  = 0.93;
	%let n2  = 0.05;
	%let n3  = 0.02;
	%let mev = -1.0;
	%let rnp = 0.01;

	array mev_ {10} (0.2 0.1 0 -0.1 -0.2 -0.3 -0.2 -0.1 0 0.1);
	retain mev_:; drop mev_:;

	array lo_base_{3};
	retain lo_base_; drop lo_base_:;

	lo_base_1 = -3.89182; *current;
	lo_base_2 = -2.19722; *x dpd;
	lo_base_3 = -0.84730; *30+ dpd;

	call streaminit(111);



	do t = 1 to 10; 
		*random movement. so that odr(t-1)  comes as strong predictor;
		rand = max(-abs(&mev.),min(abs(&mev.),rand('normal')*&mev./2)); *the effect should not be stronger than mev. this constraint can be removed;

		do b = 1 to 3; drop b;
			lo_base_{b}= (lo_base_{b})+rand*&rnp.;
		end;


	do c = 1 to &n.;
		if c <= floor(&n1.*&n.)        then seg = 1; else 
		if c <= floor((&n1.+&n2.)*&n.) then seg = 2; else 
		 seg = 3; 

			lo_base = lo_base_{seg};
			mev = mev_{t};
			lo = lo_base + &mev.*mev;
			pd = 1/(1+exp(-lo));
			dv = rand('uniform')<=pd;

			output;

		end;


	end;

run;

proc sql;
	create table summary as select 
	t,seg,mean(mev) as mev, mean(dv) as odr
	from sim group by 1,2 order by 2,1;
quit;

proc sgplot data=summary;
	series x = t y = mev;
	series x = t y = odr /y2axis;
	by seg;
run;
