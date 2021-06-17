/*
Extended Gini : Consider a game where you are presented scores of two samples and you are to pick one
                of those two, just based on the score.So, to reduce the default rate you have to pick 
                the sample with the highest score. 
                The pay-off from the game depends on whether the sample is actually default or not.
                For e.g. you get 1 point whenever you choose a good account and -1 when you choose a 
                bad one. Extended GINI is the normalized expected pay-off under this game. It is equivalent
                to the usual GINI metric (2*AUC-1) in binary case.   
                The advantage of extended gini is that we can generalize it to non-binary cases easily. 
                For e.g: You could define a pay-off structure based on max-delinquency. It is handy when
                one wants to assess predictive power of a variable/score in a low-default portfolio. You
                get more ?accurate? estimates as you are making more efficient use of the data available. 

*/
%macro egini(
     data =         /*dataset containing the input variables                                  */
    ,x    =         /*the variable based on which the choice has to be made : e.g. score      */
    ,y    =         /*the variable,the payoff matrix is based on            : e.g. bad flag   */
    ,l    = 0 1     /*levels y can take : e.g. bad flag takes either 0 or 1 value             */
    ,v    = 1 0     /*pay-offs for each y-level : e.g. 1 for good, 0 for bad                  */ 
    ,w    = 1       /*observation weights*/
    ,o    = +       /*+:we always choose the sample with highest x : e.g. in case of score    */
                    /*-:we always choose the sample with lowest x : e.g. in case of PD        */
                    /*a:choose the correct order automatically                                */
    ,pfx  = ___     /*intermediate files and variables will have this prefix                  */
    ,out  = _null_  /*the dataset to which gini should be output                              */
);

option nosource nonotes;

%local ln nl pb pu; %let ln = %sysfunc(countw(&l.,%str( )));

proc sql noprint;
    create table &pfx.counts as
    select
    &x.,
    sum(&w.) as &pfx.sumwgt
    %do nl = 1 %to &ln.;
        ,sum(case when &y. = %scan(&l.,&nl.,%str( )) then &w. else 0 end) as &pfx.sumwgt_l&nl.
    %end;
    from &data.
    where &w.>0 and &x. ne . and &y. in (&l.)
    group by 1;

    select 
    sum(&pfx.sumwgt)
    %do nl = 1 %to &ln.;
        ,sum(&pfx.sumwgt_l&nl.) as &pfx.sumwgt_l&nl.
    %end;

    into
    :tsumwgt
    %do nl = 1 %to &ln.;
        ,:tsumwgt_l&nl.
    %end;
    from &pfx.counts;
quit;

*cumsums,pay-offs and egini calculations;
data &out.;
    set &pfx.counts end=&pfx.last;
    retain
    &pfx.cu_sumwgt 0         %do nl = 1 %to &ln.; &pfx.cu_sumwgt_l&nl. 0                %end;
    &pfx.cl_sumwgt &tsumwgt. %do nl = 1 %to &ln.; &pfx.cl_sumwgt_l&nl. &&tsumwgt_l&nl.. %end;
    ;
    
    &pfx.cl_sumwgt + -&pfx.sumwgt;
    %do nl = 1 %to &ln.; 
        &pfx.cl_sumwgt_l&nl. + -&pfx.sumwgt_l&nl.;
    %end;

    retain &pfx.maxegini &pfx.randegini &pfx.egini (3*0);
    keep   &pfx.maxegini &pfx.randegini &pfx.egini;

    if _N_ = 1 then do;
        %do nl1 = 1 %to &ln.;
        %do nl2 = 1 %to &ln.;
            &pfx.maxegini  + (&&tsumwgt_l&nl1../&tsumwgt.)*(&&tsumwgt_l&nl2../&tsumwgt.)*max (%scan(&v.,&nl1.,%str( )),%scan(&v.,&nl2.,%str( ))); *pay-off under perfect strategy(PP);
            &pfx.randegini + (&&tsumwgt_l&nl1../&tsumwgt.)*(&&tsumwgt_l&nl2../&tsumwgt.)*mean(%scan(&v.,&nl1.,%str( )),%scan(&v.,&nl2.,%str( ))); *pay-off under random  strategy(PR);
        %end;       
        %end;
    end;

    %local pb pu; 
    %if "&o." = "+" %then %do;
        %let pb = 1;
        %let pu = 2;
    %end; 
    %else %do;
        %let pb = 2;
        %let pu = 1;
    %end;

    %do nl1 = 1 %to &ln.;
    %do nl2 = 1 %to &ln.;
        *pay-off under score strategy(PS);
        *below; &pfx.egini+(&pfx.sumwgt_l&nl1./&tsumwgt.)*(&pfx.cu_sumwgt_l&nl2./&tsumwgt.)*%scan(&v.,&&nl&pb..,%str( ));
        *same ; &pfx.egini+(&pfx.sumwgt_l&nl1./&tsumwgt.)*(&pfx.sumwgt_l&nl2./&tsumwgt.)   *mean(%scan(&v.,&nl1.,%str( )),%scan(&v.,&nl2.,%str( )));
        *above; &pfx.egini+(&pfx.sumwgt_l&nl1./&tsumwgt.)*(&pfx.cl_sumwgt_l&nl2./&tsumwgt.)*%scan(&v.,&&nl&pu..,%str( ));
    %end;
    %end;

    
    if &pfx.last then do;
        &pfx.egini = (&pfx.egini-&pfx.randegini)/(&pfx.maxegini-&pfx.randegini); *normalization EGINI = (PS-PR)/(PP-PR);

        %if "&o." = "a" %then %do;
            &pfx.egini = abs(&pfx.egini);
        %end; 

        put "eGINI is " &pfx.egini percent10.4;
        output;
    end;

    &pfx.cu_sumwgt + +&pfx.sumwgt;
    %do nl = 1 %to &ln.; 
        &pfx.cu_sumwgt_l&nl. + +&pfx.sumwgt_l&nl.;
    %end;


run;
option source notes;
%mend egini;


*sample run 1 : simulated data;
data sim;
    do c = 1 to 10000;
        v  = rand('uniform');
        l  = 4*v*v*v -3;
        p  = 1/(1+exp(-l));
        s  = -round(100*l,1)+300;
        w  = 1; 
        dv = p>=rand('uniform');
        output;
    end;
run;    
%egini(
     data = sim
    ,x    = s
    ,y    = dv
    ,l    = 0 1
    ,v    = 1 0
    ,w    = w
    ,o    = +
    ,pfx  = ___
    ,out  = egini
);

*sample run 2 : HKMG data;
libname lib "Y:\Experiments\Extended Gini";

%egini(
     data = lib.hk_mg_data
    ,x    = score
    ,y    = max_dlq_24m
    ,l    = 'A'   'B'  'C'  'D'  'E'   
 /* ,v    =  1.00 1.00 0.00 0.00 0.00  */   /*using these values is equivalent to using the bad flag bad_24m_Cp: Ever C+ in 24M            */
                                            /*but we don't value an account that hit 30+DPD the same as one that hit 60+DPD                */
    ,v    =  1.00 0.97 0.33 0.05 0.00       /*these values could for e.g. be designed to closely resemble one minus flow rate to charge-off*/

    ,w    = 1
    ,o    = +
    ,pfx  = ___
    ,out  = egini
);
%egini(
     data = lib.hk_mg_data
    ,x    = score
    ,y    = bad_24m_Cp
    ,l    = 0 1
    ,v    = 1 0
    ,w    = 1
    ,o    = +
    ,pfx  = ___
    ,out  = egini
);

*sample run 3: IDPL Data;
libname lib "Y:\Experiments\Multi Level DV\";

option nomacrogen;

%egini(
     data = lib.idpl_data
    ,x    = v_bur_cc_sum_slim_all
    ,y    = max_dlq12m
    ,l    = 'A'   'B'  'C'  'D'  'E'   
 /* ,v    =  1.00 1.00 0.00 0.00 0.00  */   
                                            
    ,v    =  1.00 0.97 0.33 0.05 0.00       

    ,w    = 1
    ,o    = +
    ,pfx  = ___
    ,out  = egini
);
%egini(
     data = lib.idpl_data
    ,x    = v_bur_cc_sum_slim_all
    ,y    = bad90
    ,l    = 0 1
    ,v    = 1 0
    ,w    = 1
    ,o    = +
    ,pfx  = ___
    ,out  = egini
);

%egini(
     data = lib.idpl_data
    ,x    = v_bur_all_mob_avg_maxa
    ,y    = max_dlq12m
    ,l    = 'A'   'B'  'C'  'D'  'E'   
 /* ,v    =  1.00 1.00 0.00 0.00 0.00  */   
                                            
    ,v    =  1.00 0.97 0.33 0.05 0.00       

    ,w    = 1
    ,o    = +
    ,pfx  = ___
    ,out  = egini
);
%egini(
     data = lib.idpl_data
    ,x    = v_bur_all_mob_avg_maxa
    ,y    = bad90
    ,l    = 0 1
    ,v    = 1 0
    ,w    = 1
    ,o    = +
    ,pfx  = ___
    ,out  = egini
);

