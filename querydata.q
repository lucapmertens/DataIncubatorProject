/ set working directory
\cd /q/scripts

// set parameters
.par.npoints:181;
.par.mesh:1;
.par.timegrid:(til .par.npoints)*.par.mesh*1%24*60*60; 
.par.daterange:"date>=2013.12.31,date<=2015.01.17";

///////////////////////////////////////////////////////////////select symbols
/ get CL contracts (25 selected)
h:hopen `:atlas:5012

h"flip enlist tables[]"
tickers:h"select distinct sym from childords where ",.par.daterange;
tickers:tickers[where (string tickers`sym) like\: "CL*"];
/debug
count tickers
flip tickers
tickers:tickers _6

symlist:"(`",(",`" sv string tickers`sym),")";
symlist

///////////////////////////////////////////////////////////////create orders table 
h"flip enlist cols childords"
cldords:h"select ordid,date,sym,time,size,limitpx,side,pricetype,algoparams from childords where sym in ",symlist,",",.par.daterange;
count cldords  

cldords[`ordid]:`$cldords.ordid; 
cldords:update ord_id:i+1 from cldords;
/`:orders.csv set cldords 

///////////////////////////////////////////////////////////////create pricedata 
/ set working directory
cldords: get `:orders.csv 
.par.Nord: count cldords;

/ord_id:raze(.par.npoints#/: cldords.ord_id);
/`:ord_id.csv set ord_id
/ord_id: get `:ord_id.csv

/tptime:raze cldords.time +\:.par.timegrid;
/`:tptime.csv set tptime
/tptime: get `:tptime.csv

/k:(.par.Nord*.par.npoints)# 1+til .par.npoints
/`:k.csv set k
/k: get `:k.csv

/`:db/pricedata/ set ([] ord_id:ord_id; k:k; tptime:tptime) 

/ add the price column
/n:count select from pricedata
/@[`:/db/pricedata;`p;:;p]
/@[`:/db/pricedata;`.d;,;`p]

\cd /q/scripts

\l /db/pricedata
count select from pricedata
cols pricedata


ttable:select distinct date,sym from cldords;
count ttable

/ Debug
/d:ttable[0]`date
/s:ttable[0]`sym
/d,s

h:hopen `:atlas:10012;

\t {[x]
    d:x`date;
    s:x`sym;
    /quotes:([]tptime:();p:())
    quotes:h"select p:last 0.5*bid+ask by tptime from bestquote where date=",(string d),",sym=`",(string s);
    
    flag_r:where ((cldords.sym)=s) and (cldords.date)=d;
    flag_p:where ((cldords.sym)=s) and ((cldords.date)=d) and (cldords.k)=0;
    cldords[flag_r;`p]: "f"$ deltas aj[`tptime;select tptime from cldords where date=d,sym=s;quotes]`p;
    cldords[flag_p;`p]: "f"$ aj[`tptime;select tptime from cldords where date=d,sym=s,k=0;quotes]`p;
    .Q.gc[];
    }each ttable;

/`:CLdata10.csv set cldords


///////////////////////////////////////////////////////////////Relaod data
\cd /q/scripts

cldords: get `:CLdata1.csv
pricedata: select ord_id,k,p from cldords; 
`:pricedata1.csv set pricedata



