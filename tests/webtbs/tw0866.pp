{$mode objfpc}
Type
     ts = set of (tse);
     ts2 = set of (t1,t2);
     enum3 = (tm1:=-1,t0,tp1);
     ts3 = set of t0 .. tp1;
 var
    f:ts;
    f2 : ts2;
    f3 : ts3;

begin
 f2:=f2+[t2];
 f2:=f2+[t1];
 f:=f+[tse]; // compiler says that set elements are not compatible
 { f3:=[tm1];}
end.
