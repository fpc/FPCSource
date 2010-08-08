program test2;
//The i's are used to have a better understanding of what is actually happening...
type
  tsrec = record i: single; end;
var i,new_i:longword;
    j,new_j:single;
    k:double;
    s:string;
    Err:integer;
    count:int64;
begin
   randomize;
   count:=0;
   repeat
      //As k is set to be a single-precision number, there should not be 
      //any rounding off or truncation problem...
      k:=2*random-1;
      j:=k;
      i:=longword(tsrec(j));
      Str(j,s);
      Val(s,new_j,Err);
      if (err<>0) then
        break;
      new_i:=longword(tsrec(new_j));
      count:=count+1;
   until count=50000;
   if (new_i<>i) then
     begin
       writeln;
       writeln('Error occurs');
       writeln;
       writeln(' err=',err);
       writeln(' i=',i);
       writeln(' j=',j);
       writeln(' k=',k);
       writeln;
       writeln(' s=',s);
       writeln;
       writeln('new_i=',new_i);
       writeln('new_j=',new_j);
       writeln(' k=',k);
       halt(1);
     end;
end.
