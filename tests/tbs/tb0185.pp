{ Old file: tbs0218.pp }
{ rounding errors with write/str (the bugs is fixed,    OK 0.99.11 (FK) }

Program Wrong_Output;
{}
Var r,rr,error:Extended;
    s:String;
    code : word;
{}
Begin
  Writeln('Size of Extended type (r)=',SizeOf(r),' bytes');
  r:=0.000058184639;
  Writeln('r=',r);
  Writeln('r=',r:16:13);
  Writeln('r=',r:15:12);
  Writeln('r=',r:14:11);
  Writeln('r=',r:13:10);
  Writeln('r=',r:12:9);
  Writeln('r=',r:11:8);
  Writeln('r=',r:10:7);
  Writeln('r=',r:9:6);
  Writeln('r=',r:8:5);
  Writeln('r=',r:7:4);
  Str(r,s);
  Writeln('r=',s,' (as string)');
  str(r,s);
  val(s,rr,code);
  { calculate maximum possible precision }
  if sizeof(extended) = 12 then
    error := exp(17*ln(10))
  else if sizeof(extended) = 10 then
    error := exp(17*ln(10))
  else if sizeof(extended) = 8 then
    error := exp(14*ln(10))
  else if sizeof(extended) = 4 then
    { the net may have to be 9 instead of 8, not sure }
    error := exp(8*ln(10))
  else
    begin
      Writeln('unknown extended type size!');
      halt(1)
    end;
  if abs(r-rr) > error then
    begin
      Writeln('r=',r);
      Writeln('is different from rr=',rr);
      halt(1);
    end;
End.
