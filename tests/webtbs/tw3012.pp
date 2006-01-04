Type Char2=Array[1..2] of char;

var C1,C2:Char2;
    st:string;

Procedure WriteLength(s:string; shouldbe: longint);
begin
  WriteLn(s+' ',Length(s));
  if length(s) <> shouldbe then
    halt(1);
end;

begin
  C1:=#0#65;
  C2:=#66#0;
  st:=C1+C2;
  WriteLength(st,4);	{BP:4; FP:1}
  WriteLength(C1,2);	{BP:2; FP:0}
  WriteLength(C2,2);	{BP:2; FP:1}
  WriteLength(C1+C2,4);	{BP:4; FP:1}
  WriteLength(C2+C1,4);	{BP:4; FP:1}
end.
