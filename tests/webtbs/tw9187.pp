var
 wstr1: widestring;

procedure testproc(const avalue: widestring);
begin
 wstr1:= avalue;
end;

var
  i: longint;
  w2: widestring;

begin
 setlength(w2, 200000);
 for i:=1 to length(w2) do
   w2[i]:=Chr(i mod $60 + $20);
 wstr1:=w2;
 testproc(wstr1);
 if wstr1<>w2 then begin
   writeln('Test failed!');
   Halt(1);
 end;
 writeln('Test OK.');
end.
