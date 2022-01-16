

uses
  dos;

var
  s : string;
  s50 : string[50];
  s123 : string[123];
  pp : ppchar;
  p : pchar;
  i, k,tot : longint;
begin
  tot:=0;
  p:=nil;
  s:='Dummy test 255';
  writeln('s=',s);
  s:=p;
  i:=length(s);
  writeln('Length of s is ',i);
  if (i>0) then
    begin
      writeln('s=#',s,'#');
      for k:=1 to i do
        write(k,' #',ord(s[k]),' ');
      writeln;
      tot:=tot+i;
    end;
  s50:='Dummy test 50';
  writeln('s50=',s50);
  s50:=p;
  i:=length(s50);
  writeln('Length of s50 is ',i);
  if (i>0) then
    begin
      writeln('s50=#',s50,'#');
      for k:=1 to i do
        write(k,' #',ord(s50[k]),' ');
      writeln;
      tot:=tot+i;
    end;
  s123:='Dummy test 255';
  writeln('s123=',s123);
  s123:=p;
  i:=length(s123);
  writeln('Length of s123 is ',i);
  if (i>0) then
    begin
      writeln('s123=#',s123,'#');
      for k:=1 to i do
        write(k,' #',ord(s123[k]),' ');
      writeln;
      tot:=tot+i;
    end;
  if tot>0 then
    begin
      writeln('There are errors in the conversion of nil pchars to short strings');
      halt(tot);
    end;
end.


