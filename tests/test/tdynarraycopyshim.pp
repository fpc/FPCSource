{ %VERSION=3.2.0 }
{ %MAXVERSION=3.2.99 }
{ copy() of a dynamic array as a 3.2 compiler lowers it, which reaches a routine of the run time
  library only that compiler uses. Run it with a 3.2 compiler as TEST_FPC, which then builds
  this run time library too; a newer compiler skips it, by its version. }
program tdynarraycopyshim;

{$mode objfpc}{$H+}

var
  a,b : array of longint;
  s,t : array of ansistring;
  bad : longint;

procedure Check(aCond : boolean; const aText : string);
  begin
    if not aCond then
      begin
        writeln('failed: ',aText);
        inc(bad);
      end;
  end;

begin
  bad:=0;
  setlength(a,4);
  a[0]:=10;
  a[1]:=11;
  a[2]:=12;
  a[3]:=13;
  b:=copy(a);
  Check(length(b)=4,'the whole array: length');
  if length(b)=4 then
    begin
      Check((b[0]=10) and (b[3]=13),'the whole array: values');
      b[0]:=99;
      Check(a[0]=10,'the whole array: a copy, not the same array');
    end;
  b:=copy(a,1,2);
  Check(length(b)=2,'a range: length');
  if length(b)=2 then
    Check((b[0]=11) and (b[1]=12),'a range: values');
  b:=copy(a,2);
  Check(length(b)=2,'from an index: length');
  if length(b)=2 then
    Check((b[0]=12) and (b[1]=13),'from an index: values');
  b:=copy(a,8,1);
  Check(length(b)=0,'a range past the end');
  b:=copy(a,-1,2);
  Check(length(b)=1,'a negative index: length');
  if length(b)=1 then
    Check(b[0]=10,'a negative index: value');
  a:=nil;
  b:=copy(a);
  Check(length(b)=0,'an empty array');

  setlength(s,2);
  s[0]:='first ';
  s[0]:=s[0]+'string';
  s[1]:='second';
  t:=copy(s);
  Check(length(t)=2,'managed elements: length');
  if length(t)=2 then
    begin
      Check((t[0]='first string') and (t[1]='second'),'managed elements: values');
      s:=nil;
      Check(t[0]='first string','managed elements: the copy holds its own references');
    end;
  t:=nil;

  if bad<>0 then
    halt(1);
  writeln('ok');
end.
