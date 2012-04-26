Program tdynarrec;

{$mode objfpc}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
  tdynrec = record
    s: string[10];
  end;


procedure error(l: longint);
begin
  JLSystem.fout.print('error: ');
  JLSystem.fout.println(l);
  raise jlexception.create('fatal');
end;

var
  r1,r2: array of tdynrec;
  rr: tdynrec;
begin
  setlength(r1,5);
  r2:=r1;
  rr.s:='abc';
  r1[0]:=rr;
  if r2[0].s<>'abc' then
    error(0);
  rr.s:='def';
  if r1[0].s<>'abc' then
    error(1);
  r1[1]:=rr;
  if r1[0].s<>'abc' then
    error(2);
  setlength(r2,6);
  if r1[0].s<>'abc' then
    error(3);
  if r2[0].s<>'abc' then
    error(4);
  if r2[1].s<>'def' then
    error(3);
  rr.s:='ghi';
  r1[0]:=rr;
  if r2[0].s<>'abc' then
    error(5);
end.
