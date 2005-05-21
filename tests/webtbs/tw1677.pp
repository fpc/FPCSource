{ Source provided for Free Pascal Bug Report 1677 }
{ Submitted by "Anders Lindeberg" on  2001-11-10 }
{ e-mail: anders.lindeberg@telia.com }
program test;
type trec = record i:integer; s:ansistring end;

procedure RefCount(const s : ansistring;var rc:sizeint;expect:sizeint);
type
        PLongint = ^Longint;
var
        P : psizeint;
begin
        P := psizeint(s);
        rc:=0;
        if (p = nil)
        then writeln('Nil string.')
        else
{$ifdef  fpc}
  {$if defined(ver1_0) or defined(ver1_9_4)}
         rc:=(p-1)^;
  {$else}
         rc:=psizeint(pchar(p)-sizeof(sizeint)*2)^;
  {$endif}
{$else}
         rc:=plongint(pchar(p)-8)^;
{$endif}
  if expect<>-1 then
    begin
      writeln('Ref count is ',rc,' expected ',expect);
      if rc<>expect then
        halt(1);
    end
  else
    writeln('Ref count is ',rc);
end;


procedure p1(const r:trec);
  begin
  end;

procedure p2(r:trec);
  begin
  end;

procedure p3(const a:ansistring);
  begin
  end;

procedure p4(a:ansistring);
  begin
  end;

var r:trec; s:ansistring;
    hrc,rc : sizeint;
begin
  s:=chr(ord('A')+random(26));
  r.s:=s;
  writeln('init');
  RefCount(s,rc,-1);
  writeln('p1()');
  p1(r);
  RefCount(s,hrc,rc);
  writeln('p2()');
  p2(r);
  RefCount(s,hrc,rc);
  writeln('ok');
end.
