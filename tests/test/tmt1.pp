{ %version=1.1 }

{$mode objfpc}
{$threading on}

uses sysutils;

const
   threadcount = 100;
   stringlen = 10000;
var
   finished : longint;
threadvar
   thri : longint;

function f(p : pointer) : longint;
  var
     s : ansistring;
  begin
     writeln('thread ',longint(p),' started');
     thri:=0;
     while (thri<stringlen) do
      begin
        s:=s+'1';
        inc(thri);
      end;
     writeln('thread ',longint(p),' finished');
     InterLockedIncrement(finished);
     f:=0;
  end;

var
   i : longint;
begin
   finished:=0;

   for i:=1 to threadcount do
     BeginThread({$ifdef fpc}@{$endif}f,pointer(i));

   while finished<threadcount do
     ;
   writeln(finished);
end.
