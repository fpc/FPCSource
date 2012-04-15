{ %skiptarget=go32v2 }
{ %version=1.1 }

{$mode objfpc}

uses
  sysutils
{$ifdef unix}
  ,cthreads
{$endif}
  ;

const
{$ifdef cpuarm}
  {$define slowcpu}
{$endif cpuarm}

{$ifdef slowcpu}
   threadcount = 40;
   stringlen = 2000;
{$else slowcpu}
   threadcount = 100;
   stringlen = 10000;
{$endif slowcpu}

var
   finished : longint;
threadvar
   thri : longint;

function f(p : pointer) : ptrint;
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
   i : ptrint;
   started: longint;
begin
   finished:=0;
   started:=0;

   for i:=1 to threadcount do
     if BeginThread({$ifdef fpc}@{$endif}f,pointer(i)) <> tthreadid(0) then
       inc(started);

   while finished<started do
     {$ifdef wince}sleep(10){$endif};
   writeln(finished);
end.
