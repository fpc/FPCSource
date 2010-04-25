{$mode objfpc}{$H+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  Classes;

begin
  writeln('GetThreadID=', ptrint(GetThreadID));
  writeln('GetCurrentThreadID=', ptrint(GetCurrentThreadId));
  writeln('MainThreadID=', ptrint(MainThreadID));
  if (GetThreadID<>GetCurrentThreadID) or
     (MainThreadID<>GetThreadID) then
    halt(1);
end.


