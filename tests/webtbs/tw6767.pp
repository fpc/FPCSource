{ %OPT=-gl -gh }
program t3;

uses
{$ifdef unix}
 cthreads,
{$endif}
 Sysutils,uw6767;


var
 CheckThread : TCheckConnThread;
begin
  HaltOnNotReleased := true;
  CheckThread := TCheckConnThread.Create(false);
  CheckThread.Terminate;
  { not really clean, but waitfor is not possible since the thread may }
  { already have freed itself                                          }
  sleep(500);
end.
