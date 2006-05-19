{ %OPT=-gl -gh }
program t3;

uses
 Sysutils,uw6767;


var
 CheckThread : TCheckConnThread;
begin
  CheckThread := TCheckConnThread.Create(false);
  CheckThread.Terminate;
  CheckThread.Waitfor;
end.
