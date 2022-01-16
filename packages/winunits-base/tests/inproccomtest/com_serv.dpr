program com_serv;
// Comtest from Anton K. mantis #35013
uses
  windows,
  messages,
  sysutils,
  com_serv_TLB in 'com_serv_TLB.pas',
  com_impl in 'com_impl.pas' {TestApp: CoClass};

{$R *.TLB}

var msg:TMsg;
   res:integer;
   fTerminate:boolean;
begin
  AllocConsole;

  fTerminate:=false;

  repeat
  if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
  begin
    if Msg.Message <> WM_QUIT then
    begin
        TranslateMessage(Msg);

        writeln(format('msg.message=%.08x msg.wparam=%.08x msg.lparam=%.08x',[msg.message,msg.wparam,msg.lparam]));
        res:=DispatchMessage(Msg);
        writeln(format('result=%.08x',[res]));
    end
    else
      FTerminate := True;
  end;
  until fterminate;


  (*Application.Run;
  repeat
    Application.ProcessMessages;
  until Application.Terminated;*)

end.
