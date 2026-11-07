{
    Out-of-process (LocalServer32) COM server with a type library, for the per-user registration test.
    Switches: /regserver, /unregserver, /regserverperuser, /unregserverperuser, /embedding.
}
program comperuserexe;

{$mode objfpc}{$H+}

{$R comperuser.rc}

uses
  Windows, ComServ, comperuserexeobj;

var
  lMsg: TMsg;

begin
  if ComServer.StartMode=smAutomation then
    while GetMessage(lMsg,0,0,0) do
      begin
      TranslateMessage(lMsg);
      DispatchMessage(lMsg);
      end;
end.
