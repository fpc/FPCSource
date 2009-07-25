{$mode objfpc}
program testm;

uses
  Strings;

Type
  TMyObject = Class(TObject)
  public
    Procedure MyMessage(Var Msg); message 'somestring';
  end;

  TMyMessage = packed record
    MsgStr : ShortString;
    Data : Pointer;
  end;

Var
  MyExitCode : Longint;

Procedure TMyObject.MyMessage(Var Msg);

begin
  Writeln('Got Message');
  MyExitCode:=0;
end;

var
  msg : TMyMessage;
  M : TMyObject;
  s : shortstring;
begin
  MyExitCode:=1;
  M:=TMyObject.Create;
  try
    msg.MsgStr:='somestring';
    M.DispatchStr(Msg);
  finally
    M.Free;
  end;
  halt(MyExitCode);
end.
