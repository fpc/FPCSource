{$mode objfpc}
program testm;

uses
  Strings;

Type
  TMyObject = Class(TObject)
  public
    Procedure MyMessage(Var Msg); message 'somestring';
    Procedure MyMessage2(Var Msg); message 'otherstring';

    procedure Message2(var msg); message 1;
    procedure Message3(var msg); message 10000;
  end;

  TMyMessage = record
    MsgStr : ShortString;
    Data : pointer;
  end;
  
  TMyIntMessage = record
    Id: integer;
    Data : pointer;
  end;

Var
  MyExitCode : Longint;

Procedure TMyObject.MyMessage(Var Msg);

begin
  Writeln('Got Message');
  dec(MyExitCode);
end;

Procedure TMyObject.MyMessage2(Var Msg);

begin
  Writeln('Got Message');
  dec(MyExitCode);
end;

procedure TMyObject.Message2(var msg);
begin
  Writeln('Got Message 2');
  dec(MyExitCode)
end;

procedure TMyObject.Message3(var msg);
begin
  Writeln('Got message 3');
  dec(MyExitCode);
end;

var
  msg : TMyMessage;
  msgi : TMyIntMessage;
  M : TMyObject;
  s : shortstring;
begin
  MyExitCode:=4;
  M:=TMyObject.Create;
  try
    msg.MsgStr:='somestring';
    M.DispatchStr(Msg);

    msg.MsgStr:='otherstring';
    M.DispatchStr(msg);

    msgi.id := 10000;
    M.Dispatch(msgi);

    msgi.id := 1;
    M.Dispatch(msgi);
  finally
    M.Free;
  end;
  halt(MyExitCode);
end.
