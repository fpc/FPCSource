{ %OPT=-gh }
// Creating a suspended TThread and then destroying it should not cause memory leaks.
// Also, Execute() method should not be called.

{$ifdef fpc}{$mode objfpc}{$endif}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils, Classes;

type
  TMyThread = class(TThread)
    procedure Execute; override;
  end;

var
  t: TThread;
  Flag: Boolean;
  
procedure TMyThread.Execute;
begin
  flag := True;
end;

begin
  Flag := False;
  t := TMyThread.Create(True);
  t.Free;
  Halt(ord(Flag));
end.
