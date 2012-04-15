{ %norun }
program tclass15;

{$MODE DELPHI}

uses
  Classes;

type
  TMyClass=class
  private
  type
    TMyMemoryStream=class(TCustomMemoryStream) end;
  public
    procedure MyProc;
  end;

procedure TMyClass.MyProc;
begin
  with TMyMemoryStream.Create do
    SetPointer(nil,0)
end;

begin
end.

