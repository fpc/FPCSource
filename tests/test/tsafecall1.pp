{ %TARGET=win32,win64,wince}
{$ifdef fpc}
{$mode objfpc}
{$endif}
uses
  SysUtils;
type
  TTest = class
  public
    procedure SomeError; safecall;
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

var
  Handled: Boolean;

procedure TTest.SomeError; safecall;
begin
  raise Exception.Create('SomeException');
end;

function TTest.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;
begin
  Handled := True;
  Result := 0;
end;

begin
  Handled := False;
  TTest.Create.SomeError;
  if not Handled then
    halt(1);
end.