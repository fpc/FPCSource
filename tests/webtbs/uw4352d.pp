{$mode delphi}
unit uw4352d;

interface

uses
  uw4352b, SysUtils;

type
  TIdSysWin32 = class(TIdSysNativeVCL)
  public
    class function Win32MajorVersion : Integer;
  end;


implementation

uses  uw4352e; // commenting this helps.

class function TIdSysWin32.Win32MajorVersion: Integer;
begin
  Result := 100;
end;

end.
