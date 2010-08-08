{ %TARGET=win32,win64}

program tw15530;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  SysUtils, ActiveX, ComObj;

type
  IIE = dispinterface
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure Quit; dispid 300;
    property Visible: wordbool dispid 402;
  end;
var
  II: IIE;
begin
  OleInitialize(nil);

  II := CreateOleObject('InternetExplorer.Application') as IIE;

  if II = nil then
    halt(1);

  if not II.Visible then // test dispid property getter
    II.Visible := True;  // test dispid property setter

  II.Quit; // test dipid method call

  OleUninitialize;
end.
