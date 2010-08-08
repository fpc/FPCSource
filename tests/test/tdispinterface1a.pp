{ %TARGET=win32,win64,wince}
{ %FAIL}
{ %NORUN }

program tdispinterface1a;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  SysUtils, ActiveX, ComObj;

type
  IIE = dispinterface
    ['{0002DF05-0000-0000-C000-000000000046}']
    property Visible: wordbool readonly dispid 402;
  end;

var
  II: IIE;
begin
  II := CreateOleObject('InternetExplorer.Application') as IIE;
  if not II.Visible then
    II.Visible := True;  // must fail because property is readonly
end.
