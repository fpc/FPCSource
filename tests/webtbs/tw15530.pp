{ %TARGET=win32}

program tw15530;

{$mode objfpc}

uses
  ComObj;

type
  IIE = dispinterface
    ['{0002DF05-0000-0000-C000-000000000046}']
    property Visible: wordbool dispid 402;
  end;

var
  II: IIE;
begin
  II := CreateOleObject('InternetExplorer.Application') as IIE;
  if II <> nil then
    ;
end.