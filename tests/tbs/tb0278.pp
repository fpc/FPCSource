{ Old file: tbs0330.pp }
{  }

{$ifdef fpc}{$mode objfpc}{$endif}
uses
  Classes;

type
  TMyClass = class(TPersistent);

var
  MyVar: Integer;


type
  TMyClass2 = class(TObject)
    procedure MyProc;
  end;

  TMyOtherClass = class(TPersistent);

procedure TMyClass2.MyProc;
var
  MyImportantVar: Integer;
begin
end;

begin
end.
