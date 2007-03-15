{$ifdef fpc}{$mode delphi}{$endif}

uses
  Classes;

type
  TTestProc = function(Index: Integer): String;

  TMyObject = class(TObject)
    procedure Test(Proc: TTestProc); overload;
    procedure Test(Vals: TStrings); overload;
  end;

function GetString(Index: Integer): String;
begin
  Result := '';
end;

procedure TMyObject.Test(Proc: TTestProc);
begin
end;

procedure TMyObject.Test(Vals: TStrings);
begin
end;

var
  O: TMyObject;
  P: TTestProc;
begin
  O.Test(P);
  O.Test(GetString);
end.

