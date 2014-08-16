unit tw25210;

{$mode objfpc}

interface

uses
  Classes;

type
  TStringsHelper = class helper for TStrings
    procedure Test;
  end;

implementation

procedure TStringsHelper.Test;
begin
  Writeln('Foobar');
end;

var
  s: TStrings;
initialization
  s := TStringList.Create;
  s.Test;
  s.Free;
end.
