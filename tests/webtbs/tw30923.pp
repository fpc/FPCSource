program project1;

{$mode objfpc}{$H+}

Type
  QStringListH = class(TObject) end;

function QStringList_size(handle: QStringListH): Integer; cdecl;
begin
  Result := 1;
end;

procedure QStringList_at(handle: QStringListH; retval: PWideString; i: Integer); cdecl;
begin

end;

procedure Test;
Var
  AQStringListH : QStringListH;
  AWideString : WideString;
  I : Integer;
begin
  For I := 0 To QStringList_size(AQStringListH) - 1  do
    QStringList_at(AQStringListH, @AWideString, i);
end;

Var
  I : Integer;
begin
  Test;
  I := 0;
end.

