program testbug;
{$mode delphi}{$H+}
type
  PLstGrandTyp = ^TLstGrandForwardTyp;

  TLstGrandForwardTyp = packed record
    early, Next: PLstGrandTyp;
  end;


  PLstTyp01 = ^TLstForwardTyp01;
  TLstForwardTyp01 = packed record
    early, Next: PLstTyp01;
    Value: byte;
  end;

  PLstTyp02 = ^TLstForwardTyp02;
  TLstForwardTyp02 = packed record
    early, Next: PLstTyp02;
    Value: string[255];
  end;

  TLstEnumerator<T> = record
  private
    lst, lst_save: T;
  public
    constructor Create(const Value: T);
    function GetEnumerator: TLstEnumerator<T>;
    function MoveNext: boolean;
    property Current: T read lst;
  end;

constructor TLstEnumerator<T>.Create(const Value: T);
begin
  lst := Value;
  lst_save := nil;
end;

function TLstEnumerator<T>.GetEnumerator: TLstEnumerator<T>;
begin
  Result := Self;
end;

function TLstEnumerator<T>.MoveNext: boolean;
begin
  if lst <> nil then
  begin
    // At this point it is simply not known that lst is a type that has
    // a field called next. So the compiler throws an illegal qualifier.
    // The compiler is correct. This is not a bug!
    lst:=lst^.next;
    Result := True;
  end else Result := False;
end;


var
  i01:PLstTyp01 = nil;
  lst01: PLstTyp01 = nil;
  i02:PlstTyp02 = nil;
  lst02: PLstTyp02 = nil;
  i03:PlstGrandTyp = nil;
  lst03: PLstGrandTyp = nil;
  en01: TLstEnumerator<PLstTyp01>;
  en02: TLstEnumerator<PLstTyp02>;
  en03: TLstEnumerator<PLstGrandTyp>;
begin

  for i01 in en01.Create(lst01) do
  begin
    i01^.Value := 10;
  end;

  for i02 in en02.Create(lst02) do
  begin
    i02^.Value := 'ten';
  end;

  for i03 in en03.Create(lst03) do
  begin
  end;

end.

