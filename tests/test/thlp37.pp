{ published properties/methods are available in the helper's RTTI }
program thlp37;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

uses
  typinfo;

type
  TFoo = class

  end;

{$M+}
  TFooHelper = class helper for TFoo
  private
    function GetTest: Integer;
  published
    property Test: Integer read GetTest;
  end;
{$M-}

function TFooHelper.GetTest: Integer;
begin
  Result := 2;
end;

var
  ti: PTypeInfo;
  td: PTypeData;
begin
  ti := TypeInfo(TFooHelper);
  if ti = Nil then begin
    Writeln('TypeInfo is Nil');
    Halt(1);
  end;
  if ti^.Kind <> tkHelper then begin
    Writeln('Type kind is not a helper');
    Halt(2);
  end;
  td := GetTypeData(ti);
  if td = Nil then begin
    Writeln('TypeData is Nil');
    Halt(3);
  end;
  Writeln('Property count: ', td^.PropCount);
  if td^.PropCount <> 1 then
    Halt(4);
  Writeln('ok');
end.
