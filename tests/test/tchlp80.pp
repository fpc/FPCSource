program tchlp80;

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
  if ti^.Kind = tkClass then begin
    Writeln('Type kind is a class');
    Writeln(ti^.Name);
  end;
  td := GetTypeData(ti);
  if td = Nil then begin
    Writeln('TypeData is Nil');
    Halt(2);
  end;
  Writeln('Property count: ', td^.PropCount);
  Writeln('ok');
end.
