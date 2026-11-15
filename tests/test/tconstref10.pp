{ a RefAttribute declared by the program is not the system unit's: [Ref] on a
  parameter is then an ordinary attribute in the parameter's RTTI and the
  parameter is passed as usual; [System.Ref] still passes by reference }
program tconstref10;

{$mode objfpc}
{$modeswitch prefixedattributes}

uses
  typinfo;

type
  RefAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  [Ref]
  TMarked = class
  end;

  TSmall = record
    X: Integer;
  end;

  {$M+}
  IThing = interface
    ['{0B7C3A52-6E1D-4F3B-9C2A-7D4E5F6A8B90}']
    procedure M(const [Ref] a: TSmall);
  end;
  {$M-}

var
  gS: TSmall;

constructor RefAttribute.Create;
begin
end;

function ByRef(const [Ref] a: TSmall): Boolean;
begin
  Result := @a = @gS;
end;

function BySystemRef(const [System.Ref] a: TSmall): Boolean;
begin
  Result := @a = @gS;
end;

var
  lMethods: PIntfMethodTable;
  lParam: PVmtMethodParam;
  lAttr: TCustomAttribute;
begin
  if ByRef(gS) then
    begin
      Writeln('the program''s [Ref] passes by reference');
      Halt(1);
    end;
  if not BySystemRef(gS) then
    begin
      Writeln('[System.Ref] does not pass by reference');
      Halt(2);
    end;
  { parameter 0 of an interface method is its Self }
  lMethods := PInterfaceData(GetTypeData(TypeInfo(IThing)))^.MethodTable;
  lParam := lMethods^.Method[0]^.Param[1];
  if not Assigned(lParam^.AttributeTable) or (lParam^.AttributeTable^.AttributeCount <> 1) then
    begin
      Writeln('the program''s [Ref] is not in the parameter''s RTTI');
      Halt(3);
    end;
  lAttr := GetAttribute(lParam^.AttributeTable, 0);
  if not (lAttr is RefAttribute) then
    begin
      Writeln('the parameter''s attribute is not the program''s RefAttribute');
      Halt(4);
    end;
  lAttr.Free;
  Writeln('ok');
end.
