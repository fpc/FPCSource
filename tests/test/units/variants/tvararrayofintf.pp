
// Tests storing interfaces in VariantArray
{$ifdef fpc}{$mode objfpc}{$h+}{$endif}
{$apptype console}

uses sysutils, variants;

type
  ITag = interface(IInterface)['{26EBC417-D394-4561-906A-202F32A919EA}']
    function GetTag: Integer;
  end;

  tmyobj=class(TInterfacedObject,ITag)
  private
    FTag: Integer;
    function GetTag: Integer;
  public
    constructor Create(aTag: Integer);
    destructor Destroy; override;
  end;

var
  FreeCount: Integer;

constructor tmyobj.create(aTag: Integer);
begin
  inherited Create;
  FTag:=aTag;
end;

destructor tmyobj.destroy;
begin
  writeln('Destroy: ', FTag);
  Inc(FreeCount);
  inherited;
end;

function tmyobj.gettag: integer;
begin
  result:=FTag;
end;

var
  values: Variant;
  i: Integer;

begin
  Values := VarArrayCreate([0, 4], varUnknown);
  for i := 0 to 4 do
    Values[i] := tmyobj.Create(i) as IInterface;
  for i := 0 to 4 do
  begin
    if (IInterface(Values[i]) as ITag).GetTag <> i then
      Halt(i);
  end;
  FreeCount := 0;
  Values := 0;
  writeln(FreeCount);
  // check for correct number of destroyed objects won't work because one of them
  // is released after this point.
end.