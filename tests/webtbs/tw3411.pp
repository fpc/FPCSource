{ %opt=-ghl }

{ Source provided for Free Pascal Bug Report 3411 }
{ Submitted by "Dean Zobec" on  2004-11-28 }
{ e-mail: dezobec@tin.it }
{$mode objfpc}{$H+}
{$interfaces com}
program interfaceleak;
// compile with option -ghl
type

  IMoney = interface
  ['{AAD734A1-6F35-D911-9C73-C6AC7996EDD0}']
  function Add(aMoney: IMoney): IMoney;
  end;

  TMoney = class(TInterfacedObject, IMoney)
  private
    FAmount: Int64;
    FCurrencyUnit: string;
  public
    function Add(aMoney: IMoney): IMoney;
    constructor Create(aAmount: int64;
      aUnit: string);
    destructor Destroy; override;
  end;

  function TMoney.Add(aMoney: IMoney): IMoney;
  begin
    Result := nil;
  end;

  constructor TMoney.Create(aAmount: int64;
    aUnit: string);
  begin
    Inherited Create;
    FAmount := aAmount;
    FCurrencyUnit := aUnit;
  end;

  destructor TMoney.Destroy;
  begin
    FCurrencyUnit := '';
    writeln('Destroyed');
    inherited Destroy;
  end;

procedure TestLeak;
var
  a: IMoney;
begin
  a := TMoney.Create(12, 'EUR');
end;

begin
   HaltOnNotReleased := true;
  TestLeak;
end.
