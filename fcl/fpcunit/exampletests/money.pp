unit money;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TMoney = class;
  TMoneyBag = class;

  IMoney = interface
  ['{2E0160F6-312C-D911-8DE5-DD8AC3E7C6F4}']
  function add(m: IMoney): IMoney;
  function addMoney(m: TMoney): IMoney;
  function addMoneyBag(mb: TMoneyBag): IMoney;
  function isZero: boolean;
  function multiply(factor: integer): IMoney;
  function negate: IMoney;
  function subtract(m: IMoney): IMoney;
  procedure appendTo(m: TMoneyBag);
  function toString: String;
  function equals(m: IMoney): boolean;
  function Count: integer;
  function _Self: TObject;
  end;

  ISingleCurrencyMoney = interface(IMoney)
  ['{D6D97717-E52D-D911-83C4-8233402A6B6C}']
  function GetCurrencyUnit: string;
  function GetAmount: int64;
  property Amount: int64 read GetAmount;
  property CurrencyUnit: string read GetCurrencyUnit;
  end;

  TMoney = class(TInterfacedObject, IMoney, ISingleCurrencyMoney)
  private
    FAmount: int64;
    FCurrencyUnit: String;
    function GetAmount: int64;
    function GetCurrencyUnit: string;
  public
    constructor Create(aAmount: int64; aCurrencyUnit: String);
    function add(m: IMoney): IMoney;
    function addMoney(m: TMoney): IMoney;
    function addMoneyBag(mb: TMoneyBag): IMoney;
    function isZero: Boolean;
    function multiply(factor: Integer): IMoney;
    function negate: IMoney;
    function subtract(m: IMoney): IMoney;
    procedure appendTo(m: TMoneyBag);
    function toString: String;
    function equals(m: IMoney): boolean;
    property Amount: int64 read GetAmount;
    property CurrencyUnit: string read GetCurrencyUnit;
    function Count: integer;
    function _Self: TObject;
  end;

  TMoneyBag = class(TInterfacedObject, IMoney)
  private
    FMonies: TInterfaceList;
    function FindMoney(aCurrencyUnit: string): Integer;
    function Contains(m: ISingleCurrencyMoney): boolean;
  public
    constructor Create;
    class function CreateWith(m1: IMoney; m2: IMoney): IMoney;
    destructor Destroy; override;
    function Simplify: IMoney;
    function add(m: IMoney): IMoney;
    function addMoney(m: TMoney): IMoney;
    function addMoneyBag(mb: TMoneyBag): IMoney;
    procedure appendBag(aBag: TMoneyBag);
    procedure appendMoney(aMoney: ISingleCurrencyMoney);
    function isZero: boolean;
    function multiply(factor: integer): IMoney;
    function negate: IMoney;
    function subtract(m: IMoney): IMoney;
    procedure appendTo(m: TMoneyBag);
    function toString: String;
    function equals(m: IMoney): boolean;
    function Count: integer;
    function _Self: TObject;
  end;

  Operator + (c: IMoney; c1: IMoney) c2: IMoney;
  Operator - (c: IMoney; c1: IMoney) c2: IMoney;
  Operator * (c: IMoney; i: integer) c2: IMoney;

 implementation

Operator + (c: IMoney; c1: IMoney) c2: IMoney;
begin
  c2 := c.add(c1);
end;

Operator - (c: IMoney; c1: IMoney) c2: IMoney;
begin
  c2 := c.subtract(c1);
end;

Operator * (c: IMoney; i: integer) c2: IMoney;
begin
  c2 := c.multiply(i);
end;


function TMoneyBag.FindMoney(aCurrencyUnit: string): Integer;
var
  i: Integer;
begin
  for i := 0 to FMonies.Count - 1 do
    if (FMonies.items[i] as ISingleCurrencyMoney).CurrencyUnit = aCurrencyUnit then
    begin
      Result := i;
      Exit;
    end;
  result := -1;
end;

function TMoneyBag.Contains(m: ISingleCurrencyMoney): boolean;
var
  idx: integer;
begin
  idx := FindMoney(m.CurrencyUnit);
  if idx = -1 then
  begin
    Result := false;
    Exit;
  end;
  Result := ((FMonies[idx] as ISingleCurrencyMoney).Amount = m.amount);
end;

class function TMoneyBag.CreateWith(m1: IMoney; m2: IMoney): IMoney;
var
  mb: IMoney;
begin
  mb := TMoneyBag.Create;
  m1.AppendTo(TMoneyBag(mb._Self));
  m2.AppendTo(TMoneyBag(mb._Self));
  Result := TMoneyBag(mb._Self).Simplify;
end;

constructor TMoneyBag.Create;
begin
  FMonies := TInterfaceList.Create;
end;

destructor TMoneyBag.Destroy;
begin
  FMonies.Free;
  inherited Destroy;
end;

function TMoneyBag.Simplify: IMoney;
begin
  if FMonies.Count = 1 then
    Result := FMonies.items[0] as IMoney
  else
    Result := Self;
end;

function TMoneyBag.add(m: IMoney): IMoney;
begin
  Result := m.AddMoneyBag(Self);
end;

function TMoneyBag.addMoney(m: TMoney): IMoney;
begin
  Result := TMoneyBag.CreateWith(m, Self);
end;

function TMoneyBag.addMoneyBag(mb: TMoneyBag): IMoney;
begin
  Result := TMoneyBag.CreateWith(mb, Self);
end;

procedure TMoneyBag.appendBag(aBag: TMoneyBag);
var
  i: integer;
begin
  for i := 0 to aBag.FMonies.Count - 1 do
    appendMoney(aBag.FMonies.Items[i] as ISingleCurrencyMoney);
end;

procedure TMoneyBag.appendMoney(aMoney: ISingleCurrencyMoney);
var
  i: integer;
  old: IMoney;
  sum: IMoney;
begin
  if aMoney.isZero then Exit;
  i := FindMoney(aMoney.CurrencyUnit);
  if i = -1 then
  begin
    FMonies.add(aMoney);
    Exit;
  end;
  old := FMonies[i] as IMoney;
  sum := old.Add(aMoney);
  FMonies.Delete(i);
  if sum.isZero then Exit;
  FMonies.Add(sum);
end;

function TMoneyBag.isZero: boolean;
begin
  Result := FMonies.Count = 0;
end;

function TMoneyBag.multiply(factor: integer): IMoney;
var
  i: Integer;
begin
  Result := TMoneyBag.Create;
  if factor <> 0 then
    for i := 0 to FMonies.Count - 1 do
    begin
      TMoneyBag(Result._Self).appendMoney(
      (FMonies.items[i] as ISingleCurrencyMoney).Multiply(factor) as ISingleCurrencyMoney);
    end;
end;

function TMoneyBag.negate: IMoney;
var
  i: integer;
begin
  Result := TMoneyBag.Create;
  for i := 0 to FMonies.Count - 1 do
  begin
    TMoneyBag(Result._Self).appendMoney(
(FMonies.items[i] as ISingleCurrencyMoney).negate as ISingleCurrencyMoney);
  end;
end;

function TMoneyBag.subtract(m: IMoney): IMoney;
begin
  Result := Add(m.negate);
end;

procedure TMoneyBag.appendTo(m: TMoneyBag);
begin
  m.AppendBag(Self);
end;

function TMoneyBag.toString: String;
var
  i: integer;
begin
  Result := '{';
  for i := 0 to FMonies.Count - 1 do
    Result := Result + (FMonies.items[i] as IMoney).ToString;
  Result := Result + '}';
end;

function TMoneyBag.equals(m: IMoney): boolean;
var
  aMoneyBag: TMoneyBag;
  i: integer;
  ism: ISingleCurrencyMoney;
begin
  if m = nil then
  begin
    Result := false;
    Exit;
  end;
  if isZero then
  begin
    Result := m.isZero;
    Exit;
  end;
  if m._Self.ClassType = TMoneyBag then
  begin
    aMoneyBag := TMoneyBag(m._Self);
    if aMoneyBag.FMonies.count <> FMonies.Count then
    begin
      Result := false;
      Exit;
    end;
    for i := 0 to FMonies.Count - 1 do
    begin
      ism := FMonies.items[i] as ISingleCurrencyMoney;
      if not aMoneyBag.Contains(ism) then
      begin
        Result := false;
        Exit;
      end;
    end;
    Result := true;
    Exit;
  end;
  Result := false;
end;

function TMoneyBag.Count: integer;
begin
  Result := FMonies.Count;
end;

function TMoneyBag._Self: TObject;
begin
  Result := Self;
end;


{ TMoney }

function TMoney.GetCurrencyUnit: string;
begin
  Result := FCurrencyUnit;
end;

function TMoney.GetAmount: int64;
begin
  Result := FAmount;
end;

constructor TMoney.Create(aAmount: int64; aCurrencyUnit: string);
begin
  FAmount := aAmount;
  FCurrencyUnit := aCurrencyUnit;
end;

function TMoney.add(m: IMoney): IMoney;
begin
  Result := m.AddMoney(Self);
end;

function TMoney.addMoney(m: TMoney): IMoney;
begin
  if (m.CurrencyUnit = Self.CurrencyUnit) then
    Result := TMoney.Create(Self.Amount + m.Amount, Self.CurrencyUnit)
  else
    Result := TMoneyBag.CreateWith(Self, M);
end;

function TMoney.addMoneyBag(mb: TMoneyBag): IMoney;
begin
  Result := mb.AddMoney(Self);
end;

function TMoney.isZero: Boolean;
begin
  Result := Amount = 0;
end;

function TMoney.multiply(factor: Integer): IMoney;
begin
  Result := TMoney.Create(Amount * factor, CurrencyUnit);
end;

function TMoney.negate: IMoney;
begin
  Result := TMoney.Create(- Amount, CurrencyUnit);
end;

function TMoney.subtract(m: IMoney): IMoney;
begin
  Result := Add(m.negate);
end;

procedure TMoney.appendTo(m: TMoneyBag);
begin
  m.AppendMoney(Self);
end;

function TMoney.toString: String;
begin
  Result := '[' + IntToStr(FAmount) + ' '+ FCurrencyUnit + ']';
end;

function TMoney.equals(m: IMoney): boolean;
var
  ism: ISingleCurrencyMoney;
begin
  if Assigned(m) then
  begin
    if isZero then
         Result := m.isZero;
    if m._Self.ClassType = TMoney  then
    begin
      ism := m as ISingleCurrencyMoney;
       Result := (ism.Amount = Amount) and
          (ism.CurrencyUnit = CurrencyUnit)
    end
    else
      Result := false;
  end
  else
    Result := false;
end;

function TMoney.Count: integer;
begin
  Result := 1;
end;

function TMoney._Self: TObject;
begin
  Result := Self;
end;

end.
