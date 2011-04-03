{$mode objfpc}

unit gvector;

interface

type
  generic TVector<T>=class
  private
  type
    PT=^ T;
    TArr=array of T;
  var
    FCapacity:SizeUInt;
    FDataSize:SizeUInt;
    FData:TArr;

    procedure SetValue(Position:SizeUInt; Value:T);inline;
    function GetValue(Position:SizeUInt):T;inline;
    function GetMutable(Position:SizeUInt):PT;inline;
    procedure IncreaseCapacity;inline;
  public
    constructor Create;
    function Size:SizeUInt;inline;
    procedure PushBack(Value:T);inline;
    procedure PopBack;inline;
    function IsEmpty:boolean;inline;
    procedure Insert(Position:SizeUInt; Value:T);inline;
    procedure Erase(Position:SizeUInt);inline;
    procedure Clear;inline;
    function Front:T;inline;
    function Back:T;inline;
    procedure Reserve(Num:SizeUInt);inline;
    procedure Resize(Num:SizeUInt);inline;

    property Items[i : SizeUInt]: T read getValue write setValue; default;
    property Mutable[i : SizeUInt]: PT read getMutable;
end;

implementation

constructor TVector.Create();
begin
  FCapacity:=0;
  FDataSize:=0;
end;

procedure TVector.SetValue(Position:SizeUInt; Value:T);inline;
begin
  Assert(position < size, 'Vector position out of range');
  FData[Position]:=Value;
end;

function TVector.GetValue(Position:SizeUInt):T;inline;
begin
  Assert(position < size, 'Vector position out of range');
  GetValue:=FData[Position];
end;

function TVector.GetMutable(Position:SizeUInt):PT;inline;
begin
  Assert(position < size, 'Vector position out of range');
  GetMutable:=@FData[Position];
end;

function TVector.Front():T;inline;
begin
  Assert(size > 0, 'Accessing element of empty vector');
  Front:=FData[0];
end;

function TVector.Back():T;inline;
begin
  Assert(size > 0, 'Accessing element of empty vector');
  Back:=FData[FDataSize-1];
end;

function TVector.Size():SizeUInt;inline;
begin
  Size:=FDataSize;
end;

function TVector.IsEmpty():boolean;inline;
begin
  if Size()=0 then 
    IsEmpty:=true
  else 
    IsEmpty:=false;
end;

procedure TVector.PushBack(Value:T);inline;
begin
  if FDataSize=FCapacity then
    IncreaseCapacity;
  FData[FDataSize]:=Value;
  inc(FDataSize);
end;

procedure TVector.IncreaseCapacity();inline;
begin
  if FCapacity=0 then
    FCapacity:=1
  else
    FCapacity:=FCapacity*2;
  SetLength(FData, FCapacity);
end;

procedure TVector.PopBack();inline;
begin
  if FDataSize>0 then
    FDataSize:=FDataSize-1;
end;

procedure TVector.Insert(Position:SizeUInt; Value: T);inline;
var i:SizeUInt;
begin
  pushBack(Value);
  for i:=Size-1 downto Position+1 do 
  begin
    FData[i]:=FData[i-1];
  end;
  FData[Position]:=Value;
end;

procedure TVector.Erase(Position:SizeUInt);inline;
var i:SizeUInt;
begin
  if Position <= Size then 
  begin
    for i:=Position to Size-2 do
    begin
      FData[i]:=FData[i+1];
    end;
    popBack();
  end;
end;

procedure TVector.Clear;inline;
begin
  FDataSize:=0;
end;

procedure TVector.Reserve(Num:SizeUInt);inline;
begin
  if(Num < FCapacity) then 
    exit
  else if(Num <= 2*FCapacity) then 
    IncreaseCapacity
  else begin 
    SetLength(FData, Num);
    FCapacity:=Num;
  end;
end;

procedure TVector.Resize(Num:SizeUInt);inline;
begin
  Reserve(Num);
  FDataSize:=Num;
end;

end.
