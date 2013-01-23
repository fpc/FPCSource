program terecs15;

{$mode delphi}

type

  { TRec }

  TRec = record
  private
    X: Integer;
    Y: Integer;
  public
    // delphi does not allow constructors without arguments
    constructor CreateAndTest(dummy: byte);
    constructor Create(dummy: boolean); overload;
    constructor Create(AX, AY: Integer); overload;
    constructor Create(AY: Integer); overload;
  end;

{ TRec }

constructor TRec.CreateAndTest(dummy: byte);
begin
  X := 1;
  if X <> 1 then
    halt(1);
  Y := 2;
  if Y <> 2 then
    halt(2);
end;

constructor TRec.Create(dummy: boolean);
begin
  X := 10;
  Y := 20;
end;

constructor TRec.Create(AX, AY: Integer);
begin
  X := AX;
  Y := AY;
end;

constructor TRec.Create(AY: Integer);
begin
  Create(false);
  Y := AY;
end;

procedure TestRec(R: TRec; ExpectedX, ExpectedY: Integer; ErrorX, ErrorY: Integer);
begin
  if R.X <> ExpectedX then
    halt(ErrorX);
  if R.Y <> ExpectedY then
    halt(ErrorY);
end;

var
  R: TRec;
begin
  R.CreateAndTest(0);
  R := TRec.Create(false);
  if R.X <> 10 then
    halt(3);
  if R.Y <> 20 then
    halt(4);
  TestRec(TRec.Create(1, 2), 1, 2, 5, 6);
  TestRec(TRec.Create(2), 10, 2, 7, 8);
  // delphi has an internal error here
  TestRec(R.Create(false), 10, 20, 9, 10);
end.

