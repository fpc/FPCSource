program tw41006;
{$mode delphi}
{$H+} {$M+}
{$RTTI EXPLICIT METHODS([vcPublished]) PROPERTIES([vcPublished]) FIELDS([vcPublished])}
uses
  SysUtils, TypInfo, Rtti
{$ifndef windows}
  , ffi.manager
{$endif}
;

type
  TRec1 = record
    Intf: IUnknown;
  end;
  TRec2 = record
    P: Pointer;
    Intf: IUnknown;
    B: Byte;
  end;
  TRec3 = record
    R: TRec1;
  end;
  TRec4 = record
    R: TRec2;
  end;
  TRec5 = record
    I: Integer;
  end;
  TRec6 = record
    I1: Int64;
    I2: Int64;
  end;
  TRec7 = record
     R: TRec5;
  end;
  TRec8 = record
     R: TRec6;
  end;


type

  { TMyObj }

  TMyObj = class
  strict private
    FStr: string;
  public
    constructor Create;
  published
    procedure Test1Value(ARec: TRec1);
    procedure Test1Const(const ARec: TRec1);
    procedure Test2Value(ARec: TRec2);
    procedure Test2Const(const ARec: TRec2);
    procedure Test3Value(ARec: TRec3);
    procedure Test3Const(const ARec: TRec3);
    procedure Test4Value(ARec: TRec4);
    procedure Test4Const(const ARec: TRec4);
    procedure Test5Value(ARec: TRec5);
    procedure Test5Const(const ARec: TRec5);
    procedure Test6Value(ARec: TRec6);
    procedure Test6Const(const ARec: TRec6);
    procedure Test7Value(ARec: TRec7);
    procedure Test7Const(const ARec: TRec7);
    procedure Test8Value(ARec: TRec8);
    procedure Test8Const(const ARec: TRec8);

    function Test1Ret: TRec1;
    function Test2Ret: TRec2;
    function Test3Ret: TRec3;
    function Test4Ret: TRec4;
    function Test5Ret: TRec5;
    function Test6Ret: TRec6;
    function Test7Ret: TRec7;
    function Test8Ret: TRec8;
  end;

var
  ErrorCount: Integer;

procedure Check(ACondition: boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('ERROR: ' + AMessage);
    INc(ErrorCount);
  end;
end;

var
  Intf: IUnknown;

{ TMyObj }

constructor TMyObj.Create;
begin
  FStr := '123';
end;

procedure TMyObj.Test1Value(ARec: TRec1);
begin
  Check(FStr = '123', 'Test1Value: Self is broken');
  Check(ARec.Intf = Intf, 'Test1Value: ARec is broken');
end;

procedure TMyObj.Test1Const(const ARec: TRec1);
begin
  Check(FStr = '123', 'Test1Const: Self is broken');
  Check(ARec.Intf = Intf, 'Test1Const: ARec is broken');
end;

procedure TMyObj.Test2Value(ARec: TRec2);
begin
  Check(FStr = '123', 'Test2Value: Self is broken');
  Check(ARec.Intf = Intf, 'Test2Value: ARec is broken');
  Check(ARec.B = 59, 'Test2Value: ARec is broken');
end;

procedure TMyObj.Test2Const(const ARec: TRec2);
begin
  Check(FStr = '123', 'Test2Const: Self is broken');
  Check(ARec.Intf = Intf, 'Test2Const: ARec is broken');
  Check(ARec.B = 59, 'Test2Const: ARec is broken');
end;

procedure TMyObj.Test3Value(ARec: TRec3);
begin
  Check(FStr = '123', 'Test3Value: Self is broken');
  Check(ARec.R.Intf = Intf, 'Test3Value: ARec is broken');
end;

procedure TMyObj.Test3Const(const ARec: TRec3);
begin
  Check(FStr = '123', 'Test3Const: Self is broken');
  Check(ARec.R.Intf = Intf, 'Test3Const: ARec is broken');
end;

procedure TMyObj.Test4Value(ARec: TRec4);
begin
  Check(FStr = '123', 'Test4Value: Self is broken');
  Check(ARec.R.Intf = Intf, 'Test4Value: ARec is broken');
  Check(ARec.R.B = 81, 'Test4Value: ARec is broken');
end;

procedure TMyObj.Test4Const(const ARec: TRec4);
begin
  Check(FStr = '123', 'Test4Const: Self is broken');
  Check(ARec.R.Intf = Intf, 'Test4Const: ARec is broken');
  Check(ARec.R.B = 81, 'Test4Const: ARec is broken');
end;

procedure TMyObj.Test5Value(ARec: TRec5);
begin
  Check(FStr = '123', 'Test5Value: Self is broken');
  Check(ARec.I = 15, 'Test5Value: ARec is broken');
end;

procedure TMyObj.Test5Const(const ARec: TRec5);
begin
  Check(FStr = '123', 'Test5Const: Self is broken');
  Check(ARec.I = 15, 'Test5Const: ARec is broken');
end;

procedure TMyObj.Test6Value(ARec: TRec6);
begin
  Check(FStr = '123', 'Test6Value: Self is broken');
  Check(ARec.I1 = 98, 'Test6Value: ARec is broken');
  Check(ARec.I2 = 102, 'Test6Value: ARec is broken');
end;

procedure TMyObj.Test6Const(const ARec: TRec6);
begin
  Check(FStr = '123', 'Test6Const: Self is broken');
  Check(ARec.I1 = 98, 'Test6Const: ARec is broken');
  Check(ARec.I2 = 102, 'Test6Const: ARec is broken');
end;

procedure TMyObj.Test7Value(ARec: TRec7);
begin
  Check(FStr = '123', 'Test7Value: Self is broken');
  Check(ARec.R.I = 98, 'Test7Value: ARec is broken');
end;

procedure TMyObj.Test7Const(const ARec: TRec7);
begin
  Check(FStr = '123', 'Test7Const: Self is broken');
  Check(ARec.R.I = 98, 'Test7Const: ARec is broken');
end;

procedure TMyObj.Test8Value(ARec: TRec8);
begin
  Check(FStr = '123', 'Test8Value: Self is broken');
  Check(ARec.R.I1 = 792, 'Test8Value: ARec is broken');
  Check(ARec.R.I2 = 153, 'Test8Value: ARec is broken');
end;

procedure TMyObj.Test8Const(const ARec: TRec8);
begin
  Check(FStr = '123', 'Test8Const: Self is broken');
  Check(ARec.R.I1 = 792, 'Test8Const: ARec is broken');
  Check(ARec.R.I2 = 153, 'Test8Const: ARec is broken');
end;

function TMyObj.Test1Ret: TRec1;
begin
  Check(FStr = '123', 'Test2Ret: Self is broken');
  Result.Intf := Intf;
end;

function TMyObj.Test2Ret: TRec2;
begin
  Check(FStr = '123', 'Test2Ret: Self is broken');
  Result.Intf := Intf;
  Result.B := 24;
  Result.P := Pointer(8);
end;

function TMyObj.Test3Ret: TRec3;
begin
  Check(FStr = '123', 'Test3Ret: Self is broken');
  Result.R.Intf := Intf;
end;

function TMyObj.Test4Ret: TRec4;
begin
  Check(FStr = '123', 'Test4Ret: Self is broken');
  Result.R.Intf := Intf;
  Result.R.P := Pointer(46);
  Result.R.B := 13;
end;

function TMyObj.Test5Ret: TRec5;
begin
  Check(FStr = '123', 'Test5Ret: Self is broken');
  Result.I := 465;
end;

function TMyObj.Test6Ret: TRec6;
begin
  Check(FStr = '123', 'Test6Ret: Self is broken');
  Result.I1 := 136846;
  Result.I2 := 8642;
end;

function TMyObj.Test7Ret: TRec7;
begin
  Check(FStr = '123', 'Test7Ret: Self is broken');
  Result.R.I := 6943;
end;

function TMyObj.Test8Ret: TRec8;
begin
  Check(FStr = '123', 'Test8Ret: Self is broken');
  Result.R.I1 := 984376;
  Result.R.I2 := 937;
end;

procedure ZeroRecord(var Rec; ATypeInfo: PTypeInfo);
begin
  FinalizeArray(@Rec, ATypeInfo, 1);
  FillChar(Rec, GetTypeData(ATypeInfo).RecSize, 0);
end;

var
  O: TMyObj;
  Context: TRttiContext;
  R1: TRec1; R2: TRec2; R3: TRec3; R4: TRec4;
  R5: TRec5; R6: TRec6; R7: TRec7; R8: TRec8;
begin
  O := TMyObj.Create;
  Intf := TInterfacedObject.Create;
  try
  Context := TRttiContext.Create;
  try
    R1.Intf := Intf;
    Context.GetType(TMyObj).GetMethod('Test1Value').Invoke(O, [TValue.From<TRec1>(R1)]);
    Context.GetType(TMyObj).GetMethod('Test1Const').Invoke(O, [TValue.From<TRec1>(R1)]);

    R2.Intf := Intf;
    R2.B := 59;
    Context.GetType(TMyObj).GetMethod('Test2Value').Invoke(O, [TValue.From<TRec2>(R2)]);
    Context.GetType(TMyObj).GetMethod('Test2Const').Invoke(O, [TValue.From<TRec2>(R2)]);

    R3.R.Intf := Intf;
    Context.GetType(TMyObj).GetMethod('Test3Value').Invoke(O, [TValue.From<TRec3>(R3)]);
    Context.GetType(TMyObj).GetMethod('Test3Const').Invoke(O, [TValue.From<TRec3>(R3)]);

    R4.R.Intf := Intf;
    R4.R.B := 81;
    Context.GetType(TMyObj).GetMethod('Test4Value').Invoke(O, [TValue.From<TRec4>(R4)]);
    Context.GetType(TMyObj).GetMethod('Test4Const').Invoke(O, [TValue.From<TRec4>(R4)]);

    R5.I := 15;
    Context.GetType(TMyObj).GetMethod('Test5Value').Invoke(O, [TValue.From<TRec5>(R5)]);
    Context.GetType(TMyObj).GetMethod('Test5Const').Invoke(O, [TValue.From<TRec5>(R5)]);

    R6.I1 := 98;
    R6.I2 := 102;
    Context.GetType(TMyObj).GetMethod('Test6Value').Invoke(O, [TValue.From<TRec6>(R6)]);
    Context.GetType(TMyObj).GetMethod('Test6Const').Invoke(O, [TValue.From<TRec6>(R6)]);

    R7.R.I := 98;
    Context.GetType(TMyObj).GetMethod('Test7Value').Invoke(O, [TValue.From<TRec7>(R7)]);
    Context.GetType(TMyObj).GetMethod('Test7Const').Invoke(O, [TValue.From<TRec7>(R7)]);

    R8.R.I1 := 792;
    R8.R.I2 := 153;
    Context.GetType(TMyObj).GetMethod('Test8Value').Invoke(O, [TValue.From<TRec8>(R8)]);
    Context.GetType(TMyObj).GetMethod('Test8Const').Invoke(O, [TValue.From<TRec8>(R8)]);

    ZeroRecord(R1, TypeInfo(R1));
    R1 := Context.GetType(TMyObj).GetMethod('Test1Ret').Invoke(O, []).AsType<TRec1>();
    Check(R1.Intf = Intf, 'Test1Ret: Result variable');

    ZeroRecord(R2, TypeInfo(R2));
    R2 := Context.GetType(TMyObj).GetMethod('Test2Ret').Invoke(O, []).AsType<TRec2>();
    Check(R2.Intf = Intf, 'Test2Ret: Result variable');
    Check(R2.B = 24, 'Test2Ret: Result variable');

    ZeroRecord(R3, TypeInfo(R3));
    R3 := Context.GetType(TMyObj).GetMethod('Test3Ret').Invoke(O, []).AsType<TRec3>();
    Check(R3.R.Intf = Intf, 'Test3Ret: Result variable');

    ZeroRecord(R4, TypeInfo(R4));
    R4 := Context.GetType(TMyObj).GetMethod('Test4Ret').Invoke(O, []).AsType<TRec4>();
    Check(R4.R.Intf = Intf, 'Test4Ret: Result variable');
    Check(R4.R.B = 13, 'Test4Ret: Result variable');

    ZeroRecord(R5, TypeInfo(R5));
    R5 := Context.GetType(TMyObj).GetMethod('Test5Ret').Invoke(O, []).AsType<TRec5>();
    Check(R5.I = 465, 'Test5Ret: Result variable');

    ZeroRecord(R6, TypeInfo(R6));
    R6 := Context.GetType(TMyObj).GetMethod('Test6Ret').Invoke(O, []).AsType<TRec6>();
    Check(R6.I1 = 136846, 'Test6Ret: Result variable');
    Check(R6.I2 = 8642, 'Test6Ret: Result variable');

    ZeroRecord(R7, TypeInfo(R7));
    R7 := Context.GetType(TMyObj).GetMethod('Test7Ret').Invoke(O, []).AsType<TRec7>();
    Check(R7.R.I = 6943, 'Test7Ret: Result variable');

    ZeroRecord(R8, TypeInfo(R8));
    R8 := Context.GetType(TMyObj).GetMethod('Test8Ret').Invoke(O, []).AsType<TRec8>();
    Check(R8.R.I1 = 984376, 'Test8Ret: Result variable');
    Check(R8.R.I2 = 937, 'Test8Ret: Result variable');
  finally
    Context.Free; O.Free;
  end;
  if ErrorCount > 0 then
    Halt(ErrorCount);
  WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName + ': ' + E.Message);
      Halt(1);
    end;
  end;
end.
