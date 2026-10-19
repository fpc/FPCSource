{ Binary operations of custom variants with other operands: FmtBCD and test-only custom types }
program tw38496;

{$mode objfpc}{$H+}

uses
  SysUtils, Variants, FmtBCD;

type
  TOperandKind = (okSmallInt, okShortInt, okInteger, okByte, okWord, okLongWord, okInt64, okQWord,
    okCurrency, okDate, okBoolean, okAnsiString, okUnicodeString, okOleStr, okEmpty, okByRefInteger,
    okDouble, okSingle, okBCD);

  { Custom variant with an Int64 in vInt64, using the default promotions }
  TPromotedVariant = class(TCustomVariantType)
  public
    // Sets V to empty
    procedure Clear(var V: TVarData); override;
    // Copies the Int64 value of Source to Dest
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    // Converts Source to this type
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    // Converts Source of this type to aVarType
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const aVarType: TVarType); override;
    // Adds or multiplies two operands of this type
    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operation: TVarOp); override;
  end;

  { Custom variant with an Int64 in vInt64, promoting a left operand to varInt64 and refusing right operands }
  TStdPromotedVariant = class(TPromotedVariant)
  public
    // Number of LeftPromotion calls
    LeftCalls: Integer;
    // Requires varInt64 for any left operand
    function LeftPromotion(const V: TVarData; const Operation: TVarOp; out RequiredVarType: TVarType): Boolean; override;
    // Refuses every right operand
    function RightPromotion(const V: TVarData; const Operation: TVarOp; out RequiredVarType: TVarType): Boolean; override;
    // Adds an operand of this type to a varInt64 left operand
    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operation: TVarOp); override;
  end;

  EUnpromoted = class(Exception);

const
  cKindNames: array[TOperandKind] of String = ('SmallInt', 'ShortInt', 'Integer', 'Byte', 'Word', 'LongWord',
    'Int64', 'QWord', 'Currency', 'Date', 'Boolean', 'AnsiString', 'UnicodeString', 'OleStr', 'Empty',
    'ByRef Integer', 'Double', 'Single', 'BCD');
  cKindValues: array[TOperandKind] of String = ('-7', '-100', '123456', '200', '60000', '4000000000',
    '-9000000000000', '18000000000000000000', '7.25', '7', '-1', '-3.75', '1234.5', '0.125', '0',
    '42', '1.25', '0.5', '-12.375');
  cBCDValues: array[1..7] of String = ('2', '-0.5', '1000000.001', '-3.2', '0.04', '2.5', '-0.75');
  cOps: array[0..3] of TVarOp = (opAdd, opSubtract, opMultiply, opDivide);
  cOpNames: array[0..3] of String = ('+', '-', '*', '/');

var
  Failures: Integer = 0;
  Checks: Integer = 0;
  ByRefValue: Integer;
  Promoted: TPromotedVariant;
  StdPromoted: TStdPromotedVariant;

// Records the result of a check
procedure Check(aCondition: Boolean; const aMsg: String);

begin
  Inc(Checks);
  if not aCondition then
    begin
    Inc(Failures);
    if Failures<=30 then
      Writeln('FAIL: ',aMsg);
    end;
end;


// Returns a variant of kind aKind with value cKindValues[aKind]
function MakeOperand(aKind: TOperandKind): Variant;

begin
  case aKind of
    okSmallInt: Result:=SmallInt(-7);
    okShortInt: Result:=ShortInt(-100);
    okInteger: Result:=Integer(123456);
    okByte: Result:=Byte(200);
    okWord: Result:=Word(60000);
    okLongWord: Result:=LongWord(4000000000);
    okInt64: Result:=Int64(-9000000000000);
    okQWord: Result:=QWord(18000000000000000000);
    okCurrency: Result:=Currency(7.25);
    okDate: Result:=VarFromDateTime(7);
    okBoolean: Result:=True;
    okAnsiString: Result:=AnsiString('-3.75');
    okUnicodeString: Result:=UnicodeString('1234.5');
    okOleStr: Result:=WideString('0.125');
    okEmpty: Result:=Unassigned;
    okByRefInteger:
      begin
      ByRefValue:=42;
      Result:=Unassigned;
      TVarData(Result).vType:=varInteger or varByRef;
      TVarData(Result).vPointer:=@ByRefValue;
      end;
    okDouble: Result:=Double(1.25);
    okSingle: Result:=Single(0.5);
    okBCD: Result:=VarFmtBCDCreate(StrToBCD('-12.375'));
  end;
end;


// Returns aLeft aOp aRight computed on variants
function DoOp(const aLeft, aRight: Variant; aOp: TVarOp): Variant;

begin
  case aOp of
    opAdd: Result:=aLeft+aRight;
    opSubtract: Result:=aLeft-aRight;
    opMultiply: Result:=aLeft*aRight;
    opDivide: Result:=aLeft/aRight;
  else
    raise Exception.Create('unexpected operation');
  end;
end;


// Returns aLeft aOp aRight computed on BCD values
function DoBCDOp(const aLeft, aRight: TBCD; aOp: TVarOp): TBCD;

begin
  case aOp of
    opAdd: Result:=aLeft+aRight;
    opSubtract: Result:=aLeft-aRight;
    opMultiply: Result:=aLeft*aRight;
    opDivide: Result:=aLeft/aRight;
  else
    raise Exception.Create('unexpected operation');
  end;
end;


// Returns True if aValue equals aExpected within a relative tolerance
function SameDouble(aValue, aExpected: Double): Boolean;

begin
  Result:=Abs(aValue-aExpected)<=1e-12*Abs(aExpected)+1e-300;
end;


// Checks aResult against aExpected, as a BCD or, if aAsDouble, as a Double
procedure CheckResult(const aName: String; const aResult: Variant; const aExpected: TBCD; aAsDouble: Boolean);

begin
  if aAsDouble then
    begin
    Check(VarType(aResult)=varDouble,aName+': result type is '+VarTypeAsText(VarType(aResult))+', expected Double');
    if VarType(aResult)=varDouble then
      Check(SameDouble(aResult,BCDToDouble(aExpected)),aName+': result '+VarToStr(aResult)+', expected '+BCDToStr(aExpected));
    end
  else
    begin
    Check(VarIsFmtBCD(aResult),aName+': result type is '+VarTypeAsText(VarType(aResult))+', expected FmtBCD');
    if VarIsFmtBCD(aResult) then
      Check(BCDCompare(VarToBCD(aResult),aExpected)=0,aName+': result '+VarToStr(aResult)+', expected '+BCDToStr(aExpected));
    end;
end;


// Checks that aValue still has kind aKind and its original value
procedure CheckUnchanged(const aName: String; const aValue: Variant; aKind: TOperandKind);

var
  lOriginal: Variant;

begin
  lOriginal:=MakeOperand(aKind);
  Check(TVarData(aValue).vType=TVarData(lOriginal).vType,aName+': operand type changed to '+VarTypeAsText(VarType(aValue)));
  if aKind=okByRefInteger then
    Check(ByRefValue=42,aName+': referenced integer changed to '+IntToStr(ByRefValue))
  else if TVarData(aValue).vType=TVarData(lOriginal).vType then
    Check(VarToStr(aValue)=VarToStr(lOriginal),aName+': operand value changed to '+VarToStr(aValue));
end;


// Every operand kind with a BCD operand, in both orders and with all four operations
procedure TestMatrix;

var
  lKind: TOperandKind;
  I, J: Integer;
  lOperand, lBCD, lResult: Variant;
  lName: String;

begin
  for lKind:=Low(TOperandKind) to High(TOperandKind) do
    for I:=Low(cBCDValues) to High(cBCDValues) do
      for J:=Low(cOps) to High(cOps) do
        begin
        lBCD:=VarFmtBCDCreate(StrToBCD(cBCDValues[I]));
        lOperand:=MakeOperand(lKind);
        lName:=cKindNames[lKind]+' '+cKindValues[lKind]+' '+cOpNames[J]+' BCD '+cBCDValues[I];
        try
          lResult:=DoOp(lOperand,lBCD,cOps[J]);
          CheckResult(lName,lResult,DoBCDOp(StrToBCD(cKindValues[lKind]),StrToBCD(cBCDValues[I]),cOps[J]),
            lKind in [okDouble,okSingle]);
        except
          on E: Exception do
            Check(False,lName+': '+E.ClassName+': '+E.Message);
        end;
        CheckUnchanged(lName,lOperand,lKind);
        Check(BCDCompare(VarToBCD(lBCD),StrToBCD(cBCDValues[I]))=0,lName+': BCD operand changed to '+VarToStr(lBCD));

        lOperand:=MakeOperand(lKind);
        lName:='BCD '+cBCDValues[I]+' '+cOpNames[J]+' '+cKindNames[lKind]+' '+cKindValues[lKind];
        try
          if (cOps[J]=opDivide) and (lKind=okEmpty) then
            begin
            Inc(Checks);
            try
              lResult:=DoOp(lBCD,lOperand,cOps[J]);
              Check(False,lName+': no exception for division by zero');
            except
              on E: eBCDException do ;
            end;
            end
          else
            begin
            lResult:=DoOp(lBCD,lOperand,cOps[J]);
            CheckResult(lName,lResult,DoBCDOp(StrToBCD(cBCDValues[I]),StrToBCD(cKindValues[lKind]),cOps[J]),False);
            end;
        except
          on E: Exception do
            Check(False,lName+': '+E.ClassName+': '+E.Message);
        end;
        CheckUnchanged(lName,lOperand,lKind);
        end;
end;


// Expressions with several mixed operands and assignments back to the operands
procedure TestExpressions;

var
  lInt, lBCD, lDouble, lResult: Variant;
  lI: Integer;
  lD: Double;
  lS: String;

begin
  lBCD:=VarFmtBCDCreate(StrToBCD('2.5'));
  lInt:=Integer(7);
  lInt:=lInt+lBCD;
  CheckResult('in place Integer := Integer + BCD',lInt,StrToBCD('9.5'),False);
  lInt:=Integer(7);
  lInt:=lBCD+lInt;
  CheckResult('in place Integer := BCD + Integer',lInt,StrToBCD('9.5'),False);
  lInt:=Integer(3);
  lResult:=(lInt+lBCD)*Integer(2)-lBCD/Integer(5);
  CheckResult('(Integer + BCD) * Integer - BCD / Integer',lResult,StrToBCD('10.5'),False);
  lResult:=Integer(10)-(lInt*lBCD);
  CheckResult('Integer - (Integer * BCD)',lResult,StrToBCD('2.5'),False);
  lDouble:=Double(4);
  lResult:=lDouble*lBCD+Integer(1);
  Check(VarType(lResult)=varDouble,'Double * BCD + Integer: result type is '+VarTypeAsText(VarType(lResult)));
  Check(SameDouble(lResult,11),'Double * BCD + Integer: result '+VarToStr(lResult));
  lResult:=Integer(1)+lBCD;
  lI:=lResult*Integer(2);
  Check(lI=7,'(Integer + BCD) * Integer assigned to Integer: '+IntToStr(lI));
  lD:=Integer(1)/lBCD;
  Check(SameDouble(lD,0.4),'Integer / BCD assigned to Double: '+FloatToStr(lD));
  lS:=Integer(1)+lBCD;
  Check(lS='3.5','Integer + BCD assigned to String: '+lS);
  lResult:=lBCD;
  for lI:=1 to 100 do
    lResult:=lI+lResult;
  CheckResult('loop of Integer + BCD',lResult,StrToBCD('5052.5'),False);
end;


// Casts to BCD, comparisons and operations that are not supported
procedure TestCastsAndErrors;

var
  lKind: TOperandKind;
  lBCD, lZero, lInt, lResult: Variant;

  // Checks that operation number aOp raises an exception of class aClass
  procedure CheckRaises(const aName: String; aClass: ExceptClass; aOp: Integer);

  begin
    Inc(Checks);
    try
      case aOp of
        0: lResult:=lInt div lBCD;
        1: lResult:=lBCD mod lInt;
        2: lResult:=lInt and lBCD;
        3: lResult:=lBCD shl lInt;
        4: lResult:=-lBCD;
        5: lResult:=lInt/lZero;
        8: lResult:=AnsiString('abc')+lBCD;
        9: lResult:=lInt**lBCD;
      end;
      Check(False,aName+': no exception');
    except
      on E: Exception do
        Check(E is aClass,aName+': raised '+E.ClassName+', expected '+aClass.ClassName);
    end;
  end;

begin
  for lKind:=Low(TOperandKind) to High(TOperandKind) do
    begin
    lResult:=VarAsType(MakeOperand(lKind),VarFmtBCD);
    CheckResult('VarAsType('+cKindNames[lKind]+', VarFmtBCD)',lResult,StrToBCD(cKindValues[lKind]),False);
    lResult:=MakeOperand(lKind);
    lResult:=VarAsType(lResult,VarFmtBCD);
    CheckResult('in place VarAsType('+cKindNames[lKind]+', VarFmtBCD)',lResult,StrToBCD(cKindValues[lKind]),False);
    end;

  Check(BCDToStr(VarToBCD(QWord(High(QWord))))='18446744073709551615','QWord High(QWord) to BCD: '+BCDToStr(VarToBCD(QWord(High(QWord)))));
  Check(BCDToStr(VarToBCD(QWord(High(Int64))+1))='9223372036854775808','QWord High(Int64)+1 to BCD: '+BCDToStr(VarToBCD(QWord(High(Int64))+1)));
  CheckResult('QWord High(QWord) + BCD',QWord(High(QWord))+VarFmtBCDCreate(StrToBCD('0.5')),StrToBCD('18446744073709551615.5'),False);

  lBCD:=VarFmtBCDCreate(StrToBCD('2.5'));
  lZero:=VarFmtBCDCreate(StrToBCD('0'));
  lInt:=Integer(7);
  Check(lInt>lBCD,'Integer > BCD');
  Check(not (lInt<lBCD),'not (Integer < BCD)');
  Check(lBCD<lInt,'BCD < Integer');
  Check(VarFmtBCDCreate(StrToBCD('7'))=lInt,'BCD = Integer');
  Check(lInt=VarFmtBCDCreate(StrToBCD('7')),'Integer = BCD');

  CheckRaises('Integer div BCD',EVariantInvalidOpError,0);
  CheckRaises('BCD mod Integer',EVariantInvalidOpError,1);
  CheckRaises('Integer and BCD',EVariantInvalidOpError,2);
  CheckRaises('BCD shl Integer',EVariantInvalidOpError,3);
  CheckRaises('-BCD',EVariantInvalidOpError,4);
  CheckRaises('Integer / BCD zero',eBCDException,5);
  Check(VarIsNull(Null+lBCD),'Null + BCD is Null');
  Check(VarIsNull(lBCD+Null),'BCD + Null is Null');
  Check(VarIsNull(Null*lBCD),'Null * BCD is Null');
  Check(VarIsNull(lBCD/Null),'BCD / Null is Null');
  CheckRaises('non-numeric AnsiString + BCD',Exception,8);
  CheckRaises('Integer ** BCD',EVariantInvalidOpError,9);
end;


procedure TPromotedVariant.Clear(var V: TVarData);

begin
  V.vType:=varEmpty;
end;


procedure TPromotedVariant.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);

begin
  Dest.vInt64:=Source.vInt64;
  Dest.vType:=Source.vType;
end;


procedure TPromotedVariant.Cast(var Dest: TVarData; const Source: TVarData);

var
  lValue: Int64;

begin
  lValue:=Variant(Source);
  VarDataClear(Dest);
  Dest.vType:=VarType;
  Dest.vInt64:=lValue;
end;


procedure TPromotedVariant.CastTo(var Dest: TVarData; const Source: TVarData; const aVarType: TVarType);

var
  lTemp: TVarData;

begin
  if Source.vType<>VarType then
    RaiseCastError;
  VarDataInit(lTemp);
  lTemp.vType:=varInt64;
  lTemp.vInt64:=Source.vInt64;
  VarDataCastTo(Dest,lTemp,aVarType);
end;


procedure TPromotedVariant.BinaryOp(var Left: TVarData; const Right: TVarData; const Operation: TVarOp);

begin
  if (Left.vType<>VarType) or (Right.vType<>VarType) then
    raise EUnpromoted.CreateFmt('%s received types %d and %d',[ClassName,Left.vType,Right.vType]);
  case Operation of
    opAdd: Left.vInt64:=Left.vInt64+Right.vInt64;
    opMultiply: Left.vInt64:=Left.vInt64*Right.vInt64;
  else
    RaiseInvalidOp;
  end;
end;


function TStdPromotedVariant.LeftPromotion(const V: TVarData; const Operation: TVarOp; out RequiredVarType: TVarType): Boolean;

begin
  Inc(LeftCalls);
  RequiredVarType:=varInt64;
  Result:=True;
end;


function TStdPromotedVariant.RightPromotion(const V: TVarData; const Operation: TVarOp; out RequiredVarType: TVarType): Boolean;

begin
  RequiredVarType:=varEmpty;
  Result:=False;
end;


procedure TStdPromotedVariant.BinaryOp(var Left: TVarData; const Right: TVarData; const Operation: TVarOp);

begin
  if (Left.vType<>varInt64) or (Right.vType<>VarType) then
    raise EUnpromoted.CreateFmt('%s received types %d and %d',[ClassName,Left.vType,Right.vType]);
  if Operation<>opAdd then
    RaiseInvalidOp;
  Left.vInt64:=Left.vInt64+Right.vInt64;
end;


// Returns a variant of custom type aHandler with value aValue
function MakeCustom(aHandler: TCustomVariantType; aValue: Int64): Variant;

begin
  Result:=Unassigned;
  TVarData(Result).vType:=aHandler.VarType;
  TVarData(Result).vInt64:=aValue;
end;


// Checks that aResult has custom type aHandler and value aValue
procedure CheckCustom(const aName: String; const aResult: Variant; aHandler: TCustomVariantType; aValue: Int64);

begin
  Check(TVarData(aResult).vType=aHandler.VarType,aName+': result type is '+IntToStr(TVarData(aResult).vType)
    +', expected '+IntToStr(aHandler.VarType));
  if TVarData(aResult).vType=aHandler.VarType then
    Check(TVarData(aResult).vInt64=aValue,aName+': result '+IntToStr(TVarData(aResult).vInt64)+', expected '+IntToStr(aValue));
end;


// Runs aOp and checks that it raises an exception of class aClass
procedure CheckCustomRaises(const aName: String; aClass: ExceptClass; const aLeft, aRight: Variant; aOp: TVarOp);

begin
  Inc(Checks);
  try
    DoOp(aLeft,aRight,aOp);
    Check(False,aName+': no exception');
  except
    on E: Exception do
      Check(E is aClass,aName+': raised '+E.ClassName+': '+E.Message+', expected '+aClass.ClassName);
  end;
end;


// Dispatch of binary operations through the promotions of custom variant types
procedure TestCustomTypes;

var
  lP, lS, lResult: Variant;

begin
  Promoted:=TPromotedVariant.Create;
  StdPromoted:=TStdPromotedVariant.Create;
  try
    lP:=MakeCustom(Promoted,3);
    lS:=MakeCustom(StdPromoted,3);
    try
      CheckCustom('Integer + custom',Integer(4)+lP,Promoted,7);
      CheckCustom('custom + Integer',lP+Integer(4),Promoted,7);
      CheckCustom('AnsiString + custom',AnsiString('5')+lP,Promoted,8);
      CheckCustom('custom * Double',lP*Double(6),Promoted,18);
      CheckCustom('custom * custom',lP*lP,Promoted,9);
      CheckCustom('custom + custom of other type',lP+lS,Promoted,6);
      CheckCustom('custom of other type + custom',lS+lP,Promoted,6);
      CheckCustomRaises('custom - Integer',EVariantInvalidOpError,lP,Integer(1),opSubtract);

      StdPromoted.LeftCalls:=0;
      lResult:=AnsiString('5')+lS;
      Check(VarType(lResult)=varInt64,'AnsiString + custom promoted to Int64: result type is '+VarTypeAsText(VarType(lResult)));
      Check(lResult=8,'AnsiString + custom promoted to Int64: result '+VarToStr(lResult));
      Check(StdPromoted.LeftCalls=2,'AnsiString + custom promoted to Int64: '+IntToStr(StdPromoted.LeftCalls)+' LeftPromotion calls, expected 2');
      lResult:=Int64(10)+lS;
      Check(lResult=13,'Int64 + custom promoted to Int64: result '+VarToStr(lResult));
      CheckCustomRaises('custom refusing promotion + Integer',EVariantInvalidOpError,lS,Integer(1),opAdd);
      CheckCustom('custom operand unchanged',lP,Promoted,3);
      CheckCustom('custom operand of other type unchanged',lS,StdPromoted,3);
    except
      on E: Exception do
        Check(False,'custom types: '+E.ClassName+': '+E.Message);
    end;
    lP:=Unassigned;
    lS:=Unassigned;
    lResult:=Unassigned;
  finally
    StdPromoted.Free;
    Promoted.Free;
  end;
end;


begin
  DefaultFormatSettings.DecimalSeparator:='.';
  DefaultFormatSettings.ThousandSeparator:=',';
  TestMatrix;
  TestExpressions;
  TestCastsAndErrors;
  TestCustomTypes;
  if Failures>0 then
    begin
    Writeln(Failures,' of ',Checks,' checks failed');
    Halt(1);
    end;
  Writeln('ok, ',Checks,' checks');
end.
