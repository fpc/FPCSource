unit utcExprBuiltin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, punit, math, fpexprpars;

procedure RegisterTests(aTop : PSuite);

implementation

uses dateutils, typinfo;

procedure AssertEquals(Msg: String; AResultType: TResultType;  ANode: TFPExprNode); overload;
begin
  AssertNotNull(Msg+': Node not null',ANode);
  AssertEquals(Msg,ResultTypeName(AResultType),ResultTypeName(Anode.NodeType));
end;

procedure AssertEquals(Msg: String; AExpected, AActual: TResultType); overload;
begin
  AssertEquals(Msg,ResultTypeName(AExpected),ResultTypeName(AActual));
end;

type
  TMyFPExpressionParser = class(TFPExpressionParser)
  public
    property ExprNode;
    property Scanner;
    property Dirty;
  end;


var
  FValue : Integer;
  FP: TMyFPExpressionParser;
  FM : TExprBuiltInManager;
  FileFormatSettings: TFormatSettings;

procedure DummyGetDate(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
  Result.resDateTime:=Date;
end;

procedure DummyEchoDate(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
  Result.resDateTime:=Args[0].resDateTime;
end;


function SuiteSetup: string;
begin
  Result := '';
  FP := TMyFPExpressionParser.Create(nil);
  FM := TExprBuiltInManager.Create(Nil);
  FValue := 0;
end;

function SuiteTearDown : string;
begin
  Result := '';
  FValue := 0;
  FreeAndNil(FM);
  FreeAndNil(FP);
end;

function TestBuiltinsManager_TestCreate: TTestString;
begin
  Result := '';
  AssertEquals('Have no builtin expressions',0,FM.IdentifierCount);
end;

function TestBuiltinsManager_TestVariable1: TTestString;
Var
  I : TFPBuiltinExprIdentifierDef;
begin
  Result := '';
  I:=FM.AddVariable(bcuser,'a',rtBoolean,'True');
  AssertNotNull('Addvariable returns result',I);
  AssertEquals('One variable added',1,FM.IdentifierCount);
  AssertSame('Result equals variable added',I,FM.Identifiers[0]);
  AssertEquals('Variable has correct category',ord(bcUser),Ord(I.Category));
  AssertEquals('Variable has correct resulttype',rtBoolean,I.ResultType);
  AssertEquals('Variable has correct value','True',I.Value);
end;

function TestBuiltinsManager_TestVariable2: TTestString;
Var
  I : TFPBuiltinExprIdentifierDef;
begin
  Result := '';
  I:=FM.AddBooleanVariable(bcUser,'a',False);
  AssertNotNull('Addvariable returns result',I);
  AssertEquals('One variable added',1,FM.IdentifierCount);
  AssertSame('Result equals variable added',I,FM.Identifiers[0]);
  AssertEquals('Variable has correct category',ord(bcUser),Ord(I.Category));
  AssertEquals('Variable has correct resulttype',rtBoolean,I.ResultType);
  AssertEquals('Variable has correct value','False',I.Value);
end;

function TestBuiltinsManager_TestVariable3: TTestString;
Var
  I : TFPBuiltinExprIdentifierDef;
begin
  Result := '';
  I:=FM.AddIntegerVariable(bcUser,'a',123);
  AssertNotNull('Addvariable returns result',I);
  AssertEquals('One variable added',1,FM.IdentifierCount);
  AssertSame('Result equals variable added',I,FM.Identifiers[0]);
  AssertEquals('Variable has correct category',ord(bcUser),Ord(I.Category));
  AssertEquals('Variable has correct resulttype',rtInteger,I.ResultType);
  AssertEquals('Variable has correct value','123',I.Value);
end;

function TestBuiltinsManager_TestVariable4: TTestString;
Var
  I : TFPBuiltinExprIdentifierDef;
begin
  Result := '';
  I:=FM.AddFloatVariable(bcUser,'a',1.23);
  AssertNotNull('Addvariable returns result',I);
  AssertEquals('One variable added',1,FM.IdentifierCount);
  AssertSame('Result equals variable added',I,FM.Identifiers[0]);
  AssertEquals('Variable has correct category',ord(bcUser),Ord(I.Category));
  AssertEquals('Variable has correct resulttype',rtFloat,I.ResultType);
  AssertEquals('Variable has correct value',FloatToStr(1.23, FileFormatSettings),I.Value);
end;

function TestBuiltinsManager_TestVariable5: TTestString;
Var
  I : TFPBuiltinExprIdentifierDef;
begin
  Result := '';
  I:=FM.AddStringVariable(bcUser,'a','1.23');
  AssertNotNull('Addvariable returns result',I);
  AssertEquals('One variable added',1,FM.IdentifierCount);
  AssertSame('Result equals variable added',I,FM.Identifiers[0]);
  AssertEquals('Variable has correct category',ord(bcUser),Ord(I.Category));
  AssertEquals('Variable has correct resulttype',rtString,I.ResultType);
  AssertEquals('Variable has correct value','1.23',I.Value);
end;

function TestBuiltinsManager_TestVariable6: TTestString;
Var
  I : TFPBuiltinExprIdentifierDef;
  D : TDateTime;
begin
  Result := '';
  D:=Now;
  I:=FM.AddDateTimeVariable(bcUser,'a',D);
  AssertNotNull('Addvariable returns result',I);
  AssertEquals('One variable added',1,FM.IdentifierCount);
  AssertSame('Result equals variable added',I,FM.Identifiers[0]);
  AssertEquals('Variable has correct category',ord(bcUser),Ord(I.Category));
  AssertEquals('Variable has correct resulttype',rtDateTime,I.ResultType);
  AssertEquals('Variable has correct value',FormatDateTime('yyyy-mm-dd hh:nn:ss',D),I.Value);
end;

function TestBuiltinsManager_TestVariable7: TTestString;
Var
  I : TFPBuiltinExprIdentifierDef;
begin
  Result := '';
  I:=FM.AddCurrencyVariable(bcUser,'a',1.23);
  AssertNotNull('Addvariable returns result',I);
  AssertEquals('One variable added',1,FM.IdentifierCount);
  AssertSame('Result equals variable added',I,FM.Identifiers[0]);
  AssertEquals('Variable has correct category',ord(bcUser),Ord(I.Category));
  AssertEquals('Variable has correct resulttype',rtCurrency,I.ResultType);
  AssertEquals('Variable has correct value',CurrToStr(1.23, FileFormatSettings),I.Value);
end;



function TestBuiltinsManager_TestFunction1: TTestString;
Var
  I : TFPBuiltinExprIdentifierDef;
begin
  Result := '';
  I:=FM.AddFunction(bcUser,'Date','D','',@DummyGetDate);
  AssertNotNull('Addvariable returns result',I);
  AssertEquals('One variable added',1,FM.IdentifierCount);
  AssertSame('Result equals variable added',I,FM.Identifiers[0]);
  AssertEquals('Variable has correct category',ord(bcUser),Ord(I.Category));
  AssertEquals('Function has correct resulttype',rtDateTime,I.ResultType);
  AssertEquals('Function has correct address',Pointer(@DummyGetDate),Pointer(I.OnGetFunctionValueCallBack));
end;

function TestBuiltinsManager_TestFunction2: TTestString;
Var
  I,I2 : TFPBuiltinExprIdentifierDef;
  ind : Integer;
begin
  Result := '';
  FM.AddFunction(bcUser,'EchoDate','D','D',@DummyEchoDate);
  I:=FM.AddFunction(bcUser,'Echo','D','D',@DummyEchoDate);
  FM.AddFunction(bcUser,'DoEcho','D','D',@DummyEchoDate);
  ind:=FM.IndexOfIdentifier('Echo');
  AssertEquals('Found identifier',1,ind);
  I2:=FM.FindIdentifier('Echo');
  AssertNotNull('FindIdentifier returns result',I2);
  AssertSame('Findidentifier returns correct result',I,I2);
  ind:=FM.IndexOfIdentifier('NoNoNo');
  AssertEquals('Found no such identifier',-1,ind);
  I2:=FM.FindIdentifier('NoNoNo');
  AssertNull('FindIdentifier returns no result',I2);
end;

function TestBuiltinsManager_TestDelete: TTestString;
begin
  Result := '';
  FM.AddFunction(bcUser,'EchoDate','D','D',@DummyEchoDate);
  FM.AddFunction(bcUser,'EchoDate2','D','D',@DummyEchoDate);
  FM.AddFunction(bcUser,'EchoDate3','D','D',@DummyEchoDate);
  AssertEquals('Count before',3,FM.IdentifierCount);
  FM.Delete(2);
  AssertEquals('Count after',2,FM.IdentifierCount);
  AssertEquals('No more',-1,FM.IndexOfIdentifier('EchoDate3'));
  AssertEquals('Left 1',0,FM.IndexOfIdentifier('EchoDate'));
  AssertEquals('Left 2',1,FM.IndexOfIdentifier('EchoDate2'));
end;

function TestBuiltinsManager_TestRemove: TTestString;
begin
  Result := '';
  FM.AddFunction(bcUser,'EchoDate','D','D',@DummyEchoDate);
  FM.AddFunction(bcUser,'EchoDate2','D','D',@DummyEchoDate);
  FM.AddFunction(bcUser,'EchoDate3','D','D',@DummyEchoDate);
  AssertEquals('Count before',3,FM.IdentifierCount);
  AssertEquals('Result ',1,FM.Remove('EchoDate2'));
  AssertEquals('Count after',2,FM.IdentifierCount);
  AssertEquals('No more',-1,FM.IndexOfIdentifier('EchoDate2'));
  AssertEquals('Left 1',0,FM.IndexOfIdentifier('EchoDate'));
  AssertEquals('Left 2',1,FM.IndexOfIdentifier('EchoDate3'));
  AssertEquals('Result ',-1,FM.Remove('Nono'));
end;


procedure SetExpression(const AExpression: String);
Var
  Msg : String;
begin
  Msg:='';
  try
    FP.Expression:=AExpression;
  except
    On E : Exception do
      Msg:=E.message;
  end;
  If (Msg<>'') then
    Fail('Parsing of expression "'+AExpression+'" failed :'+Msg);
end;

procedure AssertResult(F: TExprFloat);
begin
  AssertEquals('Float result', F, FP.AsFloat, 1E-9);
end;

procedure AssertResult(I: Int64);
begin
  AssertEquals('Integer result', I, FP.AsInteger);
end;

procedure AssertResult(S: String);
begin
  AssertEquals('String result', S, FP.AsString);
end;

procedure AssertResult(B: Boolean);
begin
  AssertEquals('Boolean result', B, FP.AsBoolean);
end;

procedure AssertDateTimeResult(D: TDateTime);
begin
  AssertEquals('DateTime result', D, FP.AsDateTime, 2/SecsPerDay);
end;

procedure AssertCurrencyResult(C: Currency);
begin
  AssertEquals('Currency result', C, FP.AsCurrency, 1E-9);
end;

procedure AssertExpression(const AExpression: String; AResult: Int64);
begin
  FP.BuiltIns:=AllBuiltIns;
  SetExpression(AExpression);
  AssertResult(AResult);
end;

procedure AssertExpression(const AExpression: String; const AResult: String);
begin
  FP.BuiltIns:=AllBuiltIns;
  SetExpression(AExpression);
  AssertResult(AResult);
end;

procedure AssertExpression(const AExpression: String; const AResult: TExprFloat);
begin
  FP.BuiltIns:=AllBuiltIns;
  SetExpression(AExpression);
  AssertResult(AResult);
end;

procedure AssertExpression(const AExpression: String; const AResult: Boolean);
begin
  FP.BuiltIns:=AllBuiltIns;
  SetExpression(AExpression);
  AssertResult(AResult);
end;

procedure AssertDateTimeExpression(const AExpression: String; const AResult: TDateTime);
begin
  FP.BuiltIns:=AllBuiltIns;
  SetExpression(AExpression);
  AssertDateTimeResult(AResult);
end;

procedure AssertAggregateExpression(const AExpression: String; AResult: Int64; AUpdateCount: integer);
begin
  FP.BuiltIns:=AllBuiltIns;
  SetExpression(AExpression);
  AssertEquals('Has aggregate',True,FP.ExprNode.HasAggregate);
  FP.InitAggregate;
  While AUpdateCount>0 do
    begin
    FP.UpdateAggregate;
    Dec(AUpdateCount);
    end;
  AssertResult(AResult);
end;

procedure AssertAggregateExpression(const AExpression: String; AResult: TExprFloat; AUpdateCount: integer);
begin
  FP.BuiltIns:=AllBuiltIns;
  SetExpression(AExpression);
  AssertEquals('Has aggregate',True,FP.ExprNode.HasAggregate);
  FP.InitAggregate;
  While AUpdateCount>0 do
    begin
    FP.UpdateAggregate;
    Dec(AUpdateCount);
    end;
  AssertResult(AResult);
end;

procedure AssertAggregateCurrExpression(Const AExpression : String; AResult : Currency; AUpdateCount : integer);
begin
  FP.BuiltIns:=AllBuiltIns;
  SetExpression(AExpression);
  AssertEquals('Has aggregate',True,FP.ExprNode.HasAggregate);
  FP.InitAggregate;
  While AUpdateCount>0 do
    begin
    FP.UpdateAggregate;
    Dec(AUpdateCount);
    end;
  AssertCurrencyResult(AResult);
end;

function TestBuiltins_TestVariablepi: TTestString;
begin
  Result := '';
  AssertExpression('pi',Pi);
end;

function TestBuiltins_TestFunctioncos: TTestString;
begin
  Result := '';
  AssertExpression('cos(0.5)',Cos(0.5));
  AssertExpression('cos(0.75)',Cos(0.75));
end;

function TestBuiltins_TestFunctionsin: TTestString;
begin
  Result := '';
  AssertExpression('sin(0.5)',sin(0.5));
  AssertExpression('sin(0.75)',sin(0.75));
end;

function TestBuiltins_TestFunctionarctan: TTestString;
begin
  Result := '';
  AssertExpression('arctan(0.5)',arctan(0.5));
  AssertExpression('arctan(0.75)',arctan(0.75));
end;

function TestBuiltins_TestFunctionabs: TTestString;
begin
  Result := '';
  AssertExpression('abs(0.5)',0.5);
  AssertExpression('abs(-0.75)',0.75);
end;

function TestBuiltins_TestFunctionsqr: TTestString;
begin
  Result := '';
  AssertExpression('sqr(0.5)',sqr(0.5));
  AssertExpression('sqr(-0.75)',sqr(0.75));
end;

function TestBuiltins_TestFunctionsqrt: TTestString;
begin
  Result := '';
  AssertExpression('sqrt(0.5)',sqrt(0.5));
  AssertExpression('sqrt(0.75)',sqrt(0.75));
end;

function TestBuiltins_TestFunctionexp: TTestString;
begin
  Result := '';
  AssertExpression('exp(1.0)',exp(1));
  AssertExpression('exp(0.0)',1.0);
end;

function TestBuiltins_TestFunctionln: TTestString;
begin
  Result := '';
  AssertExpression('ln(0.5)',ln(0.5));
  AssertExpression('ln(1.5)',ln(1.5));
end;

function TestBuiltins_TestFunctionlog: TTestString;
begin
  Result := '';
  AssertExpression('log(0.5)',ln(0.5)/ln(10.0));
  AssertExpression('log(1.5)',ln(1.5)/ln(10.0));
  AssertExpression('log(10.0)',1.0);
end;

function TestBuiltins_TestFunctionfrac: TTestString;
begin
  Result := '';
  AssertExpression('frac(0.5)',frac(0.5));
  AssertExpression('frac(1.5)',frac(1.5));
end;

function TestBuiltins_TestFunctionint: TTestString;
begin
  Result := '';
  AssertExpression('int(0.5)',int(0.5));
  AssertExpression('int(1.5)',int(1.5));
end;

function TestBuiltins_TestFunctionround: TTestString;
begin
  Result := '';
  AssertExpression('round(0.5)',round(0.5));
  AssertExpression('round(1.55)',round(1.55));
end;

function TestBuiltins_TestFunctiontrunc: TTestString;
begin
  Result := '';
  AssertExpression('trunc(0.5)',trunc(0.5));
  AssertExpression('trunc(1.55)',trunc(1.55));
end;

function TestBuiltins_TestFunctionlength: TTestString;
begin
  Result := '';
  AssertExpression('length(''123'')',3);
end;

function TestBuiltins_TestFunctioncopy: TTestString;
begin
  Result := '';
  AssertExpression('copy(''123456'',2,4)','2345');
end;

function TestBuiltins_TestFunctiondelete: TTestString;
begin
  Result := '';
  AssertExpression('delete(''123456'',2,4)','16');
end;

function TestBuiltins_TestFunctionpos: TTestString;
begin
  Result := '';
  AssertExpression('pos(''234'',''123456'')',2);
end;

function TestBuiltins_TestFunctionlowercase: TTestString;
begin
  Result := '';
  AssertExpression('lowercase(''AbCdEf'')','abcdef');
end;

function TestBuiltins_TestFunctionuppercase: TTestString;
begin
  Result := '';
  AssertExpression('uppercase(''AbCdEf'')','ABCDEF');
end;

function TestBuiltins_TestFunctionstringreplace: TTestString;
begin
  Result := '';
  // last options are replaceall, ignorecase
  AssertExpression('stringreplace(''AbCdEf'',''C'',''Z'',false,false)','AbZdEf');
  AssertExpression('stringreplace(''AbCdEf'',''c'',''Z'',false,false)','AbCdEf');
  AssertExpression('stringreplace(''AbCdEf'',''c'',''Z'',false,true)','AbZdEf');
  AssertExpression('stringreplace(''AbCdEfC'',''C'',''Z'',false,false)','AbZdEfC');
  AssertExpression('stringreplace(''AbCdEfC'',''C'',''Z'',True,false)','AbZdEfZ');
end;

function TestBuiltins_TestFunctioncomparetext: TTestString;
begin
  Result := '';
  AssertExpression('comparetext(''AbCdEf'',''AbCdEf'')',0);
  AssertExpression('comparetext(''AbCdEf'',''ABCDEF'')',0);
  AssertExpression('comparetext(''AbCdEf'',''FEDCBA'')',comparetext('AbCdEf','FEDCBA'));
end;

function TestBuiltins_TestFunctiondate: TTestString;
begin
  Result := '';
  AssertDateTimeExpression('date',date);
end;

function TestBuiltins_TestFunctiontime: TTestString;
begin
  Result := '';
  AssertDateTimeExpression('time',time);
end;

function TestBuiltins_TestFunctionnow: TTestString;
begin
  Result := '';
  AssertDateTimeExpression('now',now);
end;

function TestBuiltins_TestFunctiondayofweek: TTestString;
begin
  Result := '';
  FP.Identifiers.AddDateTimeVariable('D',Date);
  AssertExpression('dayofweek(d)',DayOfWeek(date));
end;

function TestBuiltins_TestFunctionextractyear: TTestString;
Var
  Y,M,D : Word;
begin
  Result := '';
  DecodeDate(Date,Y,M,D);
  FP.Identifiers.AddDateTimeVariable('D',Date);
  AssertExpression('extractyear(d)',Y);
end;

function TestBuiltins_TestFunctionextractmonth: TTestString;
Var
  Y,M,D : Word;
begin
  Result := '';
  FP.Identifiers.AddDateTimeVariable('D',Date);
  DecodeDate(Date,Y,M,D);
  AssertExpression('extractmonth(d)',M);
end;

function TestBuiltins_TestFunctionextractday: TTestString;
Var
  Y,M,D : Word;
begin
  Result := '';
  DecodeDate(Date,Y,M,D);
  FP.Identifiers.AddDateTimeVariable('D',Date);
  AssertExpression('extractday(d)',D);
end;

function TestBuiltins_TestFunctionextracthour: TTestString;
Var
  T : TDateTime;
  H,m,s,ms : Word;
begin
  Result := '';
  T:=Time;
  DecodeTime(T,h,m,s,ms);
  FP.Identifiers.AddDateTimeVariable('T',T);
  AssertExpression('extracthour(t)',h);
end;

function TestBuiltins_TestFunctionextractmin: TTestString;
Var
  T : TDateTime;
  H,m,s,ms : Word;
begin
  Result := '';
  T:=Time;
  DecodeTime(T,h,m,s,ms);
  FP.Identifiers.AddDateTimeVariable('T',T);
  AssertExpression('extractmin(t)',m);
end;

function TestBuiltins_TestFunctionextractsec: TTestString;
Var
  T : TDateTime;
  H,m,s,ms : Word;
begin
  Result := '';
  T:=Time;
  DecodeTime(T,h,m,s,ms);
  FP.Identifiers.AddDateTimeVariable('T',T);
  AssertExpression('extractsec(t)',s);
end;

function TestBuiltins_TestFunctionextractmsec: TTestString;
Var
  T : TDateTime;
  H,m,s,ms : Word;
begin
  Result := '';
  T:=Time;
  DecodeTime(T,h,m,s,ms);
  FP.Identifiers.AddDateTimeVariable('T',T);
  AssertExpression('extractmsec(t)',ms);
end;

function TestBuiltins_TestFunctionencodedate: TTestString;
begin
  Result := '';
  AssertDateTimeExpression('encodedate(2008,10,11)',EncodeDate(2008,10,11));
end;

function TestBuiltins_TestFunctionencodetime: TTestString;
begin
  Result := '';
  AssertDateTimeExpression('encodetime(14,10,11,0)',EncodeTime(14,10,11,0));
end;

function TestBuiltins_TestFunctionencodedatetime: TTestString;
begin
  Result := '';
  AssertDateTimeExpression('encodedatetime(2008,12,13,14,10,11,0)',EncodeDate(2008,12,13)+EncodeTime(14,10,11,0));
end;

function TestBuiltins_TestFunctionshortdayname: TTestString;
begin
  Result := '';
  AssertExpression('shortdayname(1)',ShortDayNames[1]);
  AssertExpression('shortdayname(7)',ShortDayNames[7]);
end;

function TestBuiltins_TestFunctionshortmonthname: TTestString;
begin
  Result := '';
  AssertExpression('shortmonthname(1)',ShortMonthNames[1]);
  AssertExpression('shortmonthname(12)',ShortMonthNames[12]);
end;

function TestBuiltins_TestFunctionlongdayname: TTestString;
begin
  Result := '';
  AssertExpression('longdayname(1)',longDayNames[1]);
  AssertExpression('longdayname(7)',longDayNames[7]);
end;

function TestBuiltins_TestFunctionlongmonthname: TTestString;
begin
  Result := '';
  AssertExpression('longmonthname(1)',longMonthNames[1]);
  AssertExpression('longmonthname(12)',longMonthNames[12]);
end;

function TestBuiltins_TestFunctionformatdatetime: TTestString;
begin
  Result := '';
  AssertExpression('FormatDateTime(''cccc'',Date)',FormatDateTime('cccc',Date));
end;

function TestBuiltins_TestFunctionshl: TTestString;
Var
  I : Int64;
begin
  Result := '';
  AssertExpression('shl(12,3)',12 shl 3);
  I:=12 shl 30;
  AssertExpression('shl(12,30)',I);
end;

function TestBuiltins_TestFunctionshr: TTestString;
begin
  Result := '';
  AssertExpression('shr(12,2)',12 shr 2);
end;

function TestBuiltins_TestFunctionIFS: TTestString;
begin
  Result := '';
  AssertExpression('ifs(true,''string1'',''string2'')','string1');
  AssertExpression('ifs(false,''string1'',''string2'')','string2');
end;

function TestBuiltins_TestFunctionIFF: TTestString;
begin
  Result := '';
  AssertExpression('iff(true,1.0,2.0)',1.0);
  AssertExpression('iff(false,1.0,2.0)',2.0);
end;

function TestBuiltins_TestFunctionIFD: TTestString;
begin
  Result := '';
  FP.Identifiers.AddDateTimeVariable('A',Date);
  FP.Identifiers.AddDateTimeVariable('B',Date-1);
  AssertDateTimeExpression('ifd(true,A,B)',Date);
  AssertDateTimeExpression('ifd(false,A,B)',Date-1);
end;

function TestBuiltins_TestFunctionIFI: TTestString;
begin
  Result := '';
  AssertExpression('ifi(true,1,2)',1);
  AssertExpression('ifi(false,1,2)',2);
end;

function TestBuiltins_TestFunctioninttostr: TTestString;
begin
  Result := '';
  AssertExpression('inttostr(2)','2');
end;

function TestBuiltins_TestFunctionstrtoint: TTestString;
begin
  Result := '';
  AssertExpression('strtoint(''2'')',2);
end;

function TestBuiltins_TestFunctionstrtointdef: TTestString;
begin
  Result := '';
  AssertExpression('strtointdef(''abc'',2)',2);
end;

function TestBuiltins_TestFunctionfloattostr: TTestString;
begin
  Result := '';
  AssertExpression('floattostr(1.23)',Floattostr(1.23));
end;

function TestBuiltins_TestFunctionstrtofloat: TTestString;
Var
  S : String;
begin
  Result := '';
  S:='1.23';
  S[2]:=DecimalSeparator;
  AssertExpression('strtofloat('''+S+''')',1.23);
end;

function TestBuiltins_TestFunctionstrtofloatdef: TTestString;
begin
  Result := '';
  AssertExpression('strtofloatdef(''abc'',1.23)',1.23);
end;

function TestBuiltins_TestFunctionbooltostr: TTestString;
begin
  Result := '';
  AssertExpression('booltostr(True)','True');
end;

function TestBuiltins_TestFunctionstrtobool: TTestString;
begin
  Result := '';
  AssertExpression('strtobool(''0'')',false);
end;

function TestBuiltins_TestFunctionstrtobooldef: TTestString;
begin
  Result := '';
  AssertExpression('strtobooldef(''XYZ'',True)',True);
end;

function TestBuiltins_TestFunctiondatetostr: TTestString;
begin
  Result := '';
  FP.Identifiers.AddDateTimeVariable('A',Date);
  AssertExpression('DateToStr(A)',DateToStr(Date));
end;

function TestBuiltins_TestFunctiontimetostr: TTestString;
Var
  T : TDateTime;
begin
  Result := '';
  T:=Time;
  FP.Identifiers.AddDateTimeVariable('A',T);
  AssertExpression('TimeToStr(A)',TimeToStr(T));
end;

function TestBuiltins_TestFunctionstrtodate: TTestString;
begin
  Result := '';
  FP.Identifiers.AddStringVariable('S',DateToStr(Date));
  AssertDateTimeExpression('StrToDate(S)',Date);
end;

function TestBuiltins_TestFunctionstrtodatedef: TTestString;
begin
  Result := '';
  FP.Identifiers.AddDateTimeVariable('A',Date);
  AssertDateTimeExpression('StrToDateDef(''S'',A)',Date);
end;

function TestBuiltins_TestFunctionstrtotime: TTestString;
Var
  T : TDateTime;
begin
  Result := '';
  T:=Time;
  FP.Identifiers.AddStringVariable('S',TimeToStr(T));
  AssertDateTimeExpression('StrToTime(S)',T);
end;

function TestBuiltins_TestFunctionstrtotimedef: TTestString;
Var
  T : TDateTime;
begin
  Result := '';
  T:=Time;
  FP.Identifiers.AddDateTimeVariable('S',T);
  AssertDateTimeExpression('StrToTimeDef(''q'',S)',T);
end;

function TestBuiltins_TestFunctionstrtodatetime: TTestString;
Var
  T : TDateTime;
  S : String;
begin
  Result := '';
  T:=Now;
  S:=DateTimetostr(T);
  AssertDateTimeExpression('StrToDateTime('''+S+''')',T);
end;

function TestBuiltins_TestFunctionstrtodatetimedef: TTestString;
Var
  T : TDateTime;
  S : String;
begin
  Result := '';
  T:=Now;
  S:=DateTimetostr(T);
  FP.Identifiers.AddDateTimeVariable('S',T);
  AssertDateTimeExpression('StrToDateTimeDef('''+S+''',S)',T);
end;

function TestBuiltins_TestFunctionAggregateSum: TTestString;
begin
  Result := '';
  FP.Identifiers.AddIntegerVariable('S',2);
  AssertAggregateExpression('sum(S)',10,5);
end;

function TestBuiltins_TestFunctionAggregateSumFloat: TTestString;
begin
  Result := '';
  FP.Identifiers.AddFloatVariable('S',2.0);
  AssertAggregateExpression('sum(S)',10.0,5);
end;

function TestBuiltins_TestFunctionAggregateSumCurrency: TTestString;
begin
  Result := '';
  FP.Identifiers.AddCurrencyVariable('S',2.0);
  AssertAggregateCurrExpression('sum(S)',Currency(10.0),5);
end;

function TestBuiltins_TestFunctionAggregateCount: TTestString;
begin
  Result := '';
  AssertAggregateExpression('count',5,5);
end;

procedure DoAverage(var Result: TFPExpressionResult; ConstRef AName: ShortString);
begin
  Inc(FValue);
  Result.ResInteger:=FValue;
  Result.ResultType:=rtInteger;
end;

procedure DoSeries(var Result: TFPExpressionResult; ConstRef AName: ShortString);
Const
  Values : Array[1..10] of double =
  (1.3,1.8,1.1,9.9,1.4,2.4,5.8,6.5,7.8,8.1);
begin
  Inc(FValue);
  Result.ResFloat:=Values[FValue];
  Result.ResultType:=rtFloat;
end;

function TestBuiltins_TestFunctionAggregateAvg: TTestString;
begin
  Result := '';
  FP.Identifiers.AddVariable('S',rtInteger,@DoAverage);
  AssertAggregateExpression('avg(S)',5.5,10);
end;

function TestBuiltins_TestFunctionAggregateMin: TTestString;
begin
  Result := '';
  FP.Identifiers.AddVariable('S',rtFloat,@DoSeries);
  AssertAggregateExpression('Min(S)',1.1,10);
end;

function TestBuiltins_TestFunctionAggregateMax: TTestString;
begin
  Result := '';
  FP.Identifiers.AddVariable('S',rtFloat,@DoSeries);
  AssertAggregateExpression('Max(S)',9.9,10);
end;


procedure InitFileFormatSettings;
begin
  FileFormatSettings := DefaultFormatSettings;
  FileFormatSettings.DecimalSeparator := '.';
  FileFormatSettings.DateSeparator := '-';
  FileFormatSettings.TimeSeparator := ':';
  FileFormatsettings.ShortDateFormat := 'yyyy-mm-dd';
  FileFormatSettings.LongTimeFormat := 'hh:nn:ss';
end;

procedure RegisterTests(aTop : PSuite);
var
  lSuite : PSuite;
begin
  InitFileFormatSettings;
  lSuite:=AddSuite('TBuiltinsManagerTests', @SuiteSetup, @SuiteTearDown,aTop, true);
  AddTest('TestCreate', @TestBuiltinsManager_TestCreate, lSuite);
  AddTest('TestVariable1', @TestBuiltinsManager_TestVariable1, lSuite);
  AddTest('TestVariable2', @TestBuiltinsManager_TestVariable2, lSuite);
  AddTest('TestVariable3', @TestBuiltinsManager_TestVariable3, lSuite);
  AddTest('TestVariable4', @TestBuiltinsManager_TestVariable4, lSuite);
  AddTest('TestVariable5', @TestBuiltinsManager_TestVariable5, lSuite);
  AddTest('TestVariable6', @TestBuiltinsManager_TestVariable6, lSuite);
  AddTest('TestVariable7', @TestBuiltinsManager_TestVariable7, lSuite);
  AddTest('TestFunction1', @TestBuiltinsManager_TestFunction1, lSuite);
  AddTest('TestFunction2', @TestBuiltinsManager_TestFunction2, lSuite);
  AddTest('TestDelete', @TestBuiltinsManager_TestDelete, lSuite);
  AddTest('TestRemove', @TestBuiltinsManager_TestRemove, lSuite);

  lSuite:=AddSuite('TBuiltinsTests', @SuiteSetup, @SuiteTearDown, aTop, True);
  AddTest('TestVariablepi', @TestBuiltins_TestVariablepi, lSuite);
  AddTest('TestFunctioncos', @TestBuiltins_TestFunctioncos, lSuite);
  AddTest('TestFunctionsin', @TestBuiltins_TestFunctionsin, lSuite);
  AddTest('TestFunctionarctan', @TestBuiltins_TestFunctionarctan, lSuite);
  AddTest('TestFunctionabs', @TestBuiltins_TestFunctionabs, lSuite);
  AddTest('TestFunctionsqr', @TestBuiltins_TestFunctionsqr, lSuite);
  AddTest('TestFunctionsqrt', @TestBuiltins_TestFunctionsqrt, lSuite);
  AddTest('TestFunctionexp', @TestBuiltins_TestFunctionexp, lSuite);
  AddTest('TestFunctionln', @TestBuiltins_TestFunctionln, lSuite);
  AddTest('TestFunctionlog', @TestBuiltins_TestFunctionlog, lSuite);
  AddTest('TestFunctionfrac', @TestBuiltins_TestFunctionfrac, lSuite);
  AddTest('TestFunctionint', @TestBuiltins_TestFunctionint, lSuite);
  AddTest('TestFunctionround', @TestBuiltins_TestFunctionround, lSuite);
  AddTest('TestFunctiontrunc', @TestBuiltins_TestFunctiontrunc, lSuite);
  AddTest('TestFunctionlength', @TestBuiltins_TestFunctionlength, lSuite);
  AddTest('TestFunctioncopy', @TestBuiltins_TestFunctioncopy, lSuite);
  AddTest('TestFunctiondelete', @TestBuiltins_TestFunctiondelete, lSuite);
  AddTest('TestFunctionpos', @TestBuiltins_TestFunctionpos, lSuite);
  AddTest('TestFunctionlowercase', @TestBuiltins_TestFunctionlowercase, lSuite);
  AddTest('TestFunctionuppercase', @TestBuiltins_TestFunctionuppercase, lSuite);
  AddTest('TestFunctionstringreplace', @TestBuiltins_TestFunctionstringreplace, lSuite);
  AddTest('TestFunctioncomparetext', @TestBuiltins_TestFunctioncomparetext, lSuite);
  AddTest('TestFunctiondate', @TestBuiltins_TestFunctiondate, lSuite);
  AddTest('TestFunctiontime', @TestBuiltins_TestFunctiontime, lSuite);
  AddTest('TestFunctionnow', @TestBuiltins_TestFunctionnow, lSuite);
  AddTest('TestFunctiondayofweek', @TestBuiltins_TestFunctiondayofweek, lSuite);
  AddTest('TestFunctionextractyear', @TestBuiltins_TestFunctionextractyear, lSuite);
  AddTest('TestFunctionextractmonth', @TestBuiltins_TestFunctionextractmonth, lSuite);
  AddTest('TestFunctionextractday', @TestBuiltins_TestFunctionextractday, lSuite);
  AddTest('TestFunctionextracthour', @TestBuiltins_TestFunctionextracthour, lSuite);
  AddTest('TestFunctionextractmin', @TestBuiltins_TestFunctionextractmin, lSuite);
  AddTest('TestFunctionextractsec', @TestBuiltins_TestFunctionextractsec, lSuite);
  AddTest('TestFunctionextractmsec', @TestBuiltins_TestFunctionextractmsec, lSuite);
  AddTest('TestFunctionencodedate', @TestBuiltins_TestFunctionencodedate, lSuite);
  AddTest('TestFunctionencodetime', @TestBuiltins_TestFunctionencodetime, lSuite);
  AddTest('TestFunctionencodedatetime', @TestBuiltins_TestFunctionencodedatetime, lSuite);
  AddTest('TestFunctionshortdayname', @TestBuiltins_TestFunctionshortdayname, lSuite);
  AddTest('TestFunctionshortmonthname', @TestBuiltins_TestFunctionshortmonthname, lSuite);
  AddTest('TestFunctionlongdayname', @TestBuiltins_TestFunctionlongdayname, lSuite);
  AddTest('TestFunctionlongmonthname', @TestBuiltins_TestFunctionlongmonthname, lSuite);
  AddTest('TestFunctionformatdatetime', @TestBuiltins_TestFunctionformatdatetime, lSuite);
  AddTest('TestFunctionshl', @TestBuiltins_TestFunctionshl, lSuite);
  AddTest('TestFunctionshr', @TestBuiltins_TestFunctionshr, lSuite);
  AddTest('TestFunctionIFS', @TestBuiltins_TestFunctionIFS, lSuite);
  AddTest('TestFunctionIFF', @TestBuiltins_TestFunctionIFF, lSuite);
  AddTest('TestFunctionIFD', @TestBuiltins_TestFunctionIFD, lSuite);
  AddTest('TestFunctionIFI', @TestBuiltins_TestFunctionIFI, lSuite);
  AddTest('TestFunctioninttostr', @TestBuiltins_TestFunctioninttostr, lSuite);
  AddTest('TestFunctionstrtoint', @TestBuiltins_TestFunctionstrtoint, lSuite);
  AddTest('TestFunctionstrtointdef', @TestBuiltins_TestFunctionstrtointdef, lSuite);
  AddTest('TestFunctionfloattostr', @TestBuiltins_TestFunctionfloattostr, lSuite);
  AddTest('TestFunctionstrtofloat', @TestBuiltins_TestFunctionstrtofloat, lSuite);
  AddTest('TestFunctionstrtofloatdef', @TestBuiltins_TestFunctionstrtofloatdef, lSuite);
  AddTest('TestFunctionbooltostr', @TestBuiltins_TestFunctionbooltostr, lSuite);
  AddTest('TestFunctionstrtobool', @TestBuiltins_TestFunctionstrtobool, lSuite);
  AddTest('TestFunctionstrtobooldef', @TestBuiltins_TestFunctionstrtobooldef, lSuite);
  AddTest('TestFunctiondatetostr', @TestBuiltins_TestFunctiondatetostr, lSuite);
  AddTest('TestFunctiontimetostr', @TestBuiltins_TestFunctiontimetostr, lSuite);
  AddTest('TestFunctionstrtodate', @TestBuiltins_TestFunctionstrtodate, lSuite);
  AddTest('TestFunctionstrtodatedef', @TestBuiltins_TestFunctionstrtodatedef, lSuite);
  AddTest('TestFunctionstrtotime', @TestBuiltins_TestFunctionstrtotime, lSuite);
  AddTest('TestFunctionstrtotimedef', @TestBuiltins_TestFunctionstrtotimedef, lSuite);
  AddTest('TestFunctionstrtodatetime', @TestBuiltins_TestFunctionstrtodatetime, lSuite);
  AddTest('TestFunctionstrtodatetimedef', @TestBuiltins_TestFunctionstrtodatetimedef, lSuite);
  AddTest('TestFunctionAggregateSum', @TestBuiltins_TestFunctionAggregateSum, lSuite);
  AddTest('TestFunctionAggregateSumFloat', @TestBuiltins_TestFunctionAggregateSumFloat, lSuite);
  AddTest('TestFunctionAggregateSumCurrency', @TestBuiltins_TestFunctionAggregateSumCurrency, lSuite);
  AddTest('TestFunctionAggregateCount', @TestBuiltins_TestFunctionAggregateCount, lSuite);
  AddTest('TestFunctionAggregateAvg', @TestBuiltins_TestFunctionAggregateAvg, lSuite);
  AddTest('TestFunctionAggregateMin', @TestBuiltins_TestFunctionAggregateMin, lSuite);
  AddTest('TestFunctionAggregateMax', @TestBuiltins_TestFunctionAggregateMax, lSuite);
end;

end.
