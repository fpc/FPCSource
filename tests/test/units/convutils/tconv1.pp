program tconv1;
{$apptype console}
{$ifdef fpc}
{$mode objfpc}{$h+}
{$endif}

uses
  sysutils, classes, math,
  {$ifdef UseLocal}
   _convutils   //local copy to test with 3.2 compiler
  {$else}
  convutils
  {$endif};

var
  TheLog: TStringList;
  ErrorCount: Integer;
  Verbose: Boolean;

procedure ParseParams;
var
  i: Integer;
begin
  Verbose := False;
  for i := 1 to ParamCount do
    if (CompareText(ParamStr(i),'--verbose') = 0) then
    begin
      Verbose := True;
      Exit;
    end;
end;

procedure InitLog;
begin
  TheLog := TStringList.Create;
  ErrorCount := 0;
end;

procedure DoneLog;
begin
  TheLog.Free;
end;

procedure PrintLog;
var
  i: Integer;
begin
  if TheLog.Count > 0 then
  begin
    writeln(format('%d errors encountered',[TheLog.Count div 2]));
    for i := 0 to TheLog.Count-1 do
      writeln(TheLog[i]);
  end
  else
    writeln('No Errors encountered');
end;

procedure LogError(const Test, Line, Msg: String);
begin
  Inc(ErrorCount);
  TheLog.Add(Test);
  TheLOg.Add(format('  (%d) line %s: %s',[ErrorCount,Line, Msg]));
end;

procedure TestRegisterAndListFamilies;
var
  Fam, FirstFam, SecFam: TConvFamily;
  Fams: TConvFamilyArray;
  tuFirstType, tuSecType: TConvType;
  Len, ExpLen: Integer;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  GetConvFamilies(Fams);
  Len := Length(Fams);
  if (Len=0) then
    if Verbose then writeln('Nothing registered yet');
  if Verbose then writeln('Length(Fams)=',Len);
  if Verbose and (Len>0) then
  begin
  for Fam := Low(Fams) to High(Fams) do
    writeln(format('%d: "%s"',[ord(fam),ConvFamilyToDescription(fam)]));
  end
  else
  begin
    if Verbose then writeln('Testing with out of bound values');
    try
      if Verbose then writeln('ConvFamilyToDescription(0)="',ConvFamilyToDescription(0),'"');
      if Verbose then writeln('ConvFamilyToDescription(1)="',ConvFamilyToDescription(1),'"');
    except
      on E: Exception do LogError({$I %CurrentRoutine%},  {$I %Line%}, 'Unexpected exception testing ConvTypeToDescrition with out of bounds value: '+
        E.ClassName+': '+E.Message);
    end;
  end;
  if Verbose then writeln;

  if Verbose then writeln('Registering First and Second Family');
  FirstFam := RegisterConversionFamily('First Family');
  tuFirstType := RegisterConversionType(FirstFam, 'FirstType', 123.0);
  SecFam := RegisterConversionFamily('Second Family');
  tuSecType := RegisterConversionType(SecFam, 'SecondType', 321.0);
  GetConvFamilies(Fams);
  ExpLen := Len+2;
  Len := Length(Fams);
  if Verbose then write('Length(Fams)=',Len);
  if (Len<>ExpLen) then
  begin
    if Verbose then writeln('  FAIL: Expected ',ExpLen);
    LogError({$I %CurrentRoutine%},  {$I %Line%}, format('Unexpected value of Len, Expected %d, Got %d',[Len,ExpLen]));
    Exit;
  end;
  if Verbose then writeln;
  if Verbose and (Len>0) then
  begin
  for Fam := Low(Fams) to High(Fams) do
    writeln(format('%d: "%s"',[ord(fam),ConvFamilyToDescription(fam)]));
  end;
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;


procedure TestIllegalConvTypeToFamily;
var
  Fam: TConvFamily;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  try
    Fam := ConvTypeToFamily($FFFF);
    if Verbose then writeln('ConvTypeToFamily($FFFF)=',Fam);
    LogError({$I %CurrentRoutine%},  {$I %Line%}, 'Exception expected but not raised in ConvTypToFamily($FFFF)');
  except
    //Exception in D7, docs say it should return 0   EConversionError: Unknown conversion type [$0000FFFF]
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,' [as expected]');
  end;
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;

procedure TestIllegalConvTypeToFamily2;
var
  Fam, Fam1, Fam2: TConvFamily;
  atype1, atype2: TConvType;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  Fam1 := RegisterConversionFamily('TestIllegalConvTypeToFamily2_1');
  Fam2 := RegisterConversionFamily('TestIllegalConvTypeToFamily2_2');
  atype1 := RegisterConversionType(Fam1, 'foo', 1.0);
  atype2 := RegisterConversionType(Fam2, 'bar', 1.0);
  try
    Fam := ConvTypeToFamily(atype1, atype2);
    if Verbose then writeln('ConvTypeToFamily(atype, atype2)=',Fam);
    LogError({$I %CurrentRoutine%},  {$I %Line%}, 'Exception expected but not raised in ConvTypeToFamily(atype1, atype2)');
    Exit;
  except
    //Exception in D7, docs say it should return 0   EConversionError: Unknown conversion type [foo, bar]
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,'[ as expected]');
  end;
  try
    Fam := ConvTypeToFamily($1234, atype2);
    if Verbose then writeln('ConvTypeToFamily(atype, atype2)=',Fam);
    LogError({$I %CurrentRoutine%},  {$I %Line%}, 'Exception expected but not raised in ConvTypeToFamily($1234, atype2)');
  except
    //Exception in D7, docs say it should return 0   EConversionError: Unknown conversion type [$1234, bar]
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,'[ as expected]');
  end;
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;

procedure TestDuplicateConvFamilyDescription;
var
  Fam: TConvFamily;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  Fam := RegisterConversionFamily('Another Family Just To Make The Next On Non Zero');
  if Verbose then writeln('First call to RegisterConversionFamily(''Another Family Just To Make The Next On Non Zero'')=',Fam);

  Fam := RegisterConversionFamily('Gets_A_Duplicate');
  if Verbose then writeln('First call to RegisterConversionFamily(''Gets_A_Duplicate'')=',Fam);

  try
    Fam := RegisterConversionFamily('Gets_A_Duplicate');
    if Verbose then writeln('Second call to RegisterConversionFamily(''Gets_A_Duplicate'')=',Fam);
    LogError({$I %CurrentRoutine%},  {$I %Line%}, 'Exception expected but not raised in RegisterConversionFamily(''Gets_A_Duplicate'')');
  except
    //D7: EConversionError: Conversion family (Gets_A_Duplicate) already registered  //Doc's don't specify the behaviour
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,' [as expected]');
  end;
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;

procedure TestRegisterAndUnregisterFamilies;
var
  Fam, FamLast, CheckLast: TConvFamily;
  foo, bar: TConvType;
  Fams: TConvFamilyArray;
  Len, NewLen: Integer;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  Fam := RegisterConversionFamily('1');
  Fam := RegisterConversionFamily('2');
  Fam := RegisterConversionFamily('3');
  Fam := RegisterConversionFamily('To Be UnRegistered');
  FamLast := RegisterConversionFamily('Last');
  DescriptionToConvFamily('Last', CheckLast);
  GetConvFamilies(Fams);
  Len := Length(Fams);
  if Verbose then writeln('Fam=',Fam,', Nr. of Families=',len, ', CheckLast=',CheckLast);

  foo := RegisterConversionType(Fam,'foo',1.0);
  UnregisterConversionFamily(Fam);
  GetConvFamilies(Fams);
  NewLen := Length(Fams);
  if (NewLen <> Len-1) then
  begin
    LogError({$I %CurrentRoutine%},  {$I %Line%},format('After unexpected value for NewLen after UnregisterConversionFamily: Expected %d, got %d ',[Len-1,NewLen]));
    Exit;
  end;
  DescriptionToConvFamily('Last', CheckLast);
  if (CheckLast<>FamLast) then
  begin
    LogError({$I %CurrentRoutine%},  {$I %Line%},format('After unexpected value for CheckLast after UnregisterConversionFamily: Expected %d, got %d ',[FamLAst,CheckLast]));
  end;
  if Verbose then writeln('After UnregisterFamily, Nr. of Families=',NewLen, ', CheckLast=',CheckLast);
  try
    bar := RegisterConversionType(Fam, 'bar',1.0);
    if Verbose then writeln('bar=',bar);
    LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception expected but not raised in RegisterConversionType(Fam, ''bar'',1.0)');
    Exit;
  except
    //D7 EConversionError: Unknown conversion family [$00000001]
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,' [as expected]');
  end;
  if Verbose then write('UnregisterConversionFamily($FFFF): ');
  try
    UnregisterConversionFamily($FFFF);
  except
    on E: Exception do
      LogError({$I %CurrentRoutine%},  {$I %Line%},format('Unexpected exception raised in UnregisterConversionFamily($FFFF): %s: %s',[E.ClassName,E.Message]));
  end;
  if Verbose then writeln('Done');
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;

procedure TestRegisterAndUnregisterConvTypes;
var
  Fam: TConvFamily;
  AType, ALastType, AFirstType: TConvType;
  ConvTypes: TConvTypeArray;
  Len, NewLen: Integer;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  Fam := RegisterConversionFamily('TestRegisterAndUnregisterConvTypes');
  AFirstType := RegisterConversionType(Fam,'1',1);
  AType := RegisterConversionType(Fam,'2',2);
  AType := RegisterConversionType(Fam,'3',3);
  ALastType := RegisterConversionType(Fam,'Last',4);
  GetConvTypes(Fam,ConvTypes);
  Len := Length(ConvTypes);
  if Verbose then writeln('Length(ConvTypes)=',Len,', ALastType=',ALastType);
  UnregisterConversionType(AFirstType);
  GetConvTypes(Fam,ConvTypes);
  NewLen := Length(ConvTypes);
  if (NewLen<>Len-1) then
  begin
    LogError({$I %CurrentRoutine%},  {$I %Line%},format('Unexpected value for NewLen after UnregisterConversionType, Expected %d, Got %d',[Len-1,NewLen]));
  end;
  try
    Fam := ConvTypeToFamily(AFirstType);
    if Verbose then writeln('ConvTypeToFamily(AFirstType)=',Fam,', tuFirstType=',AFirstType);
    LogError({$I %CurrentRoutine%},  {$I %Line%}, 'Exception expected but not raised in ConvTypeToFamily(tuFirstType) after unregister');
  except
    //D7 EConversionError: Unknown conversion type [$00000001], tuFirstType=1
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,', AFirstType=',AFirstType,' [as expected]');
  end;
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;


procedure TestDuplicateConvTypeDescriptions;
var
  Fam: TConvFamily;
  foo, bar: TConvType;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  Fam := RegisterConversionFamily('TestIdenticalConvDescriptions');
  if Verbose then writeln('Fam=',Fam);
  foo := RegisterConversionType(Fam,'foo',1.0);
  if Verbose then writeln('Foo=',foo);
  try
    bar := RegisterConversionType(Fam,'foo',2.0);
    if Verbose then writeln('FAIL: Exception expected but got: bar=',bar);
    LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception expected but not raised in RegisterConversionType');
  except
    //D7: EConversionError: Conversion type (foo) already registered in TestDuplicateConvTypeDescriptions
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,' [as expected]');
  end;

  try
    bar := RegisterConversionFamily('TestIdenticalConvDescriptions');
    if Verbose then writeln('FAIL: Exception expected but got: Fam=',Fam);
    LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception expected but not raised in RegisterConversionFamily');
  except
    //D7: EConversionError: Conversion family (TestDuplicateConvTypeDescriptions) already registered
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,' [as expected]');
  end;
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;

procedure TestZeroFactor;
var
  Fam: TConvFamily;
  uzero, ubase: TConvType;
  D: Double;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  Fam := RegisterConversionFamily('TestZeroFactor');
  try
    uzero := RegisterConversionType(Fam, 'uzero', 0);
    LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception (EZeroDivide) expected but not raised in when registering uzero)');
  except
    on E: EZeroDivide do if Verbose then writeln('Exception ',E.ClassName,': ',E.Message,' [as expected]');
    else LogError({$I %CurrentRoutine%},  {$I %Line%},'Expected EZeroDivde, got another type of Exception');
  end;
  if Verbose then writeln('uzero=',uzero);
  ubase := RegisterConversionType(Fam, 'ubase', 1.0);
  if Verbose then writeln('ubase=',ubase);
  try
    if Verbose then write('Convert(123.0, ubase, uzero)=');
    D := Convert(123.0, ubase, uzero);
    if Verbose then writeln(D:10:4);
    LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception (EZeroDivide) expected but not raised in Convert(123.0, ubase, uzero)');
  except
    //D7: Exception EZeroDivide in module dconv.exe at 00009EE9.
    // uzero has a factor of zero.                                    // = SConvFactorZero
    // program crashes and Except statement is not executed...
    on E: EZeroDivide do if Verbose then writeln('Exception ',E.ClassName,': ',E.Message,' [as expected]');
    on Ex : Exception do
       LogError({$I %CurrentRoutine%},  {$I %Line%},'Expected EZeroDivde, got another type of Exception: '+Ex.ClassName);
  end;

  try
    D := Convert(123.0, uzero,ubase);
    if Verbose then writeln('Convert(123.0, uzero,ubase)=',D:10:4);
  except
    //D7: Exception EZeroDivide in module dconv.exe at 00009EE9.
    //by design we do not, since no division by zero is actually involved here, we multiply bu zero, whic AFAIC is allowed in the real world
    on E: Exception do
    begin
      if Verbose then writeln('Unexpected Exception ',E.ClassName,': ',E.Message);
      LogError({$I %CurrentRoutine%},  {$I %Line%},format('Unexpected Exception %s: %s',[E.ClassName, E.Message]));
    end;
  end;
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;

procedure TestRegisterTypeTwice;
var
  Fam: TConvFamily;
  base, foo: TConvType;
  D, Exp: Double;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  Fam := RegisterConversionFamily('TestRegisterTypeTwice');
  base := RegisterConversionType(Fam, 'base', 1.0);
  foo := RegisterConversionType(Fam, 'foo', 2.0);
  foo := RegisterConversionType(Fam, 'bar', 100);
  Exp := 0.01;
  D := Convert(1.0, base, foo);
  if Verbose then writeln('D=',D:10:4,' [Expected=0.0100]');   //uses last registered conversionfactor in both fpc and D7
  if not SameValue(D, Exp, 1E-9) then
    LogError({$I %CurrentRoutine%},  {$I %Line%},format('Unexpected value for D, Got %.4f, expected .4f',[D,0.01]));
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;


procedure RegisterWithNegativeFactor;
var
  Fam: TConvFamily;
  atype: TConvType;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  if Verbose then writeln;
  Fam := RegisterConversionFamily('NegativeFactor');
  try
    atype := RegisterConversionType(Fam, 'foo', -10.0);
  except
    on E: Exception do LogError({$I %CurrentRoutine%},  {$I %Line%},format('Unexpected excpetion %s: %s',[E.ClassName,E.Message]));
  end;
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;

procedure TestNoDescription;
var
  Fam, Fam2: TConvFamily;
  atype, atype2: TConvType;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  try
    Fam := RegisterConversionFamily('');
    if Verbose then writeln('Fam=',Fam);
    if Verbose then writeln('ConvFamilyToDescription(Fam)="',ConvFamilyToDescription(Fam),'"');
    if DescriptionToConvFamily('',Fam2) then
    begin
      if Verbose then writeln('DescriptionToConvFamily('''')->',Fam)
    end
    else
    begin
      if Verbose then writeln('DescriptionToConvFamily=FALSE');
      LogError({$I %CurrentRoutine%},  {$I %Line%},'DescriptionToConvFamily('''') returned False but expected True');
    end;
    atype := RegisterConversionType(Fam, '', 1.0);
    if Verbose then writeln('atype=',atype);
    if Verbose then writeln('ConvTypeToDescription(atype)="',ConvTypeToDescription(atype),'"');
    if Verbose then
    begin
      if DescriptionToConvType('',atype2) then
      writeln('DescriptionToConvType('''',atype2)->',atype2)
      else
      begin
        writeln('DescriptionToConvType=FALSE');
        LogError({$I %CurrentRoutine%},  {$I %Line%},'DescriptionToConvFamily('''') returned False, but expected True');
      end;
    end;

  except
    on E: EConversionError do LogError({$I %CurrentRoutine%},  {$I %Line%},format('EConversionError: %s',[E.Message]));
  end;
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;

procedure TestCompatibleConversionType;
var
  Fam1, Fam2: TConvFamily;
  atype1, atype2: TConvType;
  B: Boolean;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  Fam1 := RegisterConversionFamily('TestCompatibleConversionType_1');
  atype1 := RegisterConversionType(Fam1,'TestCompatibleConversionType_1',1.0);
  Fam2 := RegisterConversionFamily('TestCompatibleConversionType_2');
  atype2 := RegisterConversionType(Fam2,'TestCompatibleConversionType_2',1.0);
  //D7: CompatibleConversionType(atype1, atype2)=FALSE

  B := CompatibleConversionTypes(TConvType(atype1), TConvType(atype2)); {notice: the s at the end: xxxTypes, not xxxType}
  if Verbose then writeln('CompatibleConversionTypes(atype1, atype2)=',B);
  if B then
    LogError({$I %CurrentRoutine%},  {$I %Line%},'CompatibleConversionType(atype1, atype2) returned True, but expected False');

  B := CompatibleConversionType(atype1, fam2);
  if Verbose then writeln('CompatibleConversionType(atype1, fam2)=',B);
  if B then
    LogError({$I %CurrentRoutine%},  {$I %Line%},'CompatibleConversionType(atype1, fam2) returned True, but expected False');

  B := CompatibleConversionType(atype1, $FFFF);
  if Verbose then writeln('CompatibleConversionType(atype1, $FFFF)=',B);
  if B then
    LogError({$I %CurrentRoutine%},  {$I %Line%},'CompatibleConversionType(atype1, $FFFF) returned True, but expected False');

  try
  B := CompatibleConversionType($FFFF, fam1);
  if Verbose then writeln('CompatibleConversionType($FFFF, fam1)=',B);
  LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception expected but not raised in CompatibleConversionType($FFFF, fam1)');
  except
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,' [as expected]');
  end;
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;

function DummyToProc(const AValue: Double): Double;
begin
  Result := 123.456 + random(10);
end;

function DummyFromProc(const AValue: Double): Double;
begin
  Result := -987.654+ random(10);
end;

procedure TestNilProc;
var
  Fam: TConvFamily;
  dummy_1, dummy_2: TConvType;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  Fam := RegisterConversionFamily('TestNilProc');
  try
    if Verbose then write('RegisterConversionType(Fam, ''dummy_1'', @DummyToProc, nil)=');
    dummy_1 := RegisterConversionType(Fam, 'dummy_1', {$ifdef fpc}@{$endif}DummyToProc, nil);
    if Verbose then writeln(dummy_1);
  except
    on E: Exception do
    begin
      if Verbose then writeln(E.ClassName,': ',E.Message);
      LogError({$I %CurrentRoutine%},  {$I %Line%},format('Unexpected Exception raised in RegsiterConversionType with nil as 3rd parameter: %s: %s',[E.ClassName,E.Message]));
      Exit;
    end;
  end;
  try
    if Verbose then write('RegisterConversionType(Fam, ''dummy_2'', nil, @DummyFromProc)=');
    dummy_2 := RegisterConversionType(Fam, 'dummy_2', nil, {$ifdef fpc}@{$endif}DummyFromProc);
    if Verbose then writeln(dummy_2);
    //Currently by design we raise an exception here (EZeroDivide), but not compatible with D7
    {$ifdef fpc}
    LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception expected but not raised in RegisterConversionType with nil as 4th parameter');
    Exit;
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef fpc}
      if Verbose then writeln(E.ClassName,': ',E.Message,'[ as expected: by design but not Delphi compatible]');
      {$else}
      LogError({$I %CurrentRoutine%},  {$I %Line%},format('Unexpected exception raised in RegisterConversionType with nil as 4th parameter: %s: %s',[E.ClassName, E.Message]));
      Exit;
      {$endif}
    end;
  end;
  //D7 raise EAccesViolations on Convert()
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;


begin
  Randomize;
  ParseParams;
  InitLog;
  {$ifdef fpc}
  writeln('FPC');
  {$else}
  writeln('Delphi');
  {$endif}

  try

    //{
    TestRegisterAndListFamilies;
    TestRegisterAndUnregisterFamilies;
    TestRegisterAndUnregisterConvTypes;
    TestIllegalConvTypeToFamily;
    TestIllegalConvTypeToFamily2;
    TestDuplicateConvFamilyDescription;
    TestDuplicateConvTypeDescriptions;
    TestRegisterTypeTwice;
    RegisterWithNegativeFactor;
    TestNoDescription;
    TestCompatibleConversionType;
    {$ifndef fpc}
    try
    {$endif}
   // TestZeroFactor;
    {$ifndef fpc}
    except
      //My D7 refues to chat the exception inside the TestZeroFactor procedure
      on E: EZeroDivide do if Verbose then writeln('Catched an EZeroDivide after/outside TestZeroFactor (D7?)');
    end;
    {$endif}
    TestNilProc;
    //}

  finally
    PrintLog;
    DoneLog;
    ExitCode := ErrorCount;
  end;
end.



