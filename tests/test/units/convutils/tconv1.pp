program tconv1;
{$ifdef windows}
{$apptype console}
{$endif}
{$mode objfpc}{$h+}

uses
  sysutils, classes, math,
  {$ifdef UseLocal}
   _convutils   //local copy to test with older compiler (BB)
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
    writeln(format('%d errors encountered',[ErrorCount]));
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
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  Fam := RegisterConversionFamily('TestZeroFactor');
  ubase := RegisterConversionType(Fam,'ubase',1);
  try
    uzero := RegisterConversionType(Fam, 'uzero', 0);
    if Verbose then writeln('uzero=',uzero);
    LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception (EZeroDivide) expected but not raised in RegisterConversionType(Fam, ''uzero'', 0)');
  except
    // D7: Exception EZeroDivide uzero has a factor of zero.
    on E: EZeroDivide do if Verbose then writeln(E.ClassName,': ',E.Message,' [as expected]');
    on Ex: Exception do LogError({$I %CurrentRoutine%},  {$I %Line%},'Expected EZeroDivide, got another type of Exception: '+Ex.ClassName);
  end;
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
    //raised an exception in 3.2 series
    on E: Exception do LogError({$I %CurrentRoutine%},  {$I %Line%},format('Unexpected excpetion %s: %s',[E.ClassName,E.Message]));
  end;
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;

procedure TestNoDescription;
var
  Fam: TConvFamily;
  atype: TConvType;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  try
    Fam := RegisterConversionFamily('');
    if Verbose then writeln('Fam=',Fam);
    LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception expected but not raised in RegsiterConversionFamily');
  except
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,' [as expected]');
  end;
  try
    Fam := RegisterConversionFamily(' ');
    if Verbose then writeln('Fam=',Fam);
    LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception expected but not raised in RegisterConversionFamily');
  except
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,' [as expected]');
  end;
  Fam := RegisterConversionFamily('TestNoDescription');
  try
    atype := RegisterConversionType(Fam,'',1);
    if Verbose then writeln('atype=',atype);
    LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception expected but not raised in RegisterConversionType');
  except
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,' [as expected]');
  end;
  try
    atype := RegisterConversionType(Fam,' ',1);
    if Verbose then writeln('atype=',atype);
    LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception expected but not raised in RegisterConversionType');
  except
    on E: EConversionError do if Verbose then writeln('EConversionError: ',E.Message,' [as expected]');
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

  B := CompatibleConversionTypes(atype1, atype2); {notice: the s at the end: xxxTypes, not xxxType}
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
  Result := AValue*2;
end;

function DummyFromProc(const AValue: Double): Double;
begin
  Result := AValue/2;
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
    dummy_1 := RegisterConversionType(Fam, 'dummy_1', @DummyToProc, nil);
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
    dummy_2 := RegisterConversionType(Fam, 'dummy_2', nil, @DummyFromProc);
    if Verbose then writeln(dummy_2);
    //Currently by design we raise an exception here (EZeroDivide), but not compatible with D7
    {$ifdef fpc}
    LogError({$I %CurrentRoutine%},  {$I %Line%},'Exception expected but not raised in RegisterConversionType with nil as 4th parameter');
    Exit;
    {$endif}
  except
    on E: Exception do
    begin
      if Verbose then writeln(E.ClassName,': ',E.Message,'[ as expected: by design but not Delphi compatible...]');
    end;
  end;
  //D7 raise EAccesViolations on Convert()
  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;

procedure TestTryStrToConvUnit;
var
  Fam1, Fam2: TConvFamily;
  type_1, type_leading, type_trailing, FoundType,
    type_leading_and_trailing: TConvType;
  Value, ExpValue: Double;
  FoundRes: Boolean;
  CurRoutine: String;
  procedure Check(Line: String; ExpRes: Boolean; ExpType: TConvType; ExpValue: Double);
  const
    BoolStr: array[Boolean] of string = ('False','True');
  begin
    if Verbose then writeln('Got ',FoundRes,', Expected ',ExpRes);
    if (ExpRes <> FoundRes) then
    begin
      LogError(CurRoutine, Line ,format('Expected TryStrToConvUnit()=%s, Got %s',[BoolStr[ExpRes],BoolStr[FoundRes]]));
    end
    else
    begin
      if not ExpRes then Exit;  //if TryStrToConvUnit should fail, the returned values are undefined
      if Verbose then writeln(format('Expected restype=%d, got restype=%d',[ExpType,FoundType]));
      if Verbose then writeln(format('Expected Value=%1.8f, got Value=%1.8f',[ExpValue, Value]));
      if (ExpType <> FoundType) then
      begin
        LogError(CurRoutine, Line, format('Expected restype=%d, got restype=%d',[ExpType,FoundType]));
      end
      else
      begin
        if not SameValue(Value, ExpValue, 1E-9) then
          LogError(CurRoutine, Line, format('Expected Value=%1.8f, got Value=%1.8f',[ExpValue, Value]));
      end;
    end;
    if Verbose then writeln;
  end;
begin
  if Verbose then writeln({$I %CurrentRoutine%});
  CurRoutine := {$I %CurrentRoutine%};
  Fam1 := RegisterConversionFamily('TestTryStrToConvUnit_1');
  Fam2 := RegisterConversionFamily('TestTryStrToConvUnit_2');
  type_1 := RegisterConversionType(Fam1, 'type1',1.0);
  type_leading := RegisterConversionType(Fam2, '  leading',1.0);
  type_trailing:= RegisterConversionType(Fam1, 'trailing  ',1.0);
  type_leading_and_trailing := RegisterConversionType(Fam1, '  leading and trailing  ',1);
  DefaultFormatSettings.DecimalSeparator := '.';
  ExpValue := 1.23;

  if Verbose then writeln('TryStrToConvUnit(''1.23 type1'',Value, FoundType)');
  FoundRes := TryStrToConvUnit('1.23 type1',Value, FoundType);
  Check({$I %Line%}, True, type_1, ExpValue);

  if Verbose then writeln('TryStrToConvUnit(''1.23  '',Value, FoundType)  //no description');
  FoundRes := TryStrToConvUnit('1.23  ',Value, FoundType);
  Check({$I %Line%}, False, $FFFF, 0);

  if Verbose then writeln('TryStrToConvUnit(''1.23       leading'',Value, FoundType)  //leading spaces in description');
  FoundRes := TryStrToConvUnit('1.23        leading',Value, FoundType);
  Check({$I %Line%}, True, type_leading, ExpValue);

  if Verbose then writeln('TryStrToConvUnit(''1.23 trailing      '',Value, FoundType)  //trailing spaces in description');
  FoundRes := TryStrToConvUnit('1.23 trailing      ',Value, FoundType);
  Check({$I %Line%}, True, type_trailing, ExpValue);

  if Verbose then writeln('TryStrToConvUnit(''1.23    leading and trailing      '',Value, FoundType)  //leading and trailing spaces in description');
  FoundRes := TryStrToConvUnit('1.23    leading and trailing      ',Value, FoundType);
  Check({$I %Line%}, True, type_leading_and_trailing, ExpValue);

  if Verbose then writeln('TryStrToConvUnit('' type1'',AValue, FoundType)');
  FoundRes := TryStrToConvUnit(' type1',Value, FoundType);
  Check({$I %Line%}, False, $FFFF, 0);

  if Verbose then writeln('TryStrToConvUnit('' 1.23 type1'',AValue, FoundType)  //leading space before value');
  FoundRes := TryStrToConvUnit(' 1.23 type1',Value, FoundType);
  Check({$I %Line%}, False, $FFFF, 0);

  if Verbose then writeln('TryStrToConvUnit(''1.23 nonexisting'',AValue, FoundType)');
  FoundRes := TryStrToConvUnit('1.23 nonexisting',Value, FoundType);
  Check({$I %Line%}, False, $FFFF, 0);

  //previously caused a range check error
  if Verbose then writeln('TryStrToConvUnit(''1.23'',AValue, FoundType)  //no description');
  FoundRes := TryStrToConvUnit('1.23',Value, FoundType);
  Check({$I %Line%}, False, $FFFF, 0);

  if Verbose then writeln('TryStrToConvUnit('''',AValue, FoundType)  //empty string');
  FoundRes := TryStrToConvUnit('',Value, FoundType);
  Check({$I %Line%}, False, $FFFF, 0);

  if Verbose then writeln({$I %CurrentRoutine%},' end.');
  if Verbose then writeln;
end;

begin
  ParseParams;
  InitLog;
  try
    TestRegisterAndListFamilies;
    TestRegisterAndUnregisterFamilies;
    TestRegisterAndUnregisterConvTypes;
    TestIllegalConvTypeToFamily;
    TestIllegalConvTypeToFamily2;
    TestDuplicateConvFamilyDescription;
    TestDuplicateConvTypeDescriptions;
    RegisterWithNegativeFactor;
    TestNoDescription;
    TestCompatibleConversionType;
    TestZeroFactor;
    TestNilProc;
    TestTryStrToConvUnit;
  finally
    PrintLog;
    DoneLog;
    ExitCode := ErrorCount;
  end;
end.
