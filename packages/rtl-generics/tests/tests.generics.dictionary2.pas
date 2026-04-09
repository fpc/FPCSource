unit tests.generics.dictionary2;

{$mode objfpc}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Generics.Defaults, Generics.Collections;

Type

  { TTestGenDictionary }

  generic TTestGenDictionary<T> = Class(TTestCase)
  Private
    FDict : T;
    FAddMethod: (amAdd, amAddPair, amTryAdd, amAddOrSet, amGetOrAdd);
    //FCheckOrder = ()
  Public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure RecreateDict;

    procedure DoTestAddByMethod(ATestVal: integer; AnAddDup: Boolean = False); virtual;
    procedure DoTestAdd(ATestVal: integer);
    procedure DoTestRemove(ATestVal: integer);
    procedure AssertKeyVal(ATestVal: integer);
    procedure AssertKeyNotFound(ATestVal: integer);
    procedure AssertEmpty;
    procedure AssertCount(AnExpCount: integer);

    procedure DoRunTestAdd;
  Published
    Procedure TestAddModify;

    Procedure TestAdd;
    Procedure TestAddPair;
    //Procedure TestTryAdd;
    Procedure TestAddOrSet;
    //Procedure TestGetOrAdd;
  end;

  { TTestGenDictionary_TryAdd }

  generic TTestGenDictionary_TryAdd<T> = Class(specialize TTestGenDictionary<T>)
  public
    procedure DoTestAddByMethod(ATestVal: integer; AnAddDup: Boolean = False); override;
  published
    Procedure TestTryAdd;
    Procedure TestGetOrAdd;
  end;


  TTestHashFactory = class(TGenericsHashFactory)
  public
    class function GetHashService: THashServiceClass; override;
    class function GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32 = 0): UInt32; override;
  end;

  TTestHashFactory2 = class(TGenericsHashFactory)
  public
    class function GetHashService: THashServiceClass; override;
    class function GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32 = 0): UInt32; override;
  end;


  TTestDoubleHashFactory = class(TDelphiDoubleHashFactory)
  public
    class function GetHashService: THashServiceClass; override;
    class function GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32 = 0): UInt32; override;
    //class procedure GetHashList(AKey: Pointer; ASize: SizeInt; AHashList: PUInt32; AOptions: TGetHashListOptions = []); override;
  end;

  TTestDoubleHashFactory2 = class(TDelphiDoubleHashFactory)
  public
    class function GetHashService: THashServiceClass; override;
    class function GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32 = 0): UInt32; override;
    //class procedure GetHashList(AKey: Pointer; ASize: SizeInt; AHashList: PUInt32; AOptions: TGetHashListOptions = []); override;
  end;



implementation

class function TTestHashFactory.GetHashService: THashServiceClass;
begin
  Result := specialize THashService<TTestHashFactory>;
end;

class function TTestHashFactory.GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32
  ): UInt32;
var
  s: AnsiString;
begin
  s := pchar(AKey);
  result := UInt32(StrToInt(s));
end;

class function TTestHashFactory2.GetHashService: THashServiceClass;
begin
  Result := specialize THashService<TTestHashFactory2>;
end;

class function TTestHashFactory2.GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32
  ): UInt32;
var
  s: AnsiString;
begin
  s := pchar(AKey);
  result := UInt32(StrToInt(s)) << 12; // lots of clashes for: bucket := hash and mask;
end;

class function TTestDoubleHashFactory.GetHashService: THashServiceClass;
begin
  Result := specialize TExtendedHashService<TTestDoubleHashFactory>;
end;

class function TTestDoubleHashFactory.GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32
  ): UInt32;
var
  s: AnsiString;
begin
  s := pchar(AKey);
  result := UInt32(StrToInt(s));
end;

class function TTestDoubleHashFactory2.GetHashService: THashServiceClass;
begin
  Result := specialize TExtendedHashService<TTestDoubleHashFactory2>;
end;

class function TTestDoubleHashFactory2.GetHashCode(AKey: Pointer; ASize: SizeInt; AInitVal: UInt32
  ): UInt32;
var
  s: AnsiString;
begin
  s := pchar(AKey);
  result := UInt32(StrToInt(s)) << 12; // lots of clashes for: bucket := hash and mask;
end;

{ TTestGenDictionary }

procedure TTestGenDictionary.SetUp;
begin
  inherited SetUp;
  FDict := T.Create;
end;

procedure TTestGenDictionary.TearDown;
begin
  FDict.Destroy;
end;

procedure TTestGenDictionary.RecreateDict;
begin
  FDict.Destroy;
  FDict := T.Create;
end;

procedure TTestGenDictionary.DoTestAddByMethod(ATestVal: integer; AnAddDup: Boolean);
var
  k, v: AnsiString;
  p: PAnsiString;
  APair: specialize TPair<AnsiString, AnsiString>;
  GotExcept: Boolean;
begin
  k := IntToStr(ATestVal);
  v := 'Val'+k;
  GotExcept := False;
  case FAddMethod of
    amAdd,
    amTryAdd, // TryAdd not supported
    amGetOrAdd: // not supported
    begin
      try FDict.Add(k, v); except GotExcept := True; end;
      AssertEquals('dup exception', GotExcept, AnAddDup);
    end;
    amAddPair:
    begin
      APair.Key := k;
      APair.Value := v;
      try FDict.Add(APair); except GotExcept := True; end;
      AssertEquals('dup exception', GotExcept, AnAddDup);
    end;
    amAddOrSet:
      FDict.AddOrSetValue(k, v);
  end;
end;

procedure TTestGenDictionary.DoTestAdd(ATestVal: integer);
begin
  FDict.Add(IntToStr(ATestVal), 'Val'+IntToStr(ATestVal));
end;

procedure TTestGenDictionary.DoTestRemove(ATestVal: integer);
begin
  FDict.Remove(IntToStr(ATestVal));
end;

procedure TTestGenDictionary.AssertKeyVal(ATestVal: integer);
var
  ValGotten, k: AnsiString;
begin
  k := IntToStr(ATestVal);
  AssertTrue('Has key '+k, FDict.ContainsKey(k));
  AssertEquals('Value for Key '+k, FDict[k], 'Val'+k);
  AssertTrue('Try key '+k, FDict.TryGetValue(k, ValGotten));
  AssertEquals('Tried value for Key '+k, ValGotten, 'Val'+k);
end;

procedure TTestGenDictionary.AssertKeyNotFound(ATestVal: integer);
var
  ValGotten, k: AnsiString;
begin
  k := IntToStr(ATestVal);
  AssertFalse('Not has key '+k, FDict.ContainsKey(k));
  AssertFalse('Not try key '+k, FDict.TryGetValue(k, ValGotten));
end;

procedure TTestGenDictionary.AssertEmpty;
begin
  AssertKeyNotFound(0);
  AssertKeyNotFound(1);
  AssertCount(0);
end;

procedure TTestGenDictionary.AssertCount(AnExpCount: integer);
begin
  AssertEquals('Count', FDict.Count, AnExpCount);
end;

procedure TTestGenDictionary.DoRunTestAdd;
var
  i: Integer;
  GotExcept: Boolean;
begin
  AssertEmpty;

  DoTestAddByMethod(-999);
  AssertKeyVal(-999);
  AssertKeyNotFound(0);
  AssertKeyNotFound(1);
  AssertCount(1);

  DoTestRemove(-999);
  AssertEmpty;
  AssertKeyNotFound(-999);


  // Add again
  DoTestAddByMethod(-999);
  AssertKeyVal(-999);
  AssertKeyNotFound(0);
  AssertKeyNotFound(1);
  AssertCount(1);

  FDict.Clear;
  AssertEmpty;
  AssertKeyNotFound(-999);


  // Add again
  DoTestAddByMethod(-999);
  AssertKeyVal(-999);
  AssertKeyNotFound(0);
  AssertKeyNotFound(1);
  AssertCount(1);

  // Add duplicate
  DoTestAddByMethod(-999, True);
  AssertKeyVal(-999);
  AssertKeyNotFound(0);
  AssertKeyNotFound(1);
  AssertCount(1);


  RecreateDict;

  for i :=   0 to  99 do DoTestAddByMethod(i);
  for i :=   0 to  99 do AssertKeyVal(i);
  for i := 100 to 199 do AssertKeyNotFound(i);
  for i := -99 to  -1 do AssertKeyNotFound(i);
  AssertCount(100);

  DoTestRemove(99);
  AssertCount(99);
  AssertKeyNotFound(99);

  DoTestRemove(98);
  AssertCount(98);
  AssertKeyNotFound(99);
  AssertKeyNotFound(98);

  for i := 200 to 999 do DoTestAddByMethod(i);
  for i := 200 to 999 do AssertKeyVal(i);
  for i :=   0 to  97 do AssertKeyVal(i);
  for i :=  98 to 199 do AssertKeyNotFound(i);

  for i :=1000 to 9999 do DoTestAddByMethod(i);
  for i := 200 to 9999 do AssertKeyVal(i);
  for i :=   0 to  97 do AssertKeyVal(i);
  for i :=  98 to 199 do AssertKeyNotFound(i);

  FDict.TrimExcess;
  for i := 200 to 9999 do AssertKeyVal(i);
  for i :=   0 to  97 do AssertKeyVal(i);
  for i :=  98 to 199 do AssertKeyNotFound(i);

  FDict.TrimExcess;
  for i :=10000 to 10100 do DoTestAddByMethod(i);
  for i := 200 to 10100 do AssertKeyVal(i);
  for i :=   0 to  97 do AssertKeyVal(i);
  for i :=  98 to 199 do AssertKeyNotFound(i);

  FDict.Clear;
  AssertCount(0);
  for i :=   0 to  999 do AssertKeyNotFound(i);
  for i := 9999 to 10100 do AssertKeyNotFound(i);


  RecreateDict;

  // call add multiple times
  for i :=   0 to  199 do begin
    DoTestAddByMethod(i);
    DoTestAddByMethod(i, True);
    DoTestAddByMethod(i, True);
  end;
  for i :=   0 to  199 do AssertKeyVal(i);
  AssertCount(200);

end;

procedure TTestGenDictionary.TestAddModify;
var
  i: Integer;
  GotExcept: Boolean;
begin
  GotExcept := False;
  DoTestAdd(-999);
  AssertKeyVal(-999);
  AssertKeyNotFound(0);
  AssertKeyNotFound(1);
  AssertCount(1);

  FDict.AddOrSetValue('998', 'B');
  AssertEquals(FDict['998'], 'B');
  AssertKeyVal(-999);
  AssertCount(2);

  FDict.AddOrSetValue('-999', 'A');
  AssertEquals(FDict['-999'], 'A');
  AssertEquals(FDict['998'], 'B');
  AssertCount(2);

end;

procedure TTestGenDictionary.TestAdd;
begin
  FAddMethod := amAdd;
  DoRunTestAdd;
end;

procedure TTestGenDictionary.TestAddPair;
begin
  FAddMethod := amAddPair;
  DoRunTestAdd;
end;

//procedure TTestGenDictionary.TestTryAdd;
//begin
//  FAddMethod := amTryAdd;
//  DoRunTestAdd;
//end;

procedure TTestGenDictionary.TestAddOrSet;
begin
  FAddMethod := amAddOrSet;
  DoRunTestAdd;
end;

//procedure TTestGenDictionary.TestGetOrAdd;
//begin
//  FAddMethod := amGetOrAdd;
//  DoRunTestAdd;
//end;

{ TTestGenDictionary_TryAdd }

procedure TTestGenDictionary_TryAdd.DoTestAddByMethod(ATestVal: integer; AnAddDup: Boolean);
var
  k, v: AnsiString;
  p: PAnsiString;
  APair: specialize TPair<AnsiString, AnsiString>;
  GotExcept: Boolean;
begin
  k := IntToStr(ATestVal);
  v := 'Val'+k;
  GotExcept := False;
  case FAddMethod of
    amAdd:
    begin
      try FDict.Add(k, v); except GotExcept := True; end;
      AssertEquals('dup exception', GotExcept, AnAddDup);
    end;
    amAddPair:
    begin
      APair.Key := k;
      APair.Value := v;
      try FDict.Add(APair); except GotExcept := True; end;
      AssertEquals('dup exception', GotExcept, AnAddDup);
    end;
    amTryAdd:
    begin
      if AnAddDup then
        AssertFalse( FDict.TryAdd(k, v) )
      else
        AssertTrue( FDict.TryAdd(k, v) );
    end;
    amAddOrSet:
      FDict.AddOrSetValue(k, v);
    amGetOrAdd:
    begin
      p := FDict.GetOrAddMutableValue(k);
      AssertTrue(p <> nil);
      if AnAddDup then
        AssertTrue(p^ = v)
      else
        AssertTrue(p^ = '');
      p^ := v;
    end;
  end;
end;

procedure TTestGenDictionary_TryAdd.TestTryAdd;
begin
  FAddMethod := amTryAdd;
  DoRunTestAdd;
end;

procedure TTestGenDictionary_TryAdd.TestGetOrAdd;
begin
  FAddMethod := amGetOrAdd;
  DoRunTestAdd;
end;


begin
  RegisterTest(specialize TTestGenDictionary_TryAdd< specialize TDictionary<AnsiString, AnsiString> >);
  RegisterTest(specialize TTestGenDictionary_TryAdd< specialize TOpenAddressingLP<AnsiString, AnsiString, TTestHashFactory> >);
  RegisterTest(specialize TTestGenDictionary_TryAdd< specialize TOpenAddressingLP<AnsiString, AnsiString, TTestHashFactory2> >);

  RegisterTest(specialize TTestGenDictionary_TryAdd< specialize TOpenAddressingLPT<AnsiString, AnsiString> >);
  RegisterTest(specialize TTestGenDictionary_TryAdd< specialize TOpenAddressingLPT<AnsiString, AnsiString, TTestHashFactory> >);
  RegisterTest(specialize TTestGenDictionary_TryAdd< specialize TOpenAddressingLPT<AnsiString, AnsiString, TTestHashFactory2> >);

  RegisterTest(specialize TTestGenDictionary_TryAdd< specialize TOpenAddressingQP<AnsiString, AnsiString> >);
  RegisterTest(specialize TTestGenDictionary_TryAdd< specialize TOpenAddressingQP<AnsiString, AnsiString, TTestHashFactory> >);
  RegisterTest(specialize TTestGenDictionary_TryAdd< specialize TOpenAddressingQP<AnsiString, AnsiString, TTestHashFactory2> >);

  RegisterTest(specialize TTestGenDictionary_TryAdd< specialize TOpenAddressingDH<AnsiString, AnsiString> >);
  RegisterTest(specialize TTestGenDictionary_TryAdd< specialize TOpenAddressingDH<AnsiString, AnsiString, TTestDoubleHashFactory> >);
  RegisterTest(specialize TTestGenDictionary_TryAdd< specialize TOpenAddressingDH<AnsiString, AnsiString, TTestDoubleHashFactory2> >);

  RegisterTest(specialize TTestGenDictionary< specialize TCuckooD2<AnsiString, AnsiString> >);
  //RegisterTest(specialize TTestGenDictionary< specialize TCuckooD2<AnsiString, AnsiString, TTestDoubleHashFactory> >);
  //RegisterTest(specialize TTestGenDictionary< specialize TCuckooD2<AnsiString, AnsiString, TTestDoubleHashFactory2> >);

  RegisterTest(specialize TTestGenDictionary< specialize TCuckooD4<AnsiString, AnsiString> >);
  RegisterTest(specialize TTestGenDictionary< specialize TCuckooD6<AnsiString, AnsiString> >);
  RegisterTest(specialize TTestGenDictionary< specialize TFastHashMap<AnsiString, AnsiString> >);
  RegisterTest(specialize TTestGenDictionary< specialize THashMap<AnsiString, AnsiString> >);

end.

