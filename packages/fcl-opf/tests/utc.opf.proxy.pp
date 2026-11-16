unit utc.opf.proxy;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, Contnrs,
  fpcunit, testregistry,
{$ENDIF}
  Opf.Proxy;

type

  TOPLazyObjectList = specialize TOPLazy<TObjectList>;

  { TTestHelper — helper class that provides loader callbacks }

  TTestHelper = class
  private
    FLoadCount: Integer;
    FLastTarget: TObject;
  public
    constructor Create;
    procedure DoLoadList(aTarget: TObject);
    procedure DoLoadAndRaise(aTarget: TObject);
    property LoadCount: Integer read FLoadCount;
    property LastTarget: TObject read FLastTarget;
  end;

  { TTestLazyRecord }

  TTestLazyRecord = class(TTestCase)
  private
    FHelper: TTestHelper;
    FList: TObjectList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestValueTriggersLoaderOnce;
    procedure TestValueWithoutLoaderReturnsDefault;
    procedure TestInvalidateReTriggersLoader;
    procedure TestIsLoadedInitiallyFalse;
    procedure TestIsLoadedAfterAccess;
    procedure TestIsLoadedAfterAccessNoLoader;
    procedure TestInitSetsValueAndResets;
    procedure TestInitWithLoader;
    procedure TestSetLoaderReplaces;
    procedure TestReEntrantGetValueIsSafe;
    procedure TestLoaderReceivesCorrectTarget;
  end;

implementation

{ TTestHelper }

constructor TTestHelper.Create;
begin
  inherited Create;
  FLoadCount := 0;
  FLastTarget := nil;
end;

procedure TTestHelper.DoLoadList(aTarget: TObject);
begin
  Inc(FLoadCount);
  FLastTarget := aTarget;
end;

procedure TTestHelper.DoLoadAndRaise(aTarget: TObject);
begin
  Inc(FLoadCount);
  raise Exception.Create('Loader error');
end;

{ TTestLazyRecord }

procedure TTestLazyRecord.SetUp;
begin
  inherited SetUp;
  FHelper := TTestHelper.Create;
  FList := TObjectList.Create(True);
end;

procedure TTestLazyRecord.TearDown;
begin
  FList.Free;
  FHelper.Free;
  inherited TearDown;
end;

procedure TTestLazyRecord.TestValueTriggersLoaderOnce;
var
  Lazy: TOPLazyObjectList;
begin
  Lazy.Init(FList, @FHelper.DoLoadList);
  { First access triggers loader }
  AssertSame('Value returns FList', FList, Lazy.Value);
  AssertEquals('Loader called once', 1, FHelper.LoadCount);
  { Second access does NOT trigger loader again }
  AssertSame('Value still returns FList', FList, Lazy.Value);
  AssertEquals('Loader still called once', 1, FHelper.LoadCount);
end;

procedure TTestLazyRecord.TestValueWithoutLoaderReturnsDefault;
var
  Lazy: TOPLazyObjectList;
begin
  Lazy.Init(FList);
  { No loader set — Value returns FList without calling anything }
  AssertSame('Value returns FList', FList, Lazy.Value);
  AssertEquals('No loader called', 0, FHelper.LoadCount);
end;

procedure TTestLazyRecord.TestInvalidateReTriggersLoader;
var
  Lazy: TOPLazyObjectList;
begin
  Lazy.Init(FList, @FHelper.DoLoadList);
  { Trigger first load }
  Lazy.Value;
  AssertEquals('Loader called once', 1, FHelper.LoadCount);
  { Invalidate and re-access }
  Lazy.Invalidate;
  AssertFalse('IsLoaded false after Invalidate', Lazy.IsLoaded);
  Lazy.Value;
  AssertEquals('Loader called twice after Invalidate', 2, FHelper.LoadCount);
end;

procedure TTestLazyRecord.TestIsLoadedInitiallyFalse;
var
  Lazy: TOPLazyObjectList;
begin
  Lazy.Init(FList, @FHelper.DoLoadList);
  AssertFalse('IsLoaded initially false', Lazy.IsLoaded);
end;

procedure TTestLazyRecord.TestIsLoadedAfterAccess;
var
  Lazy: TOPLazyObjectList;
begin
  Lazy.Init(FList, @FHelper.DoLoadList);
  Lazy.Value;
  AssertTrue('IsLoaded true after Value access', Lazy.IsLoaded);
end;

procedure TTestLazyRecord.TestIsLoadedAfterAccessNoLoader;
var
  Lazy: TOPLazyObjectList;
begin
  Lazy.Init(FList);
  Lazy.Value;
  AssertTrue('IsLoaded true even without loader', Lazy.IsLoaded);
end;

procedure TTestLazyRecord.TestInitSetsValueAndResets;
var
  Lazy: TOPLazyObjectList;
  OtherList: TObjectList;
begin
  Lazy.Init(FList, @FHelper.DoLoadList);
  Lazy.Value;
  AssertTrue('IsLoaded after first access', Lazy.IsLoaded);
  { Re-init with new value resets state }
  OtherList := TObjectList.Create(True);
  try
    Lazy.Init(OtherList);
    AssertFalse('IsLoaded false after re-Init', Lazy.IsLoaded);
    AssertSame('Value returns new list', OtherList, Lazy.Value);
  finally
    OtherList.Free;
  end;
end;

procedure TTestLazyRecord.TestInitWithLoader;
var
  Lazy: TOPLazyObjectList;
begin
  Lazy.Init(FList, @FHelper.DoLoadList);
  Lazy.Value;
  AssertEquals('Loader called', 1, FHelper.LoadCount);
end;

procedure TTestLazyRecord.TestSetLoaderReplaces;
var
  Lazy: TOPLazyObjectList;
  Helper2: TTestHelper;
begin
  Helper2 := TTestHelper.Create;
  try
    Lazy.Init(FList, @FHelper.DoLoadList);
    Lazy.SetLoader(@Helper2.DoLoadList);
    Lazy.Value;
    AssertEquals('Original helper not called', 0, FHelper.LoadCount);
    AssertEquals('New helper called', 1, Helper2.LoadCount);
  finally
    Helper2.Free;
  end;
end;

procedure TTestLazyRecord.TestReEntrantGetValueIsSafe;
var
  Lazy: TOPLazyObjectList;
begin
  { A loader that raises leaves the value not loaded: an empty list must not
    pass for the loaded one. The next access tries again. }
  Lazy.Init(FList, @FHelper.DoLoadAndRaise);
  try
    Lazy.Value;
    Fail('Expected exception from loader');
  except
    on E: Exception do
      AssertEquals('Correct exception', 'Loader error', E.Message);
  end;
  AssertFalse('Not loaded after exception', Lazy.IsLoaded);
  try
    Lazy.Value;
    Fail('Expected exception from the second load');
  except
    on E: Exception do ;
  end;
  AssertEquals('Loader called again', 2, FHelper.LoadCount);
end;

procedure TTestLazyRecord.TestLoaderReceivesCorrectTarget;
var
  Lazy: TOPLazyObjectList;
begin
  Lazy.Init(FList, @FHelper.DoLoadList);
  Lazy.Value;
  AssertSame('Loader received FList as target', FList, FHelper.LastTarget);
end;

initialization
  RegisterTest(TTestLazyRecord);
end.
