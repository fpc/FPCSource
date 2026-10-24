unit tests.rtti.pools;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit, testregistry,
  SysUtils, TypInfo, Rtti;

{$RTTI EXPLICIT METHODS([vcPrivate,vcProtected,vcPublic,vcPublished]) PROPERTIES([vcPrivate,vcProtected,vcPublic,vcPublished]) FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}

type
  TPoolEnum = (peA, peB, peC);
  TPoolSubEnum = peB..peC;

  TPoolRec = record
    Value: Integer;
    procedure Touch;
  end;
  PPoolRec = ^TPoolRec;
  TPoolRecArray = array[0..1] of TPoolRec;

  TPoolObj = class;
  TPoolObjArray = array of TPoolObj;
  TPoolRecMethod = function(aRec: TPoolRec): TPoolObj of object;
  TPoolRecProc = function(aRec: TPoolRec): TPoolObj;

  {$M+}
  IPoolParent = interface
    ['{5B0C7D52-4E0B-4C83-9F59-2C3E4E4B7A11}']
    procedure ParentProc;
  end;

  IPoolChild = interface(IPoolParent)
    ['{0D8E3C2A-7F3B-4E5D-8A41-6B9C2D1E0F22}']
    procedure ChildProc(aRec: TPoolRec);
  end;
  {$M-}

  TPoolBase = class
  private
    FBaseValue: Integer;
    procedure BaseProc;
  end;

  TPoolObj = class(TPoolBase)
  private
    FRec: TPoolRec;
    FOnRec: TPoolRecMethod;
    procedure Hidden;
  public
    procedure UseRec(aRec: TPoolRec);
    property Rec: TPoolRec read FRec write FRec;
  published
    property OnRec: TPoolRecMethod read FOnRec write FOnRec;
  end;

  { TTestRttiPools }

  TTestRttiPools = class(TTestCase)
  private
    // Asserts that aActual is the object the context of aPublishedOnly returns for aTypeInfo
    procedure CheckPoolType(const aMsg: string; aPublishedOnly: Boolean; aTypeInfo: PTypeInfo; aActual: TRttiType);
  published
    procedure TestDynArrayElementType;
    procedure TestStaticArrayElementType;
    procedure TestPointerReferredType;
    procedure TestEnumerationUnderlyingType;
    procedure TestMethodTypeParameters;
    procedure TestProcedureTypeParameters;
    procedure TestInterfaceParent;
    procedure TestInterfaceMethodParameter;
    procedure TestClassParent;
    procedure TestFieldType;
    procedure TestPropertyType;
    procedure TestClassMethodParameter;
    procedure TestExtendedViewThroughElementType;
    procedure TestMethodPropertyParametersWithoutKeepContext;
    procedure TestDefaultContextFollowsGlobal;
  end;

implementation

const
  cPoolNames: array[Boolean] of string = ('extended', 'published only');

procedure TPoolRec.Touch;

begin
end;


procedure TPoolBase.BaseProc;

begin
end;


procedure TPoolObj.Hidden;

begin
end;


procedure TPoolObj.UseRec(aRec: TPoolRec);

begin
end;


procedure TTestRttiPools.CheckPoolType(const aMsg: string; aPublishedOnly: Boolean; aTypeInfo: PTypeInfo; aActual: TRttiType);

var
  lCtx: TRttiContext;

begin
  lCtx:=TRttiContext.Create(aPublishedOnly);
  AssertNotNull(aMsg+' ('+cPoolNames[aPublishedOnly]+') is assigned',aActual);
  AssertSame(aMsg+' ('+cPoolNames[aPublishedOnly]+') is the pool''s type',lCtx.GetType(aTypeInfo),aActual);
end;


procedure TTestRttiPools.TestDynArrayElementType;

var
  lPublishedOnly: Boolean;
  lType: TRttiType;

begin
  for lPublishedOnly in Boolean do
    begin
    lType:=(TRttiContext.Create(lPublishedOnly).GetType(TypeInfo(TPoolObjArray)) as TRttiDynamicArrayType).ElementType;
    CheckPoolType('Dynamic array element type',lPublishedOnly,TypeInfo(TPoolObj),lType);
    end;
end;


procedure TTestRttiPools.TestStaticArrayElementType;

var
  lPublishedOnly: Boolean;
  lType: TRttiType;

begin
  for lPublishedOnly in Boolean do
    begin
    lType:=(TRttiContext.Create(lPublishedOnly).GetType(TypeInfo(TPoolRecArray)) as TRttiArrayType).ElementType;
    CheckPoolType('Static array element type',lPublishedOnly,TypeInfo(TPoolRec),lType);
    end;
end;


procedure TTestRttiPools.TestPointerReferredType;

var
  lPublishedOnly: Boolean;
  lType: TRttiType;

begin
  for lPublishedOnly in Boolean do
    begin
    lType:=(TRttiContext.Create(lPublishedOnly).GetType(TypeInfo(PPoolRec)) as TRttiPointerType).ReferredType;
    CheckPoolType('Pointer referred type',lPublishedOnly,TypeInfo(TPoolRec),lType);
    end;
end;


procedure TTestRttiPools.TestEnumerationUnderlyingType;

var
  lPublishedOnly: Boolean;
  lType: TRttiType;

begin
  for lPublishedOnly in Boolean do
    begin
    lType:=(TRttiContext.Create(lPublishedOnly).GetType(TypeInfo(TPoolSubEnum)) as TRttiEnumerationType).UnderlyingType;
    CheckPoolType('Enumeration underlying type',lPublishedOnly,TypeInfo(TPoolEnum),lType);
    end;
end;


procedure TTestRttiPools.TestMethodTypeParameters;

var
  lPublishedOnly: Boolean;
  lMethod: TRttiMethodType;
  lParams: TRttiParameterArray;

begin
  for lPublishedOnly in Boolean do
    begin
    lMethod:=TRttiContext.Create(lPublishedOnly).GetType(TypeInfo(TPoolRecMethod)) as TRttiMethodType;
    lParams:=lMethod.GetParameters;
    AssertEquals('Method type parameter count',1,Length(lParams));
    CheckPoolType('Method type parameter type',lPublishedOnly,TypeInfo(TPoolRec),lParams[0].ParamType);
    CheckPoolType('Method type return type',lPublishedOnly,TypeInfo(TPoolObj),lMethod.ReturnType);
    end;
end;


procedure TTestRttiPools.TestProcedureTypeParameters;

var
  lPublishedOnly: Boolean;
  lProc: TRttiProcedureType;
  lParams: TRttiParameterArray;

begin
  for lPublishedOnly in Boolean do
    begin
    lProc:=TRttiContext.Create(lPublishedOnly).GetType(TypeInfo(TPoolRecProc)) as TRttiProcedureType;
    lParams:=lProc.GetParameters;
    AssertEquals('Procedure type parameter count',1,Length(lParams));
    CheckPoolType('Procedure type parameter type',lPublishedOnly,TypeInfo(TPoolRec),lParams[0].ParamType);
    CheckPoolType('Procedure type return type',lPublishedOnly,TypeInfo(TPoolObj),lProc.ReturnType);
    end;
end;


procedure TTestRttiPools.TestInterfaceParent;

var
  lPublishedOnly: Boolean;
  lType: TRttiType;

begin
  for lPublishedOnly in Boolean do
    begin
    lType:=(TRttiContext.Create(lPublishedOnly).GetType(TypeInfo(IPoolChild)) as TRttiInterfaceType).BaseType;
    CheckPoolType('Interface parent type',lPublishedOnly,TypeInfo(IPoolParent),lType);
    end;
end;


procedure TTestRttiPools.TestInterfaceMethodParameter;

var
  lPublishedOnly: Boolean;
  lMethod: TRttiMethod;
  lParams: TRttiParameterArray;

begin
  for lPublishedOnly in Boolean do
    begin
    lMethod:=TRttiContext.Create(lPublishedOnly).GetType(TypeInfo(IPoolChild)).GetMethod('ChildProc');
    AssertNotNull('Interface method ('+cPoolNames[lPublishedOnly]+') found',lMethod);
    lParams:=lMethod.GetParameters;
    AssertEquals('Interface method parameter count',1,Length(lParams));
    CheckPoolType('Interface method parameter type',lPublishedOnly,TypeInfo(TPoolRec),lParams[0].ParamType);
    end;
end;


procedure TTestRttiPools.TestClassParent;

var
  lPublishedOnly: Boolean;

begin
  for lPublishedOnly in Boolean do
    CheckPoolType('Class parent type',lPublishedOnly,TypeInfo(TPoolBase),
      TRttiContext.Create(lPublishedOnly).GetType(TypeInfo(TPoolObj)).BaseType);
end;


procedure TTestRttiPools.TestFieldType;

var
  lField: TRttiField;

begin
  lField:=TRttiContext.Create(False).GetType(TypeInfo(TPoolObj)).GetField('FRec');
  AssertNotNull('Field FRec found',lField);
  CheckPoolType('Field type',False,TypeInfo(TPoolRec),lField.FieldType);
end;


procedure TTestRttiPools.TestPropertyType;

var
  lProp: TRttiProperty;

begin
  lProp:=TRttiContext.Create(False).GetType(TypeInfo(TPoolObj)).GetProperty('Rec');
  AssertNotNull('Property Rec found',lProp);
  CheckPoolType('Property type',False,TypeInfo(TPoolRec),lProp.PropertyType);
end;


procedure TTestRttiPools.TestClassMethodParameter;

var
  lMethod: TRttiMethod;
  lParams: TRttiParameterArray;

begin
  lMethod:=TRttiContext.Create(False).GetType(TypeInfo(TPoolObj)).GetMethod('UseRec');
  AssertNotNull('Method UseRec found',lMethod);
  lParams:=lMethod.GetParameters;
  AssertEquals('Method parameter count',1,Length(lParams));
  CheckPoolType('Method parameter type',False,TypeInfo(TPoolRec),lParams[0].ParamType);
end;


procedure TTestRttiPools.TestExtendedViewThroughElementType;

var
  lType: TRttiType;

begin
  lType:=(TRttiContext.Create(False).GetType(TypeInfo(TPoolObjArray)) as TRttiDynamicArrayType).ElementType;
  AssertNotNull('Private method of element type visible in the extended pool',lType.GetMethod('Hidden'));
  lType:=(TRttiContext.Create(False).GetType(TypeInfo(PPoolRec)) as TRttiPointerType).ReferredType;
  AssertNotNull('Record method of referred type visible in the extended pool',lType.GetMethod('Touch'));
end;


procedure TTestRttiPools.TestMethodPropertyParametersWithoutKeepContext;

var
  lMethod: TRttiMethodType;
  lParams: TRttiParameterArray;

begin
  lMethod:=TRttiContext.Create(False).GetType(TypeInfo(TPoolObj)).GetProperty('OnRec').PropertyType as TRttiMethodType;
  lParams:=lMethod.GetParameters;
  AssertEquals('OnRec parameter count',1,Length(lParams));
  AssertEquals('OnRec parameter name','aRec',lParams[0].Name);
  CheckPoolType('OnRec parameter type',False,TypeInfo(TPoolRec),lParams[0].ParamType);
  CheckPoolType('OnRec return type',False,TypeInfo(TPoolObj),lMethod.ReturnType);
end;


procedure TTestRttiPools.TestDefaultContextFollowsGlobal;

var
  lOld, lValue: Boolean;

  // Returns the flag of an implicitly initialized context
  function InitializedFlag: Boolean;

  var
    lCtx: TRttiContext;

  begin
    Result:=lCtx.UsePublishedOnly;
  end;

begin
  lOld:=GlobalUsePublishedOnly;
  try
    for lValue in Boolean do
      begin
      GlobalUsePublishedOnly:=lValue;
      AssertEquals('Created context follows GlobalUsePublishedOnly',lValue,TRttiContext.Create.UsePublishedOnly);
      AssertEquals('Initialized context follows GlobalUsePublishedOnly',lValue,InitializedFlag);
      AssertSame('Default context uses the pool of GlobalUsePublishedOnly',
        TRttiContext.Create(lValue).GetType(TypeInfo(TPoolObj)),TRttiContext.Create.GetType(TypeInfo(TPoolObj)));
      end;
  finally
    GlobalUsePublishedOnly:=lOld;
  end;
end;


initialization
  RegisterTest(TTestRttiPools);
end.
