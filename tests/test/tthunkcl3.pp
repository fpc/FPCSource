{ %CPU=wasm32 }
program tthunkcl3;

{$mode objfpc}
{$h+}

uses sysutils, typinfo;

Type
  {$M+}
  TMyInterface = Interface ['{76DC0D03-376C-45AA-9E0C-B3546B0C7208}']
    Procedure DoA(a : Integer);
    Procedure DoA;
    function doB : Integer;
    function doc(a : integer) : integer;
  end;

  { TTestThunk }

  TTestThunk = class(TObject)
  private
    FExpectMethod,
    FExpectArgInt : Integer;
    FExpectCount : Integer;
    FExpectResult : Boolean;
    FReturnResultInt : Integer;
    FExpectArgTypeInfo : TTypeKind;
    FExpectResultTypeInfo : TTypeKind;
    FTest : String;
    Procedure AssertEquals(Msg : string; aExpect,aActual : Integer);
    Procedure AssertTrue(Msg : string; aValue : Boolean);
    Procedure AssertNotNull(Msg : string; aValue : Pointer);
    procedure ThunkCallBack(aInstance: Pointer; aMethod, aCount: Longint; aData : TInterfaceThunk.PargData);
  Public
    Procedure DoTest;
  end;

procedure TTestThunk.AssertEquals(Msg: string; aExpect, aActual: Integer);
begin
  AssertTrue(Msg+': '+IntToStr(aExpect)+'<>'+IntToStr(aActual),aExpect=aActual);
end;

procedure TTestThunk.AssertTrue(Msg: string; aValue: Boolean);
begin
  if not aValue then
    begin
    Writeln(FTest+' failed: ',Msg);
    Halt(1);
    end;
end;

procedure TTestThunk.AssertNotNull(Msg: string; aValue: Pointer);
begin
  AssertTrue(Msg+': not null',Assigned(aValue));
end;

procedure TTestThunk.ThunkCallBack(aInstance: Pointer; aMethod,
  aCount: Longint; aData : TInterfaceThunk.PargData);

begin
  AssertEquals('Correct method called',FExpectMethod,aMethod);
  AssertEquals('Correct argument count',FExpectMethod,aMethod);
  AssertTrue('Have result',Assigned(aData[0].Addr)=FExpectResult);
  if ACount>0 then
    begin
    AssertTrue('Have arg 0 type info',Assigned(aData[1].info));
    AssertTrue('Have arg 0 correct type info',PTypeInfo(aData[1].info)^.Kind=FExpectArgTypeInfo);
    Case FExpectArgTypeInfo of
      tkInteger: AssertEquals('Correct arg 0 integer argument value ',FExpectArgInt,PInteger(aData[1].Addr)^);
    end;
    end;
 if FExpectResult then
    begin
    AssertTrue('Have correct result type info',PTypeInfo(aData[0].info)^.Kind=FExpectArgTypeInfo);
    Case FExpectResultTypeInfo of
      tkInteger: PInteger(aData[0].addr)^:=FReturnResultInt;
    end;
    end;
end;

procedure TTestThunk.DoTest;

var
  PI,PC : PTypeInfo;
  PT : PTypeData;
  I : TMyInterface;
  TC : TInterfaceThunkClass;
  R : Integer;

begin
  PI:=TypeInfo(TMyInterface);
  AssertNotNull('Type info',PI);
  PT:=GetTypeData(PI);
  AssertNotNull('Type data ',PT);
  AssertNotNull('Thunk class',PInterfaceData(PT)^.ThunkClass);
  PC:=PInterfaceData(PT)^.ThunkClass^;
  TC:=TInterfaceThunkClass(GetTypeData(PC)^.ClassType);
  I:=TC.create(@ThunkCallBack) as TMyInterface;
  FTest:='DoA1';
  FExpectMethod:=3; // Skip 0..2, part of IInterface.
  FExpectCount:=1;
  FExpectResult:=False;
  FExpectArgTypeInfo:=tkInteger;
  FExpectArgInt:=12;
  I.DoA(12);
  FTest:='DoA2';
  FExpectMethod:=4;
  FExpectCount:=0;
  FExpectResult:=False;
  I.DoA;
  FTest:='DoB';
  FExpectMethod:=5;
  FExpectCount:=0;
  FExpectResult:=True;
  FReturnResultint:=42;
  FExpectResultTypeInfo:=tkInteger;
  R:=I.DoB;
  AssertEquals('Result',FReturnResultint,R);
  FTest:='DoC';
  FExpectMethod:=6;
  FExpectCount:=1;
  FExpectResult:=True;
  FExpectArgTypeInfo:=tkInteger;
  FExpectArgInt:=41;
  FExpectResultTypeInfo:=tkInteger;
  FReturnResultInt:=43;
  R:=I.DoC(41);
  AssertEquals('Result',FReturnResultint,R);
  Writeln('All OK');
end;

begin
  With TTestThunk.Create do
   try
     DoTest;
   finally
     Free;
   end;
end.

