unit uexrttiutil;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, TypInfo;

// Low level tests
Procedure AssertEquals(Msg : String; aExpected,aActual : Boolean);
Procedure AssertEquals(Msg : String; aExpected,aActual : Integer);
Procedure AssertEquals(Msg : String; aExpected,aActual : String);
Procedure AssertEquals(Msg : String; aExpected,aActual : TVisibilityClass);
Procedure AssertEquals(Msg : String; aExpected,aActual : TTypeKind);
Procedure AssertNotNull(Msg : String; aPointer : Pointer);
Procedure AssertNull(Msg : String; aPointer : Pointer);
Procedure AssertSame(Msg : String; aExpected,aActual : Pointer);

// Combined tests
Procedure CheckProperty(aIdx : Integer; aData: TPropInfoEx; aName : String; aKind : TTypeKind; aVisibility : TVisibilityClass; isStrict : Boolean = False);
Procedure CheckField(aIdx : Integer; aData: PExtendedVmtFieldEntry; aName : String; aKind : TTypeKind; aVisibility : TVisibilityClass; aStrict : Boolean = False);
Procedure CheckMethod(aPrefix : string; aIdx : Integer; aData: PVmtMethodExEntry; aName : String; aVisibility : TVisibilityClass; aStrict : Boolean = False);
Procedure CheckMethod(aPrefix : string; aIdx : Integer; aData: PRecMethodExEntry; aName : String; aVisibility : TVisibilityClass; aStrict : Boolean = False);

implementation

Procedure CheckMethod(aPrefix : string; aIdx : Integer; aData: PRecMethodExEntry; aName : String; aVisibility : TVisibilityClass; aStrict : Boolean = False);

Var
  Msg : String;

begin
  Msg:=aPrefix+': Checking method '+IntToStr(aIdx)+' ('+aName+') ';
  AssertEquals(Msg+'name',aData^.Name,aName);
  AssertEquals(Msg+'visibility',aVisibility,aData^.MethodVisibility);
  AssertEquals(Msg+'strict',aData^.StrictVisibility,aStrict);
end;


Procedure CheckMethod(aPrefix : string; aIdx : Integer; aData: PVmtMethodExEntry; aName : String; aVisibility : TVisibilityClass; aStrict : Boolean = False);

Var
  Msg : String;

begin
  Msg:=aPrefix+': Checking method '+IntToStr(aIdx)+' ('+aName+') ';
  AssertEquals(Msg+'name',aData^.Name,aName);
  AssertEquals(Msg+'visibility',aVisibility,aData^.MethodVisibility);
  AssertEquals(Msg+'strict',aData^.StrictVisibility,aStrict);
end;

Procedure CheckProperty(aIdx : Integer; aData: TPropInfoEx; aName : String; aKind : TTypeKind; aVisibility : TVisibilityClass; isStrict : Boolean = False);

Var
  Msg : String;

begin
  Msg:='Checking prop '+IntToStr(aIdx)+' ('+aName+') ';
  AssertEquals(Msg+'name',aName, aData.Info^.Name);
  AssertEquals(Msg+'kind',aKind, aData.Info^.PropType^.Kind);
  AssertEquals(Msg+'visibility',aVisibility,aData.Visibility);
  AssertEquals(Msg+'strict',isStrict,aData.StrictVisibility);
end;


Procedure CheckField(aIdx : Integer; aData: PExtendedVmtFieldEntry; aName : String; aKind : TTypeKind; aVisibility : TVisibilityClass; aStrict : Boolean = False);

Var
  Msg : String;

begin
  Msg:='Checking field '+IntToStr(aIdx)+' ('+aName+') ';
  AssertEquals(Msg+'name',aName,aData^.Name^);
  AssertEquals(Msg+'kind',aKind,PPTypeInfo(aData^.FieldType)^^.Kind);
  AssertEquals(Msg+'visibility',aVisibility,aData^.FieldVisibility);
  AssertEquals(Msg+'strict',aStrict,aData^.StrictVisibility);
end;


Procedure AssertEquals(Msg : String; aExpected,aActual : Integer);

begin
  If AExpected<>aActual then
    begin
    Msg:=Msg+': expected: '+IntToStr(aExpected)+' got: '+IntToStr(aActual);
    Writeln(Msg);
    Halt(1);
    end;
end;

Procedure AssertEquals(Msg : String; aExpected,aActual : String);

begin
  If AExpected<>aActual then
    begin
    Msg:=Msg+': expected: <'+aExpected+'> got: <'+aActual+'>';
    Writeln(Msg);
    Halt(1);
    end;
end;

Procedure AssertEquals(Msg : String; aExpected,aActual : TVisibilityClass);

begin
  If AExpected<>aActual then
    begin
    Msg:=Msg+': expected: '+IntToStr(Ord(aExpected))+' got: '+IntToStr(Ord(aActual));
    Writeln(Msg);
    Halt(1);
    end;
end;

Procedure AssertEquals(Msg : String; aExpected,aActual : TTypeKind);

begin
  If AExpected<>aActual then
    begin
    Msg:=Msg+': expected: '+IntToStr(Ord(aExpected))+' got: '+IntToStr(Ord(aActual));
    Writeln(Msg);
    Halt(1);
    end;
end;

procedure AssertNotNull(Msg: String; aPointer: Pointer);
begin
  if aPointer=Nil then
    begin
    Msg:=Msg+': expected not Nil pointer, but got Nil.';
    Writeln(Msg);
    Halt(1);
    end;
end;

procedure AssertNull(Msg: String; aPointer: Pointer);
begin
  if aPointer<>Nil then
    begin
    Msg:=Msg+': expected Nil pointer, but got '+HexStr(aPointer);
    Writeln(Msg);
    Halt(1);
    end;
end;

procedure AssertSame(Msg: String; aExpected, aActual: Pointer);
begin
  If AExpected<>aActual then
    begin
    Msg:=Msg+': expected: '+HexStr(aExpected)+' got: '+HexStr(aActual);
    Writeln(Msg);
    Halt(1);
    end;
end;

Procedure AssertEquals(Msg : String; aExpected,aActual : Boolean);

begin
  If AExpected<>aActual then
    begin
    Msg:=Msg+': expected: '+BoolToStr(aExpected,True)+' got: '+BoolToStr(aActual,True);
    Writeln(Msg);
    Halt(1);
    end;
end;


end.

