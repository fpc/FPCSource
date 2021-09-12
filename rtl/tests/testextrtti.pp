{$MODE OBJFPC}
{$M+}
{$Modeswitch advancedrecords}

program testextrtti;

uses typinfo, sysutils;

Type
  {$RTTI EXPLICIT
     PROPERTIES([vcPrivate,vcProtected,vcPublic,vcPublished])
     FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])
     METHODS([vcPrivate,vcProtected,vcPublic,vcPublished])}
//  {$RTTI EXPLICIT }
//  {$RTTI EXPLICIT }

  { TFieldRTTI }

  TFieldRTTI = Class
  private
    FPrivateA: Integer;
    FPrivateB: Integer;
    Property PrivateA : Integer Read FPrivateA Write FPrivateA;
    Property PrivateB : Integer Read FPrivateB Write FPrivateB;
  Protected
    FProtectedA: Integer;
    FProtectedB: Integer;
    Property ProtectedA : Integer Read FProtectedA Write FProtectedA;
    Property ProtectedB : Integer Read FProtectedB Write FProtectedB;
  Public
    FPublicA: Integer;
    FPublicB: Integer;
    Property PublicA : Integer Read FPublicA Write FPublicA;
    Property PublicB : Integer Read FPublicA Write FPublicB;
  Published
    FPublishedA: Integer;
    FPublishedB: Integer;
    Property PublishedA : Integer Read FPublishedA Write FPublishedA;
    Property PublishedB : Integer Read FPublishedA Write FPublishedB;
  end;

  // Use different names, so we can distinguish RTTI in asm file...
  TRecordFieldRTTI = record
    private
      FRPrivateA: Integer;
      FRPrivateB: Integer;
      Property RPrivateA : Integer Read FRPrivateA Write FRPrivateA;
      Property RPrivateB : Integer Read FRPrivateB Write FRPrivateB;
    Public
      FRPublicA: Integer;
      FRPublicB: Integer;
      Property RPublicA : Integer Read FRPublicA Write FRPublicA;
      Property RPublicB : Integer Read FRPublicA Write FRPublicB;
   end;

  { TMethodClassRTTI }

  TMethodClassRTTI = Class (TObject)
  private
    Procedure PrivateMethodA;
    Procedure PrivateMethodB; virtual;
    Procedure PrivateMethodC; virtual; abstract;
  protected
    Procedure ProtectedMethodA;
    Procedure ProtectedMethodB; virtual;
    Procedure ProtectedMethodC; virtual; abstract;
  public
    Procedure PublicMethodA;
    Procedure PublicMethodB; virtual;
    Procedure PublicMethodC; virtual; abstract;
  published
    Procedure PublishedMethodA;
    Procedure PublishedMethodB; virtual;
    Procedure PublishedMethodC; virtual; abstract;
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

Procedure CheckProperty(aIdx : Integer; aData: TPropInfoEx; aName : String; aKind : TTypeKind; aVisibility : TVisibilityClass);

Var
  Msg : String;

begin
  Msg:='Checking prop '+IntToStr(aIdx)+' ('+aName+') ';
  AssertEquals(Msg+'name',aData.Info^.Name,aName);
  AssertEquals(Msg+'kind',aData.Info^.PropType^.Kind,aKind);
  AssertEquals(Msg+'visibility',TVisibilityClass(aData.Flags),aVisibility);
end;

Procedure TestProperties;

Var
  A : PPropListEx;
  aCount : Integer;

begin
  aCount:=GetPropListEx(TFieldRTTI,A);
  try
    AssertEquals('Count',8,aCount);
    CheckProperty(0, A^[0]^,'PrivateA',tkInteger,vcPrivate);
  finally
    Freemem(A);
  end;
end;

Procedure CheckField(aIdx : Integer; aData: PExtendedVmtFieldEntry; aName : String; aKind : TTypeKind; aVisibility : TVisibilityClass);

Var
  Msg : String;

begin
  Msg:='Checking field '+IntToStr(aIdx)+' ('+aName+') ';
  AssertEquals(Msg+'name',aData^.Name^,aName);
  AssertEquals(Msg+'kind',PPTypeInfo(aData^.FieldType)^^.Kind, aKind);
  AssertEquals(Msg+'visibility',aData^.FieldVisibility,aVisibility);
end;


Procedure TestClassFields;

Var
  A : PExtendedFieldInfoTable;
  aCount : Integer;

begin
  // O:=TFieldRTTI.Create;
  // aCount:=TFieldRTTI.InstanceSize;
  // aCount:=PtrInt(O.FieldAddress('PublField'))-PtrInt(O);
  aCount:=GetFieldList(TFieldRTTI,A);
  AssertEquals('Count',8,aCount);
  CheckField(0, A^[0],'FPrivateA',tkInteger,vcPrivate);
  CheckField(1, A^[1],'FPrivateB',tkInteger,vcPrivate);
  CheckField(2, A^[2],'FProtectedA',tkInteger,vcProtected);
  CheckField(3, A^[3],'FProtectedB',tkInteger,vcProtected);
  CheckField(4, A^[4],'FPublicA',tkInteger,vcPublic);
  CheckField(5, A^[5],'FPublicB',tkInteger,vcPublic);
  CheckField(6, A^[6],'FPublishedA',tkInteger,vcPublished);
  CheckField(7, A^[7],'FPublishedB',tkInteger,vcPublished);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcPrivate]);
  AssertEquals('Count',2,aCount);
  CheckField(0, A^[0],'FPrivateA',tkInteger,vcPrivate);
  CheckField(1, A^[1],'FPrivateB',tkInteger,vcPrivate);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcProtected]);
  AssertEquals('Count',2,aCount);
  CheckField(2, A^[0],'FProtectedA',tkInteger,vcProtected);
  CheckField(3, A^[1],'FProtectedB',tkInteger,vcProtected);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcPublic]);
  AssertEquals('Count',2,aCount);
  CheckField(4, A^[0],'FPublicA',tkInteger,vcPublic);
  CheckField(5, A^[1],'FPublicB',tkInteger,vcPublic);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcPublished]);
  AssertEquals('Count',2,aCount);
  CheckField(6, A^[0],'FPublishedA',tkInteger,vcPublished);
  CheckField(7, A^[1],'FPublishedB',tkInteger,vcPublished);
  FreeMem(A);
end;

Procedure TestRecordFields;

Var
  A : PExtendedFieldInfoTable;
  aCount : Integer;

begin
  aCount:=GetFieldList(TypeInfo(TRecordFieldRTTI),A);
  AssertEquals('Count',4,aCount);
  CheckField(0, A^[0],'FRPrivateA',tkInteger,vcPrivate);
  CheckField(1, A^[1],'FRPrivateB',tkInteger,vcPrivate);
  CheckField(4, A^[2],'FRPublicA',tkInteger,vcPublic);
  CheckField(5, A^[3],'FRPublicB',tkInteger,vcPublic);
  FreeMem(A); A:=Nil;
  aCount:=GetFieldList(TypeInfo(TRecordFieldRTTI),A,[vcPrivate]);
  AssertEquals('Count',2,aCount);
  CheckField(0, A^[0],'FRPrivateA',tkInteger,vcPrivate);
  CheckField(1, A^[1],'FRPrivateB',tkInteger,vcPrivate);
  FreeMem(A); A:=Nil;
  aCount:=GetFieldList(TypeInfo(TRecordFieldRTTI),A,[vcProtected]);
  AssertEquals('Count',0,aCount);
  A:=Nil;
  aCount:=GetFieldList(TypeInfo(TRecordFieldRTTI),A,[vcPublic]);
  AssertEquals('Count',2,aCount);
  CheckField(0, A^[0],'FRPublicA',tkInteger,vcPublic);
  CheckField(1, A^[1],'FRPublicB',tkInteger,vcPublic);
  FreeMem(A); A:=Nil;
  aCount:=GetFieldList(TypeInfo(TRecordFieldRTTI),A,[vcPublished]);
  AssertEquals('Count',0,aCount);
  A:=Nil;
end;


Procedure CheckMethod(aIdx : Integer; aData: PVmtMethodExEntry; aName : String; aVisibility : TVisibilityClass);

Var
  Msg : String;

begin
  Msg:='Checking method '+IntToStr(aIdx)+' ('+aName+') ';
  AssertEquals(Msg+'name',aData^.Name,aName);
  AssertEquals(Msg+'visibility',aData^.MethodVisibility,aVisibility);
end;

procedure TestClassMethods;

Var
  A : PExtendedMethodInfoTable;
  aCount : Integer;

begin
  aCount:=GetMethodList(TMethodClassRTTI,A,[]);
  AssertEquals('Full Count',12,aCount);
  CheckMethod(0, A^[0],'PrivateMethodA',vcPrivate);
  CheckMethod(1, A^[1],'PrivateMethodB',vcPrivate);
  CheckMethod(2, A^[2],'PrivateMethodC',vcPrivate);
  CheckMethod(3, A^[3],'ProtectedMethodA',vcProtected);
  CheckMethod(4, A^[4],'ProtectedMethodB',vcProtected);
  CheckMethod(5, A^[5],'ProtectedMethodC',vcProtected);
  CheckMethod(6, A^[6],'PublicMethodA',vcPublic);
  CheckMethod(7, A^[7],'PublicMethodB',vcPublic);
  CheckMethod(8, A^[8],'PublicMethodC',vcPublic);
  CheckMethod(9, A^[9],'PublishedMethodA',vcPublished);
  CheckMethod(10, A^[10],'PublishedMethodB',vcPublished);
  CheckMethod(11, A^[11],'PublishedMethodC',vcPublished);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcPrivate]);
  AssertEquals('Private Count',3,aCount);
  CheckMethod(0, A^[0],'PrivateMethodA',vcPrivate);
  CheckMethod(1, A^[1],'PrivateMethodA',vcPrivate);
  CheckMethod(2, A^[2],'PrivateMethodC',vcPrivate);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcProtected]);
  AssertEquals('Protected Count',3,aCount);
  CheckMethod(0, A^[0],'ProtectedMethodA',vcProtected);
  CheckMethod(1, A^[1],'ProtectedMethodB',vcProtected);
  CheckMethod(2, A^[2],'ProtectedMethodB',vcProtected);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcPublic]);
  AssertEquals('Public Count',3,aCount);
  CheckMethod(0, A^[0],'PublicMethodA',vcProtected);
  CheckMethod(1, A^[1],'PublicMethodB',vcProtected);
  CheckMethod(2, A^[2],'PublicMethodB',vcProtected);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcPublished]);
  AssertEquals('Published Count',3,aCount);
  CheckMethod(0, A^[0],'PublishedMethodA',vcPublished);
  CheckMethod(1, A^[1],'PublishedMethodB',vcPublished);
  CheckMethod(2, A^[2],'PublishedMethodB',vcPublished);
  FreeMem(A);
end;

{ TMethodClassRTTI }

procedure TMethodClassRTTI.PrivateMethodA;
begin
  //
end;

procedure TMethodClassRTTI.PrivateMethodB;
begin
  //
end;

procedure TMethodClassRTTI.ProtectedMethodA;
begin
  //
end;

procedure TMethodClassRTTI.ProtectedMethodB;
begin
  //
end;

procedure TMethodClassRTTI.PublicMethodA;
begin
  //
end;

procedure TMethodClassRTTI.PublicMethodB;
begin
  //
end;

procedure TMethodClassRTTI.PublishedMethodA;
begin
  //
end;

procedure TMethodClassRTTI.PublishedMethodB;
begin
  //
end;

begin
  // TestProperties;
  // TestClassFields;
  // TestRecordFields;
  TestClassMethods;
end.

