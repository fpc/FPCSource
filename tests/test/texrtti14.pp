{$MODE OBJFPC}
{$M+}
{$Modeswitch advancedrecords}

program texrtti14;

uses typinfo, sysutils, uexrttiutil;

Type
  {$RTTI EXPLICIT
     PROPERTIES([vcPrivate,vcProtected,vcPublic,vcPublished])
     FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])
     METHODS([vcPrivate,vcProtected,vcPublic,vcPublished])}

  { TFieldRTTI }

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

  TRecordFieldRTTIMixed = record
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
      Procedure DoA;
   end;
  // Use different names, so we can distinguish RTTI in asm file...

  { TRecordMethodRTTI }

  TRecordMethodRTTI = record
    a,b,c : Integer;
  private
    Procedure PrivateMethodA;
    Procedure PrivateMethodB;
  Public
    Procedure PublicMethodA;
    Procedure PublicMethodB;
   end;

  { TRecordMethodRTTI }

  procedure TRecordMethodRTTI.PrivateMethodA;
  begin
    //
  end;

  procedure TRecordMethodRTTI.PrivateMethodB;
  begin
    //
  end;

  procedure TRecordMethodRTTI.PublicMethodA;
  begin
    //
  end;

  procedure TRecordMethodRTTI.PublicMethodB;
  begin
    //
  end;

  Procedure TRecordFieldRTTIMixed.DoA;

  begin
  //
  end;


Procedure TestRecordProperties;

Var
  A : PPropListEx;
  aCount : Integer;

begin
  aCount:=GetPropListEx(TypeInfo(TRecordFieldRTTI),A);
  try
    AssertEquals('Record property Count',4,aCount);
    CheckProperty(0, A^[0]^,'RPrivateA',tkInteger,vcPrivate);
    CheckProperty(1, A^[1]^,'RPrivateB',tkInteger,vcPrivate);
    CheckProperty(2, A^[2]^,'RPublicA',tkInteger,vcPublic);
    CheckProperty(3, A^[3]^,'RPublicB',tkInteger,vcPublic);
  finally
    Freemem(A);
  end;
  aCount:=GetPropListEx(TypeInfo(TRecordFieldRTTIMixed),A);
  try
    AssertEquals('Record mixed property Count',4,aCount);
    CheckProperty(0, A^[0]^,'RPrivateA',tkInteger,vcPrivate);
    CheckProperty(1, A^[1]^,'RPrivateB',tkInteger,vcPrivate);
    CheckProperty(2, A^[2]^,'RPublicA',tkInteger,vcPublic);
    CheckProperty(3, A^[3]^,'RPublicB',tkInteger,vcPublic);
  finally
    Freemem(A);
  end;
end;



Procedure TestRecordFields;

Var
  A : PExtendedFieldInfoTable;
  aCount : Integer;

begin
  aCount:=GetFieldList(TypeInfo(TRecordFieldRTTI),A);
  AssertEquals('Record fields Count',4,aCount);
  CheckField(0, A^[0],'FRPrivateA',tkInteger,vcPrivate);
  CheckField(1, A^[1],'FRPrivateB',tkInteger,vcPrivate);
  CheckField(4, A^[2],'FRPublicA',tkInteger,vcPublic);
  CheckField(5, A^[3],'FRPublicB',tkInteger,vcPublic);
  FreeMem(A); A:=Nil;
  aCount:=GetFieldList(TypeInfo(TRecordFieldRTTI),A,[vcPrivate]);
  AssertEquals('Private Record fields Count',2,aCount);
  CheckField(0, A^[0],'FRPrivateA',tkInteger,vcPrivate);
  CheckField(1, A^[1],'FRPrivateB',tkInteger,vcPrivate);
  FreeMem(A); A:=Nil;
  aCount:=GetFieldList(TypeInfo(TRecordFieldRTTI),A,[vcProtected]);
  AssertEquals('Protected record fields Count',0,aCount);
  A:=Nil;
  aCount:=GetFieldList(TypeInfo(TRecordFieldRTTI),A,[vcPublic]);
  AssertEquals('Public record fields Count',2,aCount);
  CheckField(0, A^[0],'FRPublicA',tkInteger,vcPublic);
  CheckField(1, A^[1],'FRPublicB',tkInteger,vcPublic);
  FreeMem(A); A:=Nil;
  aCount:=GetFieldList(TypeInfo(TRecordFieldRTTI),A,[vcPublished]);
  AssertEquals('Published record fields count Count',0,aCount);
  A:=Nil;
  aCount:=GetFieldList(TypeInfo(TRecordFieldRTTIMixed),A);
  AssertEquals('Mixed record fields Count',4,aCount);
  CheckField(0, A^[0],'FRPrivateA',tkInteger,vcPrivate);
  CheckField(1, A^[1],'FRPrivateB',tkInteger,vcPrivate);
  CheckField(4, A^[2],'FRPublicA',tkInteger,vcPublic);
  CheckField(5, A^[3],'FRPublicB',tkInteger,vcPublic);
  FreeMem(A); A:=Nil;
end;

procedure TestRecordMethods;

Var
  A : PRecordMethodInfoTable;
  aCount : Integer;

begin
  aCount:=GetMethodList(PTypeInfo(TypeInfo(TRecordMethodRTTI)),A,True,[]);
  AssertEquals('Method Full Count',4,aCount);
  CheckMethod('Full',0, A^[0],'PrivateMethodA',vcPrivate);
  CheckMethod('Full',1, A^[1],'PrivateMethodB',vcPrivate);
  CheckMethod('Full',2, A^[2],'PublicMethodA',vcPublic);
  CheckMethod('Full',3, A^[3],'PublicMethodB',vcPublic);
  FreeMem(A);
  aCount:=GetMethodList(PTypeInfo(TypeInfo(TRecordMethodRTTI)),A,False,[vcPrivate]);
  AssertEquals('Method Private Count',2,aCount);
  CheckMethod('Priv',0, A^[0],'PrivateMethodA',vcPrivate);
  CheckMethod('Priv',1, A^[1],'PrivateMethodB',vcPrivate);
  FreeMem(A);
  aCount:=GetMethodList(PTypeInfo(TypeInfo(TRecordMethodRTTI)),A,False,[vcProtected]);
  AssertEquals('Method Protected Count',0,aCount);
  if A<>Nil then
   FreeMem(A);
  aCount:=GetMethodList(PTypeInfo(TypeInfo(TRecordMethodRTTI)),A,False,[vcPublic]);
  AssertEquals('Method Public Count',2,aCount);
  CheckMethod('Publ',0, A^[0],'PublicMethodA',vcPublic);
  CheckMethod('Publ',1, A^[1],'PublicMethodB',vcPublic);
  FreeMem(A);
  aCount:=GetMethodList(PTypeInfo(TypeInfo(TRecordMethodRTTI)),A,False,[vcPublished]);
  AssertEquals('Method Published Count',0,aCount);
  if A<>Nil then
    FreeMem(A);
end;


begin
  TestRecordFields;
//  TestRecordProperties;
  TestRecordMethods;
end.

