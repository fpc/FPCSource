{$MODE OBJFPC}
{$M+}
{$Modeswitch advancedrecords}

program texrtti10;

uses typinfo, sysutils, uexrttiutil;

Type
  {$RTTI EXPLICIT
     PROPERTIES([vcPrivate,vcProtected,vcPublic,vcPublished])
     FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])
     METHODS([vcPrivate,vcProtected,vcPublic,vcPublished])}

  { TFieldRTTI }

  TFieldRTTI = Class
  private
    FPrivateA: Integer;
    Property PrivateA : Integer Read FPrivateA Write FPrivateA;
  strict private
    FPrivateB: Integer;
    Property PrivateB : Integer Read FPrivateB Write FPrivateB;
  Protected
    FProtectedA: Integer;
    Property ProtectedA : Integer Read FProtectedA Write FProtectedA;
  Strict Protected
    FProtectedB: Integer;
    Property ProtectedB : Integer Read FProtectedB Write FProtectedB;
  Public
    FPublicA: Integer;
    FPublicB: Integer;
    Property PublicA : Integer Read FPublicA Write FPublicA;
    Property PublicB : Integer Read FPublicA Write FPublicB;
  Private
    FPublishedA: Integer;
    FPublishedB: Integer;
  Published
    FPublishedC: TFieldRTTI;
    FPublishedD: TFieldRTTI;
    Property PublishedA : Integer Read FPublishedA Write FPublishedA;
    Property PublishedB : Integer Read FPublishedA Write FPublishedB;
  end;

  { TMethodClassRTTI }

  TMethodClassRTTI = Class (TObject)
  private
    Procedure PrivateMethodA;
  strict private
    Procedure PrivateMethodB; virtual;
  private
    Procedure PrivateMethodC; virtual; abstract;
  protected
    Procedure ProtectedMethodA;
  strict protected
    Procedure ProtectedMethodB; virtual;
  protected
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

(*
  // No published section

  { TMethodClassRTTI2 }

  TMethodClassRTTI2 = Class (TObject)
  private
    Procedure PrivateMethodA;
  strict private
    Procedure PrivateMethodB; virtual;
  private
    Procedure PrivateMethodC; virtual; abstract;
  protected
    Procedure ProtectedMethodA;
  strict protected
    Procedure ProtectedMethodB; virtual;
  protected
    Procedure ProtectedMethodC; virtual; abstract;
  public
    Procedure PublicMethodA;
    Procedure PublicMethodB; virtual;
    Procedure PublicMethodC; virtual; abstract;
  end;

{ TMethodClassRTTI2 }

procedure TMethodClassRTTI2.PrivateMethodA;
begin

end;

procedure TMethodClassRTTI2.PrivateMethodB;
begin

end;

procedure TMethodClassRTTI2.ProtectedMethodA;
begin

end;

procedure TMethodClassRTTI2.ProtectedMethodB;
begin

end;

procedure TMethodClassRTTI2.PublicMethodA;
begin

end;

procedure TMethodClassRTTI2.PublicMethodB;
begin

end;

*)

{ TMethodClassRTTI }

procedure TMethodClassRTTI.PrivateMethodA;
begin

end;

procedure TMethodClassRTTI.PrivateMethodB;
begin

end;

procedure TMethodClassRTTI.ProtectedMethodA;
begin

end;

procedure TMethodClassRTTI.ProtectedMethodB;
begin

end;

procedure TMethodClassRTTI.PublicMethodA;
begin

end;

procedure TMethodClassRTTI.PublicMethodB;
begin

end;

procedure TMethodClassRTTI.PublishedMethodA;
begin

end;

procedure TMethodClassRTTI.PublishedMethodB;
begin

end;


Procedure TestProperties;

Var
  A : PPropListEx;
  aCount : Integer;

begin
  aCount:=GetPropListEx(TFieldRTTI,A);
  try
    AssertEquals('Property Count',8,aCount);
    CheckProperty(0, A^[0]^,'PrivateA',tkInteger,vcPrivate);
    CheckProperty(1, A^[1]^,'PrivateB',tkInteger,vcPrivate,True);
    CheckProperty(2, A^[2]^,'ProtectedA',tkInteger,vcProtected);
    CheckProperty(3, A^[3]^,'ProtectedB',tkInteger,vcProtected,True);
    CheckProperty(4, A^[4]^,'PublicA',tkInteger,vcPublic);
    CheckProperty(5, A^[5]^,'PublicB',tkInteger,vcPublic);
    CheckProperty(6, A^[6]^,'PublishedA',tkInteger,vcPublished);
    CheckProperty(7, A^[7]^,'PublishedB',tkInteger,vcPublished);
  finally
    Freemem(A);
  end;
end;

Procedure TestClassFields;

Var
  A : PExtendedFieldInfoTable;
  aCount : Integer;
  t : TFieldRTTI;

begin
  // O:=TFieldRTTI.Create;
  // aCount:=TFieldRTTI.InstanceSize;
  // aCount:=PtrInt(O.FieldAddress('PublField'))-PtrInt(O);
  aCount:=GetFieldList(TFieldRTTI,A);
  AssertEquals('Class field Count',10,aCount);
  CheckField(0, A^[0],'FPrivateA',tkInteger,vcPrivate);
  CheckField(1, A^[1],'FPrivateB',tkInteger,vcPrivate,True);
  CheckField(2, A^[2],'FProtectedA',tkInteger,vcProtected);
  CheckField(3, A^[3],'FProtectedB',tkInteger,vcProtected,True);
  CheckField(4, A^[4],'FPublicA',tkInteger,vcPublic);
  CheckField(5, A^[5],'FPublicB',tkInteger,vcPublic);
  CheckField(6, A^[6],'FPublishedA',tkInteger,vcPrivate);
  CheckField(7, A^[7],'FPublishedB',tkInteger,vcPrivate);
  CheckField(8, A^[8],'FPublishedC',tkClass,vcPublished);
  CheckField(9, A^[9],'FPublishedD',tkClass,vcPublished);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcPrivate]);
  AssertEquals('Count',4,aCount);
  CheckField(0, A^[0],'FPrivateA',tkInteger,vcPrivate);
  CheckField(1, A^[1],'FPrivateB',tkInteger,vcPrivate,True);
  CheckField(2, A^[2],'FPublishedA',tkInteger,vcPrivate);
  CheckField(3, A^[3],'FPublishedB',tkInteger,vcPrivate);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcProtected]);
  AssertEquals('Count',2,aCount);
  CheckField(2, A^[0],'FProtectedA',tkInteger,vcProtected);
  CheckField(3, A^[1],'FProtectedB',tkInteger,vcProtected,True);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcPublic]);
  AssertEquals('Count',2,aCount);
  CheckField(4, A^[0],'FPublicA',tkInteger,vcPublic);
  CheckField(5, A^[1],'FPublicB',tkInteger,vcPublic);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcPublished]);
  AssertEquals('Count',2,aCount);
  CheckField(8, A^[0],'FPublishedC',tkClass,vcPublished);
  CheckField(9, A^[1],'FPublishedD',tkClass,vcPublished);
  t := TFieldRTTI.Create;
  AssertEquals('Legacy Field 0', A^[0]^.FieldOffset, Integer(PByte(t.FieldAddress('FPublishedC')) - PByte(t)));
  AssertEquals('Legacy Field 1', A^[1]^.FieldOffset, Integer(PByte(t.FieldAddress('FPublishedD')) - PByte(t)));
  t.Free;
  //FreeMem(A);
end;


procedure TestClassMethods;

Var
  A : PExtendedMethodInfoTable;
  aCount : Integer;
  AInstance : TMethodClassRTTI;

begin
  aCount:=GetMethodList(TMethodClassRTTI,A,[]);
  AssertEquals('Full Count',12,aCount);
  CheckMethod('Full',0, A^[0],'PrivateMethodA',vcPrivate);
  CheckMethod('Full',1, A^[1],'PrivateMethodB',vcPrivate,True);
  CheckMethod('Full',2, A^[2],'PrivateMethodC',vcPrivate);
  CheckMethod('Full',3, A^[3],'ProtectedMethodA',vcProtected);
  CheckMethod('Full',4, A^[4],'ProtectedMethodB',vcProtected,True);
  CheckMethod('Full',5, A^[5],'ProtectedMethodC',vcProtected);
  CheckMethod('Full',6, A^[6],'PublicMethodA',vcPublic);
  CheckMethod('Full',7, A^[7],'PublicMethodB',vcPublic);
  CheckMethod('Full',8, A^[8],'PublicMethodC',vcPublic);
  CheckMethod('Full',9, A^[9],'PublishedMethodA',vcPublished);
  CheckMethod('Full',10, A^[10],'PublishedMethodB',vcPublished);
  CheckMethod('Full',11, A^[11],'PublishedMethodC',vcPublished);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcPrivate]);
  AssertEquals('Private Count',3,aCount);
  CheckMethod('Priv',0, A^[0],'PrivateMethodA',vcPrivate);
  CheckMethod('Priv',1, A^[1],'PrivateMethodB',vcPrivate,True);
  CheckMethod('Priv',2, A^[2],'PrivateMethodC',vcPrivate);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcProtected]);
  AssertEquals('Protected Count',3,aCount);
  CheckMethod('Prot',0, A^[0],'ProtectedMethodA',vcProtected);
  CheckMethod('Prot',1, A^[1],'ProtectedMethodB',vcProtected,True);
  CheckMethod('Prot',2, A^[2],'ProtectedMethodC',vcProtected);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcPublic]);
  AssertEquals('Public Count',3,aCount);
  CheckMethod('Publ',0, A^[0],'PublicMethodA',vcPublic);
  CheckMethod('Publ',1, A^[1],'PublicMethodB',vcPublic);
  CheckMethod('Publ',2, A^[2],'PublicMethodC',vcPublic);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcPublished]);
  AssertEquals('Published Count',3,aCount);
  CheckMethod('Pubs',0, A^[0],'PublishedMethodA',vcPublished);
  CheckMethod('Pubs',1, A^[1],'PublishedMethodB',vcPublished);
  CheckMethod('Pubs',2, A^[2],'PublishedMethodC',vcPublished);
  AssertSame('Method',@TMethodClassRTTI.PublishedMethodA, TMethodClassRTTI.MethodAddress('PublishedMethodA'));
  AssertSame('Method',@TMethodClassRTTI.PublishedMethodB, TMethodClassRTTI.MethodAddress('PublishedMethodB'));
  AssertNull('Method',TMethodClassRTTI.MethodAddress('PublishedMethodC'));
  FreeMem(A);
end;

(*
procedure TestClassMethods2;

Var
  A : PExtendedMethodInfoTable;
  aCount : Integer;

begin
  aCount:=GetMethodList(TMethodClassRTTI2,A,[]);
  AssertEquals('Full Count',9,aCount);
  CheckMethod('Full',0, A^[0],'PrivateMethodA',vcPrivate);
  CheckMethod('Full',1, A^[1],'PrivateMethodB',vcPrivate,True);
  CheckMethod('Full',2, A^[2],'PrivateMethodC',vcPrivate);
  CheckMethod('Full',3, A^[3],'ProtectedMethodA',vcProtected);
  CheckMethod('Full',4, A^[4],'ProtectedMethodB',vcProtected,True);
  CheckMethod('Full',5, A^[5],'ProtectedMethodC',vcProtected);
  CheckMethod('Full',6, A^[6],'PublicMethodA',vcPublic);
  CheckMethod('Full',7, A^[7],'PublicMethodB',vcPublic);
  CheckMethod('Full',8, A^[8],'PublicMethodC',vcPublic);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI2,A,[vcPrivate]);
  AssertEquals('Private Count',3,aCount);
  CheckMethod('Priv',0, A^[0],'PrivateMethodA',vcPrivate);
  CheckMethod('Priv',1, A^[1],'PrivateMethodB',vcPrivate,True);
  CheckMethod('Priv',2, A^[2],'PrivateMethodC',vcPrivate);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI2,A,[vcProtected]);
  AssertEquals('Protected Count',3,aCount);
  CheckMethod('Prot',0, A^[0],'ProtectedMethodA',vcProtected);
  CheckMethod('Prot',1, A^[1],'ProtectedMethodB',vcProtected,True);
  CheckMethod('Prot',2, A^[2],'ProtectedMethodC',vcProtected);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI2,A,[vcPublic]);
  AssertEquals('Public Count',3,aCount);
  CheckMethod('Publ',0, A^[0],'PublicMethodA',vcPublic);
  CheckMethod('Publ',1, A^[1],'PublicMethodB',vcPublic);
  CheckMethod('Publ',2, A^[2],'PublicMethodC',vcPublic);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI2,A,[vcPublished]);
  AssertEquals('Published Count',0,aCount);
  FreeMem(A);
end;
*)

begin
  TestProperties;
  TestClassFields;
  TestClassMethods;
  // TestClassMethods2;
end.

