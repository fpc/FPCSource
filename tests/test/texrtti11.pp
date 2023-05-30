{$MODE OBJFPC}
{$M+}

{ Test partial generation of RTTI: private/public properties/methods only }

program texrtti11;

uses typinfo, sysutils, uexrttiutil;

Type
  {$RTTI EXPLICIT
     PROPERTIES([vcPrivate,vcPublic])
     FIELDS([vcPrivate,vcPublic])
     METHODS([vcPrivate,vcPublic])}

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
    FPublishedA: Integer;
    FPublishedB: Integer;
    Property PublicA : Integer Read FPublicA Write FPublicA;
    Property PublicB : Integer Read FPublicA Write FPublicB;
  Published
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
  AL : PPropList;
  aCount : Integer;

begin
  aCount:=GetPropListEx(TFieldRTTI,A);
  try
    AssertEquals('Count',4,aCount);
    CheckProperty(0, A^[0]^,'PrivateA',tkInteger,vcPrivate);
    CheckProperty(1, A^[1]^,'PrivateB',tkInteger,vcPrivate,True);
    CheckProperty(2, A^[2]^,'PublicA',tkInteger,vcPublic);
    CheckProperty(3, A^[3]^,'PublicB',tkInteger,vcPublic);
  finally
    Freemem(A);
  end;
  // Check legacy published property list
  aCount:=GetPropList(TFieldRTTI,AL);
  try
    AssertEquals('Legacy Count',2,aCount);
  finally
    Freemem(AL);
  end;
end;

Procedure TestClassFields;

Var
  A : PExtendedFieldInfoTable;
  aCount : Integer;

begin
  aCount:=GetFieldList(TFieldRTTI,A);
  AssertEquals('Count',6,aCount);
  CheckField(0, A^[0],'FPrivateA',tkInteger,vcPrivate);
  CheckField(1, A^[1],'FPrivateB',tkInteger,vcPrivate,True);
  CheckField(2, A^[2],'FPublicA',tkInteger,vcPublic);
  CheckField(3, A^[3],'FPublicB',tkInteger,vcPublic);
  CheckField(4, A^[4],'FPublishedA',tkInteger,vcPublic);
  CheckField(5, A^[5],'FPublishedB',tkInteger,vcPublic);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcPrivate]);
  AssertEquals('Count',2,aCount);
  CheckField(0, A^[0],'FPrivateA',tkInteger,vcPrivate);
  CheckField(1, A^[1],'FPrivateB',tkInteger,vcPrivate,True);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcProtected]);
  AssertEquals('Count',0,aCount);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcPublic]);
  AssertEquals('Count',4,aCount);
  CheckField(0, A^[0],'FPublicA',tkInteger,vcPublic);
  CheckField(1, A^[1],'FPublicB',tkInteger,vcPublic);
  CheckField(2, A^[2],'FPublishedA',tkInteger,vcPublic);
  CheckField(3, A^[3],'FPublishedB',tkInteger,vcPublic);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcPublished]);
  AssertEquals('Count',0,aCount);
  FreeMem(A);
end;


procedure TestClassMethods;

Var
  A : PExtendedMethodInfoTable;
  aCount : Integer;

begin
  aCount:=GetMethodList(TMethodClassRTTI,A,[]);
  AssertEquals('Full Count',6,aCount);
  CheckMethod('Full',0, A^[0],'PrivateMethodA',vcPrivate);
  CheckMethod('Full',1, A^[1],'PrivateMethodB',vcPrivate,True);
  CheckMethod('Full',2, A^[2],'PrivateMethodC',vcPrivate);
  CheckMethod('Full',3, A^[3],'PublicMethodA',vcPublic);
  CheckMethod('Full',4, A^[4],'PublicMethodB',vcPublic);
  CheckMethod('Full',5, A^[5],'PublicMethodC',vcPublic);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcPrivate]);
  AssertEquals('Private Count',3,aCount);
  CheckMethod('Priv',0, A^[0],'PrivateMethodA',vcPrivate);
  CheckMethod('Priv',1, A^[1],'PrivateMethodB',vcPrivate,True);
  CheckMethod('Priv',2, A^[2],'PrivateMethodC',vcPrivate);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcProtected]);
  AssertEquals('Protected Count',0,aCount);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcPublic]);
  AssertEquals('Public Count',3,aCount);
  CheckMethod('Publ',0, A^[0],'PublicMethodA',vcPublic);
  CheckMethod('Publ',1, A^[1],'PublicMethodB',vcPublic);
  CheckMethod('Publ',2, A^[2],'PublicMethodC',vcPublic);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcPublished]);
  AssertEquals('Published Count',0,aCount);
  FreeMem(A);
end;

begin
  TestProperties;
  TestClassFields;
  TestClassMethods;
end.

