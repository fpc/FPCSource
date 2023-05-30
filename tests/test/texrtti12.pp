{$MODE OBJFPC}
{$M+}

{ Test partial generation of RTTI: protected/published properties/methods only }

program texrtti12;

uses typinfo, sysutils, uexrttiutil;

Type
  {$RTTI EXPLICIT
        PROPERTIES([vcProtected,vcPublished])
        FIELDS([vcProtected,vcPublished])
        METHODS([vcProtected,vcPublished])}

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
  aCount : Integer;

begin
  aCount:=GetPropListEx(TFieldRTTI,A);
  try
    AssertEquals('Property Count',4,aCount);
    CheckProperty(0, A^[0]^,'ProtectedA',tkInteger,vcProtected);
    CheckProperty(1, A^[1]^,'ProtectedB',tkInteger,vcProtected,True);
    CheckProperty(2, A^[2]^,'PublishedA',tkInteger,vcPublished);
    CheckProperty(3, A^[3]^,'PublishedB',tkInteger,vcPublished);
  finally
    Freemem(A);
  end;
end;

Procedure TestClassFields;

Var
  A : PExtendedFieldInfoTable;
  aCount : Integer;

begin
  aCount:=GetFieldList(TFieldRTTI,A);
  AssertEquals('Total field Count',2,aCount);
  CheckField(0, A^[0],'FProtectedA',tkInteger,vcProtected);
  CheckField(1, A^[1],'FProtectedB',tkInteger,vcProtected,True);
//  CheckField(2, A^[2],'FPublishedA',tkInteger,vcPublished);
//  CheckField(3, A^[3],'FPublishedB',tkInteger,vcPublished);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcProtected]);
  AssertEquals('Protected field Count',2,aCount);
  CheckField(0, A^[0],'FProtectedA',tkInteger,vcProtected);
  CheckField(1, A^[1],'FProtectedB',tkInteger,vcProtected,True);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcPrivate]);
  AssertEquals('Private field Count',0,aCount);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcPublished]);
  AssertEquals('Published field Count',0,aCount);
//  CheckField(4, A^[0],'FPublishedA',tkInteger,vcPublished);
//  CheckField(5, A^[1],'FPublishedB',tkInteger,vcPublished);
  FreeMem(A);
  aCount:=GetFieldList(TFieldRTTI,A,[vcPublic]);
  AssertEquals('Public field Count',0,aCount);
  FreeMem(A);
end;


procedure TestClassMethods;

Var
  A : PExtendedMethodInfoTable;
  aCount : Integer;

begin
  aCount:=GetMethodList(TMethodClassRTTI,A,[]);
  AssertEquals('Full method Count',6,aCount);
  CheckMethod('Full',0, A^[0],'ProtectedMethodA',vcProtected);
  CheckMethod('Full',1, A^[1],'ProtectedMethodB',vcProtected,True);
  CheckMethod('Full',2, A^[2],'ProtectedMethodC',vcProtected);
  CheckMethod('Full',3, A^[3],'PublishedMethodA',vcPublished);
  CheckMethod('Full',4, A^[4],'PublishedMethodB',vcPublished);
  CheckMethod('Full',5, A^[5],'PublishedMethodC',vcPublished);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcProtected]);
  AssertEquals('Protected method Count',3,aCount);
  CheckMethod('Priv',0, A^[0],'ProtectedMethodA',vcProtected);
  CheckMethod('Priv',1, A^[1],'ProtectedMethodB',vcProtected,True);
  CheckMethod('Priv',2, A^[2],'ProtectedMethodC',vcProtected);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcPrivate]);
  AssertEquals('Private method Count',0,aCount);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcPublished]);
  AssertEquals('Published method Count',3,aCount);
  CheckMethod('Publ',0, A^[0],'PublishedMethodA',vcPublished);
  CheckMethod('Publ',1, A^[1],'PublishedMethodB',vcPublished);
  CheckMethod('Publ',2, A^[2],'PublishedMethodC',vcPublished);
  FreeMem(A);
  aCount:=GetMethodList(TMethodClassRTTI,A,[vcPublic]);
  AssertEquals('Public method Count',0,aCount);
  FreeMem(A);
end;

begin
  TestProperties;
  TestClassFields;
  TestClassMethods;
end.

