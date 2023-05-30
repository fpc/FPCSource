{$MODE OBJFPC}
{$M+}

{ Test disabling generation of RTTI: no extra RTTI }

program texrtti13;

uses typinfo, sysutils, uexrttiutil;

Type
  {$RTTI EXPLICIT
        PROPERTIES([])
        FIELDS([])
        METHODS([])}

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
    AssertEquals('Property Count',0,aCount);
  finally
    Freemem(A);
  end;
  // Check that legacy property info is still there
  aCount:=GetPropList(TFieldRTTI,AL);
  try
    AssertEquals('Legacy property Count',2,aCount);
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
  AssertEquals('Total field Count',0,aCount);
  FreeMem(A);
end;


procedure TestClassMethods;

Var
  A : PExtendedMethodInfoTable;
  aCount : Integer;

begin
  aCount:=GetMethodList(TMethodClassRTTI,A,[]);
  AssertEquals('Full method Count',0,aCount);
  FreeMem(A);
end;

begin
  TestProperties;
  TestClassFields;
  TestClassMethods;
end.

