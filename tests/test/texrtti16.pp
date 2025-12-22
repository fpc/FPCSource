{$MODE OBJFPC}
{$M+}

{
  test that TMethodClassRTTI inherits RTTI settings of TBase
  Note that the system unit must be compiled without extended RTTI generation TObject
}


program texrtti16;

uses typinfo, sysutils, uexrtti16, uexrttiutil;

Type
  { TMethodClassRTTI }

  TMethodClassRTTI = Class (TBase)
  Private
    FY : Boolean;
  public
    Procedure PublicMethod;
    Property Y : Boolean Read FY Write FY;
  end;

procedure TMethodClassRTTI.PublicMethod;
begin
  Writeln('hiero')
end;

procedure TestClassMethods;

Var
  A : PExtendedMethodInfoTable;
  aCount : Integer;
  AInstance : TMethodClassRTTI;
  P: PPropListEx;

begin
  aCount:=GetMethodList(TMethodClassRTTI,A,[]);
  AssertEquals('Public method has extended RTTI',1,aCount);
  CheckMethod('Full',0, A^[0],'PublicMethod',vcPublic);
  aCount:=GetPropListEx(TMethodClassRTTI,P);
  AssertEquals('Public property has RTTI',1,aCount);
  CheckProperty(0, P^[0]^,'Y',tkBool,vcPublic,false);
end;

begin
  TestClassMethods;
end.

