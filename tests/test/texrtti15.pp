{$MODE OBJFPC}
{$M+}
{$Modeswitch advancedrecords}

program texrtti15;

uses typinfo, sysutils, uexrttiutil;

Type
  {$RTTI EXPLICIT
     PROPERTIES([vcPublic])
     FIELDS([vcPublic])
     METHODS([vcPublic])}

  { TMethodClassRTTI }

  TMethodClassRTTI = Class (TObject)
  public
    generic Procedure PublicMethodA<T>(arg : T);
  end;

generic procedure TMethodClassRTTI.PublicMethodA<T>(arg : T);
begin
  Writeln('hiero')
end;

procedure TestClassMethods;

Var
  A : PExtendedMethodInfoTable;
  aCount : Integer;
  AInstance : TMethodClassRTTI;

begin
  aCount:=GetMethodList(TMethodClassRTTI,A,[]);
  AssertEquals('Generic methods do not generate RTTI',0,aCount);
//  CheckMethod('Full',0, A^[0],'PublicMethodA',vcPublic);
end;

begin
  TestClassMethods;
end.

