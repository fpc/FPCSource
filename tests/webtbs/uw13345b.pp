unit uw13345b;
 {$mode DELPHI}
interface
uses uw13345c;
implementation
  type
    TTestIntf=class(TInterfacedObject,ITestIF)
      procedure Test;
    end;

  procedure TTestIntf.Test;
  begin
    writeln('OK');
  end;
initialization
 GTEST:=TTestIntf.Create;
 writeln('ASSIGNED IF');
end.
