procedure TestProc1; external;
procedure TestProc2; pascal; external;

procedure Ext_TestProc1; public name 'TestProc1';
begin
end;

procedure Ext_TestProc2; pascal; public name 'TestProc2';
begin
end;

begin
  TestProc1;
  TestProc2;
end.
