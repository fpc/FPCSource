{ %version=1.1 }
{ %fail }
uses
  sysutils;

begin
  // this should fail with a runtime error
  AcquireExceptionObject;
end.
