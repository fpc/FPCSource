{ %version=1.1 }
{ %result=231 }

uses
  sysutils;

begin
  // this should fail with a runtime error
  AcquireExceptionObject;
end.
