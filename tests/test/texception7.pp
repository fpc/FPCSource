{ %version=1.1 }
{ %rte=0 }

uses
  sysutils;

begin
  // this should fail with a runtime error
  AcquireExceptionObject;
end.
