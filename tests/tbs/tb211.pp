{ Old file: tbs0241.pp }
{ Problem with importing function from a DLL with .drv suffix ! OK 0.99.11 (PM) }

{$ifdef win32}
program test_win32_drv;

procedure printer;external 'winspool.drv' name 'AbortPrinter';
procedure test;

 begin
   Writeln('Loading of Winspool works ');
 end;

begin
  test;
{$else}
begin
{$endif}
end.
