{*******************************************************************
*  Test library of the Apache Pascal Headers
*******************************************************************}
library testmodule;

{$i define.inc}

{*******************************************************************
*  Assembler code to export variables on UNIXes
*******************************************************************}
uses
 httpd,
 minimain in 'minimain.pas';

var
 test_module: module; {$ifdef Unix} public name 'test_module'; {$endif}
 default_module_ptr: Pmodule;

{*******************************************************************
*  Free Pascal only supports exporting variables on Windows
*******************************************************************}
{$ifdef WINDOWS}
exports
 test_module name 'test_module';
{$endif}

{*******************************************************************
*  Library initialization code
*******************************************************************}
begin
  default_module_ptr := @test_module;
  FillChar(default_module_ptr^, SizeOf(default_module_ptr^), 0);
  with default_module_ptr^ do begin
    version := MODULE_MAGIC_NUMBER_MAJOR;
    minor_version := MODULE_MAGIC_NUMBER_MINOR;
    module_index := -1;
    name := 'testmodule.so';
    magic := MODULE_MAGIC_COOKIE;
    register_hooks := @RegisterHooks;
  end;
end.

