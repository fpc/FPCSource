unit gmp_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

{$include gmp_test_intf}

implementation

uses
  math, strutils, gmp;

{$include gmp_test_impl}

initialization
  RegisterTests([TTestGmpBinding, TTestGmpExtensions, TTestGmpOperators]);

end.

