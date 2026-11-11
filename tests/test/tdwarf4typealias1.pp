{ %opt=-gw4 -godwarfproperty }
{ %target=linux,win32,win64 }

program tdwarf4typealias1;
{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch recordcomposition}
{$ModeSwitch statementexpressions}

uses
  udwarf4typealias1_a,
  udwarfhelper, udwarfdump, regexpr, SysUtils;

const
  PROG_NAME='tdwarf4typealias1.pp';
  UNIT_NAME='udwarf4typealias1_a.pp';
  HAS_DWARF_CPP: boolean = False;
  DWARF_VERS: integer = 4;

{$include tdwarftypealias1.inc}
{$include tdwarftypealias1_test.inc}

