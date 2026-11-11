{ %opt=-gw3 -godwarfproperty }
{ %target=linux,win32,win64 }

program tdwarf3typealias1;
{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch recordcomposition}
{$ModeSwitch statementexpressions}

uses
  udwarf3typealias1_a,
  udwarfhelper, udwarfdump, regexpr, SysUtils;

const
  PROG_NAME='tdwarf3typealias1.pp';
  UNIT_NAME='udwarf3typealias1_a.pp';
  HAS_DWARF_CPP: boolean = False;
  DWARF_VERS: integer = 3;

{$include tdwarftypealias1.inc}
{$include tdwarftypealias1_test.inc}

