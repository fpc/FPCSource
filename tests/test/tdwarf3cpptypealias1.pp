{ %opt=-gw3 -godwarfcpp -godwarfproperty }
{ %target=linux,win32,win64 }

program tdwarf3cpptypealias1;
{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch recordcomposition}
{$ModeSwitch statementexpressions}

uses
  udwarf3cpptypealias1_a,
  udwarfhelper, udwarfdump, regexpr, SysUtils;

const
  PROG_NAME='tdwarf3cpptypealias1.pp';
  UNIT_NAME='udwarf3cpptypealias1_a.pp';
  HAS_DWARF_CPP: boolean = True;
  DWARF_VERS: integer = 3;

{$include tdwarftypealias1.inc}
{$include tdwarftypealias1_test.inc}

