{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Shared embedded source fixtures for the core test units

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit UtstCoreFixtures;

{ Shared embedded source fixtures used by more than one core test unit }

{$mode objfpc}{$H+}

interface

const
  cSmokeFixture: array[0..38] of string = (
    'unit SmokeFixture;',
    '',
    '{ Trivial, valid fixture parsed by the foundation smoke test.',
    '  Lightly exercises both global build prerequisites:',
    '    - an array type with an explicit index range (po_ArrayRangeExpr path)',
    '    - a for loop (TPasImplForLoop, the node patched with IsVarDef) }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TIntTriple = array[1..3] of Integer;',
    '',
    '// Sums a fixed three-element array via a counted for loop.',
    'function SumTriple: Integer;',
    '',
    '',
    'implementation',
    '',
    'function SumTriple: Integer;',
    '',
    'var',
    '  lValues: TIntTriple;',
    '  lIndex: Integer;',
    '',
    'begin',
    '  lValues[1] := 1;',
    '  lValues[2] := 2;',
    '  lValues[3] := 3;',
    '  Result := 0;',
    '  for lIndex := 1 to 3 do',
    '    begin',
    '      Result := Result + lValues[lIndex];',
    '    end;',
    'end;',
    '',
    '',
    'end.');

  cScannerFixture: array[0..13] of string = (
    'unit ScannerFixture;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    '// a line comment',
    'interface',
    '',
    'const',
    '  { a block comment }',
    '  cAnswer = 42;',
    '',
    'implementation',
    '',
    'end.');

  cFaultBad: array[0..13] of string = (
    'unit FaultBad;',
    '',
    '{ deliberate syntax-error for the fault-isolation test. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cBad = ;',
    '',
    'implementation',
    '',
    'end.');


implementation

end.
