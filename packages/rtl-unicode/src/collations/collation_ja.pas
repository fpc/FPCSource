    {   Unicode implementation tables. 
 
        Copyright (c) 2013 - 2017 by Inoussa OUEDRAOGO 
 
        Permission is hereby granted, free of charge, to any person 
        obtaining a copy of the Unicode data files and any associated 
        documentation (the "Data Files") or Unicode software and any 
        associated documentation (the "Software") to deal in the Data 
        Files or Software without restriction, including without 
        limitation the rights to use, copy, modify, merge, publish, 
        distribute, and/or sell copies of the Data Files or Software, 
        and to permit persons to whom the Data Files or Software are 
        furnished to do so, provided that (a) the above copyright 
        notice(s) and this permission notice appear with all copies 
        of the Data Files or Software, (b) both the above copyright 
        notice(s) and this permission notice appear in associated 
        documentation, and (c) there is clear notice in each modified 
        Data File or in the Software as well as in the documentation 
        associated with the Data File(s) or Software that the data or 
        software has been modified. 
 
 
        This program is distributed in the hope that it will be useful, 
        but WITHOUT ANY WARRANTY; without even the implied warranty of 
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$IFDEF FPC}
  {$mode DELPHI}{$H+}
{$ENDIF FPC}

{$IFNDEF FPC}
  {$DEFINE ENDIAN_LITTLE}
{$ENDIF !FPC}

unit collation_ja;

interface

implementation
uses
  unicodedata, unicodeducet;

const
  UPDATED_FIELDS = [ TCollationField.VariableLowLimit, TCollationField.VariableHighLimit ];
  COLLATION_NAME = 'ja';
  BASE_COLLATION = '';
  VARIABLE_LOW_LIMIT = 264;
  VARIABLE_HIGH_LIMIT = 664;
  VARIABLE_WEIGHT = 0;
  BACKWARDS_0 = False;
  BACKWARDS_1 = False;
  BACKWARDS_2 = False;
  BACKWARDS_3 = False;
  PROP_COUNT  = 121294;
  NO_STRING_NORMALIZATION = False;
  COMPARISON_STRENGTH = 3;

const
  UCA_TABLE_1 : array[0..255] of Byte = (
    0,0,0,0,0,0,1,2,0,3,4,5,6,7,8,9,
    10,0,0,11,0,0,0,12,13,14,15,16,17,0,0,0,
    0,18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    19,20,21,22,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,23,24,
    25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
    41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,
    57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,
    73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,
    89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,
    0,0,0,0,0,0,105,0,106,107,108,109,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110
  );

const
  UCAO_TABLE_1 : array[0..1023] of Word = (
    0,1,2,3,4,5,6,7,8,9,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,10,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,11,8,8,8,8,8,12,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
  );

{$ifdef ENDIAN_LITTLE}
  {$include collation_ja_le.inc}
{$else ENDIAN_LITTLE}
  {$include collation_ja_be.inc}
{$endif ENDIAN_LITTLE}
var
  CLDR_Collation : TUCA_DataBook = (
    Base               : nil;
    Version            : 
      (
        Ord('$'),Ord('R'),Ord('e'),Ord('v'),Ord('i'),Ord('s'),Ord('i'),Ord('o'),
        Ord('n'),Ord(':'),Ord(' '),Ord('1'),Ord('1'),Ord('9'),Ord('1'),Ord('4'),
        Ord(' '),Ord('$'),
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      );
    CollationName      : 
      (
        Ord('j'),Ord('a'),
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0
      );
    VariableWeight     : TUCA_VariableKind(VARIABLE_WEIGHT);
    Backwards          : (BACKWARDS_0,BACKWARDS_1,BACKWARDS_2,BACKWARDS_3);
    BMP_Table1         : @UCA_TABLE_1[0];
    BMP_Table2         : @UCA_TABLE_2[0];
    OBMP_Table1        : @UCAO_TABLE_1[0];
    OBMP_Table2        : @UCAO_TABLE_2[0];
    PropCount          : PROP_COUNT;
    Props              : @UCA_PROPS[0];
    VariableLowLimit   : VARIABLE_LOW_LIMIT;
    VariableHighLimit  : VARIABLE_HIGH_LIMIT;
    NoNormalization    : NO_STRING_NORMALIZATION;
    ComparisonStrength : COMPARISON_STRENGTH;
    Dynamic            : False;
  );

procedure Register();
begin
  PrepareCollation(@CLDR_Collation,BASE_COLLATION,UPDATED_FIELDS);
  RegisterCollation(@CLDR_Collation);
end;

initialization
  Register();

finalization
  UnregisterCollation(COLLATION_NAME);

end.
