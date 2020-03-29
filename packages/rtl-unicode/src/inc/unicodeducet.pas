{   Unicode DUCET unit.

    Copyright (c) 2013 by Inoussa OUEDRAOGO

    The source code is distributed under the Library GNU
    General Public License with the following modification:

        - object files and libraries linked into an application may be
          distributed without source code.

    If you didn't receive a copy of the file COPYING, contact:
          Free Software Foundation
          675 Mass Ave
          Cambridge, MA  02139
          USA

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit unicodeducet;
{$IFDEF FPC}
  {$mode delphi}
  {$H+}
  {$PACKENUM 1}
{$ENDIF FPC}

{$IFNDEF FPC}
  {$DEFINE ENDIAN_LITTLE}
{$ENDIF !FPC}

{$SCOPEDENUMS ON}

interface

implementation
uses
  unicodedata;

{$INCLUDE ucadata.inc}
{$IFDEF ENDIAN_LITTLE}
  {$INCLUDE ucadata_le.inc}
{$ENDIF ENDIAN_LITTLE}
{$IFDEF ENDIAN_BIG}
  {$INCLUDE ucadata_be.inc}
{$ENDIF ENDIAN_BIG}

const
  UCA_DataBookRecord : TUCA_DataBook = (
    Base               : nil;
    Version            :
      ( Ord('9'),Ord('.'),Ord('0'),Ord('.'),Ord('0'),0,0,0,
        0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0
      );
    CollationName      :
      (
        Ord('D'),Ord('U'),Ord('C'),Ord('E'),Ord('T'),0,0,0,
        0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0
      );//'Default Unicode Collation Element Table (DUCET)'
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
    NoNormalization    : False;
    ComparisonStrength : DEFAULT_UCA_COMPARISON_STRENGTH;
    Dynamic            : False;
  );

 initialization
   RegisterCollation(@UCA_DataBookRecord,['ROOT']);

end.

