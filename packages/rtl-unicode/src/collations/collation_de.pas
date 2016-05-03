    {   Unicode implementation tables. 
 
        Copyright (c) 2013 by Inoussa OUEDRAOGO 
 
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

{$mode objfpc}{$H+}
unit collation_de;

interface

implementation
uses
  unicodedata, unicodeducet;

const
  UPDATED_FIELDS = [  ];
  COLLATION_NAME = 'de';
  BASE_COLLATION = '';
  VERSION_STRING = '$Revision: 6365 $';
  VARIABLE_LOW_LIMIT = 65535;
  VARIABLE_HIGH_LIMIT = 0;
  VARIABLE_WEIGHT = 0;
  BACKWARDS_0 = False;
  BACKWARDS_1 = False;
  BACKWARDS_2 = False;
  BACKWARDS_3 = False;

var
  CLDR_Collation : TUCA_DataBook = (
    Base               : nil;
    Version            : VERSION_STRING;
    CollationName      : COLLATION_NAME;
    VariableWeight     : TUCA_VariableKind(VARIABLE_WEIGHT);
    Backwards          : (BACKWARDS_0,BACKWARDS_1,BACKWARDS_2,BACKWARDS_3);
    BMP_Table1         : nil;
    BMP_Table2         : nil;
    OBMP_Table1        : nil;
    OBMP_Table2        : nil;
    PropCount          : 0;
    Props              : nil;
    VariableLowLimit   : VARIABLE_LOW_LIMIT;
    VariableHighLimit  : VARIABLE_HIGH_LIMIT;
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
