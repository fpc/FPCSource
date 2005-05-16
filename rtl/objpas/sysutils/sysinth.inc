{
    $Id: sysinth.inc,v 1.3 2005/02/26 14:38:14 florian Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    International settings for Sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  All the variables presented here must be set by the InitInternational
  routine. They must be set to match the 'local' settings, although
  most have an initial value.


  These routines are OS-dependent.
}

{ ---------------------------------------------------------------------
    Upper/lowercase translations
  ---------------------------------------------------------------------}

type
   TCaseTranslationTable = array[0..255] of char;

var
   { Tables with upper and lowercase forms of character sets.
     MUST be initialized with the correct code-pages }
   UpperCaseTable: TCaseTranslationTable;
   LowerCaseTable: TCaseTranslationTable;

{ ---------------------------------------------------------------------
    Date formatting settings
  ---------------------------------------------------------------------}

Const

   { Character to be put between date, month and year }
   DateSeparator: char = '-';

   { Format used for short date notation }
   ShortDateFormat: string = 'd/m/y';

   { Format used for long date notation }
   LongDateFormat: string = 'dd" "mmmm" "yyyy';


   { Short names of months. }
   ShortMonthNames: array[1..12] of string[128] =
     ('Jan','Feb','Mar','Apr','May','Jun',
      'Jul','Aug','Sep','Oct','Nov','Dec');

   { Long names of months. }
   LongMonthNames: array[1..12] of string[128] =
     ('January','February','March','April','May','June',
      'July','August','September','October','November','December');

   { Short names of days }
   ShortDayNames: array[1..7] of string[128] =
     ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');

   { Full names of days }
   LongDayNames: array[1..7] of string[128] =
     ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');

   { Format used for short time notation }
   ShortTimeFormat: string[128] = 'hh:nn';

   { Format used for long time notation }
   LongTimeFormat: string[128] = 'hh:nn:ss';

   { Character to be put between hours and minutes }
   TimeSeparator: char = ':';

   { String to indicate AM time when using 12 hour clock. }
   TimeAMString: string[7] = 'AM';

   { String to indicate PM time when using 12 hour clock. }
   TimePMString: string[7] = 'PM';



{ ---------------------------------------------------------------------
    Number formatting constants
  ---------------------------------------------------------------------}


  { Character that comes between integer and fractional part of a number }
  DecimalSeparator : Char = '.';

  { Character that is put every 3 numbers in a currency }
  ThousandSeparator : Char = ',';

  { Number of decimals to use when formatting a currency.  }
  CurrencyDecimals : Byte = 2;

  { Format to use when formatting currency :
    0 = $1
    1 = 1$
    2 = $ 1
    3 = 1 $
    4 = Currency string replaces decimal indicator. e.g. 1$50
   }
  CurrencyFormat : Byte = 1;

  { Same as above, only for negative currencies:
    0 = ($1)
    1 = -$1
    2 = $-1
    3 = $1-
    4 = (1$)
    5 = -1$
    6 = 1-$
    7 = 1$-
    8 = -1 $
    9 = -$ 1
    10 = $ 1-
   }
  NegCurrFormat : Byte = 5;

  { Currency notation. Default is $ for dollars. }
  CurrencyString : String[7] = '$';

type
  TSysLocale = record
    case byte of
      { win32 names }
      1 : (FarEast: boolean; MiddleEast: Boolean);
      { real meaning }
      2 : (MBCS : boolean; RightToLeft: Boolean);
  end;

var
  SysLocale : TSysLocale;


{
  $Log: sysinth.inc,v $
  Revision 1.3  2005/02/26 14:38:14  florian
    + SysLocale

  Revision 1.2  2005/02/14 17:13:31  peter
    * truncate log

}
