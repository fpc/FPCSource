{
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
  TMonthNameArray = array[1..12] of string;
  TWeekNameArray = array[1..7] of string;

  TFormatSettings = record
    CurrencyFormat: Byte;
    NegCurrFormat: Byte;
    ThousandSeparator: Char;
    DecimalSeparator: Char;
    CurrencyDecimals: Byte;
    DateSeparator: Char;
    TimeSeparator: Char;
    ListSeparator: Char;
    CurrencyString: string;
    ShortDateFormat: string;
    LongDateFormat: string;
    TimeAMString: string;
    TimePMString: string;
    ShortTimeFormat: string;
    LongTimeFormat: string;
    ShortMonthNames: TMonthNameArray;
    LongMonthNames: TMonthNameArray;
    ShortDayNames: TWeekNameArray;
    LongDayNames: TWeekNameArray;
    TwoDigitYearCenturyWindow: Word;
  end;

var
  DefaultFormatSettings : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun', 
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );

  FormatSettings : TFormatSettings absolute DefaultFormatSettings;
{ ---------------------------------------------------------------------
    Date formatting settings
  ---------------------------------------------------------------------}

Var

   { Character to be put between date, month and year }
   DateSeparator: char absolute DefaultFormatSettings.DateSeparator deprecated;

   { Format used for short date notation }
   ShortDateFormat: string absolute DefaultFormatSettings.ShortDateFormat deprecated;

   { Format used for long date notation }
   LongDateFormat: string absolute DefaultFormatSettings.LongDateFormat deprecated;


   { Short names of months. }
   ShortMonthNames: TMonthNameArray absolute DefaultFormatSettings.ShortMonthNames deprecated;

   { Long names of months. }
   LongMonthNames: TMonthNameArray absolute DefaultFormatSettings.LongMonthNames deprecated;

   { Short names of days }
   ShortDayNames: TWeekNameArray absolute DefaultFormatSettings.ShortDayNames deprecated;

   { Full names of days }
   LongDayNames: TWeekNameArray absolute DefaultFormatSettings.LongDayNames deprecated;

   { Format used for short time notation }
   ShortTimeFormat: string absolute DefaultFormatSettings.ShortTimeFormat deprecated;

   { Format used for long time notation }
   LongTimeFormat: string absolute DefaultFormatSettings.LongTimeFormat deprecated;

   { Character to be put between hours and minutes }
   TimeSeparator: char absolute DefaultFormatSettings.TimeSeparator deprecated;

   { String to indicate AM time when using 12 hour clock. }
   TimeAMString: string absolute DefaultFormatSettings.TimeAMString deprecated;

   { String to indicate PM time when using 12 hour clock. }
   TimePMString: string absolute DefaultFormatSettings.TimePMString deprecated;



{ ---------------------------------------------------------------------
    Number formatting constants
  ---------------------------------------------------------------------}


  { Character that comes between integer and fractional part of a number }
  DecimalSeparator : Char absolute DefaultFormatSettings.DecimalSeparator deprecated;

  { Character that is put every 3 numbers in a currency }
  ThousandSeparator : Char absolute DefaultFormatSettings.ThousandSeparator deprecated;

  { Number of decimals to use when formatting a currency.  }
  CurrencyDecimals : Byte absolute DefaultFormatSettings.CurrencyDecimals deprecated;

  { Format to use when formatting currency :
    0 = $1
    1 = 1$
    2 = $ 1
    3 = 1 $
    4 = Currency string replaces decimal indicator. e.g. 1$50
   }
  CurrencyFormat : Byte absolute DefaultFormatSettings.CurrencyFormat deprecated;

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
  NegCurrFormat : Byte absolute DefaultFormatSettings.NegCurrFormat deprecated;

  { Currency notation. Default is $ for dollars. }
  CurrencyString : String absolute DefaultFormatSettings.CurrencyString deprecated;

  ListSeparator: Char absolute DefaultFormatSettings.ListSeparator deprecated;

type
  TSysLocale = record
    { Delphi compat fields}
    DefaultLCID,
    PriLangID,
    SubLangID  : Integer;

    case byte of
      { win32 names }
      1 : (FarEast: boolean; MiddleEast: Boolean);
      { real meaning }
      2 : (MBCS : boolean; RightToLeft: Boolean);
  end;

var
  SysLocale : TSysLocale;


