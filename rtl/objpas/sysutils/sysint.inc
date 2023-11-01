{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by the Free Pascal development team

    International settings for Sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

class function TFormatSettings.Invariant : TFormatSettings; 

const
  DefShortMonthNames : TMonthNameArray = ('Jan','Feb','Mar','Apr','May','Jun', 
                      'Jul','Aug','Sep','Oct','Nov','Dec');
  DefLongMonthNames : TMonthNameArray = ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
  DefShortDayNames : TWeekNameArray = ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
  DefLongDayNames : TWeekNameArray = ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
                     
begin
  With Result do
    begin
    CurrencyFormat:=1;
    NegCurrFormat:=5;
    ThousandSeparator:=',';
    DecimalSeparator:='.';
    CurrencyDecimals:=2;
    DateSeparator:='-';
    TimeSeparator:=':';
    ListSeparator:=',';
    CurrencyString:='$';
    ShortDateFormat:='d/m/y';
    LongDateFormat:='dd" "mmmm" "yyyy';
    TimeAMString:='AM';
    TimePMString:='PM';
    ShortTimeFormat:='hh:nn';
    LongTimeFormat:='hh:nn:ss';
    ShortMonthNames:=DefShortMonthNames;
    LongMonthNames:=DefLongMonthNames;
    ShortDayNames:=DefShortDayNames;
    LongDayNames:=DefLongDayNames;
    end;
end;

class function TFormatSettings.Create: TFormatSettings; inline;

begin
  Result:=DefaultFormatSettings;
end;

procedure InitInternationalGeneric;
  begin
    fillchar(SysLocale,sizeof(SysLocale),0);

    { keep these routines out of the executable? }
{$ifndef FPC_NOGENERICANSIROUTINES}
    { don't override a previously installed widestring manager }
    if not assigned(widestringmanager.UpperAnsiStringProc) then
      widestringmanager.UpperAnsiStringProc:=@GenericAnsiUpperCase;
    if not assigned(widestringmanager.LowerAnsiStringProc) then
      widestringmanager.LowerAnsiStringProc:=@GenericAnsiLowerCase;
    if not assigned(widestringmanager.CompareStrAnsiStringProc) then
      widestringmanager.CompareStrAnsiStringProc:=@GenericAnsiCompareStr;
    if not assigned(widestringmanager.CompareTextAnsiStringProc) then
      widestringmanager.CompareTextAnsiStringProc:=@GenericAnsiCompareText;
    if not assigned(widestringmanager.StrCompAnsiStringProc) then
      widestringmanager.StrCompAnsiStringProc:=@GenericAnsiStrComp;
    if not assigned(widestringmanager.StrICompAnsiStringProc) then
      widestringmanager.StrICompAnsiStringProc:=@GenericAnsiStrIComp;
    if not assigned(widestringmanager.StrLCompAnsiStringProc) then
      widestringmanager.StrLCompAnsiStringProc:=@GenericAnsiStrLComp;
    if not assigned(widestringmanager.StrLICompAnsiStringProc) then
      widestringmanager.StrLICompAnsiStringProc:=@GenericAnsiStrLIComp;
    if not assigned(widestringmanager.StrLowerAnsiStringProc) then
      widestringmanager.StrLowerAnsiStringProc:=@GenericAnsiStrLower;
    if not assigned(widestringmanager.StrUpperAnsiStringProc) then
      widestringmanager.StrUpperAnsiStringProc:=@GenericAnsiStrUpper;
{$endif FPC_NOGENERICANSIROUTINES}
  end;

