(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: DateTime.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Date and Time calculations
 *
 * History:
 *    1/19/95  rsf - Created by Roger Flores
 *    7/15/99  rsf - moved some types in from Preferences.h
 *   12/23/99  jmp - eliminated bogus maxTime definition
 *    05/16/00 CS    Changed DayOfWeekType ot DayOfMonthType.
 *
 *****************************************************************************)
unit datetime;

interface

uses palmos, coretraps, chars, localemgr;

type
  TimeFormatType = Enum;

const
  tfColon = 0;
  tfColonAMPM = Succ(tfColon);    // 1:00 pm
  tfColon24h = Succ(tfColonAMPM); // 13:00
  tfDot = Succ(tfColon24h);
  tfDotAMPM = Succ(tfDot);        // 1.00 pm
  tfDot24h = Succ(tfDotAMPM);     // 13.00
  tfHoursAMPM = Succ(tfDot24h);   // 1 pm
  tfHours24h = Succ(tfHoursAMPM); // 13
  tfComma24h = Succ(tfHours24h);  // 13,00

type
  DaylightSavingsTypes = Enum;

const
  dsNone = 0;                                 // Daylight Savings Time not observed
  dsUSA = Succ(dsNone);                       // United States Daylight Savings Time
  dsAustralia = Succ(dsUSA);                  // Australian Daylight Savings Time
  dsWesternEuropean = Succ(dsAustralia);      // Western European Daylight Savings Time
  dsMiddleEuropean = Succ(dsWesternEuropean); // Middle European Daylight Savings Time
  dsEasternEuropean = Succ(dsMiddleEuropean); // Eastern European Daylight Savings Time
  dsGreatBritain = Succ(dsEasternEuropean);   // Great Britain and Eire Daylight Savings Time
  dsRumania = Succ(dsGreatBritain);           // Rumanian Daylight Savings Time
  dsTurkey = Succ(dsRumania);                 // Turkish Daylight Savings Time
  dsAustraliaShifted = Succ(dsTurkey);        // Australian Daylight Savings Time with shift in 1986

// pass a TimeFormatType
// Use24HourFormat(t) ((t) == tfColon24h || (t) == tfDot24h || (t) == tfHours24h || (t) == tfComma24h)
// TimeSeparator(t) ((Char) ( t <= tfColon24h ? ':' : (t <= tfDot24h ? '.' : ',')))

type
  DateFormatType = Enum;

const
  dfMDYWithSlashes = 0;                        // 12/31/95
  dfDMYWithSlashes = Succ(dfMDYWithSlashes);   // 31/12/95
  dfDMYWithDots = Succ(dfDMYWithSlashes);      // 31.12.95
  dfDMYWithDashes = Succ(dfDMYWithDots);       // 31-12-95
  dfYMDWithSlashes = Succ(dfDMYWithDashes);    // 95/12/31
  dfYMDWithDots = Succ(dfYMDWithSlashes);      // 95.12.31
  dfYMDWithDashes = Succ(dfYMDWithDots);       // 95-12-31

  dfMDYLongWithComma = Succ(dfYMDWithDashes);  // Dec 31, 1995
  dfDMYLong = Succ(dfMDYLongWithComma);        // 31 Dec 1995
  dfDMYLongWithDot = Succ(dfDMYLong);          // 31. Dec 1995
  dfDMYLongNoDay = Succ(dfDMYLongWithDot);     // Dec 1995
  dfDMYLongWithComma = Succ(dfDMYLongNoDay);   //  31 Dec, 1995
  dfYMDLongWithDot = Succ(dfDMYLongWithComma); //  1995.12.31
  dfYMDLongWithSpace = Succ(dfYMDLongWithDot); //  1995 Dec 31

  dfMYMed = Succ(dfYMDLongWithSpace);          //  Dec '95
  dfMYMedNoPost = Succ(dfMYMed);               //  Dec 95     (added for French 2.0 ROM)
  dfMDYWithDashes = Succ(dfMYMedNoPost);       // 12-31-95    (added for 4.0 ROM)

type
  DateTimeType = record
    second: Int16;
    minute: Int16;
    hour: Int16;
    day: Int16;
    month: Int16;
    year: Int16;
    weekDay: Int16; // Days since Sunday (0 to 6)
  end;

  DateTimePtr = ^DateTimeType;

// This is the time format.  Times are treated as words so don't
// change the order of the members in this structure.
//

  TimeType = record
    hours: UInt8;
    minutes: UInt8;
  end;

  TimePtr = ^TimeType;

const
  noTime = -1; // The entire TimeType is -1 if there isn't a time.


// This is the date format.  Dates are treated as words so don't
// change the order of the members in this structure.
//

type
  DateType = record
    Bits: UInt16;
{
    UInt16 year  :7;                   // years since 1904 (MAC format)
    UInt16 month :4;
    UInt16 day   :5;
}
  end;

  DatePtr = ^DateType;

(************************************************************
 * Date Time Constants
 *************************************************************)

// Maximum lengths of strings return by the date and time formating
// routine DateToAscii and TimeToAscii.
const
  timeStringLength     = 9;
  dateStringLength     = 9;
  longDateStrLength    = 15;
  dowDateStringLength  = 19;
  dowLongDateStrLength = 25;
  timeZoneStringLength = 50;

  firstYear            = 1904;
  numberOfYears        = 128;
  lastYear             = firstYear + numberOfYears - 1;

// Constants for time calculations
// Could change these from xIny to yPerX
  secondsInSeconds     = 1;
  minutesInSeconds     = 60;
  hoursInMinutes       = 60;
  hoursInSeconds       = hoursInMinutes * minutesInSeconds;
  hoursPerDay          = 24;
// daysInSeconds     ((Int32)(hoursPerDay) * ((Int32)hoursInSeconds))
  daysInSeconds        = $15180; // cc bug

  daysInWeek           = 7;
  daysInYear           = 365;
  daysInLeapYear       = 366;
  daysInFourYears      = daysInLeapYear + 3 * daysInYear;

  monthsInYear         = 12;

  maxDays              = UInt32(numberOfYears div 4 * daysInFourYears - 1);
  maxSeconds           = UInt32((maxDays+1) * daysInSeconds - 1);

// Values returned by DayOfWeek routine.
  sunday               = 0;
  monday               = 1;
  tuesday              = 2;
  wednesday            = 3;
  thursday             = 4;
  friday               = 5;
  saturday             = 6;

// Months of the year
  january              = 1;
  february             = 2;
  march                = 3;
  april                = 4;
  may                  = 5;
  june                 = 6;
  july                 = 7;
  august               = 8;
  september            = 9;
  october              = 10;
  november             = 11;
  december             = 12;

// Values returned by DayOfMonth routine.
type
  DayOfMonthType = Enum;

// It would have been cool to have a real DayOfWeekType, but we #define the
// following for compatibility with existing code.  Please use the new name
// (DayOfMonthType).
  DayOfWeekType = DayOfMonthType;

const
  dom1stSun = 0;
  dom1stMon = Succ(dom1stSun);
  dom1stTue = Succ(dom1stMon);
  dom1stWen = Succ(dom1stTue);
  dom1stThu = Succ(dom1stWen);
  dom1stFri = Succ(dom1stThu);
  dom1stSat = Succ(dom1stFri);
  dom2ndSun = Succ(dom1stSat);
  dom2ndMon = Succ(dom2ndSun);
  dom2ndTue = Succ(dom2ndMon);
  dom2ndWen = Succ(dom2ndTue);
  dom2ndThu = Succ(dom2ndWen);
  dom2ndFri = Succ(dom2ndThu);
  dom2ndSat = Succ(dom2ndFri);
  dom3rdSun = Succ(dom2ndSat);
  dom3rdMon = Succ(dom3rdSun);
  dom3rdTue = Succ(dom3rdMon);
  dom3rdWen = Succ(dom3rdTue);
  dom3rdThu = Succ(dom3rdWen);
  dom3rdFri = Succ(dom3rdThu);
  dom3rdSat = Succ(dom3rdFri);
  dom4thSun = Succ(dom3rdSat);
  dom4thMon = Succ(dom4thSun);
  dom4thTue = Succ(dom4thMon);
  dom4thWen = Succ(dom4thTue);
  dom4thThu = Succ(dom4thWen);
  dom4thFri = Succ(dom4thThu);
  dom4thSat = Succ(dom4thFri);
  domLastSun = Succ(dom4thSat);
  domLastMon = Succ(domLastSun);
  domLastTue = Succ(domLastMon);
  domLastWen = Succ(domLastTue);
  domLastThu = Succ(domLastWen);
  domLastFri = Succ(domLastThu);
  domLastSa = Succ(domLastFri);

// Values used by DateTemplateToAscii routine.
  dateTemplateChar = chrCircumflexAccent;

  dateTemplateDayNum = '0';
  dateTemplateDOWName = Succ(dateTemplateDayNum);
  dateTemplateMonthName = Succ(dateTemplateDOWName);
  dateTemplateMonthNum = Succ(dateTemplateMonthName);
  dateTemplateYearNum = Succ(dateTemplateMonthNum);

  dateTemplateShortModifier    = 's';
  dateTemplateRegularModifier  = 'r';
  dateTemplateLongModifier     = 'l';
  dateTemplateLeadZeroModifier = 'z';

//************************************************************
//* Date and Time macros
//***********************************************************

{
// Convert a date in a DateType structure to an UInt16.
  DateToInt(date) (*(UInt16 *) &date)


// Convert a date in a DateType structure to a signed int.
  TimeToInt(time) (*(Int16 *) &time)
}

//************************************************************
//* Date Time procedures
//************************************************************

procedure TimSecondsToDateTime(seconds: UInt32; dateTimeP: DateTimePtr); syscall sysTrapTimSecondsToDateTime;

function TimDateTimeToSeconds(dateTimeP: DateTimePtr): UInt32; syscall sysTrapTimDateTimeToSeconds;

procedure TimAdjust(dateTimeP: DateTimePtr; adjustment: Int32); syscall sysTrapTimAdjust;

procedure TimeToAscii(hours, minutes: UInt8; timeFormat: TimeFormatType; pString: PChar); syscall sysTrapTimeToAscii;

function TimTimeZoneToUTC(seconds: UInt32; timeZone: Int16; daylightSavingAdjustment: Int16): UInt32; syscall sysTrapTimTimeZoneToUTC;

function TimUTCToTimeZone(seconds: UInt32; timeZone: Int16; daylightSavingAdjustment: Int16): UInt32; syscall sysTrapTimUTCToTimeZone;

procedure TimeZoneToAscii(timeZone: Int16; localeP: LmLocalePtr; string_: PChar); syscall sysTrapTimeZoneToAscii;

function DaysInMonth(month, year: Int16): Int16; syscall sysTrapDaysInMonth;

function DayOfWeek(month, day, year: Int16): Int16; syscall sysTrapDayOfWeek;

function DayOfMonth(month, day, year: Int16): Int16; syscall sysTrapDayOfMonth;

// Date routines.
procedure DateSecondsToDate(seconds: UInt32; date: DatePtr); syscall sysTrapDateSecondsToDate;

procedure DateDaysToDate(days: UInt32; date: DatePtr); syscall sysTrapDateDaysToDate;

function DateToDays(date: DateType): UInt32; syscall sysTrapDateToDays;

procedure DateAdjust(dateP: DatePtr; adjustment: Int32); syscall sysTrapDateAdjust;

procedure DateToAscii(months, days: UInt8; years: UInt16;
                      dateFormat: DateFormatType; pString: PChar); syscall sysTrapDateToAscii;

procedure DateToDOWDMFormat(months, days: UInt8; years: UInt16;
                            dateFormat: DateFormatType; pString: PChar); syscall sysTrapDateToDOWDMFormat;

function DateTemplateToAscii(const templateP: PChar; months, days: UInt8;
                             years: UInt16; stringP: PChar; stringLen: Int16): UInt16; syscall sysTrapDateTemplateToAscii;

implementation

end.
