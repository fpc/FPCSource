{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SelTimeZone.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines select time zone structures and routines.
 *
 * History:
 *    03/02/2000  peter Created by Peter Epstein.
 *    08/02/2000  kwk   Updated APIs to include country code.
 *    08/21/2000  kwk   Deleted obsolete SelectDaylightSavingAdjustment.
 *    11/17/2000  CS    Change SelectTimeZone's ioCountryInTimeZoneP parameter
 *                      to ioLocaleInTimeZoneP, (and anyCountry to anyLocale,
 *                      but that doesn't really matter), since CountryType is
 *                      only a UInt8, and this may change someday.
 *                CS    Change GetTimeZoneTriggerText's countryInTimeZone
 *                      parameter to localeInTimeZoneP, since CountryType is
 *                      only a UInt8, and this may change someday.
 *
 *****************************************************************************)

unit seltimezone;

interface

uses palmos, coretraps, localemgr;

function SelectTimeZone(var ioTimeZoneP: Int16; var ioLocaleInTimeZoneP: LmLocaleType;
                        {const} titleP: PChar; showTimes, anyLocale: Boolean): Boolean; syscall sysTrapSelectTimeZone;

implementation

end.
