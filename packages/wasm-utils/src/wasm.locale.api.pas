{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2025 by the Free Pascal development team.

    WASM API calls for internationalization/localization.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit wasm.locale.api;

interface


uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types,
  {$ELSE}
  types,
  {$ENDIF}
  wasm.locale.shared;

function __locale_SetWasmLocale(aName: PAnsiChar; aNameLen: Longint): TLocaleError;
                                   external modHostLocale name HostLocale_FNSetWasmLocale;


function __locale_GetNameOfDay(aDay: Integer;
                               aLong: Integer;
                               aName: PAnsiChar;
                               aNameLen: PLongint): TLocaleError;
                                 external modHostLocale name HostLocale_FNGetNameOfDay;

function __locale_GetNameOfMonth(aMonth: Integer;
                                 aLong: Integer;
                                 aName: PAnsiChar;
                                 aNameLen: PLongint): TLocaleError;
                                   external modHostLocale name HostLocale_FNGetNameOfMonth;

function __locale_GetDateSeparator(aSeparator: PAnsiChar;
                                   aSeparatorLen: PLongint): TLocaleError;
                                     external modHostLocale name HostLocale_FNGetDateSeparator;

function __locale_GetTimeSeparator(aSeparator: PAnsiChar;
                                      aSeparatorLen: PLongint): TLocaleError;
                                        external modHostLocale name HostLocale_FNGetTimeSeparator;


function __locale_GetDecimalSeparator(aSeparator: PAnsiChar;
                                      aSeparatorLen: PLongint): TLocaleError;
                                        external modHostLocale name HostLocale_FNGetDecimalSeparator;

function __locale_GetThousandSeparator(aSeparator: PAnsiChar;
                                       aSeparatorLen: PLongint): TLocaleError;
                                         external modHostLocale name HostLocale_FNGetThousandsSeparator;

function __locale_GetCurrencySymbol(aSymbol: PAnsiChar;
                                    aSymbolLen: PLongint): TLocaleError;
                                      external modHostLocale name HostLocale_FNGetCurrencyChar;

function __locale_GetTimeZoneOffset : Integer;
                           external modHostLocale name HostLocale_FNGetTimezoneOffset;

implementation

end.
