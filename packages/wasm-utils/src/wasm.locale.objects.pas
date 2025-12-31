{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2025 by the Free Pascal development team.

    WASM API object for internationalization/localization.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit wasm.locale.objects;

{$mode objfpc}

interface

uses 
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
  {$ELSE}
  sysutils,
  {$ENDIF} 
  wasm.locale.shared, wasm.locale.api;

Type
  EWasmLocale = class(Exception);

  { TWasmHostLocale }

  TWasmHostLocale = class(TObject)
  private
    FCurrencySymbol: String;
    FDateSeparator: String;
    FDecimalSeparator: String;
    FLongDayNames: TWeekNameArray;
    FLongMonthNames: TMonthNameArray;
    FShortDayNames: TWeekNameArray;
    FShortMonthNames: TMonthNameArray;
    FThousandSeparator: String;
    FTimeSeparator: string;
    FTimeZoneOffset: Integer;
  Protected
    function ConvertBuf(var Buf : RawByteString; aLen : integer) : string;
    procedure InitMonths; virtual;
    procedure InitDays; virtual;
    procedure InitSeparators; virtual;
    procedure InitTimeZone; virtual;
  public
    constructor create(aLocale : string = '');
    procedure TransferToFormatSettings;
    property LongMonthNames : TMonthNameArray Read FLongMonthNames;
    property ShortMonthNames : TMonthNameArray Read FShortMonthNames;
    property LongDayNames : TWeekNameArray Read FLongDayNames;
    property ShortDayNames : TWeekNameArray Read FShortDayNames;
    Property DecimalSeparator : String Read FDecimalSeparator;
    Property ThousandSeparator : String Read FThousandSeparator;
    Property CurrencySymbol: String Read FCurrencySymbol;
    Property TimeZoneOffset: Integer Read FTimeZoneOffset;
    Property DateSeparator: string Read FDateSeparator;
    Property TimeSeparator: string Read FTimeSeparator;
  end;

implementation

function TWasmHostLocale.ConvertBuf(var Buf: RawByteString; aLen: integer): string;
begin
  SetLength(Buf,aLen);
  {$IF SIZEOF(char)=1}
  Result:=Buf;
  {$ELSE}
  Result:=UTF8Decode(Buf);
  {$ENDIF}
end;

procedure TWasmHostLocale.InitMonths;

var
  buf : RawByteString;
  S : String;
  i,res,len : longint;
  long : boolean;

  procedure initbuffer; inline;

  begin
    len:=512;
    SetLength(buf,len);
  end;

begin
  Buf:='';
  InitBuffer;
  for long in Boolean do
    begin
    I:=1;
    While I<=12 do
      begin
      res:=__locale_GetNameOfMonth(i,ord(long),@buf[1],@len);
      case res of
        ELocale_SUCCESS:
          begin
          S:=ConvertBuf(Buf,Len);
          if Long then
            FLongMonthNames[i]:=S
          else
            FShortMonthNames[i]:=S;
          inc(i);
          InitBuffer;
          end;
        ELocale_SIZETOOSMALL:
          SetLength(Buf,Len); // Redo the loop
      else
        Raise EWasmLocale.CreateFmt('Failed to get month %d name. Error code: %d',[i,res]);
      end;
      end;
    end;

end;

procedure TWasmHostLocale.InitDays;
var
  buf : RawByteString;
  S : String;
  i,res,len : longint;
  long : boolean;

  procedure initbuffer; inline;

  begin
    len:=512;
    SetLength(buf,len);
  end;

begin
  Buf:='';
  InitBuffer;
  for long in Boolean do
    begin
    I:=1;
    While (I<=7) do
      begin
      res:=__locale_GetNameOfDay(i,ord(long),@buf[1],@len);
      case res of
        ELocale_SUCCESS:
          begin
          S:=ConvertBuf(Buf,Len);
          if Long then
            FLongDayNames[i]:=S
          else
            FShortDayNames[i]:=S;
          inc(i);
          InitBuffer;
          end;
        ELocale_SIZETOOSMALL:
          SetLength(Buf,Len); // Redo the loop
      else
        Raise EWasmLocale.CreateFmt('Failed to get day %d name. Error code: %d',[i,res]);
      end;
      end;
    end;
end;

procedure TWasmHostLocale.InitSeparators;

var
  buf : RawByteString;
  res, len : longint;
  S : string;

  procedure InitBuffer; inline;
  begin
    len:=24;
    setlength(buf,len);
  end;

begin
//  buf:='';

  // Currency
  InitBuffer;
  res:=__locale_GetCurrencySymbol(@Buf[1],@len);
  if res=ELocale_SUCCESS then
    FCurrencySymbol:=ConvertBuf(Buf,Len)
  else
    Raise EWasmLocale.CreateFmt('Failed to get currency symbol. Error code: %d',[res]);
  // Browser has actually no way to determine this.
  if FCurrencySymbol='' then
    FCurrencySymbol:='$';

  // Decimal separator
  InitBuffer;
  res:=__locale_GetDecimalSeparator(@Buf[1],@len);
  if res=ELocale_SUCCESS then
    FDecimalSeparator:=ConvertBuf(Buf,Len)
  else
    Raise EWasmLocale.CreateFmt('Failed to get decimal separator. Error code: %d',[res]);

  // Thousands separator
  InitBuffer;
  res:=__locale_GetThousandSeparator(@Buf[1],@len);
  if res=ELocale_SUCCESS then
    FThousandSeparator:=ConvertBuf(Buf,Len)
  else
    Raise EWasmLocale.CreateFmt('Failed to thousands separator. Error code: %d',[res]);

  // Date separator
  InitBuffer;
  res:=__locale_GetDateSeparator(@Buf[1],@len);
  if res=ELocale_SUCCESS then
    FDateSeparator:=ConvertBuf(Buf,Len)
  else
    Raise EWasmLocale.CreateFmt('Failed to get date separator. Error code: %d',[res]);
  // Time separator
  InitBuffer;
  res:=__locale_GetTimeSeparator(@Buf[1],@len);
  if res=ELocale_SUCCESS then
    FTimeSeparator:=ConvertBuf(Buf,Len)
  else
    Raise EWasmLocale.CreateFmt('Failed to get time separator. Error code: %d',[res]);
end;

procedure TWasmHostLocale.InitTimeZone;
begin
  FTimeZoneOffset:=__locale_GetTimeZoneOffset;
end;

constructor TWasmHostLocale.create(aLocale: string);

var
  lLocale : AnsiString;

begin
  {$IF SIZEOF(CHAR)=1}
  lLocale:=aLocale;
  {$ELSE}
  lLocale:=UTF8Encode(aLocale);
  {$ENDIF}
  if lLocale<>'' then
    __locale_SetWasmLocale(PAnsiChar(lLocale),Length(lLocale));
  InitMonths;
  InitDays;
  InitSeparators;
  InitTimeZone;
end;

procedure TWasmHostLocale.TransferToFormatSettings;

  function First(aString : String; aDefault : char) : char;
  begin
    if aString='' then
      Result:=aDefault
    else
      Result:=aString[1];
  end;

var
  I : Integer;

begin
  DefaultFormatSettings.DateSeparator:=First(FDateSeparator,'/');
  DefaultFormatSettings.TimeSeparator:=First(FTimeSeparator,':');
  DefaultFormatSettings.DecimalSeparator:=First(FDecimalSeparator,'.');
  DefaultFormatSettings.CurrencyString:=FCurrencySymbol;
  DefaultFormatSettings.ThousandSeparator:=First(FThousandSeparator,',');
  for I:=1 to 12 do
    begin
    DefaultFormatSettings.LongMonthNames[i]:=LongMonthNames[i];
    DefaultFormatSettings.ShortMonthNames[i]:=ShortMonthNames[i];
    end;
  for I:=1 to 7 do
    begin
    DefaultFormatSettings.LongDayNames[i]:=LongDayNames[i];
    DefaultFormatSettings.ShortDayNames[i]:=ShortDayNames[i];
    end;
//  Sysutils.
end;

end.

