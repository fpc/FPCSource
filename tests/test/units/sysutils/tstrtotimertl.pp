{ %target=win32,win64 }
{ The time parser reads ASCII digits, in both RTL flavours. In a Unicode RTL (the unicodertl subtarget) on Win32 and
  Win64 a character outside ASCII that the conversion code page turns into a digit (U+FF11, fullwidth one, under code
  page 1252) is not a digit, also when looking for the end of an AM/PM string. }
program tstrtotimertl;

{$mode delphiunicode}

uses
  SysUtils;

var
  ErrCount: Integer = 0;
  lTime, lDef: TDateTime;
  lSettings, lSaved: TFormatSettings;
{$if defined(FPC_SUBTARGET_UNICODERTL) and (defined(win32) or defined(win64))}
  lSavedCodePage: TSystemCodePage;
{$endif}

procedure Check(const aName: UnicodeString; aOk: Boolean);
begin
  if not aOk then
    begin
      WriteLn('FAIL ', aName);
      Inc(ErrCount);
    end;
end;

begin
  lSettings := DefaultFormatSettings;
  lSettings.TimeSeparator := ':';
  lSettings.DecimalSeparator := '.';
  lSettings.TimeAMString := 'AM';
  lSettings.TimePMString := 'PM';
  lDef := EncodeTime(1, 2, 3, 0);

  Check('plain', TryStrToTime('12:30:15', lTime, lSettings) and (lTime = EncodeTime(12, 30, 15, 0)));
  Check('plain, milliseconds', TryStrToTime('12:30:15.250', lTime, lSettings) and (lTime = EncodeTime(12, 30, 15, 250)));
  Check('plain, pm', TryStrToTime('1:30 PM', lTime, lSettings) and (lTime = EncodeTime(13, 30, 0, 0)));
  Check('plain, letter', not TryStrToTime('12:3x:15', lTime, lSettings));

{$if defined(FPC_SUBTARGET_UNICODERTL) and (defined(win32) or defined(win64))}
  { the conversion to code page 1252 maps U+FF11 .. U+FF19 to the ASCII digits, whatever the system's code page is }
  lSavedCodePage := DefaultSystemCodePage;
  SetMultiByteConversionCodePage(1252);
  try
    Check('first digit', not TryStrToTime(#$FF11'2:30:15', lTime, lSettings));
    Check('second digit', not TryStrToTime('1'#$FF12':30:15', lTime, lSettings));
    Check('only digit', not TryStrToTime(#$FF11':30:15', lTime, lSettings));
    Check('minutes', not TryStrToTime('12:'#$FF13'0:15', lTime, lSettings));
    Check('seconds', not TryStrToTime('12:30:1'#$FF15, lTime, lSettings));
    Check('milliseconds', not TryStrToTime('12:30:15.'#$FF12'50', lTime, lSettings));
    Check('after pm', not TryStrToTime('PM 1:'#$FF13'0', lTime, lSettings));
    { a fullwidth digit inside a PM string does not end it: the whole string is compared }
    lSettings.TimePMString := 'P'#$FF11'X';
    Check('pm string', TryStrToTime('1:30 P'#$FF11'X', lTime, lSettings) and (lTime = EncodeTime(13, 30, 0, 0)));
    lSettings.TimePMString := 'PM';

    lSaved := DefaultFormatSettings;
    DefaultFormatSettings.TimeSeparator := ':';
    Check('default settings', not TryStrToTime(#$FF11'2:30:15', lTime));
    Check('with a separator', not TryStrToTime(#$FF11'2:30:15', lTime, ':'));
    Check('StrToTimeDef', StrToTimeDef(#$FF11'2:30:15', lDef) = lDef);
    try
      lTime := StrToTime(#$FF11'2:30:15');
      Check('StrToTime raises', False);
    except
      on EConvertError do
        ;
    end;
    DefaultFormatSettings := lSaved;
  finally
    SetMultiByteConversionCodePage(lSavedCodePage);
  end;
{$endif}

  if ErrCount <> 0 then
    Halt(1);
  WriteLn('ok');
end.
