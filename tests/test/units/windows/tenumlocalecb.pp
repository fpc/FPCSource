{ %TARGET=win32,win64 }
{ locale, code page, date and time format enumerations continue past the first item, issue #35205 }
program tenumlocalecb;

{$mode objfpc}{$h+}

uses
  Windows;

var
  lCount: Integer;

// Count one enumerated item and continue
function CountA(aStr: LPSTR): Boolean32; stdcall;

begin
  Inc(lCount);
  Result:=True;
end;


// Count one enumerated item and continue
function CountW(aStr: LPWSTR): Boolean32; stdcall;

begin
  Inc(lCount);
  Result:=True;
end;


// Halt with aCode and write aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg,' (count ',lCount,')');
    halt(aCode);
    end;
end;


var
  lCPA: CODEPAGE_ENUMPROCA;
  lCPW: CODEPAGE_ENUMPROCW;
  lLocA: LOCALE_ENUMPROCA;
  lLocW: LOCALE_ENUMPROCW;
  lTimeA: TIMEFMT_ENUMPROCA;
  lTimeW: TIMEFMT_ENUMPROCW;
  lDateA: DATEFMT_ENUMPROCA;
  lDateW: DATEFMT_ENUMPROCW;
  lFlag: Boolean32;

begin
  lFlag:=True;
  Check(1,LongInt(lFlag)=1,'Boolean32 True is 1');

  lCPA:=@CountA;
  lCPW:=@CountW;
  lLocA:=@CountA;
  lLocW:=@CountW;
  lTimeA:=@CountA;
  lTimeW:=@CountW;
  lDateA:=@CountA;
  lDateW:=@CountW;

  lCount:=0;
  EnumSystemCodePagesA(lCPA,CP_INSTALLED);
  Check(11,lCount>1,'EnumSystemCodePagesA enumerates more than one code page');
  lCount:=0;
  EnumSystemCodePagesW(lCPW,CP_INSTALLED);
  Check(12,lCount>1,'EnumSystemCodePagesW enumerates more than one code page');

  lCount:=0;
  EnumSystemLocalesA(lLocA,LCID_SUPPORTED);
  Check(21,lCount>1,'EnumSystemLocalesA enumerates more than one locale');
  lCount:=0;
  EnumSystemLocalesW(lLocW,LCID_SUPPORTED);
  Check(22,lCount>1,'EnumSystemLocalesW enumerates more than one locale');

  lCount:=0;
  EnumTimeFormatsA(lTimeA,LOCALE_USER_DEFAULT,0);
  Check(31,lCount>0,'EnumTimeFormatsA enumerates a time format');
  lCount:=0;
  EnumTimeFormatsW(lTimeW,LOCALE_USER_DEFAULT,0);
  Check(32,lCount>0,'EnumTimeFormatsW enumerates a time format');

  lCount:=0;
  EnumDateFormatsA(lDateA,LOCALE_USER_DEFAULT,DATE_SHORTDATE);
  Check(41,lCount>0,'EnumDateFormatsA enumerates a date format');
  lCount:=0;
  EnumDateFormatsW(lDateW,LOCALE_USER_DEFAULT,DATE_SHORTDATE);
  Check(42,lCount>0,'EnumDateFormatsW enumerates a date format');
  writeln('ok');
end.
