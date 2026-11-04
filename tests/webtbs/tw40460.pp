{ %TARGET = win32,win64,wince }

program tw40460;

{$mode objfpc}{$H+}

uses
  SysUtils, Windows, GetText;

var
  Errors: Integer = 0;

// Checks the language IDs that GetLanguageIDs returns for locale aLocale.
procedure Check(aLocale: LongWord; const aName, aLang, aFallbackLang: AnsiString);

var
  lLang, lFallbackLang: AnsiString;

begin
  GetLanguageIDs(aLocale, lLang, lFallbackLang);
  if (lLang = aLang) and (lFallbackLang = aFallbackLang) then
    Writeln('OK:   ', aName, ' gives ', lLang, ' / ', lFallbackLang)
  else
  begin
    Writeln('FAIL: ', aName, ' gives ', lLang, ' / ', lFallbackLang,
      ', expected ', aLang, ' / ', aFallbackLang);
    Inc(Errors);
  end;
end;


var
  lLang, lFallbackLang: AnsiString;

begin
  Check($0409, 'en-US', 'en_US', 'en');
  Check($0809, 'en-GB', 'en_GB', 'en');
  Check($0816, 'pt-PT', 'pt_PT', 'pt');
  Check($0416, 'pt-BR', 'pt_BR', 'pt');
  Check($0804, 'zh-CN', 'zh_CN', 'zh');
  Check($0404, 'zh-TW', 'zh_TW', 'zh');
  Check($041D, 'sv-SE', 'sv_SE', 'sv');
  Check($0406, 'da-DK', 'da_DK', 'da');
  Check($0C07, 'de-AT', 'de_AT', 'de');
  Check($0813, 'nl-BE', 'nl_BE', 'nl');
  Check($0414, 'nb-NO', 'nb_NO', 'nb');
  Check($0436, 'af-ZA', 'af_ZA', 'af');

  GetLanguageIDs(lLang, lFallbackLang);
  Writeln('User default locale gives ', lLang, ' / ', lFallbackLang);
  if (lFallbackLang = '') or (Pos(lFallbackLang + '_', lLang) <> 1) then
  begin
    Writeln('FAIL: user default locale language IDs are inconsistent');
    Inc(Errors);
  end;

  if Errors > 0 then
  begin
    Writeln(Errors, ' check(s) failed');
    Halt(1);
  end;
  Writeln('All checks passed');
end.
