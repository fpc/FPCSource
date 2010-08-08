{ gettext was crashing under Windows CE. Not crashing should be considered a success }

uses gettext;

var
  LangDefault, LangFallback: ansistring;
begin
  GetLanguageIDs(LangDefault, LangFallback);
end.

