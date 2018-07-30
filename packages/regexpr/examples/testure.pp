program testure;

{$mode objfpc}
{$H+}
{$CODEPAGE UTF8}

uses
  cwstring, Classes, SysUtils, uregexpr;

var
  r: TRegExpr;
  s, s2: UTF8String;
begin
  r:= TRegExpr.create;
  r.Expression:= '.+';
  s:= 'pro про';
  s2:= r.Replace(s, '\U$0', true);
  Writeln(Format('Upcase of "%s" -> "%s"', [s, s2]));

  s:= 'PRO ПРО';
  s2:= r.Replace(s, '\L$0', true);
  Writeln(Format('Lowcase of "%s" -> "%s"', [s, s2]));
end.
