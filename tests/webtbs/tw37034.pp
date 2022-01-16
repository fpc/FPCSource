program Test;

{$mode objfpc}
{$h+}

uses
  SysUtils;

{$assertions on}

function Suffix: string; inline;
begin
  Result := 'Post';
end;

const
  Expected = 'Pre_Mid_Post';
var
  s, t: string;
begin
  DefaultSystemCodePage:=1252;
  writeln(DefaultSystemCodePage);
  writeln(StringCodePage(suffix));
  s := 'Pre';
  t := s + '_Mid_' + Suffix;
  Assert(t = Expected,format('Expected "%s", Got: "%s"',[Expected,t]));
  writeln('Ok');
end.
