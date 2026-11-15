{ the [Ref] parameter attribute in mode objfpc with prefixed attributes }
program tconstref9;

{$mode objfpc}
{$modeswitch prefixedattributes}

type
  TSmall = record
    X: Integer;
  end;

var
  gS: TSmall;

function P(const [Ref] a: TSmall): Boolean;
begin
  Result := @a = @gS;
end;

begin
  if not P(gS) then
    begin
      Writeln('P does not pass by reference');
      Halt(1);
    end;
  Writeln('ok');
end.
