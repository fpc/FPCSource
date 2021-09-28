{%NORUN%}
{%FAIL%}
{$mode objfpc}

{
  Test RTTI directive (leading comma)
}
program tdirrtti3;
uses
  typinfo;

{$RTTI EXPLICIT PROPERTIES([,vcPrivate])}

begin
end.