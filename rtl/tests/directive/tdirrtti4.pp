{%NORUN%}
{%FAIL%}
{$mode objfpc}

{
  Test RTTI directive (trailing comma)
}
program tdirrtti4;
uses
  typinfo;

// TODO: Syntax error, "identifier" expected but "]"
{$RTTI EXPLICIT PROPERTIES([vcPrivate,])}

begin
end.