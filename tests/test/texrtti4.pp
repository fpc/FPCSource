{%FAIL}
{$mode objfpc}

{
  Test RTTI directive (trailing comma)
}
program texrtti4;
uses
  typinfo;

// TODO: Syntax error, "identifier" expected but "]"
{$RTTI EXPLICIT PROPERTIES([vcPrivate,])}

begin
end.
