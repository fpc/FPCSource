{%FAIL}
{$mode objfpc}

{
  Test RTTI directive (missing identifier)
}
program tdirrtti2;
uses
  typinfo;

{$RTTI EXPLICIT PROPERTIES([MissingIdentifier])}

begin
end.