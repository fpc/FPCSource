{ %FAIL }

program tfuncref15;

{$mode delphi}{$H+}
{$modeswitch functionreferences}

uses
  ufuncref10;

var
  l: LongInt;
  i: ITestFunc8;
begin
  { Delphi mode calls ITestFunc8.Invoke and thus would try to apply Foobar to
    the result type LongInt }
  l := if8.Foobar;
end.
