program tw40308;

{$mode delphi}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

//uses
//  SysUtils;

type
  TFunc1 = reference to function (P1: Integer): String;

function GetTestFunc1(P2: Integer): TFunc1;
begin
  Result := function (P1: Integer): String begin
      Result := '3'; // <-- Error: Internal error 2011010304
      //Result := IntToStr(P1 + P2);
    end;
end;

begin
  if GetTestFunc1(1)(2) <> '3' then
    Halt(1);
end.

