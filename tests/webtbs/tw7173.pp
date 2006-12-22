{ %norun }
{ %fail }

{$mode delphi}
unit tw7173;

interface

uses
  SysUtils;

function GetVal(astr : string) : Integer;


implementation

function GetVal(const astr : string) : Integer;
begin
  Result := StrToInt(astr);
end;


end.
