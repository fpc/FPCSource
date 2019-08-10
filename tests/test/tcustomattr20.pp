{ %FAIL }
program tcustomattr20;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

type
  { ensure that arguments of non existing attributes are skipped correctly }
  [TMyAttributeDoesNotExist('Alpha', 42)]
  TMyObject = class(TObject)
  end;

begin
end.

