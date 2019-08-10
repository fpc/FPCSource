{ %fail }
program tcustomattr21;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

type

  { tmyt }

  tmyt = class
    constructor create;
  end;

  { ensure that arguments are skipped correctly }
  [tmyt('Alpha', 42)]
  TMyObject = class(TObject)
  end;

constructor tmyt.create;
begin
end;

begin
end.

