{ %fail }

{$ifdef fpc}
{$mode delphi}
{$endif}

unit tw10425e;

interface

type
  tc = class
    s: string[4];
    property test: string[4] read s write s;
  end;

implementation


end.
