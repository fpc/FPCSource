{ %fail }

{$ifdef fpc}
{$mode delphi}
{$endif}

unit tsprot;

interface

uses
  usprot1,usprot2;

type
  tchild2 = class(tbase)
    f: tchild1;
    procedure test;
  end;

implementation

procedure tchild2.test;
  begin
    f.pmethod;
  end;

end.
