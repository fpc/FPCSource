{%norun}
program tdotunits3;

{$mode delphi}

uses
  tudots.dot.next, tudots;

type
  TDot = record
    test: string;
  end;

  TUdots1 = record
    dot: TDot;
  end;

var
  tudots: TUdots1;
begin
  // this identifier should be resolved to local udots variable
  tudots.dot.test := 'test';
end.

