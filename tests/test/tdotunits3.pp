{%norun}
program tdotunits3;

{$mode delphi}

uses
  udots.dot.next, udots;

type
  TDot = record
    test: string;
  end;

  TUdots = record
    dot: TDot;
  end;

var
  udots: TUdots;
begin
  // this identifier should be resolved to local udots variable
  udots.dot.test := 'test';
end.

