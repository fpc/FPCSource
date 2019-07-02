{ %fail }
program Project1;

{$mode delphi}

type
  TSuit = (suHeart, suDiamond, suClub, suSpade);
  TRedSuit = suHeart..suDiamond;

var
  Suit: TRedSuit;
begin
  // This should generate an error, but {$mode delphi} allows it
  Suit := suClub;
end.
