unit generic_changec_bird;

{$mode objfpc}

interface

uses generic_changec_cat;

type
  TBirdCat = specialize TCat<int64>;

implementation

end.
