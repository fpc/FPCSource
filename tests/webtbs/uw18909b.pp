{ %norun }
unit uw18909b;

{$mode objfpc}

interface

uses uw18909a;

operator :=(const A: TA): TB;

implementation

operator :=(const A: TA): TB; begin Result.x := A.x; end;

end.
