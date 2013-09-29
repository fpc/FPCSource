{$mode objfpc}

var
  Dividend, Divisor, Quotient : QWord;
begin
  Dividend := QWord( $ffffffffffffffff );
  Divisor := QWord( $100000003 );
  Quotient := dividend div divisor;
  if quotient <> $FFFFFFFD then
    Halt(1);
  Quotient := dividend mod divisor;
  if quotient <> 8 then
    Halt(2);
end.
