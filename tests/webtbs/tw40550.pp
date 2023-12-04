program LLVMCurrency;

uses
    Math;

var
    Ccy1,
        Ccy2    : Currency;
    Dbl         : Double;
begin
    Dbl := 1.50125;
    Ccy1 := 1000000;
    Dbl := Dbl * Ccy1;
    WriteLn('(Double) Dbl * Ccy1 = ', Dbl:6:0, ' expected 1_501_250 SameValue: ', SameValue(Dbl, Double(1501250)));
    if not SameValue(Dbl, Double(1501250)) then
      halt(1);

    Dbl := 1.50125;
    Ccy1 := 1;
    Dbl := Dbl * Ccy1;
    WriteLn('(Double) Dbl * Ccy1 = ', Dbl:6:6, ' expected 1.50125 SameValue: ', SameValue(Dbl, 1.50125));
    if not SameValue(Dbl, 1.50125) then
      halt(2);

    Dbl := 1.50125;
    Ccy1 := 1000000;
    Ccy2 := Dbl * Ccy1;
    WriteLn('(Currency) Dbl * Ccy1 = ', Ccy2:6:0, ' expected 1_501_250 SameValue: ', SameValue(Ccy2, Currency(1501250)));
    if not SameValue(Ccy2, Currency(1501250)) then
      halt(3);

    Dbl := 1.50125;
    Ccy1 := 1000000;
    Dbl := (Dbl * Int64(Ccy1)) / 10000;
    WriteLn('(Double) Dbl * Int64(Ccy1)) / 10000 = ', Dbl:6:0, ' expected 1_501_250 SameValue: ', SameValue(Dbl, Double(1501250)));
    if not SameValue(Dbl, Double(1501250)) then
      halt(4);

    Dbl := 1501250;
    Ccy1 := 1000000;
    Dbl := Dbl / Ccy1;
    WriteLn('Dbl / Ccy1 =  ', Dbl:6:6, ' expected 1.50125 SameValue: ', SameValue(Dbl, Double(1.50125)));
    if not SameValue(Dbl, Double(1.50125)) then
      halt(5);
end.
