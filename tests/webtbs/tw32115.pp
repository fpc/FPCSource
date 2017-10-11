{$mode objfpc}
program Project1;

function CalcSmth(const AValue: LongInt): Integer;
begin
  case AValue of
    -9999999..-1000000: Result := 2;
      -999999..-100000: Result := 7;
        -99999..-10000: Result := 5;
          -9999..-1000: Result := 6;
            -999..-100: Result := 3;
              -99..-10: Result := 2;
                -9..-1: Result := 3;
                  0..9: Result := 5;
                10..99: Result := 2;
              100..999: Result := 3;
            1000..9999: Result := 1;
          10000..99999: Result := 5;
        100000..999999: Result := 8;
      1000000..9999999: Result := 6;
  end;
end;

begin
  CalcSmth(0);
end.
