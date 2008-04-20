// tests writing of high()/low() of enumeration values, i.e. 
// writing and reading of rtti for enums, both "dense" and 
// "sparse" enumerations (different rtti is generated and 
// different code used for generating and reading)
type
  // "dense" unnamed enumeration
  Txx = set of (_one, _two, _three);
  // "sparse" unnamed enumeration
  Tyy = set of (_zero := 0, _ten := 10, _twenty := 20);

  // "dense" enumeration
  Tx = (one,two,three);
  Txxx = set of Tx;
  // "sparse" enumeration
  Ty = (zero := 0, ten := 10, twenty := 20);
  Tyyy = set of Ty;

procedure error(number : longint);
begin
  writeln('error ', number);
  halt(number);
end;

var
  x : txxx;
  y : tyyy;
  err : word;
  
  _x : txx;
  _y : tyy;

begin
  writeln(low(_x));
  writeln(high(_x));

  writeln(low(_y));
  writeln(high(_y));

  writeln(low(x));
  writeln(high(x));

  writeln(low(y));
  writeln(high(y));
end.
