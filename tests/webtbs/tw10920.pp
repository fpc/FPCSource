program bug_fmtcurrncy;
//  If write/writeln parameter list includes any item FOLLOWING a 
//     currency variable with format specs, the compiler throws
//     an Access violation exception.

var
  V: currency;    // currency blows up,  all other real types are Ok

BEGIN
  V := 34567;

  write( V:0:2, 'x' );   // This form produces the error

  write( V:0:2 );        // This equivalent form compiles Ok
  writeln( 'x' );

END.

