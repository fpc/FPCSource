{ %version=1.1 }

{ Source provided for Free Pascal Bug Report 2620 }
{ Submitted by "Louis Jean-Richard" on  2003-08-04 }
{ e-mail: l.jean-richard@bluewin.ch }
CONST
        prime   : ARRAY[1 .. 4] OF cardinal =
        ( 536870909, 1073741789, 2147483647, 4294967291);
BEGIN
        WriteLn( 'HIGH(cardinal) = ', HIGH(cardinal) );
        WriteLn( '4294967291 < HIGH(cardinal) ', (4294967291 < HIGH(cardinal)) , ' !?');
    if not(4294967291 < HIGH(cardinal)) then
      halt(1);
        WriteLn(prime[4])
END
.
