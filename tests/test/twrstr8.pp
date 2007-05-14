{ from GPC test suite }

program LongRealBug;
{ Dagegen ist Intels legend?rer Pentium-Bug eine Kleinigkeit!!!}

const
  Pi = 3.14159265358979323846;

var
  Pi_L : extended;
  Pi_R : Real;
  S : String [10];

begin
  Pi_L := Pi;
  Pi_R := Pi;

  WriteStr( S, sin(Pi)   :10:5 );
  if ( S <> '   0.00000' ) and ( S <> '  -0.00000' ) then
    halt(1);
  WriteStr( S, sin(Pi_L) :10:5 );
  if ( S <> '   0.00000' ) and ( S <> '  -0.00000' ) then
    halt(1);
  WriteStr( S, sin(Pi_R) :10:5 );
  if ( S <> '   0.00000' ) and ( S <> '  -0.00000' ) then
    halt(1);

  WriteStr( S, cos(Pi)   :10:5 );
  if S <> '  -1.00000' then
    halt(1);
  WriteStr( S, cos(Pi_L) :10:5 );
  if S <> '  -1.00000' then
    halt(1);
  WriteStr( S, cos(Pi_R) :10:5 );
  if S <> '  -1.00000' then
    halt(1);

  writeln ( 'OK' );
end.
