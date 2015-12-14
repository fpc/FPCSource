{$apptype console}
{$mode objfpc}
{$inline on}

{$define debug_inline}

var
    fault_mask: integer = 0;

/////////////////////////////////////////

function dummy1( x: integer; var y: integer ): boolean; {$ifdef debug_inline}inline;{$endif}
begin
    y := x + 1;
    result := ( y = x + 1 );
end;

function dummy2( x: integer; out y: integer ): boolean; {$ifdef debug_inline}inline;{$endif}
begin
    y := x + 1;
    result := ( y = x + 1 );
end;

procedure test1;
var
    y: integer;
begin

    y := 0;

    if not dummy1( y, y ) then
    begin
        writeln( 'fail 1' );
        fault_mask := fault_mask or 1;
    end;

    if not dummy2( y, y ) then
    begin
        writeln( 'fail 2' );
        fault_mask := fault_mask or 2;
    end;

end;

/////////////////////////////////////////

type
    bits64 = qword;

procedure add128( a0, a1, b0, b1 : bits64; var z0Ptr, z1Ptr : bits64); {$ifdef debug_inline}inline;{$endif}
// routine from the SOFTFPU unit
var
    z1 : bits64;
begin
    z1 := a1 + b1;
    z1Ptr := z1; // overrites "a1" when called as below and inlined
    z0Ptr := a0 + b0 + ord( z1 < a1 ); // z1 compared with wrong value
end;

const
    correct_zSig0 = bits64($0001A784379D99DB);
    correct_zSig1 = bits64($4200000000000000);

procedure test2;
var
    zSig0, zSig1, aSig0, aSig1: bits64;
begin

    zSig0 := bits64($000054B40B1F852B);
    zSig1 := bits64($DA00000000000000);
    aSig0 := bits64($000152D02C7E14AF);
    aSig1 := bits64($6800000000000000);

    // this usage pattern from routine SOFTFPU::float128_mul
    add128( zSig0, zSig1, aSig0, aSig1, zSig0, zSig1 );

    if zSig0 <> correct_zSig0 then
    begin
        writeln( 'fail 3' ); // fail if add128 is inlined
        fault_mask := fault_mask or 4;
    end;

end;

/////////////////////////////////////////

begin
    test1;
    test2;
    if fault_mask = 0 then
        writeln( 'pass' )
    else
        halt( fault_mask );
end.
