{ %OPT=-O2 }
{$ifdef fpc}
  {$mode objfpc}
{$endif}

{$apptype console}

type
    TTest = record
        x: byte;
    end;

    TByte = array [ 0 .. 0 ] of byte;

function test( dummy: integer ): TTest;
begin
    result.x := dummy;
    TByte( result.x )[0] := result.x;
end;

begin
    if test( $5A ).x = $5A then
        writeln( 'pass' )
    else
        halt( 1 );
end.
