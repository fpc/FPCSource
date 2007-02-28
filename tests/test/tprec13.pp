{ from gpc tests, original name pack1.pas }

{$ifdef fpc}
{$bitpacking on}
{$endif}

Program Pack1;

Var
  r: packed record
       a, b: Boolean;
       c: false..true;
       d: 0..3;
       e: -3..3;
       i: Integer;
     end { r };
  rb: Byte absolute r;

var
  i: integer;
begin
  rb:= 0;
  with r do
    begin
      a:= false;
      b:= true;
      c:= false;
      d:= 2;
      e:= -1;
    end { with };
  if ( SizeOf ( r ) = 1 + SizeOf (Integer) ) and ( rb = {$ifdef FPC_BIG_ENDIAN} %01010111 {$else} %11110010 {$endif} ) then
    writeln ( 'OK' )
  else
    begin
      writeln ( 'failed ', SizeOf (r), ' ', SizeOf (Integer), ' ', rb );
      halt(1);
    end;
end.
