{ %fail }

{ currently fails under FPC, because parameters to read(ln) have to be  }
{ var parameters, and you cannot pass bitpacked record fields and array }
{ elements as var parameters                                            }

{ from gpc tests, original name: bitfields.pas }

{$ifdef fpc}
{$bitpacking on}
{$endif}

Program BitFields;

Var
  Foo: packed record
    b: 0..63;
    a: 0..1;
  end { Foo };

  r: packed array [ 40..47 ] of 0..1;

  F: Text;

begin
  assign(f,'bitfields.txt');
  rewrite ( F );
  writeln ( F, '42' );
  writeln ( F, '0' );
  writeln ( F, '1' );
  with Foo do
    begin
      reset ( F );
      readln ( F, b );
      readln ( F, a );
      readln ( F, r [ 42 ] );
      close ( F );
      erase ( F );
      if ( b = 42 ) and ( a = 0 ) and ( r [ 42 ] = 1 ) then
        writeln ( 'OK' )
      else
        begin
          writeln ( 'failed: ', b, ' ', a, ' ', r [ 42 ] );
          halt(1);
        end;
    end { with };
end.
