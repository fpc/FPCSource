
type
  E = ( a, b, c );

begin
  case b of
    a: begin
         write( 'a' );
         halt(1);
       end;
    c: begin
         write( 'c' );
         halt(2);
       end;
    b:
      begin
        write( 'b' );
        halt(0);
      end;
    else
      begin
        write( '?' );
        halt(3);
      end
  end;
end.
