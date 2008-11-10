{ %NORUN }
{ %OPT=-Seh -vh }

program aFP211r;  { false hints on varrec }

  type
    t = record
      d:integer
    end;

  var
   f: file of t;

  procedure P1;
  var
    varrec : t ;
  begin
    READ( f , varrec )
  end{ P1 };

begin 
  assign( f , 'fname');
  reset(f);
  P1;
end.


