var c:byte;

  Procedure a(b:boolean);

    begin
       if b then writeln('TRUE') else writeln('FALSE');
    end;

  begin {main program}
     a(true); {works}
     a(false); {works}
     c:=0;
     a(c>0); {doesn't work}
     a(c<0); {doesn't work}
     a(c=0);
  end.
