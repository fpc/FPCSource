{ Old file: tbs0009.pp }
{  tests comperations in function calls a(c<0);        OK 0.9.2 }

var c:byte;

  Procedure a(b:boolean);

    begin
       if b then writeln('TRUE') else writeln('FALSE');
    end;

  function Test_a(b:boolean) : string;

    begin
       if b then Test_a:='TRUE' else Test_a:='FALSE';
    end;

  begin {main program}
     a(true); {works}
     if Test_a(true)<>'TRUE' then halt(1);
     a(false); {works}
     if Test_a(false)<>'FALSE' then halt(1);
     c:=0;
     a(c>0); {doesn't work}
     if Test_a(c>0)<>'FALSE' then halt(1);
     a(c<0); {doesn't work}
     if Test_a(c<0)<>'FALSE' then halt(1);
     a(c=0);
     if Test_a(c=0)<>'TRUE' then halt(1);
  end.
