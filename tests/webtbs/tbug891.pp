{ this declaration: ;}
{$mode TP}
 var
   name_a : packed array[0..255] of char;

const
   name_b : PChar = 'test';

begin
   { the FPC compiler (0.99.14a) will refuse to compile
    the line ; }

    if (name_b <> name_a) then
      writeln(' a and b are different');
    { while it works under Turbo Pascal (TP). ;}
     if (name_b <> PChar(name_a)) then
      writeln(' a and b are different');
    { is a legal FPC line, but illegal in TP.}

    { I used ; }
    if (name_b <> PChar(@name_a)) then
      writeln(' a and b are different');
    {because it seems to work for both compiler.}

end.