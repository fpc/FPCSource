{ this declaration: ;}
{$ifdef FPC}
  {$mode TP}
{$endif}
 var
   name_a : packed array[0..255] of char;

const
   name_b : PChar = 't';

begin
   { the FPC compiler (0.99.14a) will refuse to compile
    the line ; }
    name_a[0]:='x';
    name_a[1]:=#0;
    if (name_b <> name_a) then
      begin
        writeln(' a and b are different');
      end
    else
      writeln('address of name_a and name_b are equal');
    { while it works under Turbo Pascal (TP). ;}
    name_b:=@name_a;
    if name_a<>name_b then
      begin
        Writeln('Wrong result');
        Halt(1);
      end;
{$ifdef FPC}
     if (name_b <> PChar(name_a)) then
      writeln(' a and b are different');
    { is a legal FPC line, but illegal in TP.}
{$endif}
    { I used ; }
    if (name_b <> PChar(@name_a)) then
      writeln(' a and b are different');
    {because it seems to work for both compiler.}

end.
