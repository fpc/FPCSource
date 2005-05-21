{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 3131 }
{ Submitted by "Arnstein Prytz" on  2004-06-02 }
{ e-mail: Arnstein.Prytz@jcu.edu.au }
program tmp;

{$goto on}
{$asmmode intel}

procedure l;
  label l1;
  begin
    l1 : WRITELN( 'Label L1' );
  end;

procedure a; assembler;
  asm
    @_quit : ;
  end;

begin
end.
