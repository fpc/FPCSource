{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 3444 }
{ Submitted by "Arnstein" on  2004-12-09 }
{ e-mail: Arnstein.Prytz@jcu.edu.au }
{$asmmode intel}
program tmp;
begin
  asm
    fstp st(1 );
{------------^ NB space}
  end;
end.
