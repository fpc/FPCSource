{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 2158 }
{ Submitted by "Maxim Voronov" on  2002-10-03 }
{ e-mail: m_v_voronov@mail.ru }

Program test;

{$asmmode intel}
  Function GetXminusYintel(x,y : extended):extended; assembler;
      asm
        fld     x
        fld     y
        fsubp   st(1),st(0)
      end;

{$asmmode att}
  Function GetXminusYatt(x,y : extended):extended; assembler;
      asm
        fld     y
        fld     x
        fsubp   %st,%st(1)
      end;

  var
    eatt,eintel : extended;

  Begin
    writeln;

    eintel:= GetXminusYintel(2.0,1.0);
    writeln(eintel);
    eatt:= GetXminusYatt(2.0,1.0);
    writeln(eatt);
    if eintel<>eatt then
      begin
        writeln('intel fsubp is not translated properly into fsubrp');
        halt(1);
      end;
  End.

