{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 2158 }
{ Submitted by "Maxim Voronov" on  2002-10-03 }
{ e-mail: m_v_voronov@mail.ru }

{$asmmode intel}

  Function TestProc:single;
    begin
      asm
        push    eax
        fldz
        fldz
        fldz
        fsubp   st(2),st(0)
        fstp    st(0)
        pop     eax
      end;
    end;

  Begin
    writeln;
    writeln(TestProc);
  End.

