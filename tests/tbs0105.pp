{$R+}
{ BOUND check error... I don't think this is a code generator error }
{ but an error because the type casting is not considered at all!   }
{ Must be compiled with -Cr                                         }

{$ifdef go32v2}
 uses dpmiexcp;
{$endif go32v2}
{$ifdef linux}
 uses linux;
{$endif linux}

  function our_sig(l : longint) : longint;{$ifdef linux}cdecl;{$endif}
    begin
       { If we land here the program works correctly !! }
       Writeln('Bound check error signal recieved');
       our_sig:=0;
       Halt(0);
    end;

Var
 Sel: Word;
 v: longint;
Begin
 Signal(SIGSEGV,signalhandler(our_sig));
 v:=$00ffffff;
 Sel:=word(v);
 writeln(sel);
 { should trigger Bound check error }
 sel:=v;
 { we should not go to here }
 Writeln('Error : signal not called');
 Halt(1);
end.
