{ %TARGET=go32v2,linux }

{ Old file: tbs0105.pp }
{ typecasts are now ignored problem (NOT A bugs)         OK 0.99.1 }

{ Win32 signal support is still missing ! }

{$ifdef go32v2}
 uses dpmiexcp;
{$endif go32v2}
{$ifdef unix}
 {$ifdef ver1_0}
 uses linux;
 {$else}
 uses unix;
 {$endif}
{$endif unix}

  function our_sig(l : longint) : longint;{$ifdef unix}cdecl;{$endif}
    begin
       { If we land here the program works correctly !! }
       Writeln('Sigsegv signal recieved');
       our_sig:=0;
       Halt(0);
    end;

Var
 Sel: Word;
 v: pointer;
Begin
 Signal(SIGSEGV,signalhandler(@our_sig));
 { generate a sigsegv by writing to null-address }
 sel:=0;
 v:=nil;
{$ifdef go32v2}
  { on win9X no zero page protection :( }
  v:=pointer(-2);
{$endif go32v2}
 word(v^):=sel;
 { we should not go to here }
 Writeln('Error : signal not called');
 Halt(1);
end.
