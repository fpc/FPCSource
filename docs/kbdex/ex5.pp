program example5;

{ This program demonstrates the PutKeyEvent function }

uses keyboard;

Var
  K,k2 : TKeyEvent;

begin
  InitKeyBoard;
  Writeln('Press keys, press "q" to end.');
  K2:=0;
  Repeat
    K:=GetKeyEvent;
    If k<>0 then
      begin
      if (k2 mod 2)=0 then
        K2:=K+1
      else
        K2:=0;
      K:=TranslateKeyEvent(K);
      Writeln('Got key : ',KeyEventToString(K));
      if (K2<>0) then
        begin
        PutKeyEvent(k2);
        K2:=TranslateKeyEVent(K2);
        Writeln('Put key : ',KeyEventToString(K2))
        end
      end
  Until (GetKeyEventChar(K)='q');
  DoneKeyBoard;
end.
