Program Example4;

{ Program to demonstrate the GetCapabilities function. }

Uses video;

Var
  W: Word;

  Procedure TestCap(Cap: Word; Msg : String);

  begin
    Write(Msg,' : ');
    If (W and Cap=Cap) then
      Writeln('Yes')
    else
      Writeln('No');
  end;

begin
  W:=GetCapabilities;
  Writeln('Video driver supports following functionality');
  TestCap(cpUnderLine,'Underlined characters');
  TestCap(cpBlink,'Blinking characters');
  TestCap(cpColor,'Color characters');
  TestCap(cpChangeFont,'Changing font');
  TestCap(cpChangeMode,'Changing video mode');
  TestCap(cpChangeCursor,'Changing cursor shape');
end.
