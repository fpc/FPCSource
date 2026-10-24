program tw39624;

uses
  SysUtils;

const
  cZ = 'zzzzzzzzzzzzzzzzzzzz';

// Halts with aCode when wrapping aLine at aMaxCol does not give aExpected
procedure Check(aCode: Integer; const aLine, aExpected: String; aMaxCol: Integer);

var
  lResult: String;

begin
  lResult:=WrapText(aLine,'|',[' ','-'],aMaxCol);
  if lResult<>aExpected then
    begin
    Writeln(aCode,': got "',lResult,'", expected "',aExpected,'"');
    Halt(aCode);
    end;
end;


var
  lResult, lExpected: String;

begin
  lResult:=WrapText('This is quite a long string, at least 50 chars long, I am going to sleep '+cZ+#0+sLineBreak,10);
  lExpected:='This is '+sLineBreak+'quite a '+sLineBreak+'long '+sLineBreak+'string, '+sLineBreak
    +'at least '+sLineBreak+'50 chars '+sLineBreak+'long, I '+sLineBreak+'am going '+sLineBreak
    +'to sleep '+sLineBreak+cZ+#0+sLineBreak;
  if lResult<>lExpected then
    begin
    Writeln('1: got "',lResult,'"');
    Halt(1);
    end;
  Check(2,'aaaa bbbbb','aaaa bbbbb',10);
  Check(3,'aaaa bbbbbb','aaaa |bbbbbb',10);
  Check(4,'aaaa bbbbb cc','aaaa |bbbbb cc',10);
  Check(5,'aaaa bbbbbbbbbbbb cc','aaaa |bbbbbbbbbbbb |cc',10);
  Check(6,'aaaaaaaaaaaaaaa','aaaaaaaaaaaaaaa',10);
  Check(7,'aa-bbbb cccc','aa-bbbb |cccc',10);
  Check(8,'aa ''b c d e f'' gg','aa |''b c d e f'' |gg',10);
  Check(9,'aaaa|bbbbb ccccc','aaaa|bbbbb |ccccc',10);
  Writeln('ok');
end.
