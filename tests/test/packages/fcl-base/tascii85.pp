{$mode objfpc}
{$H+}
Program tidea;

Uses Classes,Ascii85;

const
  EncodedString =
    '<~9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)/0JDEF<G%<+EV:2F!,'+LineEnding+
    'O<DJ+*.@<*K0@<6L(Df-\0Ec5e;DffZ(EZee.Bl.9pF"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKY'+LineEnding+
    'i(DIb:@FD,*)+C]U=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlolDIa'+LineEnding+
    'l(DId<j@<?3r@:F%a+D58''ATD4$Bl@l3De:,-DJs`8ARoFb/0JMK@qB4^F!,R<AKZ&-DfTqBG%G'+LineEnding+
    '>uD.RTpAKYo''+CT/5+Cei#DII?(E,9)oF*2M7/c~>';
  ExpectedResult = 'Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight'+
    ' in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.';

Var M : TMemorystream;
    DS : TASCII85DecoderStream;
    I,J : longint;
    s : string;
    c : char;

begin
  M:=TMemoryStream.create;
  For i:=1 to Length(EncodedString) do
     M.Write(EncodedString[i],1);
  M.Seek(0,soBeginning);
  DS:=TASCII85DecoderStream.Create(M);
  While DS.Read(c,SizeOf(c))=1 do
    begin
      write(c);
      s:=s+c;
    end;
  writeln;
  DS.Free;
  M.Free;
  if s<>ExpectedResult then
    halt(1)
  else
    writeln('ok');
end.
