program test;

procedure write1(  var charbuf:string);
begin
  Writeln(Charbuf);
end;

procedure write2(  var charbuf:string; attrbuf:array of word);
begin
  Writeln(Charbuf);
end;

var chars : String[82];
    attrs : array [1..162] of word;
begin
  Chars := 'Das ist ein Test, den ich gerade schreibe';
  write1(chars);
  write2(chars,attrs);
end.
