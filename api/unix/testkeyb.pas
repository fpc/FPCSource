program raw_read;

uses
  keyboard;

var
  c : char;
  col : byte;
begin
  InitKeyboard;
  c:=' ';
  col:=1;
  while c<>'x' do
    begin
      c:=RawReadKey;
      if ord(c)<32 then
        begin
          write('"#',ord(c),'"');
          inc(col,4);
        end
      else
        write(c);
      inc(col);
      if col>=80 then
        begin
          writeln;
          col:=0;
        end;
    end;
  DoneKeyboard;
end.