{ %fail }
var
  i : longint;

label
  labelfor2,labelfor3,labelfor4;

begin
  case i of
    1:
      begin
        writeln('1');
        goto labelfor2;
      end;
    2:
      begin
        writeln('2');
        goto labelfor3;
      end;
    3:
      begin
        writeln('3');
        goto labelfor4;
      end;
    4:
      begin
      writeln('4');
      end;
  end;
end.

