Program Example3;
uses Crt;

{ Program to demonstrate the ReadKey function. }

var
  ch : char;
begin
  writeln('Press Left/Right, Esc=Quit');
  repeat
    ch:=ReadKey;
    case ch of
     #0 : begin
            ch:=ReadKey; {Read ScanCode}
            case ch of
             #75 : WriteLn('Left');
             #77 : WriteLn('Right');
            end;
          end;
    #27 : WriteLn('ESC');
    end;
  until ch=#27 {Esc}
end.
