{$ifdef fpc}{$goto on}{$endif}

label 000,01,1;
begin
  goto 000;
  writeln('1');
  halt(1);
000:
  writeln('2');
end.
