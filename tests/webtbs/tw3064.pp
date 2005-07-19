type
  r1 = packed record
    b : byte;
    l : longint;
  end;
  
  r2 = record
    b : byte;
    l : longint;
  end;
  
 {$a-}
  r3 = record
    b : byte;
    l : longint;
  end;
  
 {$a+}
  r4 = record
    b : byte;
    l : longint;
  end;
  
begin
  if (sizeof(r1)<>sizeof(r3)) or (sizeof(r2)<>sizeof(r4)) then
    halt(1);
  writeln('ok');       
end.
