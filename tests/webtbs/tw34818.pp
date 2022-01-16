{ %OPT=-O1s }
program Project1;

var CharValue: cardinal;
begin
  CharValue:=12;
  case CharValue of
   $00000000..$0000007f:begin
     writeln('ok');
     halt(0);
   end;
   $00000080..$000007ff:begin
     halt(1);
   end;
   $00000800..$0000ffff:begin
     halt(1);
   end;
   $00010000..$0010ffff:begin
     halt(1);
   end;
   $00200000..$03ffffff:begin
     halt(1);
   end;
  end;
end.
