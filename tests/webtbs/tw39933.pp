{ %norun }
{$mode delphi}

var a: TArray<integer>;

begin
  writeln ( a = nil );
  writeln ( a <> nil );
  writeln ( nil = a );   // project1.lpr(10,17) Error: Operator is not overloaded: "Pointer" = "TArray$1$crc9F312717"
  writeln ( nil <> a );  // project1.lpr(11,17) Error: Operator is not overloaded: "Pointer" = "TArray$1$crc9F312717"
end.
