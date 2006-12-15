{$mode tp}

{$r+}
{$q+}

FUNCTION MemCompare(VAR Rec1, Rec2; Count : WORD) : INTEGER;
TYPE PByte = ^BYTE;
VAR PB1, PB2 : PBYTE;
    i : INTEGER;
BEGIN
 MemCompare := 0;

 PB1 := PByte(@Rec1);
 PB2 := PByte(@Rec2);
 FOR i := 1 TO Count DO
  BEGIN
   IF PB1^ <> PB2^ THEN
    BEGIN
     IF PB1^ > PB2^ THEN
       MemCompare := 1
     ELSE
       MemCompare := -1;
     BREAK;
    END;
   Inc(PB1); { Error is generated at this line }
   Inc(PB2);
  END;
END;

begin
end.
