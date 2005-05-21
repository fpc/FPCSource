{ Old file: tbs0240.pp }
{ Problems with larges value is case statements        OK 0.99.11 (FK) }

Program TEST;

var CurFileCrc32f : cardinal{Longint};
    CheckThis : String;

BEGIN
  CurFileCrc32f := $C5CAF43C;
  CheckThis := '';
  Case CurFileCrc32f of
    $F3DC2AF0 :  CheckThis := ' First ';
    $27BF798B :  CheckThis := ' Second ';
    $7BA5BB19 :  CheckThis := ' Third';
    $FA246A81 :  CheckThis := ' Forth';
    $8A00B508 :  CheckThis := ' Fifth';
    $C5CAF43C :  CheckThis := ' Sixth';
  End;
  Writeln( CheckThis );
  If CheckThis<>' Sixth' then halt(1);
END.
