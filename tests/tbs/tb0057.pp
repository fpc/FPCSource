{ Old file: tbs0064.pp }
{  shows other types of problems with case statements   OK 0.99.1 (FK) }

var
 i: byte;
 j: integer;
 c: char;
Begin
  case i of
  Ord('x'): ;
  end;
  case j of
  Ord('x'): ;
  end;
  case c of
  Chr(112): ;
  end;
end.
