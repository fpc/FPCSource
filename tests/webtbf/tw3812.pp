{ %fail }

{ Source provided for Free Pascal Bug Report 3812 }
{ Submitted by "Sergey@michint" on  2005-03-22 }
{ e-mail:  }
type
  LARGE_INTEGER = record
     LowPart : Cardinal;
     HighPart : LongInt;
  end;

procedure t(li1: Large_Integer); stdcall;
begin
end;

begin
  t(Large_Integer(1111111111111111));
end.
