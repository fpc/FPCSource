{ Source provided for Free Pascal Bug Report 3774 }
{ Submitted by "Sergey@michint" on  2005-03-11 }
{ e-mail:  }
{$C+}

var
  i, ec: Integer;
begin
  Val('0x123', i, ec);
  Assert(ec=0);
  Assert(i=$123);
  writeln('ok');
end.
