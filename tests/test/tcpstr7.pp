{$mode objfpc}{$H+}
{$hints off}

procedure APos(const substr: shortstring; const s: RawByteString);
begin
end;

procedure APos(const Substr: RawByteString; const Source: RawByteString);
begin
end;

const
  C = '\';

begin
  APos(C + '.' + C, 'Test');
end.


