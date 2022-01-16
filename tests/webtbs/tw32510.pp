{ %norun }

unit tw32510;
interface

type
  RawUnicode = type AnsiString(1200);

function AnsiToRawUnicode(const AnsiText: RawByteString): RawUnicode;

implementation

function AnsiToRawUnicode(const AnsiText: RawByteString): RawUnicode;
begin
end;

initialization
end.
