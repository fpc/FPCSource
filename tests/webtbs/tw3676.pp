{ %cpu=i386 }
{ %opt=-O2 }

{$mode delphi}

uses classes;

function Read(Str: TStream): string;
begin
  SetLength(Result, Str.Size - Str.Position);
  writeln(str.size,' - ',str.Position);
  Writeln(Length(Result));
end;

var
  stream: TMemoryStream;
  buf: PChar;
  x: char;
  i: Integer;
  s : string;
begin
  stream := TMemoryStream.Create;
  x := 'A';
  buf := @x;
  for i := 0 to 26000 do begin
    stream.Write(buf, sizeof(buf^));
  end;
  stream.Position := 0;
  s:=Read(stream);
end.
