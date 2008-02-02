Program TestStream;

uses classes;

Var Stream : TStringStream;
    S,T : AnsiString;

begin
  S:='ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  T:=S;
  Writeln ('Creating stream.');
  Stream:=TStringStream.Create(S);
  Writeln ('Initial Size : ',Stream.Size);
  Writeln ('Initial Position : ',Stream.Position);
  Writeln ('Setting new size to 100');
  Stream.Size:=100;
  Writeln ('New Size : ',Stream.Size);
  Writeln ('new Position : ',Stream.Position);
  Stream.WriteByte  (1);
  Stream.WriteWord  (2);
  Stream.WriteDWord (3);
  Stream.WriteString (S);
  Writeln ('Size after write : ',Stream.Size);
  Writeln ('Position after write : ',Stream.Position);
  Writeln ('Truncating size');
  Stream.Size:=Stream.Position;
  Writeln ('Stream Size is : ',Stream.Size);
  Writeln ('Stream Position : ',Stream.Position);
  Writeln ('Seek to position 0 : ', Stream.Seek(0,soFromBeginning));
  Writeln ('new Position : ',Stream.Position);
  If Stream.ReadByte<>1 then  Writeln ('First byte not 1');
  If Stream.ReadWord<>2 then  Writeln ('First word not 2');
  If Stream.ReadDWord<>3 then Writeln ('First Word not 3');
  T:=Stream.ReadString(Length(S));
  If Length(T)<>Length(S) then
    Writeln ('Couldn''t read string.');
  Stream.WriteByte  (1);
  Stream.WriteWord  (2);
  Stream.WriteDWord (3);
  Stream.WriteBuffer (S[1],Length(S));
  Writeln ('Stream Size is : ',Stream.Size);
  Stream.Free;
end.
