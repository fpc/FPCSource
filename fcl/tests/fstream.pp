Program TestStream;

uses classes;

Var Stream : TFileStream;
    S,T : String;

begin
  S:='ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  T:=S;
  Writeln ('Creating stream.');
  Stream:=TFileStream.Create('Test2.dat',fmcreate);
  Writeln ('Initial Size : ',Stream.Size);
  Writeln ('Initial Position : ',Stream.Position);
  Stream.WriteByte  (1);
  Stream.WriteWord  (2);
  Stream.WriteDWord (3);
  Stream.WriteBuffer (S[1],Length(S));
  Writeln ('Stream Size is : ',Stream.Size);
  Stream.Seek(0,soFromBeginning);
  If Stream.ReadByte<>1 then  Writeln ('First byte not 1');
  If Stream.ReadWord<>2 then  Writeln ('First word not 2');
  If Stream.ReadDWord<>3 then Writeln ('First DWord not 3');
  If Stream.Read(T[1],Length(S))<>Length(S) then
    Writeln ('Couldn''t read string.');
  Writeln ('Second pass.');
  Stream.WriteByte  (1);
  Stream.WriteWord  (2);
  Stream.WriteDWord (3);
  Stream.WriteBuffer (S[1],Length(S));
  Writeln ('Stream Size is : ',Stream.Size);
  Writeln ('Stream Position is : ',Stream.Position);
  Writeln ('Freeing stream.');
  Stream.Free;
  Writeln ('Creating stream Read-Only');
  Stream:=TFileStream.Create('Test2.dat',fmOpenRead);
  Writeln ('Stream Size is : ',Stream.Size);
  Stream.Seek(0,soFromBeginning);
  If Stream.ReadByte<>1 then  Writeln ('First byte not 1');
  If Stream.ReadWord<>2 then  Writeln ('First word not 2');
  If Stream.ReadDWord<>3 then Writeln ('First DWord not 3');
  If Stream.Read(T[1],Length(S))<>Length(S) then
    Writeln ('Couldn''t read string.');
  If Stream.ReadByte<>1 then  Writeln ('Second byte not 1');
  If Stream.ReadWord<>2 then  Writeln ('Second word not 2');
  If Stream.ReadDWord<>3 then Writeln ('Second DWord not 3');
  If Stream.Read(T[1],Length(S))<>Length(S) then
    Writeln ('Couldn''t read string.');
  Writeln ('Stream Size is : ',Stream.Size);
  Writeln ('Stream Position is : ',Stream.Position);
  Stream.Free;
end.
