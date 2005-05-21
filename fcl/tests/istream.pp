Program TestStream;

{
  When testing, remember to send something through standard input !!
}

uses sysutils,classes,iostream;

Var Stream : TIOStream;
    S,T : String;
    i : longint;
    SS : ShortString;

begin
  S:='ABCDEFGHIJKLMNOPQRSTUVWXYZ %d'#10;
  T:=S;
  Writeln ('Creating output stream.');
  Stream:=TIOStream.Create(iosOutPut);
  For I:=1 to 10 do
    begin
    S:=Format(T,[I]);
    Stream.WriteBuffer (S[1],Length(S));
    end;
  Stream.Free;
  Writeln ('Creating error stream.');
  Stream:=TIOStream.Create(iosError);
  For I:=1 to 10 do
    begin
    S:=Format(T,[I]);
    Stream.WriteBuffer (S[1],Length(S));
    end;
  Stream.Free;
  Writeln ('Creating input stream');
  Stream:=TIOStream.Create(iosInput);
  SS:='aha';
  While Length(SS)>0 do
    begin
    SetLength(SS,Stream.Read(SS[1],255));
    Write(SS);
    end;
  Writeln ('Read ',Stream.Position,' bytes.');
  Stream.Free;
end.
