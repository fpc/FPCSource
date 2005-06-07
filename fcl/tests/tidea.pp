Program tidea;

Uses Classes,Idea;

Type
   PByte = ^Byte;

Var M : TMemorystream;
    ES : TIDeaEncryptStream;
    DS : TIdeaDecryptStream;
    StartKey : ideacryptkey;
    EnKey,DeKey : ideakey;
    I,J : longint;

begin
  M:=TMemoryStream.create;
  // generate some phoney key;
  For I:=0 to SizeOf(StartKey)-1 do
   PByte(@StartKey)[I]:=I;
  // Get encryption key
  EnKeyIdea(StartKey,enKey);
  ES:=TIDeaEncryptStream.Create(EnKey,M);
  For I:=1 to 65 do
    ES.Write(I,SizeOf(I));
  Writeln ('Position after Write : ',ES.Position);
  ES.Flush;
  Writeln ('Size of memory stream : ',M.Size);
  M.Seek(0,soFromBeginning);
  // Get decryption key
  DeKeyIdea(EnKey,DeKey);
  DS:=TIDeaDecryptStream.Create(DEKey,M);
  For I:=1 to 65 do
    begin
    DS.Read(J,SizeOf(J));
    If J<>I then
      Writeln ('Error; Read : ',J);
    end;
  Writeln ('Position after Reading : ',DS.Position);
  DS.destroy;
end.
