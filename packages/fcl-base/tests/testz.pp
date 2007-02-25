Program testz;

uses Classes,zstream;

Var F : TFileStream;
    C : TCompressionStream;
    D : TDeCompressionStream;
    I,J : longint;

begin
   Writeln ('Start Writing');
   F:=TFileStream.Create('ztest.dat',FMCreate);
   Writeln ('Created filestream');
   C:=TCompressionStream.Create(cldefault,F);
   Writeln ('Created Compressionstream. Writing');
   For I:=1 to 100000 do
     C.Write(I,SizeOf(I));
   writeln ('End of write');
   C.Free;
   writeln ('freed CompressionStream');
   F.Free;
   writeln ('freed FileStream');

   Writeln ('Start Reading');
   F:=TFileStream.Create('ztest.dat',FMOpenRead);
   Writeln ('Created filestream');
   D:=TDeCompressionStream.Create(F);
   Writeln ('Created Decompressionstream. Reading...');
   For I:=1 to 100000 do
     begin
     D.Read(J,SizeOf(J));
     If J<>I then Writeln ('Oh-Oh',J,'doesn''t match',i);
     end;
   writeln ('End of Read');
   D.Free;
   writeln ('freed CompressionStream');
   F.Free;
   writeln ('freed FileStream');
end.
