Program testz2;

uses zstream;

Var F : TGZfileStream;
    S : String;
    i :longint;
    c : char;
begin
  Writeln ('Creating file.');
  S:='This is a sentence'#10;
  F:=TGZFileStream.create('test.gz',gzopenWrite);
  For I:=1 to 10 do 
    F.Write(S[1],Length(S));
  f.Free;  
  Writeln ('Done.');
  Writeln ('Reopening file for read.');
  F:=TGZFileStream.Create('test.gz',gzopenread);
  Writeln ('Dumping contents:');
  While F.Read(C,SizeOf(c))<>0 do
    write(c);
  F.free;
  Writeln('Done.');  
end.

