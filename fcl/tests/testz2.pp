Program testz2;

uses zstream;

const
{$ifdef unix}
  eol=#10;
{$else}
  eol=#10#13;
{$endif}

Var F : TGZfileStream;
    S : String;
    i :longint;
    c : char;
begin
  Writeln ('Creating file.');
  S:='This is a sentence'+eol;
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

  $Log$
  Revision 1.3  2002-05-31 11:31:46  marco
   * 1.0.x Renamefest for FCL. Fixed some oddities in 1.1 too

  Revision 1.2  2000/07/13 11:33:05  michael
  + removed logs
 
}
