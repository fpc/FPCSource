type smallword=word;

Type  LocalHeader     = Record
        Time          : Longint;
      End;

Type PkZipObject = Object
       Buf         : longint;

       Constructor ZIPInit;
       Procedure FindFirstEntry;   Virtual;
     End; {PkzipObject}

     PkzipPtr = ^PkzipObject;


Constructor PkzipObject.ZIPInit;
Begin
End;


Procedure PkzipObject.FindFirstEntry;
var LocalHeaderBuf: LocalHeader ABSOLUTE buf;
Begin
  LocalHeaderBuf.Time:=12341234;
End;

var
  o : PkzipObject;

begin
  o.ZIPInit;
  o.FindFirstEntry;
  if o.Buf<>12341234 then
    begin
      writeln('error');
      halt(1);
    end;
End.
