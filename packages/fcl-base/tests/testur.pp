{$mode objfpc}
program testur;

uses resolve;

var
  U : TURIParser;

Procedure DumpURI(U : TURIParser);

begin
  With U do
    begin
    Writeln('Protocol :',Protocol);
    Writeln('Username :',Username);
    Writeln('Password :',Password);
    Writeln('Host     :',Host    );
    Writeln('Port     :',Port    );
    Writeln('Path     :',Path    );
    Writeln('Document :',Document);
    Writeln('Params   :',Params  );
    Writeln('Bookmark :',Bookmark);
    Writeln('URI      :',URI);
    end;
end;

begin
  U:=TURIParser.Create(Nil);
  Try
    Writeln('Parsing : http://www.freepascal.org:80/bugs/db.php?ID=20');
    U.ParseURI('http://www.freepascal.org:80/bugs/db.php?ID=20');
    DumpURI(U);
    U.Active:=True;
    Writeln('Setting URI : http://www.lazarus.freepascal.org/main.php');
    U.URI:='http://www.lazarus.freepascal.org/main.php';
    DumpUri(U);
  finally
    U.Free;
  end;
end.
