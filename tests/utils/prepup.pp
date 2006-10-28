{
    This file is part of the Free Pascal test suite.
    Copyright (c) 2006 by the Free Pascal development team.

    This program collects the results of a testsuite run
    and prepares things for an upload of the results to the
    database

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program prepup;

uses
  sysutils,libtar,zstream;

var
  tarwriter : ttarwriter;
  c : tgzfilestream;

procedure dosearch(const dir : string);

  procedure domask(const s : string);
    Var
      Info : TSearchRec;
      hs : string;
      i : integer;
    begin
      If FindFirst (dir+DirectorySeparator+s,faAnyFile,Info)=0 then
        begin
        Repeat
          With Info do
            begin
              hs:=dir+DirectorySeparator+Name;
              { strip leading ./ }
              delete(hs,1,2);
              tarwriter.addfile(hs);
            end;
        Until FindNext(info)<>0;
        end;
      FindClose(Info);
   end;

Var Info : TSearchRec;

Begin
  If FindFirst (dir+DirectorySeparator+'*',faDirectory,Info)=0 then
    begin
      Repeat
        With Info do
          begin
            If ((Attr and faDirectory) = faDirectory) and (name<>'.') and (name<>'..') then
              dosearch(dir+DirectorySeparator+name);
          end;
      Until FindNext(info)<>0;
    end;
  FindClose(Info);
  domask('*.elg');
  domask('*.log');
End;

begin
  if paramcount<>1 then
    begin
      writeln('Usage: prepup <name of .tar.gz>');
      halt(1);
    end;
    C:=TGZFileStream.Create(paramstr(1),gzOpenWrite);
    TarWriter := TTarWriter.Create (C);
  dosearch('.');
  TarWriter.AddFile('dbdigest.cfg');
  TarWriter.AddFile('log');

  TarWriter.free;
  c.free;
end.



