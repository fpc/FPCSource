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
  sysutils;


procedure dosearch(const dir : string);

  procedure domask(const s : string);
    Var Info : TSearchRec;
    begin
      If FindFirst (dir+DirectorySeparator+s,faAnyFile,Info)=0 then
        begin
        Repeat
          With Info do
            writeln (dir+DirectorySeparator+Name);
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
  dosearch('.');
end.



