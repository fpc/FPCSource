{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Convert .html to htm files, together with links.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
program makehtm;

uses sysutils;

Var
  Verbose    : Boolean;
  FileCount  : Boolean;
  DeleteHtml : Boolean;
  
Procedure ConvertFile (FileName : String);

Var
  OFileName      : String;
  Infile,OutFile : Text;
  Line           : String;

begin
  Try
    OFileName:=ChangeFileExt(FileName,'.htm');
    If Verbose then
      Writeln('Converting ',FileName,' to ',OFileName)
    else
      Write('.');  
    Assign(InFile,FileName);
    Assign(OutFile,OFileName);
    Reset(Infile);
    Try
      Rewrite(OutFile);
      Try
        While Not EOF(Infile) do
          begin
          ReadLn(Infile,Line);
          Line:=Stringreplace(Line,'.html','.htm',[rfReplaceAll]);
          Writeln(OutFile,Line);
          end
      Finally
        Close(OutFile);
      end;
    finally
      Close(InFile);
    end;  
    If DeleteHtml then
      begin
      If Verbose then
        Writeln('Deleting input file : ',FileName);
      DeleteFile(FileName);
      end;
  except
    On E : Exception do
      Writeln('Error converting ',FileName,' to ',OFileName,' : ',E.Message);
  end;  
end;

Function DoDirectory(DirName : String; Recurse : Boolean) : Integer;

Var
  Info : TSearchRec;

begin
  Result:=0;
  DirName:=IncludeTrailingPathDelimiter(DirName);
  If FindFirst(Dirname+'*.html',0,Info)=0 then
    Try
      Repeat
        ConvertFile(DirName+Info.Name);
        Inc(Result);
      Until (FindNext(Info)<>0);
    Finally
      FindClose(Info);
    end;
  If Recurse then  
    If FindFirst(Dirname+'*',faDirectory,Info)=0 then
       Try
         Repeat
           With Info do
             If ((Attr and faDirectory)<>0) and (Name<>'.') and (Name<>'..') then
             Result:=Result+DoDirectory(DirName+Name,Recurse);
         Until (FindNext(Info)<>0);
       Finally
         FindClose(Info);
       end;
end;

Procedure DoDirs;

Var
  I     : integer;
  Count,Total : Integer;
  Dir   : String;
  
begin
  Total:=0;
  for I:=1 to ParamCount do
    begin
    Dir:=Paramstr(I);
    if (Dir<>'-v') then
      begin
      Count:=DoDirectory(Dir,True);
      if Not verbose then
        Writeln;
      Writeln('Directory ',Dir,' : ',Count,' files.');
      Total:=Total+Count;
      end;
    end;  
  Writeln('Total files ',Total);    
end;

Procedure DoParams;

Var
  I : integer;

begin
  Verbose:=False;
  DeleteHtml:=False;
  For I:=1 to ParamCount do
    If paramstr(i)='-v' then
      Verbose:=True
    else if paramstr(i)='-r' then
      DeleteHtml:=True;
end;

begin
  DoParams;
  DoDirs;
end.
