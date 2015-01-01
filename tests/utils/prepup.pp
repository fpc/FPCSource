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

const
  use_longlog : boolean = false;
  has_file_errors : boolean = false;
  MAX_RETRY = 5;
  RETRY_WAIT_TIME = 1000; { One second wait time before trying again }

var
  tarwriter : ttarwriter;
  c : tgzfilestream;

procedure dosearch(const dir : string);

  procedure domask(const s : string);
    Var
      Info : TSearchRec;
      hs : string;
      tries : longint;
      write_ok : boolean;
    begin
      If FindFirst (dir+DirectorySeparator+s,faAnyFile,Info)=0 then
        begin
        Repeat
          With Info do
            begin
              hs:=dir+DirectorySeparator+Name;
              { strip leading ./ }
              delete(hs,1,2);
              if not tarwriter.addfile(hs) then
                begin
                  tries:=1;
                  write_ok:=false;
                  while tries<MAX_RETRY do
                    begin
                      sleep(RETRY_WAIT_TIME);
                      inc(tries);
                      if tarwriter.addfile(hs) then
                        begin
                          write_ok:=true;
                          tries:=MAX_RETRY;
                        end;
                    end;
                  has_file_errors:=(write_ok=false);
                  if not write_ok then
                    tarwriter.addstring('###File Open failed###',
                      ConvertFileName(hs),Info.Time);
                end;
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

var
  index : longint;
const
  has_errors : boolean = false;
begin
  index:=1;
  if paramcount<>1 then
    begin
      if paramstr(1)='-ll' then
        begin
          use_longlog:=true;
          index:=2;
        end
      else
        begin
          writeln('Usage: prepup [-ll] <name of .tar.gz>');
          Writeln('Optional -ll parameter is used to specify use of longlog');
          halt(1);
        end
    end;
    C:=TGZFileStream.Create(paramstr(index),gzOpenWrite);
    TarWriter := TTarWriter.Create (C);
  if not use_longlog then
    dosearch('.');
  if not TarWriter.AddFile('dbdigest.cfg') then
    has_errors:=true;
  if not TarWriter.AddFile('log') then
    has_errors:=true;
  if use_longlog then
    if not TarWriter.AddFile('longlog') then
      has_errors:=true;
  TarWriter.free;
  c.free;
  if has_file_errors then
    writeln(stderr,'Prepup error: some files were not copied');
  if has_errors then
    halt(2);
end.



