{
    Help program to generate html help index

    This file is part of Free Pascal.
    Copyright (c) 1993-2005 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
  uses
    insthelp,sysutils,dos,objects,WHTMLScn;

  type
    PFPHTMLFileLinkScanner = ^TFPHTMLFileLinkScanner;
    TFPHTMLFileLinkScanner = object(THTMLFileLinkScanner)
      function    CheckURL(const URL: string): boolean; virtual;
      function    CheckText(const Text: string): boolean; virtual;
      procedure   ProcessDoc(Doc: PHTMLLinkScanFile); virtual;
    end;


  const
    HTMLIndexExt = '.htx';


  procedure TFPHTMLFileLinkScanner.ProcessDoc(Doc: PHTMLLinkScanFile);
    begin
    end;


  function TFPHTMLFileLinkScanner.CheckURL(const URL: string): boolean;
    var OK: boolean;
    const HTTPPrefix = 'http:';
          FTPPrefix  = 'ftp:';
    begin
      OK:=inherited CheckURL(URL);
      if OK then OK:=DirAndNameOf(URL)<>'';
      if OK then OK:=CompareText(copy(ExtOf(URL),1,4),'.HTM')=0;
      if OK then OK:=CompareText(copy(URL,1,length(HTTPPrefix)),HTTPPrefix)<>0;
      if OK then OK:=CompareText(copy(URL,1,length(FTPPrefix)),FTPPrefix)<>0;
      CheckURL:=OK;
    end;


  function TFPHTMLFileLinkScanner.CheckText(const Text: string): boolean;
    var OK: boolean;
      S: string;
    begin
      S:=Trim(Text);
      OK:=(S<>'') and (copy(S,1,1)<>'[');
      CheckText:=OK;
    end;


  procedure doerror(const s : ansistring);
    begin
      writeln(s);
      writeln;
      writeln('Press ENTER to continue');
      readln;
    end;


  procedure writehlpindex(filename : ansistring);

    var
      LS : PFPHTMLFileLinkScanner;
      BS : PBufStream;
      Re : Word;
      params : array[0..0] of pointer;
      dir    : searchrec;

    begin
      writeln('Creating HTML index file, please wait ...');
      New(LS, Init(DirOf(FileName)));
      LS^.ProcessDocument(FileName,[soSubDocsOnly]);
      if LS^.GetDocumentCount=0 then
        doerror(format('Problem creating help index %1, aborting',[filename]))
      else
        begin
          FileName:=DirAndNameOf(FileName)+HTMLIndexExt;
          begin
            New(BS, Init(FileName, stCreate, 4096));
            if not(Assigned(BS)) then
              doerror(format('Error while writing help index! '+
                'No help index is created',[filename]))
            else
              begin
                LS^.StoreDocuments(BS^);
                if BS^.Status<>stOK then
                  doerror(format('Error while writing help index! '+
                    'No help index is created',[filename]));
                Dispose(BS, Done);
              end;
          end;
        end;
      Dispose(LS, Done);
    end;

  begin
    if paramcount<>1 then
      writeln('Usage: writeidx <index name>')
    else
      writehlpindex(paramstr(1));
  end.
