{
    $Id$

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program FPDoc;

uses
  SysUtils, Classes, Gettext, DOM, XMLWrite,
  dGlobals, PasTree, PParser, dw_XML, dw_HTML, dw_LaTeX;

resourcestring
  STitle = 'FPDoc - Free Pascal Documentation Tool';
  SCopyright = '(c) 2000 - 2003 Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org';
  SCmdLineHelp = 'See documentation for usage.';
  SCmdLineInvalidOption = 'Ignoring unknown option "%s"';
  SCmdLineInvalidFormat = 'Invalid format "%s" specified';
  SCmdLineOutputOptionMissing = 'Need an output filename, please specify one with --output=<filename>';
  SWritingPages = 'Writing %d pages...';
  SNeedPackageName = 'No package name specified. Please specify one using the --package option.';
  SDone = 'Done.';

type
  TCmdLineAction = (actionHelp, actionConvert);
  TOutputFormat = (fmtHTM, fmtHTML, fmtXHTML, fmtLaTeX, fmtXMLStruct);

const
  CmdLineAction: TCmdLineAction = actionConvert;
  OutputFormat: TOutputFormat = fmtHTML;
  OSTarget: String = {$I %FPCTARGETOS%};
  CPUTarget: String = {$I %FPCTARGETCPU%};

var
  InputFiles, DescrFiles: TStringList;
  PackageName, DocLang, ContentFile, SearchPage: String;
  Engine: TFPDocEngine;


procedure InitOptions;
begin
  InputFiles := TStringList.Create;
  DescrFiles := TStringList.Create;
  Engine := TFPDocEngine.Create;
end;

procedure FreeOptions;
begin
  Engine.Free;
  DescrFiles.Free;
  InputFiles.Free;
end;

procedure ReadContentFile(const AParams: String);
var
  i: Integer;
begin
  i := Pos(',', AParams);
  Engine.ReadContentFile(Copy(AParams, 1, i - 1),
    Copy(AParams, i + 1, Length(AParams)));
end;

procedure ParseOption(const s: String);

  procedure AddToFileList(List: TStringList; const FileName: String);
  var
    f: Text;
    s: String;
  begin
    if Copy(FileName, 1, 1) = '@' then
    begin
      Assign(f, Copy(FileName, 2, Length(FileName)));
      Reset(f);
      while not EOF(f) do
      begin
        ReadLn(f, s);
	List.Add(s);
      end;
      Close(f);
    end else
      List.Add(FileName);
  end;

var
  i: Integer;
  Cmd, Arg: String;
begin
  if (s = '-h') or (s = '--help') then
    CmdLineAction := actionHelp
  else if s = '--hide-protected' then
    Engine.HideProtected := True
  else if s = '--show-private' then
    Engine.HidePrivate := False
  else
  begin
    i := Pos('=', s);
    if i > 0 then
    begin
      Cmd := Copy(s, 1, i - 1);
      Arg := Copy(s, i + 1, Length(s));
    end else
    begin
      Cmd := s;
      SetLength(Arg, 0);
    end;
    if Cmd = '--descr' then
      AddToFileList(DescrFiles, Arg)
    else if (Cmd = '-f') or (Cmd = '--format') then
    begin
      Arg := UpperCase(Arg);
      if Arg = 'HTM' then
        OutputFormat := fmtHTM
      else if Arg = 'HTML' then
        OutputFormat := fmtHTML
      else if Arg = 'XHTML' then
        OutputFormat := fmtXHTML
      else if Arg = 'LATEX' then
        OutputFormat := fmtLaTeX
      else if Arg = 'XML-STRUCT' then
        OutputFormat := fmtXMLStruct
      else
        WriteLn(StdErr, Format(SCmdLineInvalidFormat, [Arg]));
    end else if (Cmd = '-l') or (Cmd = '--lang') then
      DocLang := Arg
    else if (Cmd = '--latex-highlight') then
      LatexHighLight:=True
    else if (Cmd = '-i') or (Cmd = '--input') then
      AddToFileList(InputFiles, Arg)
    else if (Cmd = '-o') or (Cmd = '--output') then
      Engine.Output := Arg
    else if Cmd = '--content' then
      ContentFile := Arg
    else if Cmd = '--import' then
      ReadContentFile(Arg)
    else if Cmd = '--package' then
      PackageName := Arg
    else if Cmd = '--html-search' then
      SearchPage := Arg
    else if Cmd = '--ostarget' then
      OSTarget := Arg
    else if Cmd = '--cputarget' then
      CPUTarget := Arg
    else if Cmd = '--latex-extension' then
      TexExtension:=Arg
    else
      WriteLn(StdErr, Format(SCmdLineInvalidOption, [s]));
  end;
end;

procedure ParseCommandLine;
var
  i: Integer;
begin
  for i := 1 to ParamCount do
    ParseOption(ParamStr(i));
end;


var
  i: Integer;
  XMLDoc: TXMLDocument;
  Allocator: TFileAllocator;
  HTMLWriter: THTMLWriter;
begin
{$IFDEF Unix}
  gettext.TranslateResourceStrings('/usr/local/share/locale/%s/LC_MESSAGES/fpdoc.mo');
{$ELSE}
  gettext.TranslateResourceStrings('intl/fpdoc.%s.mo');
{$ENDIF}

  WriteLn(STitle);
  WriteLn(SCopyright);
  WriteLn;

  InitOptions;
  ParseCommandLine;

  if CmdLineAction = actionHelp then
    WriteLn(SCmdLineHelp)
  else
  begin
    if (PackageName='') then
      begin
      Writeln(SNeedPackageName);
      Halt(1);
      end;
    // Action is to create documentation

    // Read all description files
    for i := 0 to DescrFiles.Count - 1 do
      Engine.AddDocFile(DescrFiles[i]);

    // Set the name of the package to be processed
    Engine.SetPackageName(PackageName);

    // Read all source files
    for i := 0 to InputFiles.Count - 1 do
      try
        ParseSource(Engine, InputFiles[i], OSTarget, CPUTarget);
      except
        on e: EParserError do
	  WriteLn(StdErr, Format('%s(%d,%d): %s',
	    [e.Filename, e.Row, e.Column, e.Message]));
      end;

    // Translate internal documentation strings
    if Length(DocLang) > 0 then
      TranslateDocStrings(DocLang);

    case OutputFormat of
      fmtHTM:
        begin
	  Allocator := TShortNameFileAllocator.Create('.htm');
	  try
	    HTMLWriter := THTMLWriter.Create(Engine, Allocator, Engine.Package);
	    try
	      HTMLWriter.SearchPage := SearchPage;
	      WriteLn(Format(SWritingPages, [HTMLWriter.PageCount]));
	      HTMLWriter.WriteHTMLPages;
	    finally
	      HTMLWriter.Free;
	    end;
	  finally
	    Allocator.Free;
	  end;
	end;
      fmtHTML:
        begin
	  Allocator := TLongNameFileAllocator.Create('.html');
	  try
	    HTMLWriter := THTMLWriter.Create(Engine, Allocator, Engine.Package);
	    try
	      HTMLWriter.SearchPage := SearchPage;
	      WriteLn(Format(SWritingPages, [HTMLWriter.PageCount]));
	      HTMLWriter.WriteHTMLPages;
	    finally
	      HTMLWriter.Free;
	    end;
	  finally
	    Allocator.Free;
	  end;
	end;
{      fmtXHTML:
        begin
	  Allocator := TLongNameFileAllocator.Create('.xml');
	  try
	    BeginXHTMLDocForPackage(Engine, XHTMLOptions, Allocator);
            for i := 0 to InputFiles.Count - 1 do
	      CreateXHTMLDocFromModule(Engine, XHTMLOptions, Allocator,
	        ParseSource(Engine, InputFiles[i]));
	    EndXHTMLDocForPackage(Engine, XHTMLOptions, Allocator);
	  finally
	    Allocator.Free;
	  end;
	end;}
      fmtLaTeX:
        begin
	  if Length(Engine.Output) = 0 then
	    WriteLn(SCmdLineOutputOptionMissing)
	  else
	    CreateLaTeXDocForPackage(Engine.Package, Engine);
	end;
{      fmtXMLStruct:
        for i := 0 to InputFiles.Count - 1 do
	begin
          XMLDoc := ModuleToXMLStruct(Module);
    	  try
           WriteXMLFile(XMLDoc, StdOut);
	  finally
	    XMLDoc.Free;
	  end;
	end;}
    end;

    if Length(ContentFile) > 0 then
      Engine.WriteContentFile(ContentFile);


  end;

  FreeOptions;

  WriteLn(SDone);
end.


{
  $Log$
  Revision 1.3  2003-03-27 17:14:13  sg
  * Added --ostarget and --cputarget

  Revision 1.2  2003/03/18 19:28:44  michael
  + Some changes to output handling, more suitable for tex output

  Revision 1.1  2003/03/17 23:03:20  michael
  + Initial import in CVS

  Revision 1.13  2003/03/13 22:02:13  sg
  * New version with many bugfixes and our own parser (now independent of the
    compiler source)

  Revision 1.12  2002/10/12 17:09:45  michael
  + Added check for package name

  Revision 1.11  2002/05/24 00:13:22  sg
  * much improved new version, including many linking and output fixes

  Revision 1.10  2002/03/12 10:58:36  sg
  * reworked linking engine and internal structure

  Revision 1.9  2002/01/08 13:00:06  michael
  + Added correct array handling and syntax highlighting is now optional

  Revision 1.8  2001/12/17 23:24:11  sg
  * Added "--package" switch
  * Now uses translation files written in lower-case

  Revision 1.7  2001/07/27 12:17:20  sg
  * Added "--html-search" command line argument

  Revision 1.6  2001/07/27 10:21:42  sg
  * Just a new, improved version ;)
    (detailed changelogs will be provided again with the next commits)
}