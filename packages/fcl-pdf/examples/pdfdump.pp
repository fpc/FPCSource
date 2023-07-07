{ **********************************************************************
  This file is part of the Free Component Library

  PDF file dumper
  Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  **********************************************************************}

program pdfdump;

{$mode objfpc}
{$h+}

uses
  cwString, sysutils, classes, contnrs, fppdfobjects, fppdfparser, fppdfpredict,
  custapp, fppdfconsts, fppdfcommands;

type

  { TPDFDumpApplication }

  TInfoSection = (isInfo, isCatalog, isTrailer, isObjects, isFonts,
                  isPages, isPageContents, isPageText, isDictionaries);
  TInfoSections = Set of TInfoSection;

  TPDFDumpApplication = class(TCustomApplication)
  Private
    FFiles : TStrings;
    FSections : TInfoSections;
    FPageNo : Integer;
    FVerbose : Boolean;
  Public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  Protected
    procedure DisplayPageText(Doc: TPDFDocument; aIndex: Integer;  aPage: TPDFPageObject);
    procedure DoLog(sender: TObject; aKind: TPDFLogkind; const aMessage: string); reintroduce;
    Procedure DoProgress(Sender: TObject; aKind: TPDFProgressKind; aCurrent, aCount : Integer);
    procedure DisplayCatalog(Doc: TPDFDocument);
    procedure DisplayInfo(Doc: TPDFDocument);
    procedure DisplayObjects(Doc: TPDFDocument);
    procedure DisplayFonts(Doc: TPDFDocument);
    procedure DisplayPageContents(Doc: TPDFDocument; aIndex: Integer; aPage: TPDFPageObject);
    procedure DisplayPages(Doc: TPDFDocument);
    procedure DisplayTrailer(Doc: TPDFDocument);
  Public
    function ProcessOptions : Boolean;
    procedure Usage(Msg: String);
    procedure DumpFile(FN: String);
    procedure DoRun; override;
  end;

{ TPDFDumpApplication }

constructor TPDFDumpApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFiles:=TStringList.Create;
end;

destructor TPDFDumpApplication.destroy;
begin
  FreeAndNil(FFiles);
  inherited destroy;
end;

procedure TPDFDumpApplication.DoRun;

var
  FN : String;
  Count,Errors : Integer;

begin
  StopOnException:=True;
  Terminate;
  if not ProcessOptions then
    exit;
  Errors:=0;
  Count:=0;
  For FN in FFiles do
    try
      Inc(Count);
      DumpFile(FN);
    except
      On E: Exception do
        begin
        ExitCode:=1;
        Writeln(Stderr,Format('Error %s examining file "%s" : %s',[E.ClassName,FN,E.Message]));
        Inc(Count);
        end;
    end;
  Flush(output);
  if Errors>0 then
    begin
    Writeln(StdErr,Format('Processed %d files, encountered an error in %f files.',[Count,Errors]));
    Flush(StdErr);
    end;
end;

function TPDFDumpApplication.ProcessOptions: Boolean;

  Procedure CheckSection(aShort : Char; aLong : String; aSection : TInfoSection);

  begin
    if HasOption(aShort,aLong) then
      Include(FSections,aSection);
  end;

Const
  ShortOpts = 'hopcdiln:vtf';
  LongOpts : Array of string = ('help','objects','pages','pagecontent','dictionaries','info','catalog','pageno:','verbose','text','fonts');

Var
  Err : String;
  S : TInfoSection;

begin
  Err:=Checkoptions(ShortOpts,LongOpts);
  GetNonOptions(ShortOpts,LongOpts,FFiles);
  if (Err<>'') or HasOption('h','help') then
    begin
    Usage(Err);
    exit(False);
    end;
  if FFiles.Count=0 then
    begin
    Usage('No filenames specified');
    Exit(False);
    end;
  CheckSection('o','objects',isObjects);
  CheckSection('p','pages',isPages);
  CheckSection('c','pagecontent',isPageContents);
  CheckSection('d','dictionaries',isDictionaries);
  CheckSection('i','info',isInfo);
  CheckSection('f','fonts',isFonts);
  CheckSection('l','catalog',isInfo);
  CheckSection('t','text',isPageText);
  fVerbose:=HasOption('v','verbose');
  if HasOption('n','pageno') then
    begin
    FPageNo:=StrToInt(GetOptionValue('n','pageno'));
    end;

  if (FSections=[]) then
    for S in TInfoSection do
      Include(FSections,S);
  Result:=true;
end;

procedure TPDFDumpApplication.Usage(Msg: String);
begin
  Writeln('Usage ',ExtractFileName(ParamStr(0)),' [options] FILE1 FILE2 ...');
  Writeln('Where options is one or more of:');
  Writeln('-h --help                This help text');
  Writeln('-c --pagecontent         Show page content stream (commands). Needs -p');
  Writeln('-d --dictionaries        Show object dictionaries. Needs -o');
  Writeln('-f --fonts               Show font info');
  Writeln('-i --info                Show document info');
  Writeln('-l --catalog             Show document catalog');
  Writeln('-n --pageno=N            Show only page N');
  Writeln('-o --objects             Show indirect objects');
  Writeln('-p --pages               Show pages');
  Writeln('-t --text                Show page text. Needs -p');
  Writeln('-v --verbose             Show warnings/extra info when parsing');
  Halt(Ord(Msg<>''));
end;

procedure TPDFDumpApplication.DisplayTrailer(Doc : TPDFDocument);

begin
  if Assigned(Doc.TrailerDict) then
    begin
    Writeln('Trailer dictionary:');
    Writeln(Doc.TrailerDict.GetDescription);
    end;
end;

procedure TPDFDumpApplication.DisplayObjects(Doc : TPDFDocument);

Var
  Obj : TPDFObject;
  Ind : TPDFIndirect absolute Obj;

begin
  Writeln('Indirect object count : ',Doc.Count);
  For obj in Doc do
    begin
    Writeln('Object (',Obj.ClassName,') : ',Obj.GetDescription);
    if Obj is TPDFIndirect then
      if Assigned(Ind.ObjectDict) and (isDictionaries in FSections) then
        begin
        Writeln('object dictionary : ',Ind.ObjectDict.GetDescription);
        Writeln;
        end;
    end;
end;

procedure TPDFDumpApplication.DisplayFonts(Doc: TPDFDocument);

Var
  Obj : TPDFObject;
//  Fnt : TPDFFontObject absolute Obj;

begin
  Writeln('Font definitions:');
  Writeln;
  For Obj in Doc do
    if (Obj is TPDFFontObject) or (Obj is TPDFFontDescriptor) then
      begin
      Writeln(Obj.GetDescription);
      Writeln;
      Writeln;
      end;

end;

procedure TPDFDumpApplication.DoProgress(Sender: TObject; aKind: TPDFProgressKind;
  aCurrent, aCount: Integer);

Const
  Kinds : Array [TPDFProgressKind] of String = ('XRef','Indirect','ContentStream');

begin
  Writeln('Loading ', Kinds[aKind],': ',aCurrent,'/',aCount);
end;

procedure TPDFDumpApplication.DoLog(sender: TObject; aKind: TPDFLogkind;
  const aMessage: string);
begin
  Writeln('[',aKind,'] : ',aMessage);
end;

procedure TPDFDumpApplication.DisplayCatalog(Doc : TPDFDocument);

begin
  if Assigned(Doc.FindCatalog) then
    begin
    Writeln('Document catalog:');
    Writeln(Doc.FindCatalog.ObjectDict.GetDescription);
    end;
end;

procedure TPDFDumpApplication.DisplayInfo(Doc : TPDFDocument);

Var
  Info : TPDFDocumentInfo;

begin
  if Not Assigned(Doc.FindDocumentInfoObject) then
    exit;
  Info:=Doc.FindDocumentInfo;
  With Info do
    Try
      Writeln('Document info:');
      Writeln('Title : ',Title);
      Writeln('Author : ',Author);
      Writeln('Subject : ',Subject);
      Writeln('Keywords : ',Keywords);
      Writeln('Creator : ',Creator);
      Writeln('Producer : ',Producer);
      Writeln('Creation Date : ',DateTimeToStr(CreationDate));
      Writeln('Modification Date : ',DateTimeToStr(ModDate));
      Writeln('Trapped : ',Trapped);
    Finally
      Free;
    end;
end;

procedure TPDFDumpApplication.DisplayPageContents(Doc : TPDFDocument; aIndex: Integer; aPage : TPDFPageObject);

Var
  I,J : Integer;
  Cmd : TPDFCommand;

begin
  For I:=0 to aPage.CommandList.Count-1 do
    begin
    Cmd:=aPage.CommandList[I];
    Write('Command ',I,' : ',Cmd.Command,' (',Cmd.ClassName,'):');
    For J:=0 to Length(Cmd.Tokens)-1 do
      Write(' ',Cmd.Tokens[J].TokenData);
    Writeln;
    end;
end;

procedure TPDFDumpApplication.DisplayPageText(Doc : TPDFDocument; aIndex: Integer; aPage : TPDFPageObject);

Var
  I : Integer;
  Cmd : TPDFCommand;
  FontName,Rawtext : RawByteString;
  aFontRef : TPDFRefData;
  UnicodeMap : TPDFCMap;
  aFontObj : TPDFFontObject;

begin
  UnicodeMap:=Nil;
  For I:=0 to aPage.CommandList.Count-1 do
    begin
    Cmd:=aPage.CommandList[I];
    if Cmd is TPDFTf_Command then
      begin
      FontName:=TPDFTf_Command(Cmd).FontName;
      if (FontName<>'') and (FontName[1]='/') then
        Delete(FontName,1,1);
      aFontRef:=aPage.FindFontRef(FontName);
      aFontObj:=Doc.FindFont(aFontRef); // TPDFFontObject
      if Assigned(aFontObj) then
        UnicodeMap:=aFontObj.UnicodeCMap
      else
        UnicodeMap:=nil;
      end
    else If cmd is TPDFTextCommand then
      begin
      rawText:=TPDFTextCommand(Cmd).GetFullText(UnicodeMap);
      //Writeln('GetCodePage : ',CodePageToCodePageName(StringCodePage(Rawtext)));
      SetCodePage(RawText,CP_UTF8);
      Writeln(RawText);
      end;
    end;
end;

procedure TPDFDumpApplication.DisplayPages(Doc : TPDFDocument);

Var
  aPage : TPDFPageObject;
  I : Integer;

begin
  Writeln('Page count : ',Doc.PageCount);
  For I:=0 to Doc.PageCount-1 do
    begin
    aPage:=Doc.Page[I];
    Write('Page object ',I,': ');
    if not Assigned(aPage) then
      Writeln('Not found')
    else
      begin
      Writeln('Object type: ',aPage.ObjectType,' (',aPage.ClassName,')');
      if isDictionaries in FSections then
        begin
        Writeln('Page dictionary : ',aPage.ObjectDict.GetDescription);
        Writeln;
        end;
      if isPageContents in FSections then
        DisplayPageContents(Doc,I,aPage);
      if isPageText in FSections then
        begin
        Writeln('Page text : ');
        Writeln;
        DisplayPageText(Doc,I,aPage)
        end;
      end;
    end;
end;

procedure TPDFDumpApplication.DumpFile(FN : String);

Var
  F : TFileStream;
  P : TPDFParser;
  Doc : TPDFDocument;
  S : TInfoSection;

begin
  P:=Nil;
  Doc:=Nil;
  Writeln('Contents of ',FN,' : ');
  F:=TFileStream.Create(FN,fmOpenRead or fmShareDenyWrite);
  try
    Doc:=TPDFDocument.Create();
    P:=TPDFParser.Create(F);
    if FVerbose then
      begin
      P.OnProgress:=@DoProgress;
      P.OnLog:=@DoLog;
      end;
    // P.ResolveObjects:=False;
    P.ParseDocument(Doc);
    if isPageText in FSections then
      P.DoResolveToUnicodeCMaps(Doc);
    For S in FSections do
      begin
      Case s of
        isObjects : DisplayObjects(Doc);
        isPages : DisplayPages(Doc);
        isCatalog : DisplayCatalog(Doc);
        isInfo : DisplayInfo(Doc);
        isFonts : DisplayFonts(Doc);
        isTrailer : DisplayTrailer(Doc);
      else
        // Do nothing
      end;
      Writeln;
      Writeln();
      end;
  finally
    Doc.Free;
    P.Free;
    F.Free;
  end;
  Flush(Output);
end;

begin
  With TPDFDumpApplication.Create(Nil) do
    try
      Initialize;
      Run;
    finally
      Free
    end;
end.

