{ Copyright (C) <2005> <Andrew Haines> chmfilewriter.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
}
unit chmfilewriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chmwriter, inifiles, contnrs;

type
  TChmProject = class;

  TChmProgressCB = procedure (Project: TChmProject; CurrentFile: String) of object;

  { TChmProject }

  TChmProject = class
  private
    FAutoFollowLinks: Boolean;
    FDefaultFont: String;
    FDefaultPage: String;
    FFiles: TStrings;
    FIndexFileName: String;
    FMakeBinaryTOC: Boolean;
    FMakeBinaryIndex: Boolean;
    FMakeSearchable: Boolean;
    FFileName: String;
    FOnProgress: TChmProgressCB;
    FOutputFileName: String;
    FTableOfContentsFileName: String;
    FTitle: String;
    FWindows : TObjectList;
    FMergeFiles : TStringlist;
    fDefaultWindow : string;
  protected
    function GetData(const DataName: String; out PathInChm: String; out FileName: String; var Stream: TStream): Boolean;
    procedure LastFileAdded(Sender: TObject);
    procedure readIniOptions(keyvaluepairs:tstringlist);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: String); virtual;
    procedure LoadFromhhp (AFileName:String;LeaveInclude:Boolean); virtual;
    procedure SaveToFile(AFileName: String); virtual;
    procedure WriteChm(AOutStream: TStream); virtual;
    function ProjectDir: String;
    procedure AddFileWithContext(contextid:integer;filename:ansistring;contextname:ansistring='');
    // though stored in the project file, it is only there for the program that uses the unit
    // since we actually write to a stream
    property OutputFileName: String read FOutputFileName write FOutputFileName;
    property FileName: String read FFileName write FFileName;
    property Files: TStrings read FFiles write FFiles;
    property AutoFollowLinks: Boolean read FAutoFollowLinks write FAutoFollowLinks;
    property TableOfContentsFileName: String read FTableOfContentsFileName write FTableOfContentsFileName;
    property MakeBinaryTOC: Boolean read FMakeBinaryTOC write FMakeBinaryTOC;
    property MakeBinaryIndex: Boolean read FMakeBinaryIndex write FMakeBinaryIndex;
    property Title: String read FTitle write FTitle;
    property IndexFileName: String read FIndexFileName write FIndexFileName;
    property MakeSearchable: Boolean read FMakeSearchable write FMakeSearchable;
    property DefaultPage: String read FDefaultPage write FDefaultPage;
    property DefaultFont: String read FDefaultFont write FDefaultFont;
    property Windows :TObjectList read FWindows write FWindows;
    property MergeFiles :TStringlist read FMergeFiles write FMergefiles;
    property OnProgress: TChmProgressCB read FOnProgress write FOnProgress;
    property DefaultWindow : String read FDefaultWindow write FDefaultWindow;
  end;

  TChmContextNode = Class
                     URLName       : AnsiString;
                     ContextNumber : Integer;
                     ContextName   : AnsiString;
                    End;

implementation

uses XmlCfg, chmsitemap, CHMTypes;

{ TChmProject }

function TChmProject.GetData(const DataName: String; out PathInChm: String; out
  FileName: String; var Stream: TStream): Boolean;
begin
  Result := False; // Return true to abort compressing files

  TMemoryStream(Stream).LoadFromFile(ProjectDir+DataName);
  // clean up the filename
  FileName := StringReplace(ExtractFileName(DataName), '\', '/', [rfReplaceAll]);
  FileName := StringReplace(FileName, '//', '/', [rfReplaceAll]);

  PathInChm := '/'+ExtractFilePath(DataName);
  if Assigned(FOnProgress) then FOnProgress(Self, DataName);
end;

procedure TChmProject.LastFileAdded(Sender: TObject);
var
  IndexStream: TFileStream;
  TOCStream: TFileStream;
  Writer: TChmWriter;
  TOCSitemap  : TChmSiteMap;
  IndexSiteMap: TChmSiteMap;
begin
  // Assign the TOC and index files
  Writer := TChmWriter(Sender);
  {$ifdef chmindex}
    Writeln('binindex filename ',IndexFileName);
  {$endif}
  if (IndexFileName <> '') and FileExists(IndexFileName) then begin
    IndexStream := TFileStream.Create(IndexFileName, fmOpenRead);
    Writer.AppendIndex(IndexStream);
    if MakeBinaryIndex then
    begin
      {$ifdef chmindex}
        Writeln('into binindex ');
      {$endif}
      IndexStream.Position := 0;
      IndexSitemap := TChmSiteMap.Create(stIndex);
      indexSitemap.LoadFromStream(IndexStream);
      Writer.AppendBinaryIndexFromSiteMap(IndexSitemap,False);
      IndexSitemap.Free;
    end;
    IndexStream.Free;
  end;
  if (TableOfContentsFileName <> '') and FileExists(TableOfContentsFileName) then begin
    TOCStream := TFileStream.Create(TableOfContentsFileName, fmOpenRead);
    Writer.AppendTOC(TOCStream);
    if MakeBinaryTOC then
    begin
      TOCStream.Position := 0;
      TOCSitemap := TChmSiteMap.Create(stTOC);
      TOCSitemap.LoadFromStream(TOCStream);
      Writer.AppendBinaryTOCFromSiteMap(TOCSitemap);
      TOCSitemap.Free;
    end;
    TOCStream.Free;
  end;
  if not assigned(sender) then
    Writer.Free;
end;

constructor TChmProject.Create;
begin
  FFiles := TStringList.Create;
  FWindows:=TObjectList.Create(True);
  FMergeFiles:=TStringlist.Create;
end;

destructor TChmProject.Destroy;
var i : integer;
begin
  for i:=0 to ffiles.count -1 do
    ffiles.objects[i].free;
  FMergeFiles.Free;
  FFiles.Free;
  FWindows.Free;
  inherited Destroy;
end;


Type
   TSectionEnum = (secOptions,secWindows,secFiles,secMergeFiles,secAlias,secMap,secInfoTypes,secTextPopups,secUnknown);
   TOptionEnum = (OPTAUTO_INDEX,OPTAUTO_TOC,OPTBINARY_INDEX,OPTBINARY_TOC,OPTCITATION,
       OPTCOMPRESS,OPTCOPYRIGHT,OPTCOMPATIBILITY,OPTCOMPILED_FILE,OPTCONTENTS_FILE,
       OPTCREATE_CHI_FILE,OPTDBCS,OPTDEFAULT_FONT,OPTDEFAULT_WINDOW,OPTDEFAULT_TOPIC,
       OPTDISPLAY_COMPILE_NOTES,OPTDISPLAY_COMPILE_PROGRESS,OPTENHANCED_DECOMPILATION,OPTERROR_LOG_FILE,OPTFLAT,
       OPTFULL_TEXT_SEARCH_STOP_LIST,OPTFULL_TEXT_SEARCH,OPTIGNORE,OPTINDEX_FILE,OPTLANGUAGE,OPTPREFIX,
       OPTSAMPLE_STAGING_PATH,OPTSAMPLE_LIST_FILE,OPTTMPDIR,OPTTITLE,OPTCUSTOM_TAB,OPTUNKNOWN);

Const
  SectionNames : Array[TSectionEnum] of String =
      ('OPTIONS','WINDOWS','FILES','MERGE FILES','ALIAS','MAP','INFOTYPES','TEXT POPUPS','UNKNOWN');

  OptionKeys : array [TOptionEnum] of String =
      ('AUTO INDEX','AUTO TOC','BINARY INDEX','BINARY TOC','CITATION',
       'COMPRESS','COPYRIGHT','COMPATIBILITY','COMPILED FILE','CONTENTS FILE',
       'CREATE CHI FILE','DBCS','DEFAULT FONT','DEFAULT WINDOW','DEFAULT TOPIC',
       'DISPLAY COMPILE NOTES','DISPLAY COMPILE PROGRESS','ENHANCED DECOMPILATION','ERROR LOG FILE','FLAT',
       'FULL-TEXT SEARCH STOP LIST','FULL TEXT SEARCH','IGNORE','INDEX FILE','LANGUAGE','PREFIX',
       'SAMPLE STAGING PATH','SAMPLE LIST FILE','TMPDIR','TITLE','CUSTOM TAB','UNKNOWN');



function FindSectionName (const name:string):TSectionEnum;

begin
  result:=low(TSectionEnum);
  while (result<secUnknown) and (name<>SectionNames[Result]) do
    inc(result);
end;

function FindOptionName(Const name:string):TOptionEnum;

begin
  result:=low(TOptionEnum);
  while (result<optUnknown) and (name<>OptionKeys[Result]) do
    inc(result);
end;

procedure TChmProject.readIniOptions(keyvaluepairs:tstringlist);
var i : integer;
    Opt : TOptionEnum;
    OptVal,
    OptValUpper : string;
begin
  for i:=0 to keyvaluepairs.count-1 do
    begin
      Opt:=findoptionname(uppercase(keyvaluepairs.names[i]));
      optval :=keyvaluepairs.valuefromindex[i];
      optvalupper:=uppercase(OptVal);
      case Opt Of
      OPTAUTO_INDEX                : ;
      OPTAUTO_TOC                  : ;
      OPTBINARY_INDEX              : MakeBinaryIndex:=optvalupper='YES';
      OPTBINARY_TOC                : MakeBinaryToc  :=optvalupper='YES';
      OPTCITATION                  : ;
      OPTCOMPRESS                  : ; // Doesn't seem to have effect in workshop
      OPTCOPYRIGHT                 : ;
      OPTCOMPATIBILITY             : ;
      OPTCOMPILED_FILE             : OutputFilename:=optval;
      OPTCONTENTS_FILE             : TableOfContentsFileName:=optval;
      OPTCREATE_CHI_FILE           : ;
      OPTDBCS                      : ; // What this field makes unicode is not known?
      OPTDEFAULT_FONT              : defaultfont:=optval;
      OPTDEFAULT_WINDOW            : defaultwindow:=optval;
      OPTDEFAULT_TOPIC             : defaultpage:=optval;
      OPTDISPLAY_COMPILE_NOTES     : ;
      OPTDISPLAY_COMPILE_PROGRESS  : ;
      OPTENHANCED_DECOMPILATION    : ;
      OPTERROR_LOG_FILE            : ;
      OPTFLAT                      : ;
      OPTFULL_TEXT_SEARCH_STOP_LIST: ;
      OPTIGNORE                    : ;
      OPTINDEX_FILE                : Indexfilename:=optval;
      OPTLANGUAGE                  : ;
      OPTPREFIX                    : ;  // doesn't seem to have effect
      OPTSAMPLE_STAGING_PATH       : ;
      OPTSAMPLE_LIST_FILE          : ;
      OPTTMPDIR                    : ;
      OPTTITLE                     : Title:=optval;
      OPTCUSTOM_TAB                : ;
      OPTUNKNOWN                   : ;  // can be used for errors on unknown keys
      end;
    end;
end;


procedure TChmProject.LoadFromFile(AFileName: String);
var
  Cfg: TXMLConfig;
  MergeFileCount,
  WinCount,
  FileCount: Integer;
  I  : Integer;
  nd : TChmContextNode;
  win: TCHMWindow;
  s  : String;

begin
  Cfg := TXMLConfig.Create(nil);
  Cfg.Filename := AFileName;
  FileName := AFileName;

  Files.Clear;
  FileCount := Cfg.GetValue('Files/Count/Value', 0);
  for I := 0 to FileCount-1 do
    begin
      nd:=TChmContextNode.Create;
      nd.urlname:=Cfg.GetValue('Files/FileName'+IntToStr(I)+'/Value','');
      nd.contextnumber:=Cfg.GetValue('Files/FileName'+IntToStr(I)+'/ContextNumber',0);
      nd.contextname:=Cfg.GetValue('Files/FileName'+IntToStr(I)+'/ContextName','');
      Files.AddObject(nd.urlname,nd);
    end;

  WinCount:= Cfg.GetValue('Windows/Count/Value', 0);
  for i:=0 To WinCount-1 do
    begin
      win:=TCHMWindow.Create;
      win.loadfromxml(cfg,'Windows/item'+inttostr(i)+'/');
      fwindows.add(win);
    end;

  Mergefilecount:=Cfg.getValue('MergeFiles/Count/Value', 0);
  for i:=0 To MergeFileCount-1 do
    Mergefiles.add(Cfg.getValue('MergeFiles/FileName'+IntToStr(I)+'/value',''));

  // load some values that changed key backwards compatible.

  IndexFileName := Cfg.GetValue('Files/IndexFile/Value','');
  if IndexFileName='' Then
    IndexFileName := Cfg.GetValue('Settings/IndexFile/Value','');

  TableOfContentsFileName := Cfg.GetValue('Files/TOCFile/Value','');
  If TableOfContentsFileName='' then
    TableOfContentsFileName := Cfg.GetValue('Settings/TOCFile/Value','');

  // For chm file merging, bintoc must be false and binindex true. Change defaults in time?
  // OTOH, merging will be mostly done for fpdoc files, and that doesn't care about defaults.

  S:=Cfg.GetValue('Files/MakeBinaryTOC/Value', '');
  if s='' Then
    MakeBinaryTOC := Cfg.GetValue('Settings/MakeBinaryTOC/Value', True)
  else
    MakeBinaryTOC := Cfg.GetValue('Files/MakeBinaryTOC/Value', True);

  S:=Cfg.GetValue('Files/MakeBinaryIndex/Value', '');
  if s='' Then
    MakeBinaryIndex := Cfg.GetValue('Settings/MakeBinaryIndex/Value', False)
  else
    MakeBinaryIndex := Cfg.GetValue('Files/MakeBinaryIndex/Value', False);

  AutoFollowLinks := Cfg.GetValue('Settings/AutoFollowLinks/Value', False);
  MakeSearchable := Cfg.GetValue('Settings/MakeSearchable/Value', False);
  DefaultPage := Cfg.GetValue('Settings/DefaultPage/Value', '');
  Title := Cfg.GetValue('Settings/Title/Value', '');
  OutputFileName := Cfg.GetValue('Settings/OutputFileName/Value', '');
  DefaultFont  := Cfg.GetValue('Settings/DefaultFont/Value', '');
  DefaultWindow:= Cfg.GetValue('Settings/DefaultWindow/Value', '');
  Cfg.Free;
end;

function cleanupstring(const s:string):string;
var
  i:integer;
begin
  i:=pos(';',s);
  if i>0 then
    result:=trim(copy(s,1,i-1))
  else
    result:=trim(s);
end;

procedure TChmProject.LoadFromhhp (AFileName:String;LeaveInclude:Boolean);
// leaveinclude=true leaves includefiles includefiles.

procedure addalias(const key,value :string);

var i,j : integer;
    node: TCHMContextNode;
    keyupper : string;
begin
 {$ifdef hhp_debug}
   writeln('alias entry:',key,'=',value);
 {$endif}
 keyupper:=uppercase(value);
 i:=0; j:=files.count;
 while (i<j) and (uppercase(TCHMContextnode(files.objects[i]).UrlName)<>keyupper) do
  inc(i);
 if i=j then
  begin
   {$ifdef hhp_debug}
    writeln('alias new node:',key);
   {$endif}
    node:=TCHMContextNode.create;
    node.URLName:=value;
    node.contextname:=key;
  end
 else
  begin
    node:=TCHMContextNode(Files.objects[i]);
    node.ContextName:=key;
  end;
end;

procedure processalias(strs:TStringlist);
var i,j : integer;
    s : string;
    strls2:tstringlist;

begin
 for i:=0 to strs.count-1 do
  begin
    s:=cleanupstring(strs[i]);
    if uppercase(copy(s,1,8))='#INCLUDE' then
      begin
        delete(s,1,8);
        s:=trim(s);
        if fileexists(s) then
          begin
            strls2:=TstringList.create;
            strls2.loadfromfile(s);
            processalias(strls2);
            strls2.free;
          end;

      end
    else
     begin
       s:=cleanupstring(s);
       j:=pos('=',s);
       if j>0 then
         addalias(trim(copy(s,1,j-1)),copy(s,j+1,length(s)-j));
     end;
  end;
end;

procedure addmap(const key,value :string);

var i,j : integer;
    node: TCHMContextNode;
    keyupper : string;
begin
 {$ifdef hhp_debug}
 writeln('map entry:',key,'=',value);
 {$endif}
 keyupper:=uppercase(key);
 i:=0; j:=files.count;
 while (i<j) and (uppercase(TCHMContextnode(files.objects[i]).contextname)<>keyupper) do
  inc(i);
 if i=j then
    raise Exception.create('context "'+key+'" not found!')
 else
  begin
    node:=TCHMContextNode(Files.objects[i]);
    node.Contextnumber:=strtointdef(value,0);
  end;
end;

procedure processmap(strs:TStringlist);
var i,j : integer;
    s : string;
    strls2:tstringlist;

begin
 for i:=0 to strs.count-1 do
  begin
    s:=cleanupstring(strs[i]);
    {$ifdef hhp_debug}
      writeln('map item:',s);
    {$endif}
    if uppercase(copy(s,1,8))='#INCLUDE' then
      begin
        delete(s,1,8);
        s:=trim(s);
        if fileexists(s) then
          begin
            strls2:=TstringList.create;
            strls2.loadfromfile(s);
            processmap(strls2);
            strls2.free;
          end;
      end
    else
     begin
       s:=cleanupstring(s);
       if uppercase(copy(s,1,7))='#DEFINE' Then
         begin
           delete(s,1,7);
           s:=trim(s);
           j:=pos(' ',s);
           if j>0 then
             addmap(trim(copy(s,1,j-1)),copy(s,j+1,length(s)-j));
         end
       else
         begin
            {$ifdef hhp_debug}
              writeln('map leftover:',s);
            {$endif}
         end;
     end;
  end;
end;

var
  Fini      : TMemIniFile;  // TMemInifile is more compatible with Delphi. Delphi's API based TIniFile fails on .hhp files.
  secs,strs : TStringList;
  i,j       : Integer;
  section   : TSectionEnum;
  nd        : TChmContextNode;

begin
  Fini:=TMeminiFile.Create(AFileName);
  secs := TStringList.create;
  strs := TStringList.create;
  fini.readsections(secs);

  // Do the files section first so that we can emit errors if
  // other sections reference unknown files.

  fini.readsectionvalues(SectionNames[secFiles] ,strs);
  if strs.count>0 then
    for j:=0 to strs.count-1 do
      begin
          nd:=TChmContextNode.Create;
          nd.urlname:=strs[j];
          nd.contextnumber:=0;
          nd.contextname:='';
          Files.AddObject(nd.urlname,nd);
        end;

  // aliases also add file nodes.

  fini.readsectionvalues(SectionNames[secAlias] ,strs); // resolve all aliases.
  if strs.count>0 then
    processalias(strs);

  // map files only add to existing file nodes.
  fini.readsectionvalues(SectionNames[secmap] ,strs);
  if strs.count>0 then
    processmap(strs);


  for i:=0 to secs.count-1 do
    begin
      section:=FindSectionName(Uppercase(Secs[i]));
      if section<>secunknown then
        fini.readsectionvalues(secs[i] ,strs);
      case section of
      secOptions   : readinioptions(strs);
      secWindows   : for j:=0 to strs.count-1 do
                       FWindows.add(TCHMWindow.Create(strs[j]));
      secFiles     : ; // already done
      secMergeFiles: FMergeFiles.Assign(Strs); // just a filelist
      secAlias     : ; // already done
      secMap       : ; // already done
      secInfoTypes : ; // unused for now.
      secTextPopups: ; // rarely used.
      end;
    end;
  secs.free;
  strs.free;
  fini.free;
end;

procedure TChmProject.AddFileWithContext(contextid:integer;filename:ansistring;contextname:ansistring='');
var x : integer;
    nd : TChmContextNode;
begin
  x:=files.indexof(filename);
  if x=-1 then
    begin
      nd:=TChmContextNode.Create;
      nd.urlname:=filename;
      nd.contextnumber:=contextid;
      nd.contextname:=contextname;
      Files.AddObject(nd.urlname,nd);
    end
  else
   begin
     nd:=TChmContextNode(files.objects[x]);
     if not assigned(nd) then
       begin
         nd:=TChmContextNode.Create;
         nd.urlname:=filename;
         files.objects[x]:=nd;
       end;
      nd.contextnumber:=contextid;
      nd.contextname:=contextname;
   end;
end;

procedure TChmProject.SaveToFile(AFileName: String);
var
  Cfg: TXMLConfig;
  I  : Integer;
  nd : TChmContextNode;
begin
  Cfg := TXMLConfig.Create(nil);
  Cfg.StartEmpty := True;
  Cfg.Filename := AFileName;
  Cfg.Clear;
  Cfg.SetValue('Files/Count/Value', Files.Count);
  for I := 0 to Files.Count-1 do
  begin
    nd:=TChmContextNode(files.objects[i]);
    Cfg.SetValue('Files/FileName'+IntToStr(I)+'/Value', Files.Strings[I]);
    if assigned(nd) then
      begin
        Cfg.SetValue('Files/FileName'+IntToStr(I)+'/ContextNumber', nd.contextnumber);
        Cfg.SetValue('Files/FileName'+IntToStr(I)+'/ContextName', nd.contextname);
      end;
  end;

  Cfg.SetValue('Windows/Count/Value', FWindows.count);
  for i:=0 To FWindows.Count-1 do
    TCHMWindow(FWindows[i]).savetoxml(cfg,'Windows/item'+inttostr(i)+'/');

  Cfg.SetValue('MergeFiles/Count/Value', FMergeFiles.count);
  for i:=0 To FMergeFiles.Count-1 do
    Cfg.SetValue('MergeFiles/FileName'+IntToStr(I)+'/value',FMergeFiles[i]);

  // delete legacy keys.
  Cfg.SetValue('Files/IndexFile/Value','');
  Cfg.SetValue('Files/TOCFile/Value', '');
  Cfg.SetValue('Files/MakeBinaryTOC/Value','');
  Cfg.SetValue('Files/MakeBinaryIndex/Value','');
  Cfg.SetValue('Settings/IndexFile/Value', IndexFileName);
  Cfg.SetValue('Settings/TOCFile/Value', TableOfContentsFileName);
  Cfg.SetValue('Settings/MakeBinaryTOC/Value',MakeBinaryTOC);
  Cfg.SetValue('Settings/MakeBinaryIndex/Value',MakeBinaryIndex);

  Cfg.SetValue('Settings/AutoFollowLinks/Value', AutoFollowLinks);
  Cfg.SetValue('Settings/MakeSearchable/Value', MakeSearchable);
  Cfg.SetValue('Settings/DefaultPage/Value', DefaultPage);
  Cfg.SetValue('Settings/Title/Value', Title);
  Cfg.SetValue('Settings/OutputFileName/Value', OutputFileName);
  Cfg.SetValue('Settings/DefaultFont/Value', DefaultFont);

  Cfg.SetValue('Settings/DefaultWindow/Value', DefaultWindow);


  Cfg.Flush;
  Cfg.Free;
end;

function TChmProject.ProjectDir: String;
begin
  Result := ExtractFilePath(FileName);
end;

procedure TChmProject.WriteChm(AOutStream: TStream);
var
  Writer     : TChmWriter;
  TOCStream,
  IndexStream: TFileStream;
  nd         : TChmContextNode;
  I          : Integer;
begin
  IndexStream := nil;
  TOCStream := nil;

  Writer := TChmWriter.Create(AOutStream, False);

  // our callback to get data
  Writer.OnGetFileData := @GetData;
  Writer.OnLastFile    := @LastFileAdded;

  // give it the list of files
  Writer.FilesToCompress.AddStrings(Files);

  // now some settings in the chm
  Writer.DefaultPage := DefaultPage;
  Writer.Title := Title;
  Writer.DefaultFont := DefaultFont;
  Writer.FullTextSearch := MakeSearchable;
  Writer.HasBinaryTOC := MakeBinaryTOC;
  Writer.HasBinaryIndex := MakeBinaryIndex;

  for i:=0 to files.count-1 do
    begin
      nd:=TChmContextNode(files.objects[i]);
      if assigned(nd) and (nd.contextnumber<>0) then
        Writer.AddContext(nd.ContextNumber,files[i]);
    end;

  // and write!
  Writer.Execute;

  if Assigned(TOCStream) then TOCStream.Free;
  if Assigned(IndexStream) then IndexStream.Free;
  Writer.Free;
end;



end.

