{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2008 by Marco van de Voort

    Wrapper for CHM reading to avoid having to import Delphi units into whtmlhlp,
      which can cause all kinds of namespace conflicts.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wchmhwrap;

interface 
{$Mode Delphi}

Uses  wutils,whelp,whtml,SysUtils,ChmReader,ChmSiteMap,Classes;

Type
//      TopicLinks: PTopicLinkCollection;IndexEntries : PUnsortedIndexEntryCollection;  

     TChmWrapper = Class
                     private
                       ffs   : Classes.TFileStream;
                       fchmr : TChmReader;
                       findex: TChmSiteMap;
                       ftopic: TChmSiteMap;
                       floaded  : boolean;
                     public    
                      constructor Create(name:String);
                      function	  LoadIndex(id:integer;TopicLinks: PTopicLinkCollection;IndexEntries : PUnsortedIndexEntryCollection;helpfacility:PHelpFacility):boolean;
                      function    GetTopic(name:string):PMemoryTextFile;
                      destructor  Destroy;override;
                    end;

function combinepaths(relpath,basepath:String):String;

implementation

function combinepaths(relpath,basepath:String):String;

begin
  {$ifdef combinedebug}
    debugmessageS({$i %file%},'combine in "'+relpath+'" and "'+basepath+'"',{$i %line%},'1',0,0);
  {$endif}

  if relpath='' then exit;
  if relpath[length(relpath)]<>'/' Then
    basepath:=extractfiledir(basepath);
  while (length(relpath)>0) and (copy(relpath,1,3)='../') do
     begin
       basepath:=extractfiledir(basepath);
       delete(relpath,1,3);
     end;
       
   {$ifdef combinedebug}
    debugmessageS({$i %file%},'combine out "'+relpath+'" and "'+basepath+'"',{$i %line%},'1',0,0);
  {$endif}
  
  result:=basepath+relpath;
end;


Constructor TChmWrapper.Create(name:string);

begin
  ffs:=Classes.TFileStream.create(name,fmOpenRead);
  fchmr:=TChmReader.Create(ffs,True); // owns ffs
  findex:=nil;
  if not fchmr.isvalidfile then
    begin
      freeandnil(fchmr);
      freeandnil(ffs);
      exit;  
    end;      
  {$ifdef wdebug}
    debugmessageS({$i %file%},'TCHMWrapper: before sitemap creation ',{$i %line%},'1',0,0);
  {$endif}
  findex:=TChmSiteMap.create(stindex);
  ftopic:=TChmSiteMap.create(sttoc);
  {$ifdef wdebug}
    debugmessageS({$i %file%},'TCHMWrapper: after sitemap creation ',{$i %line%},'1',0,0);
  {$endif}
  floaded:=false;
end;

function TChmWrapper.LoadIndex(id:integer;TopicLinks: PTopicLinkCollection;IndexEntries : PUnsortedIndexEntryCollection;helpfacility:PHelpFacility):boolean;
function FormatAlias(Alias: string): string;
begin
  if Assigned(HelpFacility) then
    if length(Alias)>HelpFacility^.IndexTabSize-4 then
       Alias:=Trim(copy(Alias,1,HelpFacility^.IndexTabSize-4-2))+'..';
//  if (length(alias)>0) and (alias[1]<>'/') then Alias:='/'+alias;
  FormatAlias:=Alias;
end;
              
var
    m : Classes.TMemoryStream;
    i,j : integer;
    item : TChmSiteMapItem;
    tli: integer;
begin
 result:=false;
 if not assigned (fchmr) then exit;
 if floaded then exit;
 {$ifdef wdebug}
     debugmessageS({$i %file%},'TCHMWrapper: indexfilename:'+fchmr.indexfile,{$i %line%},'1',0,0); 
 {$endif}
  
  m:=fchmr.getobject(fchmr.indexfile);
  try
   if assigned(m) then
     begin
      {$ifdef wdebug}
       debugmessageS({$i %file%},'TCHMWrapper: stream size loaded :'+inttostr(m.size),{$i %line%},'1',0,0); 
      {$endif}
      findex.loadfromStream(m);
    end;
  finally
    freeandnil(m);
    end;
   {$ifdef wdebug}
     debugmessageS({$i %file%},'TCHMWrapper: loadindex after final ',{$i %line%},'1',0,0); 
  {$endif}
  
  tli:=TopicLinks^.AddItem(fchmr.defaultpage); 
  TLI:=EncodeHTMLCtx(ID,TLI+1);
  IndexEntries^.Insert(NewIndexEntry(  FormatAlias('Table of contents'),ID,TLI));
  for i:=0 to findex.items.count-1 do
    begin
      item:=findex.items.item[i];
      tli:=TopicLinks^.AddItem('/'+item.local); 
      TLI:=EncodeHTMLCtx(ID,TLI+1);
      IndexEntries^.Insert(NewIndexEntry(  FormatAlias(item.text),ID,TLI));
    end;
   {$ifdef wdebug}
     debugmessageS({$i %file%},'TCHMWrapper: endloadindex ',{$i %line%},'1',0,0); 
  {$endif}
  floaded:=true;
  result:=true;
end;

function   TChmWrapper.GetTopic(name:string):PMemoryTextFile;

var
  m : Classes.TMemorystream;
  linedata:Classes.TStringList;
  i : integer;
begin
  result:=nil;
  if not assigned(fchmr) or (name='') then exit;
  
  If (name[1]<>'/') and (copy(name,1,7)<>'ms-its:') Then
    name:='/'+name;
  linedata:=Classes.TStringList.create;
  try
    {$ifdef wdebug}
     debugmessageS({$i %file%},'TCHMWrapper: Getting file '+name,{$i %line%},'1',0,0); 
    {$endif}
//    if uppercase(name)='TABLE OF CONTENTS' Then
  //    m:=fchmr.getobject(fchmr.tocfile)
//    else
      m:=fchmr.getobject(name);
    
    if not assigned(m) then exit;
    linedata.loadfromstream(m);
    result:=new(PMemoryTextFile,Init);
    for i:=0 to linedata.count-1 do
       result.addline(linedata[i]);
  finally
    m.free;
    linedata.free;
  end;
end;


destructor TChmWrapper.Destroy;

begin
  freeandnil(ftopic);
  freeandnil(findex);
  freeandnil(fchmr);
end;
// m:=r.getobject(r.indexfile);
//  siteindex.loadfromStream(m);

end.