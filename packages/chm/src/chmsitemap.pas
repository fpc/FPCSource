{ Copyright (C) <2005> <Andrew Haines> chmsitemap.pas

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
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}
{
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
}
unit chmsitemap;

{$mode Delphi}{$H+}
{define preferlower}
interface

uses
  Classes, SysUtils, fasthtmlparser, contnrs, strutils, generics.collections;

type
  TChmSiteMapItems = class; // forward
  TChmSiteMap = class;
  TChmSiteMapItem = class;

  { TChmSiteMapItem }

  TChmSiteMapItemAttrName = (siteattr_NONE,
                             siteattr_KEYWORD, // alias for name in sitemap
                             siteattr_NAME,
                             siteattr_LOCAL,
                             siteattr_URL,
                             siteattr_TYPE,
                             siteattr_SEEALSO,
                             siteattr_IMAGENUMBER,
                             siteattr_NEW,
                             siteattr_COMMENT,
                             siteattr_MERGE,
                             siteattr_FRAMENAME,
                             siteattr_WINDOWNAME,
                             siteattr_WINDOW_STYLES,
                             siteattr_EXWINDOW_STYLES,
                             siteattr_FONT,
                             siteattr_IMAGELIST,
                             siteattr_IMAGETYPE,
                             siteattr_BACKGROUND,
                             siteattr_FOREGROUND
                            );

  { TChmSiteMapSubItem }
  TChmSiteMapGenerationOptions = (Default,emitkeyword);
  TChmSiteMapSubItem = class(TPersistent)
  private
    FName,
    FType,
    FLocal,
    FUrl,
    FSeeAlso  : String;
    FOwner : TChmSiteMapItem;
  public
    constructor Create(AOwner: TChmSiteMapItem);
    destructor Destroy; override;
  published
    property Name : String read FName  write FName;  //hhk
    property ItemType : String read FType write FType; //both
    property Local: String read FLocal write FLocal; //both
    property URL  : String read FURL write FURL;     //both
    property SeeAlso: String read FSeeAlso write FSeeAlso; //hhk
  end;

// hhk items:  Merge | ([Name] ([Name] [Type...] [Local [URL] | See Also])... [FrameName] [WindowName] [Comment])
// hhc items:  Merge | ([Name] ([Type...] [Local] [URL])... [FrameName] [WindowName] [Comment] [New] [ImageNumber])
  TChmSiteMapItem = class(TPersistent)
  private
    FChildren: TChmSiteMapItems;
    FComment: String;
    FImageNumber: Integer;
    FIncreaseImageIndex: Boolean;
    FOwner: TChmSiteMapItems;
    FName   : String;
    FMerge : String;
    FFrameName : String;
    FWindowName : String;
    FSubItems : TObjectList;
    function getlocal: string;
    function getseealso:string;
    function getsubitem( index : integer): TChmSiteMapSubItem;
    function getsubitemcount: integer;
    procedure SetChildren(const AValue: TChmSiteMapItems);
  public
    constructor Create(AOwner: TChmSiteMapItems);
    destructor Destroy; override;
    procedure AddName(const Name:string);
    procedure AddLocal(const Local:string);
    procedure AddSeeAlso(const SeeAlso:string);
    procedure AddURL(const URL:string);
    procedure AddType(const AType:string);
    procedure Sort(Compare: TListSortCompare);
  published
    property Children: TChmSiteMapItems read FChildren write SetChildren;
    property Name: String read FName write FName;
    property ImageNumber: Integer read FImageNumber write FImageNumber default -1;
    property IncreaseImageIndex: Boolean read FIncreaseImageIndex write FIncreaseImageIndex;
    property Comment: String read FComment write FComment;
    property Owner: TChmSiteMapItems read FOwner;
    property Keyword : string read fname; // deprecated;             // Use name, sitemaps don't store the difference.
    property Local : string read getlocal; // deprecated;            // should work on ALL pairs
    property Text : string read fname write fname; // deprecated;    // should work on ALL pairs
    property SeeAlso : string read getseealso; // deprecated;        // should work on ALL pairs
    property FrameName: String read FFrameName write FFrameName;
    property WindowName: String read FWindowName write FWindowName;
    property Merge: String read FMerge write FMerge;
    property SubItem[ index :integer]:TChmSiteMapSubItem read getsubitem;
    property SubItemcount  :integer read getsubitemcount;
  end;

  { TChmSiteMapItems }

  TChmSiteMapItems = class(TPersistent)
  private
    FInternalData: Dword;
    FList: TList;
    FOwner: TChmSiteMap;
    FParentItem: TChmSiteMapItem;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TChmSiteMapItem;
    function getparentname: String;
    procedure SetItem(AIndex: Integer; const AValue: TChmSiteMapItem);
  public
    constructor Create(AOwner: TChmSiteMap; AParentItem: TChmSiteMapItem);
    destructor Destroy; override;
    procedure Delete(AIndex: Integer);
    function Add(AItem: TChmSiteMapItem): Integer;
    function NewItem: TChmSiteMapItem;
    function Insert(AItem: TChmSiteMapItem; AIndex: Integer): Integer;
    procedure Clear;
    procedure Sort(Compare: TListSortCompare);
    property Item[AIndex: Integer]: TChmSiteMapItem read GetItem write SetItem;
    property Count: Integer read GetCount;
    property ParentItem: TChmSiteMapItem read FParentItem;
    property Owner: TChmSiteMap read FOwner;
    property InternalData: Dword read FInternalData write FInternalData;
    property ParentName : String read getparentname;
  end;
  

  { TChmSiteMapTree }
  TSiteMapType = (stTOC, stIndex);
  
  TSiteMapTag = (smtUnknown, smtNone, smtHTML, smtHEAD, smtBODY);
  TSiteMapTags = set of TSiteMapTag;

  TSiteMapBodyTag = (smbtUnknown, smbtNone, smbtUL, smbtLI, smbtOBJECT, smbtPARAM);
  TSiteMapBodyTags = set of TSiteMapBodyTag;
  
  TLIObjectParamType = (ptName, ptLocal, ptKeyword);

  TChmSiteMap = class
  private
    FAutoGenerated: Boolean;
    FBackgroundColor: LongInt;
    FCurrentItems: TChmSiteMapItems;
    FExWindowStyles: LongInt;
    FFont: String;
    FForegroundColor: LongInt;
    FFrameName: String;
    FImageList: String;
    FImageWidth: Integer;
    FSiteMapTags: TSiteMapTags;
    FSiteMapBodyTags: TSiteMapBodyTags;
    FHTMLParser: THTMLParser;
    FItems: TChmSiteMapItems;
    FSiteMapType: TSiteMapType;
    FUseFolderImages: Boolean;
    FWindowName: String;
    FLevel: Integer;
    FLevelForced: Boolean;
    FWindowStyles: LongInt;
    FLoadDict : TDictionary<String,TChmSiteMapItemAttrName>;
    fChmSiteMapGenerationOptions:TChmSiteMapGenerationOptions;
    procedure SetItems(const AValue: TChmSiteMapItems);
    procedure CheckLookup;
  protected
    procedure FoundTag (ACaseInsensitiveTag, AActualTag: string);
    procedure FoundText(AText: string);
  public
    constructor Create(AType: TSiteMapType);
    destructor Destroy; override;
    Procedure Sort(Compare: TListSortCompare);
    procedure LoadFromFile(AFileName: String);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(AFileName:String);
    procedure SaveToStream(AStream: TStream);
    property Items: TChmSiteMapItems read FItems write SetItems;
    property SiteMapType: TSiteMapType read FSiteMapType;
    // SiteMap properties. most of these are invalid for the index
    property FrameName: String read FFrameName write FFrameName;
    property WindowName: String read FWindowName write FWindowName;
    property ImageList: String read FImageList write FImageList;
    property ImageWidth: Integer read FImageWidth write FImageWidth;
    property BackgroundColor: LongInt read FBackgroundColor write FBackgroundColor;
    property ForegroundColor: LongInt read FForegroundColor write FForegroundColor;
    property ExWindowStyles: LongInt read FExWindowStyles write FExWindowStyles;
    property WindowStyles: LongInt read FWindowStyles write FWindowStyles;
    property UseFolderImages: Boolean read FUseFolderImages write FUseFolderImages;
    property Font: String read FFont write FFont;
    property AutoGenerated: Boolean read FAutoGenerated write FAutoGenerated;
    property ChmSiteMapGenerationOptions : TChmSiteMapGenerationOptions read fChmSiteMapGenerationOptions write fChmSiteMapGenerationOptions;
  end;


function indexitemcompare(Item1, Item2: Pointer): Integer;
implementation
uses HTMLUtil;

const sitemapkws : array[TChmSiteMapItemAttrName] of string = (
                    '',
                    'KEYWORD',
                    'NAME',
                    'LOCAL',
                    'URL',
                    'TYPE',
                    'SEE ALSO',
                    'IMAGENUMBER',
                    'NEW',
                    'COMMENT',
                    'MERGE',
                    'FRAMENAME',
                    'WINDOWNAME',
                    'WINDOW STYLES',
                    'EXWINDOW STYLES',
                    'FONT',
                    'IMAGELIST',
                    'IMAGETYPE',
                    'BACKGROUND',
                    'FOREGROUND');

function indexitemcompare(Item1, Item2: Pointer): Integer;
begin
    Result := naturalComparetext(LowerCase(TChmSiteMapItem(item1).name), Lowercase(TChmSiteMapItem(item2).name));
end;
{ TChmSiteMapSubItem }

constructor TChmSiteMapSubItem.Create(AOwner: TChmSiteMapItem);
begin
  FOwner:=AOwner;
end;

destructor TChmSiteMapSubItem.Destroy;
begin
  inherited Destroy;
end;

{ TChmSiteMapTree }

procedure TChmSiteMap.SetItems(const AValue: TChmSiteMapItems);
begin
  if FItems=AValue then exit;
  FItems:=AValue;
end;

procedure TChmSiteMap.CheckLookup;
var en : TChmSiteMapItemAttrName;
begin
  if assigned(FLoadDict) then
    exit;
  FLoadDict :=TDictionary<String,TChmSiteMapItemAttrName>.Create;
  for en:=succ(low(en)) to high(en) do
    FLoadDict.add(sitemapkws[en],en);
end;

procedure TChmSiteMap.FoundTag(ACaseInsensitiveTag, AActualTag: string);
    procedure NewSiteMapItem;
    begin
      FCurrentItems.Add(TChmSiteMapItem.Create(FCurrentItems));
    end;
    function ActiveItem: TChmSiteMapItem;
    begin
      if FCurrentItems.Count=0 then
        NewSiteMapItem;

      Result := FCurrentItems.Item[FCurrentItems.Count-1]
    end;
    procedure IncreaseULevel;
    begin
      if FCurrentItems = nil then FCurrentItems := Items
      else begin
        //WriteLn('NewLevel. Count = ', FCurrentItems.Count, ' Index = ',Items.Count-1);
        FCurrentItems := ActiveItem.Children;
      end;
      Inc(FLevel);
    end;
    procedure DecreaseULevel;
    begin
      if Assigned(FCurrentItems) and Assigned(FCurrentItems.ParentItem) then
        FCurrentItems := FCurrentItems.ParentItem.Owner
      else FCurrentItems := nil;
      Dec(FLevel);
    end;

// hhk items:  Merge | ([Name] ([Name] [Type...] [Local [URL] | See Also])... [FrameName] [WindowName] [Comment])
// hhc items:  Merge | ([Name] ([Type...] [Local] [URL])... [FrameName] [WindowName] [Comment] [New] [ImageNumber])
var
  TagName,
  TagAttributeName,
  TagAttributeValue: String;
  isParam,IsMerged : string;
  TagAttrName  : TChmSiteMapItemAttrName;
begin
  TagName := GetTagName(ACaseInsensitiveTag);
   if TagName = 'UL' then begin
     IncreaseULevel;
   end
   else if TagName = '/UL' then begin
     DecreaseULevel;
   end
   else if (TagName = 'LI') and (FLevel = 0) then
     FLevelForced := True
   else if TagName = 'OBJECT' then begin
     Include(FSiteMapBodyTags, smbtOBJECT);
     if FLevelForced then
       IncreaseULevel;
     If FLevel > 0 then // if it is zero it is the site properties
       NewSiteMapItem;
   end
   else if TagName = '/OBJECT' then begin
     Exclude(FSiteMapBodyTags, smbtOBJECT);
     if FLevelForced then
     begin
       DecreaseULevel;
       FLevelForced := False;
     end;
   end
   else begin // we are the properties of the object tag
     if (smbtOBJECT in FSiteMapBodyTags) then
       begin
        if (FLevel > 0 ) then
         begin
            if LowerCase(GetTagName(AActualTag)) = 'param' then begin
              TagAttributeName := GetVal(AActualTag, 'name');
              TagAttributeValue := GetVal(AActualTag, 'value');

              // a hash reduces comparisons and casing, and generics make it easy.
              if not FLoadDict.trygetvalue(uppercase(TagAttributeName),TagAttrName) then
                 TagAttrName:=siteattr_none;

              if TagAttrName <> siteattr_none then begin
                 case TagAttrName of
                 siteattr_KEYWORD,
                 siteattr_NAME         : Activeitem.AddName(TagAttributeValue);
                 siteattr_LOCAL        : ActiveItem.AddLocal(TagAttributeValue);
                 siteattr_URL          : ActiveItem.AddURL (TagAttributeValue);
                 siteattr_TYPE         : ActiveItem.AddType (TagAttributeValue);
                 siteattr_SEEALSO      : ActiveItem.AddSeeAlso(TagAttributeValue);
                 siteattr_IMAGENUMBER  : ActiveItem.ImageNumber := StrToInt(TagAttributeValue);
                 siteattr_NEW          : ActiveItem.IncreaseImageIndex := (LowerCase(TagAttributeValue) = 'yes');
                 siteattr_COMMENT      : ActiveItem.Comment := TagAttributeValue;
                 siteattr_MERGE        : ActiveItem.Merge:= TagAttributeValue;
                 siteattr_FRAMENAME    : ActiveItem.FrameName:=TagAttributeValue;
                 siteattr_WINDOWNAME   : ActiveItem.WindowName:=TagAttributeValue;
                 end;
              end;
            end;
         end
       else
         begin // object and level is zero?
           if LowerCase(GetTagName(AActualTag)) = 'param' then begin
             begin
               TagAttributeName := uppercase(GetVal(AActualTag, 'name'));
               TagAttributeValue := GetVal(AActualTag, 'value');
               if not FLoadDict.trygetvalue(uppercase(TagAttributeName),TagAttrName) then
                  TagAttrName:=siteattr_none;
               if TagAttrName <> siteattr_none then begin
                  case TagAttrName of
                   siteattr_FRAMENAME       : FrameName:=TagAttributeValue;
                   siteattr_WINDOWNAME      : WindowName:=TagAttributeValue;
                   siteattr_WINDOW_STYLES   : WindowStyles:=StrToIntDef(TagAttributeValue,0);
                   siteattr_EXWINDOW_STYLES : ExWindowStyles:=StrToIntDef(TagAttributeValue,0);
                   siteattr_FONT            : Font:=TagAttributeValue;
                   siteattr_IMAGELIST       : ImageList:=TagAttributeValue;
                   siteattr_IMAGETYPE       : UseFolderImages:=uppercase(TagAttributeValue)='FOLDER';
                   siteattr_BACKGROUND      : BackgroundColor:=strtointdef(trim(TagAttributeValue),longint(-1));
                   siteattr_FOREGROUND      : ForegroundColor:=strtointdef(trim(TagAttributeValue),0)
                   end;
             end;
              // writeln('0:',flevel,' ' ,aactualtag,' ',tagname,' ' ,tagattributename, ' ' ,tagattributevalue);
             end;
             end;
         end;
      end;
   end;
// end; {body}
  //end   {html}
end;

procedure TChmSiteMap.FoundText(AText: string);
begin
  //WriteLn('TEXT:', AText);
end;

constructor TChmSiteMap.Create(AType: TSiteMapType);
begin
  Inherited Create;
  FSiteMapType := AType;
  FSiteMapTags := [smtNone];
  FSiteMapBodyTags := [smbtNone];
  FHTMLParser:=nil;
  FItems := TChmSiteMapItems.Create(Self, nil);  ;
end;

destructor TChmSiteMap.Destroy;
begin
  if Assigned(FHTMLParser) then FHTMLParser.Free;
  FItems.Free;
  FLoadDict.Free;

  Inherited Destroy;
end;

procedure TChmSiteMap.Sort(Compare: TListSortCompare);
begin
  FItems.sort(compare);
end;

procedure TChmSiteMap.LoadFromFile(AFileName: String);
var
  Buffer: String;
  TmpStream: TMemoryStream;
begin
  CheckLookup;
  if Assigned(FHTMLParser) then FHTMLParser.Free;
  TmpStream := TMemoryStream.Create;
  try
    TmpStream.LoadFromFile(AFileName);
    SetLength(Buffer, TmpStream.Size);
    TmpStream.Position := 0;
    TmpStream.Read(Buffer[1], TmpStream.Size);
  finally
    TmpStream.Free;
  end;
  FHTMLParser := THTMLParser.Create(Buffer);
  try
    FHTMLParser.OnFoundTag := FoundTag;
    FHTMLParser.OnFoundText := FoundText;
    FHTMLParser.Exec;
  finally
    FreeAndNil(FHTMLParser);
  end;
end;

procedure TChmSiteMap.LoadFromStream(AStream: TStream);
var
  Buffer: String;
begin
  CheckLookup;
  if Assigned(FHTMLParser) then FHTMLParser.Free;
  SetLength(Buffer, AStream.Size-AStream.Position);
  if AStream.Read(Buffer[1], AStream.Size-AStream.Position) > 0 then begin;
    FHTMLParser := THTMLParser.Create(Buffer);
    FHTMLParser.OnFoundTag := FoundTag;
    FHTMLParser.OnFoundText := FoundText;
    FHTMLParser.Exec;
    FreeAndNil(FHTMLParser);
  end;
end;

procedure TChmSiteMap.SaveToFile(AFileName:String);
var
  fs : TFileStream;
begin
  fs:=TFileStream.Create(AFileName,fmcreate);
  try
    SaveToStream(fs);
  finally
    fs.free;
    end;
end;

// hhk items:  Merge | ([Name] ([Name] [Type...] [Local [URL] | See Also])... [FrameName] [WindowName] [Comment])
// hhc items:  Merge | ([Name] ([Type...] [Local] [URL])... [FrameName] [WindowName] [Comment] [New] [ImageNumber])

procedure TChmSiteMap.SaveToStream(AStream: TStream);
var
  Indent: Integer;
  procedure WriteString(AString: String);
  var
    I: Integer;
  begin
     for I := 0 to Indent-1 do AStream.WriteByte(Byte(' '));
     AStream.Write(AString[1], Length(AString));
     AStream.WriteByte(10);
  end;
  procedure WriteStringNoIndent(AString: String);
  var
    I: Integer;
  begin
     AStream.Write(AString[1], Length(AString));
  end;

  procedure WriteParam(AName: String; AValue: String);
  begin
    WriteString('<param name="'+AName+'" value="'+AValue+'">');
  end;
  procedure WriteEntries(AItems: TChmSiteMapItems);
  var
    I,J : Integer;
    Item: TChmSiteMapItem;
    Sub : TChmSiteMapSubItem;
    lemitkeyword : boolean;
  begin
    lemitkeyword:=ChmSiteMapGenerationOptions=emitkeyword;
    for I := 0 to AItems.Count-1 do begin
      Item := AItems.Item[I];

      {$ifdef preferlower}
      WriteString('<li> <object type="text/sitemap">');
      {$else}
      WriteString('<LI> <OBJECT type="text/sitemap">');
      {$endif}
      Inc(Indent, 8);

      if Item.Name<>'' then
        begin
          if lemitkeyword then
            WriteParam('Keyword', item.Name)
          else
            WriteParam('Name', Item.Name);
        end;

      if item.FSubItems.count>0 then
        begin
          For j:=0 to item.FSubItems.count-1 do
            begin
              Sub:=TChmSiteMapSubItem(item.fsubitems[j]);
              if Sub.Name <> ''     then WriteParam('Name', Sub.Name);
              if Sub.ItemType <> '' then WriteParam('Type', Sub.ItemType);
              if Sub.Local <> ''    then WriteParam('Local', Sub.Local);
              if Sub.URL <> ''      then WriteParam('URL', Sub.URL);
              if Sub.SeeAlso <> ''  then WriteParam('See Also', Sub.SeeAlso);
            end;
        end;
      if Item.FrameName <> '' then WriteParam('FrameName', Item.FrameName);
      if Item.WindowName <> '' then WriteParam('WindowName', Item.WindowName);
      if Item.Comment <> '' then WriteParam('Comment', Item.Comment);
      if (SiteMapType = stTOC) and (Item.IncreaseImageIndex) then
          WriteParam('New', 'yes'); // is this a correct value?
      if (SiteMapType = stTOC) and (Item.ImageNumber <> -1) then
          WriteParam('ImageNumber', IntToStr(Item.ImageNumber));
      Dec(Indent, 3);
      {$ifdef preferlower}
      WriteString('</object>');
      {$else}
      WriteString('</OBJECT>');
      {$endif}
      Dec(Indent, 5);

      // Now Sub Entries
      if Item.Children.Count > 0 then begin
        {$ifdef preferlower}
        WriteString('<ul>');
        {$else}
        WriteString('<UL> ');
        {$endif}
        Inc(Indent, 8);
        WriteEntries(Item.Children);
        Dec(Indent, 8);
        {$ifdef preferlower}
        WriteString('</ul>');
        {$else}
        WriteString('</UL>'); //writestringnoident
        {$endif}

      end;
    end;
  end;
begin
  Indent := 0;
  WriteString('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">');
  WriteString('<HTML>');
  WriteString('<HEAD>');
  WriteString('<meta name="GENERATOR" content="Microsoft&reg; HTML Help Workshop 4.1">');  // Should we change this?
  WriteString('<!-- Sitemap 1.0 -->');
  WriteString('</HEAD><BODY>');

  // Site Properties
  WriteString('<OBJECT type="text/site properties">');
  Inc(Indent, 8);
    if SiteMapType = stTOC then begin
      if FrameName <> '' then WriteParam('FrameName', FrameName);
      if WindowName <> '' then WriteParam('WindowName', WindowName);
      if ImageList <> '' then WriteParam('ImageList', ImageList);
      if ImageWidth > 0 then WriteParam('Image Width', IntToStr(ImageWidth));
      if BackgroundColor <> 0 then WriteParam('Background', '0x'+hexStr(BackgroundColor, 8));
      if ForegroundColor <> 0 then WriteParam('Foreground', '0x'+hexStr(ForegroundColor, 8));
      if ExWindowStyles <> 0 then WriteParam('ExWindow Styles', '0x'+hexStr(ExWindowStyles, 8));
      if WindowStyles <> 0 then WriteParam('Window Styles', '0x'+hexStr(WindowStyles, 8));
      if UseFolderImages then WriteParam('ImageType', 'Folder');
    end;
    // both TOC and Index have font
    if Font <> '' then
      WriteParam('Font', Font);
  Dec(Indent, 8);
  WriteString('</OBJECT>');
  
  // And now the items
  if Items.Count > 0 then begin
    WriteString('<UL>');
    Inc(Indent, 8);
    // WriteEntries
    WriteEntries(Items);
    Dec(Indent, 8);
    WriteString('</UL>');
  end;
  
  WriteString('</BODY></HTML>');
  
  AStream.Size := AStream.Position;
end;

{ TChmSiteMapItem }

procedure TChmSiteMapItem.SetChildren(const AValue: TChmSiteMapItems);
begin
  if FChildren = AValue then exit;
  FChildren := AValue;
end;

function TChmSiteMapItem.getlocal: string;
begin
  result:='';
  if FSubItems.count>0 then
     result:=TChmSiteMapSubItem(FSubItems[0]).local;
end;

function TChmSiteMapItem.getseealso: string;
begin
  result:='';
  if FSubItems.count>0 then
    result:=TChmSiteMapSubItem(FSubItems[FSubItems.count-1]).SeeAlso;
end;

function TChmSiteMapItem.getsubitem( index : integer): TChmSiteMapSubItem;
begin
  result:=nil;
  if index<FSubItems.count then
    result:=TChmSiteMapSubItem(FSubItems[index]);
end;

function TChmSiteMapItem.getsubitemcount: integer;
begin
   result:=FSubItems.count;
end;

constructor TChmSiteMapItem.Create(AOwner: TChmSiteMapItems);
begin
  Inherited Create;
  FOwner := AOwner;
  FChildren := TChmSiteMapItems.Create(Owner.Owner, Self);
  FSubItems := TObjectList.Create(true);
  imagenumber:=-1;
end;

destructor TChmSiteMapItem.Destroy;
begin
  fsubitems.Free;
  FChildren.Free;
  Inherited Destroy;
end;

procedure TChmSiteMapItem.AddName(const Name: string);
var sub :TChmSiteMapSubItem;
begin
  if fname='' then
    fname:=name
  else
    begin
      sub :=TChmSiteMapSubItem.create(self);
      FSubItems.add(sub);
      sub.Name:=Name;
    end;
end;

procedure TChmSiteMapItem.AddLocal(const Local: string);
var sub :TChmSiteMapSubItem;
    addnew : boolean;
begin
   if fsubitems.count>0 then
      begin
        sub:=TChmSiteMapSubItem(fsubitems[FSubItems.count-1]);
        if sub.FLocal<>'' then
          begin
            sub.flocal:=local;
            exit;
          end;
      end;
   sub :=TChmSiteMapSubItem.create(self);
   FSubItems.add(sub);
//   sub.name:=name;
   sub.Local:=Local;
end;

procedure TChmSiteMapItem.AddSeeAlso(const SeeAlso: string);
// see also is mutually exclusive with "local url", so addition procedure is same as "local"
var sub :TChmSiteMapSubItem;
begin
   if fsubitems.count>0 then
      begin
        sub:=TChmSiteMapSubItem(fsubitems[FSubItems.count-1]);
        if sub.FSeeAlso<>'' then
          begin
            sub.FSeeAlso:=SeeAlso;
            exit;
          end;
      end;
   sub :=TChmSiteMapSubItem.create(self);
   FSubItems.add(sub);
   sub.FSeeAlso:=SeeAlso;
end;


procedure TChmSiteMapItem.AddURL(const URL: string);
var sub :TChmSiteMapSubItem;
begin
   if fsubitems.count>0 then
      begin
        sub:=TChmSiteMapSubItem(fsubitems[FSubItems.count-1]);
        if sub.FURL<>'' then
          begin
            sub.fURL:=URL;
            exit;
          end;
      end
   { else not possible according to chmspec. An URL must always follow a "local" item}
end;

procedure TChmSiteMapItem.AddType(const AType: string);
// in Tocs, Type can be the first is the same as local
var sub :TChmSiteMapSubItem;
begin
   if fsubitems.count>0 then
      begin
        sub:=TChmSiteMapSubItem(fsubitems[FSubItems.count-1]);
        if sub.ItemType<>'' then
          begin
            sub.ItemType:=AType;
            exit;
          end;
      end;
   sub :=TChmSiteMapSubItem.create(self);
   FSubItems.add(sub);
   sub.ItemType:=AType;
end;

procedure TChmSiteMapItem.Sort(Compare: TListSortCompare);
begin
  FChildren.sort(compare);
end;

{ TChmSiteMapItems }

function TChmSiteMapItems.GetItem(AIndex: Integer): TChmSiteMapItem;
begin
  Result := TChmSiteMapItem(FList.Items[AIndex]);
end;

function TChmSiteMapItems.getparentname: String;
begin
  result:='Not assigned';
  if assigned(fparentitem) then
    begin
      result:=FParentItem.name;
    end;
end;

function TChmSiteMapItems.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TChmSiteMapItems.SetItem(AIndex: Integer; const AValue: TChmSiteMapItem);
begin
  FList.Items[AIndex] := AValue;
end;

constructor TChmSiteMapItems.Create(AOwner: TChmSiteMap; AParentItem: TChmSiteMapItem);
begin
  FList := TList.Create;
  FParentItem := AParentItem;
  FOwner := AOwner;
  FInternalData := maxLongint;
end;

destructor TChmSiteMapItems.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TChmSiteMapItems.Delete(AIndex: Integer);
begin
  Item[AIndex].Free;
  FList.Delete(AIndex);
end;

function TChmSiteMapItems.Add(AItem: TChmSiteMapItem): Integer;
begin
  Result := FList.Add(AItem);
end;

function TChmSiteMapItems.NewItem: TChmSiteMapItem;
begin
  Result := TChmSiteMapItem.Create(Self);
  Add(Result);
end;

function TChmSiteMapItems.Insert(AItem: TChmSiteMapItem; AIndex: Integer): Integer;
begin
  Result := AIndex;
  FList.Insert(AIndex, AItem);
end;

procedure TChmSiteMapItems.Clear;
var
  I: LongInt;
begin
  for I := Count-1 downto 0 do Delete(I);
end;

procedure TChmSiteMapItems.Sort(Compare: TListSortCompare);
var I :Integer;
begin
  FList.Sort(Compare);
  for i:=0 to flist.Count-1 do
    TChmSiteMapItem(flist[i]).sort(Compare)
end;

end.

