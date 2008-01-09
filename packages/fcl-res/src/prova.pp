{off $DEFINE PARSEDFM}
{off $DEFINE PARSESTRINGTABLE}
{off $DEFINE DUMPBITMAP}
{off $DEFINE DUMPICON}
{off $DEFINE DUMPCURSOR}
{off $DEFINE PRINTVERSION}
{off $DEFINE PRINTACCELERATORS}

(*
da fare:
   
 - ordinare le robe registrabili (reader, writer, resource types) in modo da
   velocizzare la ricerca.
   
 - tipi di resource da fare:
   menu
   dialog
   messagetable
   
*)

program prova;

{$mode objfpc}{$H+}

{$APPTYPE CONSOLE}

uses
  Classes, SysUtils,
  resource, resfactory,
  stringtableresource, bitmapresource, groupiconresource, groupcursorresource,
  versionresource, versiontypes, acceleratorsresource,
  resreader, reswriter, coffwriter, coffreader, elfreader, elfwriter, dfmreader,
  winpeimagereader, externalreader, externalwriter, machoreader, machowriter;
  



function MemFlagsToStr(mf : word) : string;
begin
  Result:='';
  if (mf and MF_MOVEABLE) = MF_MOVEABLE then Result:=Result+' | MOVEABLE';
  if (mf and MF_PURE) = MF_PURE then Result:=Result+' | PURE';
  if (mf and MF_PRELOAD) = MF_PRELOAD then Result:=Result+' | PRELOAD';
  if (mf and MF_DISCARDABLE) = MF_DISCARDABLE then Result:=Result+' | DISCARDABLE';
  Result:=copy(Result,4,Length(Result));
end;

function ResTypeToStr(aType : TResourceDesc) : string;
begin
  if aType.DescType=dtName then Result:=''''+aType.Name+''''
  else
    case aType.ID of
      RT_CURSOR       : Result:='CURSOR';
      RT_BITMAP       : Result:='BITMAP';
      RT_ICON         : Result:='ICON';
      RT_MENU         : Result:='MENU';
      RT_DIALOG       : Result:='DIALOG';
      RT_STRING       : Result:='STRING';
      RT_FONTDIR      : Result:='FONTDIR';
      RT_FONT         : Result:='FONT';
      RT_ACCELERATOR  : Result:='ACCELERATOR';
      RT_RCDATA       : Result:='RCDATA';
      RT_MESSAGETABLE : Result:='MESSAGETABLE';
      RT_GROUP_CURSOR : Result:='GROUP_CURSOR';
      RT_GROUP_ICON   : Result:='GROUP_ICON';
      RT_VERSION      : Result:='VERSION';
      RT_DLGINCLUDE   : Result:='DLGINCLUDE';
      RT_PLUGPLAY     : Result:='PLUGPLAY';
      RT_VXD          : Result:='VXD';
      RT_ANICURSOR    : Result:='ANICURSOR';
      RT_ANIICON      : Result:='ANIICON';
      RT_HTML         : Result:='HTML';
      RT_MANIFEST     : Result:='MANIFEST'
      else Result:=aType.Name;
    end;
end;

procedure PrintRow(aStream : TStream; toread : integer);
var i,j : integer;
    b : array[0..3] of byte;
    s : string;
    ascii : string;
begin
  s:=' ';
  ascii:='';
  while toread>0 do
  begin
    i:=4;
    if toread<4 then i:=toread;
    aStream.ReadBuffer(b[0],i);
    dec(toread,i);
    for j:=0 to i-1 do
    begin
      s:=s+IntToHex(b[j],2)+' ';
      if b[j]>=$20 then ascii:=ascii+chr(b[j])
      else ascii:=ascii+'.';
    end;
    s:=s+'  ';
  end;
  if length(s)<57 then
  begin
    i:=57-length(s);
    for j:=1 to i do
      s:=s+' ';
  end;
  writeln(s+'   '+ascii);
end;

procedure PrintDFM(aStream : TStream);
var outstr : TMemoryStream;
    sl : TStringList;
    i : integer;
begin
  outstr:=TMemoryStream.Create;
  try
    sl:=TStringList.Create;
    try
      ObjectBinaryToText(aStream,outstr);
      outstr.Position:=0;
      sl.LoadFromStream(outstr);
      for i:=0 to sl.Count-1 do
        writeln(sl[i]);
    finally
      sl.Free;
    end;
  finally
    outstr.Free;
  end;
end;

procedure PrintStringTable(aRes : TAbstractResource);
var strtab : TStringTableResource;
    id : word;
begin
  strtab:=TStringTableResource(aRes);
  for id:=strtab.FirstID to strtab.LastID do
    if strtab.Strings[id]<>'' then
      writeln(Format('%.5d = %s',[id,strtab.Strings[id]]));
end;

procedure DumpBitmap(aRes : TBitmapResource);
var fs : TFileStream;
begin
  fs:=TFileStream.Create(aRes.Name.Name+'.bmp',fmCreate);
  try
    aRes.BitmapData.Position:=0;
    fs.CopyFrom(aRes.BitmapData,aRes.BitmapData.Size);
  finally
    fs.Free;
  end;
end;

{$IFDEF DUMPICON}
procedure DumpIcon(aRes : TGroupIconResource);
var fs : TFileStream;
begin
  fs:=TFileStream.Create(aRes.Name.Name+'.ico',fmCreate);
  try
    aRes.ItemData.Position:=0;
    fs.CopyFrom(aRes.ItemData,aRes.ItemData.Size);
  finally
    fs.Free;
  end;
end;
{$ENDIF}

{$IFDEF DUMPCURSOR}
procedure DumpCursor(aRes : TGroupCursorResource);
var fs : TFileStream;
begin
  fs:=TFileStream.Create(aRes.Name.Name+'.cur',fmCreate);
  try
    aRes.ItemData.Position:=0;
    fs.CopyFrom(aRes.ItemData,aRes.ItemData.Size);
  finally
    fs.Free;
  end;
end;
{$ENDIF}

procedure DumpStream(aRes : TAbstractResource);
var fs : TFileStream;
begin
  fs:=TFileStream.Create(aRes.Name.Name,fmCreate);
  try
    aRes.RawData.Position:=0;
    fs.CopyFrom(aRes.RawData,aRes.RawData.Size);
  finally
    fs.Free;
  end;
end;

function ProductVersion2Str(aData : TFileProductVersion) : string;
begin
  Result:=Format('%d,%d,%d,%d',[aData[0],aData[1],aData[2],aData[3]]);
end;

{$IFDEF PRINTVERSION}
procedure PrintVersion(aRes : TVersionResource);
var i,j : integer;
begin
  writeln(Format('File Version: %s',[ProductVersion2Str(aRes.FixedInfo.FileVersion)]));
  writeln(Format('Product Version: %s',[ProductVersion2Str(aRes.FixedInfo.ProductVersion)]));
  writeln(Format('File Flags Mask: %.8x',[aRes.FixedInfo.FileFlagsMask]));
  writeln(Format('File Flags: %.8x',[aRes.FixedInfo.FileFlags]));
  writeln(Format('File OS: %.8x',[aRes.FixedInfo.FileOS]));
  writeln(Format('File Type: %.8x',[aRes.FixedInfo.FileType]));
  writeln(Format('File SubType: %.8x',[aRes.FixedInfo.FileSubType]));
  writeln(Format('File Date: %.16x',[aRes.FixedInfo.FileDate]));
  for i:=0 to aRes.StringFileInfo.Count-1 do
  begin
    writeln('  StringTable ',aRes.StringFileInfo[i].Name);
    for j:=0 to aRes.StringFileInfo[i].Count-1 do
      writeln('    ',aRes.StringFileInfo[i].Keys[j],' = ',aRes.StringFileInfo[i].ValuesByIndex[j]);
  end;
  writeln('  VarFileInfo:');
  for i:=0 to aRes.VarFileInfo.Count-1 do
    writeln(Format('    Translation: language = %d, codepage = %d',[aRes.VarFileInfo[i].language,aRes.VarFileInfo[i].codepage]));
end;
{$ENDIF}


{$IFDEF PRINTACCELERATORS}
procedure PrintAccelerators(aRes : TAcceleratorsResource);
var i : integer;
    acc : TAccelerator;
begin
  for i:=0 to aRes.Count-1 do
  begin
    acc:=aRes[i];
    writeln(Format('Flags = %.4x, Ansi = %.4x, ID = %.4x',[acc.Flags,acc.Ansi,acc.ID]));
  end;
end;
{$ENDIF}

procedure PrintData(aStream : TStream);
var toread : integer;
    sig : array[0..3] of char;
begin
  {$IFDEF PARSEDFM}
  aStream.Position:=0;
  aStream.ReadBuffer(sig,4);
  aStream.Position:=0;
  if sig='TPF0' then PrintDFM(aStream)
  else
  {$ENDIF}
  while aStream.Position<aStream.Size do
  begin
    toread:=16;
    if (aStream.Size-aStream.Position) < toread then
      toread:=aStream.Size-aStream.Position;
    PrintRow(aStream,toread);
  end;
end;

procedure PrintResInfo(aRes : TAbstractResource);
begin
  writeln(Format('Resource Type = %s Name = %s',[ResTypeToStr(aRes._Type),aRes.Name.Name]));
  writeln('  DataSize        = ',ares.DataSize);
  writeln('  HeaderSize      = ',ares.HeaderSize);
  writeln('  DataVersion     = ',ares.DataVersion);
  writeln('  MemoryFlags     = ',MemFlagsToStr(ares.MemoryFlags));
  writeln('  LanguageID      = ',ares.LangID);
  writeln('  Version         = ',ares.Version);
  writeln('  Characteristics = ',ares.Characteristics);
  writeln('  DataOffset      = ',IntToHex(ares.DataOffset,8));
  writeln;
  writeln('  Data: ');
  {$IFDEF PARSESTRINGTABLE}
  if aRes is TStringTableResource then
    PrintStringTable(aRes as TStringTableResource)
  else
  {$ENDIF}
  {$IFDEF DUMPBITMAP}
  if aRes is TBitmapResource then
    DumpBitmap(aRes as TBitmapResource)
  else
  {$ENDIF}
  {$IFDEF DUMPICON}
  if aRes is TGroupIconResource then
    DumpIcon(aRes as TGroupIconResource)
  else
  {$ENDIF}
  {$IFDEF DUMPCURSOR}
  if aRes is TGroupCursorResource then
    DumpCursor(aRes as TGroupCursorResource)
  else
  {$ENDIF}
  {$IFDEF PRINTVERSION}
  if aRes is TVersionResource then
    PrintVersion(aRes as TVersionResource)
  else
  {$ENDIF}
  {$IFDEF PRINTACCELERATORS}
  if aRes is TAcceleratorsResource then
    PrintAccelerators(aRes as TAcceleratorsResource)
  else
  {$ENDIF}
  {$DEFINE DISABILITATO}
  {$IFDEF DISABILITATO}
  if aRes._Type.DescType=dtID then
    case aRes._Type.ID of
      RT_HTML,RT_MANIFEST : DumpStream(aRes);
//      RT_ACCELERATOR : PrintData(aRes.RawData);
    end;
  {$ENDIF}
  PrintData(aRes.RawData);
end;

var res : TResources;
    i : integer;

begin
  if not fileexists(paramstr(1)) then
  begin
    writeln('No filename specified.');
    halt(1);
  end;
  res:=TResources.Create;
  try
    res.LoadFromFile(paramstr(1));
    if paramcount>=2 then
      res.WriteToFile(paramstr(2))
    else
      for i:=0 to res.Count-1 do
        PrintResInfo(res[i]);
  finally
    res.Free;
  end;
end.

