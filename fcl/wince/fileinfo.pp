{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{

 Based on getver by Bernd Juergens - Munich, Germany
 email :bernd@promedico.com

 Usage : Drop component on form. Set desired file name using
         FileVersionInfo.filename := 'c:\winnt\system32\comctl32.dll'
         or something like that.
         Read StringLists VersionStrings and VersionCategories.

         or check a single entry:
         FileVersionInfo1.fileName := 'd:\winnt\system32\comctl32.dll';
         showMessage(FileVersionInfo1.getVersionSetting('ProductVersion'));
}
unit fileinfo;
{$mode objfpc}
interface

uses
  Windows, SysUtils, Classes;


{ Record to receive charset }
type TTranslation = record
   langID  : WORD;
   charset  : WORD;
end;

type
  TFileVersionInfo = class(TComponent)
  private
    FFileName : WideString;
    FmyVersionStrings : TStringList;
    FmyVersionCategories    : TStringList;

    procedure SetFileName (const cwsFile : Widestring);
    procedure readVersionFromFile;
  protected
  public
     constructor Create(AOwner: TComponent);  override;
     destructor Destroy; override;
     function getVersionSetting(inp : string): String;
  published
    property fileName : widestring  read FFileName write SetFileName;
    property VersionStrings  : TStringList  read FmyVersionStrings;
    property VersionCategories : TStringList read FmyVersionCategories;
  end;

implementation


{ initialize everything }
constructor TFileVersionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FmyVersionStrings := TStringList.Create;
  FmyVersionCategories  := TStringList.Create;
  FFileName := '';
end;

destructor TFileVersionInfo.Destroy;
begin
  FmyVersionCategories.Free;
  FmyVersionStrings.Free;
  inherited;
end;

{ Get filename, check if file exists and read info from file }
procedure TFileVersionInfo.SetFileName (const cwsFile : Widestring);
begin
    FmyVersionStrings.clear;
    FmyVersionCategories.clear;

    if fileexists(cwsFile) then
    begin
         FFileName := cwsFile;
         readVersionFromFile;
    end
    else
    begin
         FFileName := '';
    end;
end;

{ read info from file }
procedure TFileVersionInfo.readVersionFromFile;
var dwHandle, dwSize : Longword;
    p : pwidechar;
    i : integer;
    pp : pointer;
    theFixedInfo : TVSFixedFileInfo;
    theTrans : TTranslation;
    s : widestring;
    ts  : TStringList;
begin
  ts := TStringList.Create;
  try
    ts.add('CompanyName');
    ts.add('FileDescription');
    ts.add('FileVersion');
    ts.add('InternalName');
    ts.add('LegalCopyright');
    ts.add('OriginalFilename');
    ts.add('ProductName');
    ts.add('ProductVersion');

    { get size of data }
    dwSize := GetFileVersionInfoSize(PWidechar(FFilename),@dwHandle);
    if dwSize=0 then exit;
    p := NIL;
    try
      { get memory }
      GetMem(p,dwSize+10);
      { get data }
      if not GetFileVersionInfo(PWidechar(FFilename),0,dwSize,p) then exit;
      { get root info }
      if not VerQueryValue(p,'\',pp,PUINT(dwSize)) then exit;
      move(pp^,theFixedInfo,dwSize);

      { get translation info }
      if not VerQueryValue(p,'\VarFileInfo\Translation',pp,PUINT(dwSize)) then
        exit;
      move(pp^,theTrans,dwSize);

      { iterate over defined items }
      for i:=0 to ts.count-1 do
      begin
        s := WideFormat('\StringFileInfo\%4x%4x\%s',[theTrans.langID,theTrans.charset,ts[i]]);
        if not VerQueryValue(p,PWideChar(s),pp,PUINT(dwSize)) then Continue;
        if dwSize>0 then
        begin
         SetLength(s,dwSize);
         move(pp^,s,dwSize);
         FmyVersionCategories.add(ts[i]);
         FmyVersionStrings.add(s);
        end
      end;
    finally
      { release memory }
      FreeMem(p);
    end;
  finally ts.Free end;
end;

{ get single version string }
function TFileVersionInfo.getVersionSetting(inp : string): String;
var i : integer;
begin
  inp:=LowerCase(inp);
  for i:= 0 to FmyVersionCategories.Count -1 do
    if LowerCase(FmyVersionCategories[i])=inp then
    begin
      result := FmyVersionStrings[i];
      Exit;
    end;
  result := '';
end;

end.
