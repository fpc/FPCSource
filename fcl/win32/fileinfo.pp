{
    $Id$
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
    FFileName : String;
    FmyVersionStrings : TStringList;
    FmyVersionCategories    : TStringList;

    procedure SetFileName (inp : string);
    procedure readVersionFromFile;
  protected
  public
     constructor Create(AOwner: TComponent);  override;
     destructor Destroy; override;
     function getVersionSetting(inp : string): String;
  published
    property fileName : string  read FFileName write SetFileName;
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
procedure TFileVersionInfo.SetFileName (inp : string);
begin
    FmyVersionStrings.clear;
    FmyVersionCategories.clear;

    if fileexists(inp) then
    begin
         FFileName := inp;
         readVersionFromFile;
    end
    else
    begin
         FFileName := '';
    end;
end;

{ read info from file }
procedure TFileVersionInfo.readVersionFromFile;
var struSize : Dword;
    dwBytes,someDummy : Dword;
    a,txt : array[0..256] of char;
    p : pchar;
    i : integer;
    pp : pointer;
    theFixedInfo : TVSFixedFileInfo;
    theTrans : TTranslation;
    s : string;
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
 
    strPCopy(a,FFileName);
    { get size of data }
    struSize := GetFileVersionInfoSize(a,@someDummy);
    if struSize=0 then exit;
    p := NIL;
    try
      { get memory }
      GetMem(p,struSize+10);
      { get data }
      if not GetFileVersionInfo(a,0,struSize,p) then exit;
      { get root info }
      if not VerQueryValue(p,'\',pp,dwBytes) then exit;
      move(pp^,theFixedInfo,dwBytes);
 
      { get translation info }
      if not VerQueryValue(p,'\VarFileInfo\Translation',pp,dwBytes) then
        exit;
      move(pp^,theTrans,dwBytes);
 
      { iterate over defined items }
      for i:=0 to ts.count-1 do
      begin
        s := '\StringFileInfo\'+inttohex(theTrans.langID,4)+inttohex(theTrans.charset,4)+'\'+ts[i];
        StrPCopy(a,s);
        if not VerQueryValue(p,a,pp,dwBytes) then Continue;
        if dwBytes>0 then
        begin
         move(pp^,txt,dwBytes);
         FmyVersionCategories.add(ts[i]);
         FmyVersionStrings.add(StrPas(txt));
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
{
  $Log$
  Revision 1.5  2003-07-26 16:23:05  michael
   corrected by Michalis:
    * VerQueryValue parameters (last parameter should be dwBytes instead of
      @dwBytes; you can call
        VerQueryValue(...,pp,dwBytes) or
        VerQueryValue(...,@pp,@dwBytes) but NOT
        VerQueryValue(...,pp,@dwBytes) (and that was the case) )
    * corrected
        if not VerQueryValue(p,a,pp,dwBytes) then Exit;
      to
        if not VerQueryValue(p,a,pp,dwBytes) then Continue;
      (when some info is missing the code should skip to the next info,
      not exit)
    + added destructor to Free FmyVersionStrings and FmyVersionCategories objects
      to avoid memory leaks
    + added ts.Free (and embedded some code in try..finally..end clause)
      to avoid memory leaks
    * inherited Create should be called at the beginning of constructor
      (it's just a good coding practice)
    * getVersionSetting re-written, optimised a little (LowerCase(inp) only once;
      this function is not supposed to be really "optimised" but this little
      improvement was so simple...) (note: when TStringList.CaseSensitive will
      be implemented in FPC, this function can be implemented even simpler,
      just by calling FmyVersionCategories.IndexOf)

  Revision 1.4  2002/09/07 15:15:29  peter
    * old logs removed and tabs fixed

}
