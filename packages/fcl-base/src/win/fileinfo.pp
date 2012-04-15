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
 
 reworked by Stoian Ivanov (sdr@mail.bg)
     Added ExtraVersionStrings,TryHardcodedTrans.
     Using VersionCategories.Objects as TransID storage,
     which is used later by getVersionSetting


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
{$h+}
interface

uses
  Windows, SysUtils, Classes;


{ Record to receive charset }
type TTranslation = record
  case byte of 
   1: (langID,charset  : WORD);
   2: (transid:Dword);
end;

PTranslation=^TTranslation;

type
  { TFileVersionInfo }

  TFileVersionInfo = class(TComponent)
  private
    FFileName : String;
    FmyVersionStrings : TStringList;
    FmyVersionCategories : TStringList;

    FmyExtraVersionStrings :TStringList;
    FmyTryHardcodedTrans :TStringList;
  
    procedure SetFileName (Const inp : string);
    procedure readVersionFromFile;
  public
    constructor Create(AOwner: TComponent);  override;
    destructor Destroy; override;
    procedure AddExtraVersionString (Const extrafield:string);
    procedure AddTryHardcodedTrans (Const hardtrans:string);
    function getVersionSetting(Const inp : string; transid:dword=0): String;
  published
    property fileName : string  read FFileName write SetFileName;
    property VersionStrings  : TStringList  read FmyVersionStrings;
    property VersionCategories : TStringList read FmyVersionCategories;
    property ExtraVersionStrings : TStringList read FmyExtraVersionStrings;
    property TryHardcodedTrans :TStringList read FmyTryHardcodedTrans;
  end;

implementation


{ initialize everything }
constructor TFileVersionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FmyVersionStrings := TStringList.Create;
  FmyVersionStrings.Duplicates:=dupAccept;
  FmyVersionCategories  := TStringList.Create;
  FmyVersionCategories.Duplicates:=dupAccept;
  FmyExtraVersionStrings := TStringList.Create;
  FmyExtraVersionStrings.Duplicates:= dupIgnore;
  FmyTryHardcodedTrans:=TStringList.Create;
  FmyTryHardcodedTrans.Duplicates:=dupIgnore;
  FFileName := '';
end;

destructor TFileVersionInfo.Destroy;
begin
  FmyVersionCategories.Free;
  FmyVersionStrings.Free;
  FmyExtraVersionStrings.Free;
  FmyTryHardcodedTrans.free;
  inherited;
end;

{if you need other version strings extracted you add them here}
procedure TFileVersionInfo.AddExtraVersionString(Const extrafield: string); 
begin
  FmyExtraVersionStrings.add (extrafield);
end;

{some broken DLLs report wrong translations and you need to try some hardcoded transes
like NPSWF32.dll reports 040904b0(English/Unicode) but they are actualy 040904E4 (English/Latin1)}
procedure TFileVersionInfo.AddTryHardcodedTrans(Const hardtrans: string);
begin
  FmyTryHardcodedTrans.add(hardtrans);
end;

{ Get filename, check if file exists and read info from file }
procedure TFileVersionInfo.SetFileName (Const inp : string);
begin
  FmyVersionStrings.clear;
  FmyVersionCategories.clear;
  if FileExists(inp) then
    begin
    FFileName := inp;
    readVersionFromFile;
    end
  else
    FFileName := '';
end;

{ read info from file }
procedure TFileVersionInfo.readVersionFromFile;
var
  struSize : Dword;
  dwBytes : Dword;
  dwTransBytes:Dword;
  vi : pointer;
  ti,i,hti : integer;
  pp : pointer;
  theTrans : PTranslation;
  s : string;
  ts  : TStringList;
  transstr:String;
  //s_w : Widestring; //urf or not really does not matter
begin
  ts := TStringList.Create;
  try
    ts.Assign(FmyExtraVersionStrings);
    ts.add('Comments');
    ts.add('CompanyName');
    ts.add('FileDescription');
    ts.add('FileVersion');
    ts.add('InternalName');
    ts.add('LegalCopyright');
    ts.add('OriginalFilename');
    ts.add('ProductName');
    ts.add('ProductVersion');
    struSize := GetFileVersionInfoSize(Pchar(FFileName),nil);
    if struSize=0 then exit;
    vi := NIL;
    { get memory }
    GetMem(vi,struSize+10);
    try
      if (vi=nil) then
        exit;
      { get data }
      if not GetFileVersionInfo(PChar(FFileName),0,struSize,vi) then
        exit;
      { get translation info }
      if not VerQueryValue(vi,'\VarFileInfo\Translation',theTrans,dwTransBytes) then 
        exit;
      while (dwTransBytes>=sizeof(TTranslation)) do 
        begin
        transstr:=inttohex(theTrans^.langID,4)+inttohex(theTrans^.charset,4);
        { iterate over defined items }
        for i:=0 to ts.count-1 do 
          begin
          s:='\StringFileInfo\'+transstr+'\'+ts[i]+#0;
          if not VerQueryValue(vi,@s[1],pp,dwBytes) then continue;
          if dwBytes>0 then 
            begin
            SetLength(s,dwBytes-1);
            move(pp^,s[1],dwBytes-1);
            FmyVersionCategories.AddObject(LowerCase(ts[i]),TObject(pointer(theTrans^.transid)));
            FmyVersionStrings.add(s);
            end
          end;
        inc (theTrans);
        dec (dwTransBytes,sizeof(TTranslation));
        end;
        
      {Now with the dirty hardcoded hack}
      for hti:=0 to FmyTryHardcodedTrans.Count-1 do 
        begin
        transstr:=FmyTryHardcodedTrans[hti];
        for i:=0 to ts.count-1 do 
          begin
          s := '\StringFileInfo\'+transstr+'\'+ts[i]+#0;
          if VerQueryValue(vi,@s[1],pp,dwBytes) and (dwbytes>0) then
            begin
            SetLength(s,dwBytes-1);
            move(pp^,s[1],dwBytes-1);
            FmyVersionCategories.AddObject(LowerCase(ts[i]),TObject(pointer(theTrans^.transid)));
            FmyVersionStrings.add(s);
            end
          end;
        end;
    finally
      FreeMem(vi);
    end;  
  Finally  
    ts.Free
  end;  
end;

{ get single version string }
function TFileVersionInfo.getVersionSetting(Const inp : string; transid:dword=0): String;
var
  i : integer;
  s,v : string;
begin
  s:=LowerCase(inp);
  I:=0;
  Result:='';
  While (Result='') and (i<FmyVersionCategories.Count) do
    begin
    V:=FmyVersionCategories[i];
    if (V=S) and ((transid=0) or (transid=dword(pointer(FmyVersionCategories.Objects[i])))) then
      Result:=FmyVersionStrings[i];
    inc(I);
    end;
end;

end.
