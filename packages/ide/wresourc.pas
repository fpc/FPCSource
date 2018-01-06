{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Resource File support objects and routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WResourc;

interface

uses Objects;

const
      TPDataBlockSignature   = ord('F')+ord('B')*256;
      ResourceBlockSignature = ord('R')+ord('D')*256;

      langDefault       = 0;

      rcBinary          = 1;

type
     TResourceEntryHeader = packed record
       ID     : longint;
       LangID : longint;
       Flags  : longint;
       DataOfs: longint;
       DataLen: sw_word;
     end;

     TResourceHeader = packed record
       _Class     : longint;
       Flags      : longint;
       NameLen    : word;
       EntryCount : word;
     end;

     TResourceFileHeader = packed record
       Signature  : word;
       InfoType   : word;
       InfoSize   : longint;
     { ---- }
       TableOfs   : longint;
     end;

     PResourceEntry = ^TResourceEntry;
     TResourceEntry = object(TObject)
       constructor Init(AID, ALangID, AFlags, ADataLen: longint);
     private
       ID      : longint;
       LangID  : longint;
       Flags   : longint;
       DataOfs : longint;
       DataLen : sw_word;
       procedure   BuildHeader(var Header : TResourceEntryHeader);
     end;

     PResourceEntryCollection = ^TResourceEntryCollection;
     TResourceEntryCollection = object(TSortedCollection)
       function  At(Index: Sw_Integer): PResourceEntry;
       function  Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
       function  SearchEntryForLang(ALangID: longint): PResourceEntry;
     end;

     PGlobalResourceEntryCollection = ^TGlobalResourceEntryCollection;
     TGlobalResourceEntryCollection = object(TSortedCollection)
       function  At(Index: Sw_Integer): PResourceEntry;
       function  Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
     end;

     PResource = ^TResource;
     TResource = object(TObject)
       constructor Init(const AName: string; AClass, AFlags: longint);
       function    GetName: string; virtual;
       function    FirstThatEntry(Func: pointer): PResourceEntry; virtual;
       procedure   ForEachEntry(Func: pointer); virtual;
       destructor  Done; virtual;
     private
       Name   : PString;
       _Class : longint;
       Flags  : longint;
       Items  : PResourceEntryCollection;
       procedure   BuildHeader(var Header : TResourceHeader);
     end;

     TResourceCollection = object(TSortedCollection)
       function  At(Index: Sw_Integer): PResource;
       function  Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
       function  SearchResourceByName(const AName: string): PResource;
     end;
     PResourceCollection = ^TResourceCollection;

     TResourceFile = object(TObject)
       constructor Init(var RS: TStream; ALoad: boolean);
       constructor Create(var RS: TStream);
       constructor Load(var RS: TStream);
       constructor CreateFile(AFileName: string);
       constructor LoadFile(AFileName: string);
       function    FirstThatResource(Func: pointer): PResource; virtual;
       procedure   ForEachResource(Func: pointer); virtual;
       procedure   ForEachResourceEntry(Func: pointer); virtual;
       function    CreateResource(const Name: string; AClass, AFlags: longint): boolean; virtual;
       function    AddResourceEntry(const ResName: string; ALangID, AFlags: longint; var Data;
                   ADataSize: sw_integer): boolean; virtual;
       function    AddResourceEntryFromStream(const ResName: string; ALangID, AFlags: longint;
                   var Source: TStream; ADataSize: longint): boolean; virtual;
       function    DeleteResourceEntry(const ResName: string; ALangID: longint): boolean; virtual;
       function    DeleteResource(const ResName: string): boolean; virtual;
       function    ReadResourceEntry(const ResName: string; ALangID: longint; var Buf; BufSize: sw_word): boolean;
       function    ReadResourceEntryToStream(const ResName: string; ALangID: longint; var DestS: TStream): boolean;
       procedure   Flush; virtual;
       destructor  Done; virtual;
     public
       BaseOfs: longint;
       function    FindResource(const ResName: string): PResource;
       function    FindResourceEntry(const ResName: string; ALangID: longint): PResourceEntry;
     private
       S         : PStream;
       MyStream  : boolean;
       Resources : PResourceCollection;
       Entries   : PGlobalResourceEntryCollection;
       Header    : TResourceFileHeader;
       Modified  : boolean;
       procedure  UpdateBlockDatas;
       function   GetNextEntryID: longint;
       function   GetTotalSize(IncludeHeaders: boolean): longint;
       function   CalcSizes(IncludeHeaders, UpdatePosData: boolean): longint;
       procedure  AddResEntryPtr(P: PResource; E: PResourceEntry);
       procedure  RemoveResEntryPtr(P: PResource; E: PResourceEntry);
       function   DeleteArea(AreaStart, AreaSize, TotalSize: longint): boolean;
       procedure  BuildFileHeader;
       procedure  WriteHeader;
       procedure  WriteResourceTable;
     end;
     PResourceFile = ^TResourceFile;

implementation

uses
      WUtils;

function TResourceEntryCollection.At(Index: Sw_Integer): PResourceEntry;
begin
  At:=inherited At(Index);
end;

function TResourceEntryCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var K1: PResourceEntry absolute Key1;
    K2: PResourceEntry absolute Key2;
    Re: Sw_integer;
begin
  if K1^.LangID<K2^.LangID then Re:=-1 else
  if K1^.LangID>K2^.LangID then Re:= 1 else
  Re:=0;
  Compare:=Re;
end;

function TResourceEntryCollection.SearchEntryForLang(ALangID: longint): PResourceEntry;
var P: PResourceEntry;
    E: TResourceEntry;
    Index: sw_integer;
begin
  E.LangID:=ALangID;
  if Search(@E,Index)=false then P:=nil else
    P:=At(Index);
  SearchEntryForLang:=P;
end;

function TGlobalResourceEntryCollection.At(Index: Sw_Integer): PResourceEntry;
begin
  At:=inherited At(Index);
end;

function TGlobalResourceEntryCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var K1: PResourceEntry absolute Key1;
    K2: PResourceEntry absolute Key2;
    Re: Sw_integer;
begin
  if K1^.ID<K2^.ID then Re:=-1 else
  if K1^.ID>K2^.ID then Re:= 1 else
  Re:=0;
  Compare:=Re;
end;

constructor TResourceEntry.Init(AID, ALangID, AFlags, ADataLen: longint);
begin
  inherited Init;
  ID:=AID;
  LangID:=ALangID; Flags:=AFlags; DataLen:=ADataLen;
end;

procedure TResourceEntry.BuildHeader(var Header : TResourceEntryHeader);
begin
  FillChar(Header,SizeOf(Header),0);
  Header.ID:=ID;
  Header.LangID:=LangID;
  Header.Flags:=Flags;
  Header.DataLen:=DataLen;
  Header.DataOfs:=DataOfs;
end;

constructor TResource.Init(const AName: string; AClass, AFlags: longint);
begin
  inherited Init;
  Name:=NewStr(AName);
  _Class:=AClass;
  Flags:=AFlags;
  New(Items, Init(10,50));
end;

function TResource.GetName: string;
begin
  GetName:=GetStr(Name);
end;

function TResource.FirstThatEntry(Func: pointer): PResourceEntry;
var EP,P: PResourceEntry;
    I: sw_integer;
begin
  P:=nil;
  for I:=0 to Items^.Count-1 do
    begin
      EP:=Items^.At(I);
      if Byte(Longint(CallPointerMethodLocal(Func,
           get_caller_frame(get_frame,get_pc_addr),@Self,EP)))<>0 then
        begin
          P := EP;
          Break;
        end;
    end;
  FirstThatEntry:=P;
end;

procedure TResource.ForEachEntry(Func: pointer);
var RP: PResourceEntry;
    I: sw_integer;
begin
  for I:=0 to Items^.Count-1 do
    begin
      RP:=Items^.At(I);
      CallPointerMethodLocal(Func,
        get_caller_frame(get_frame,get_pc_addr),@Self,RP);
    end;
end;

procedure TResource.BuildHeader(var Header : TResourceHeader);
begin
  FillChar(Header,SizeOf(Header),0);
  Header._Class:=_Class;
  Header.Flags:=Flags;
  Header.NameLen:=length(GetName);
  Header.EntryCount:=Items^.Count;
end;

destructor TResource.Done;
begin
  inherited Done;
  if Name<>nil then DisposeStr(Name); Name:=nil;
  if Items<>nil then Dispose(Items, Done); Items:=nil;
end;

function TResourceCollection.At(Index: Sw_Integer): PResource;
begin
  At:=inherited At(Index);
end;

function TResourceCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var K1: PResource absolute Key1;
    K2: PResource absolute Key2;
    N1,N2: string;
    Re: Sw_integer;
begin
  N1:=UpcaseStr(K1^.GetName); N2:=UpcaseStr(K2^.GetName);
  if N1<N2 then Re:=-1 else
  if N1>N2 then Re:= 1 else
  Re:=0;
  Compare:=Re;
end;

function TResourceCollection.SearchResourceByName(const AName: string): PResource;
var P,R: PResource;
    Index: sw_integer;
begin
  New(R, Init(AName,0,0));
  if Search(R,Index)=false then P:=nil else
    P:=At(Index);
  Dispose(R, Done);
  SearchResourceByName:=P;
end;

constructor TResourceFile.Create(var RS: TStream);
begin
  if Init(RS,false)=false then
    Fail;
end;

constructor TResourceFile.Load(var RS: TStream);
begin
  if Init(RS,true)=false then
    Fail;
end;

constructor TResourceFile.Init(var RS: TStream; ALoad: boolean);
var OK: boolean;
    RH: TResourceHeader;
    REH: TResourceEntryHeader;
    EndPos,I: longint;
    P: PResource;
    E: PResourceEntry;
    St: string;
begin
  inherited Init;
  S:=@RS;
  New(Resources, Init(100, 1000));
  New(Entries, Init(500,2000));
  OK:=true;
  if ALoad=false then
    Modified:=true
  else
    begin
      S^.Reset;
      BaseOfs:=S^.GetPos;
      S^.Read(Header,SizeOf(Header));
      OK:=(S^.Status=stOK) and
          (Header.Signature=TPDataBlockSignature) and
          (Header.InfoType=ResourceBlockSignature);
      if OK then begin S^.Seek(BaseOfs+Header.TableOfs); OK:=S^.Status=stOK; end;
      EndPos:=BaseOfs+Header.InfoSize;
      if OK then
        while OK and (S^.GetPos<EndPos) do
          begin
            S^.Read(RH,SizeOf(RH)); OK:=(S^.Status=stOK);
            if OK then begin St[0]:=chr(RH.NameLen); S^.Read(St[1],RH.NameLen); OK:=(S^.Status=stOK); end;
            if OK then
              begin
                New(P, Init(St,RH._Class,RH.Flags));
                Resources^.Insert(P);
              end;
            I:=0;
            while OK and (I<RH.EntryCount) do
              begin
                S^.Read(REH,SizeOf(REH)); OK:=(S^.Status=stOK);
                if OK then
                  begin
                    New(E, Init(REH.ID,REH.LangID,REH.Flags,REH.DataLen));
                    AddResEntryPtr(P,E);
                  end;
                if OK then Inc(I);
              end;
            if OK then UpdateBlockDatas;
          end;
    end;
  if OK=false then
    begin
      Done;
      Fail;
    end;
end;

function TResourceFile.FirstThatResource(Func: pointer): PResource;
var RP,P: PResource;
    I: sw_integer;
begin
  P:=nil;
  for I:=0 to Resources^.Count-1 do
    begin
      RP:=Resources^.At(I);
      if Byte(Longint(CallPointerMethodLocal(Func,
           get_caller_frame(get_frame,get_pc_addr),@Self,RP)))<>0 then
        begin
          P := RP;
          Break;
        end;
    end;
  FirstThatResource:=P;
end;

procedure TResourceFile.ForEachResource(Func: pointer);
var RP: PResource;
    I: sw_integer;
begin
  for I:=0 to Resources^.Count-1 do
    begin
      RP:=Resources^.At(I);
      CallPointerMethodLocal(Func,get_caller_frame(get_frame,get_pc_addr),@Self,RP);
    end;
end;

procedure TResourceFile.ForEachResourceEntry(Func: pointer);
var E: PResourceEntry;
    I: sw_integer;
begin
  for I:=0 to Entries^.Count-1 do
    begin
      E:=Entries^.At(I);
      CallPointerMethodLocal(Func,get_caller_frame(get_frame,get_pc_addr),@Self,E);
    end;
end;

function TResourceFile.CreateResource(const Name: string; AClass, AFlags: longint): boolean;
var OK: boolean;
    P: PResource;
begin
  OK:=FindResource(Name)=nil;
  if OK then
    begin
      New(P, Init(Name,AClass,AFlags));
      Resources^.Insert(P);
      Modified:=true;
    end;
  CreateResource:=OK;
end;

function TResourceFile.AddResourceEntry(const ResName: string; ALangID, AFlags: longint; var Data;
           ADataSize: sw_integer): boolean;
const BlockSize = 4096;
var OK: boolean;
    P: PResource;
    E: PResourceEntry;
    RemSize,CurOfs,FragSize: longint;
begin
  P:=FindResource(ResName);
  OK:=P<>nil;
  if OK then
    OK:=(P^.Items^.SearchEntryForLang(ALangID)=nil);
  if OK then
    begin
      New(E, Init(GetNextEntryID,ALangID, AFlags, ADataSize));
      AddResEntryPtr(P,E);
      UpdateBlockDatas;
      RemSize:=ADataSize; CurOfs:=0;
      S^.Reset;
      S^.Seek(BaseOfs+E^.DataOfs);
      while (RemSize>0) do
      begin
        FragSize:=Min(RemSize,BlockSize);
        S^.Write(PByteArray(@Data)^[CurOfs],FragSize);
        Dec(RemSize,FragSize); Inc(CurOfs,FragSize);
      end;
      Modified:=true;
    end;
  AddResourceEntry:=OK;
end;

function TResourceFile.AddResourceEntryFromStream(const ResName: string; ALangID, AFlags: longint;
           var Source: TStream; ADataSize: longint): boolean;
const BufSize = 4096;
var OK: boolean;
    P: PResource;
    E: PResourceEntry;
    RemSize,FragSize: longint;
    Buf: pointer;
begin
  P:=FindResource(ResName);
  OK:=P<>nil;
  if OK then
    OK:=(P^.Items^.SearchEntryForLang(ALangID)=nil);
  if OK then
    begin
      New(E, Init(GetNextEntryID, ALangID, AFlags, ADataSize));
      AddResEntryPtr(P,E);
      UpdateBlockDatas;
      GetMem(Buf,BufSize);
      RemSize:=ADataSize;
      S^.Reset;
      S^.Seek(BaseOfs+E^.DataOfs);
      while (RemSize>0) do
      begin
        FragSize:=Min(RemSize,BufSize);
        Source.Read(Buf^,FragSize);
        S^.Write(Buf^,FragSize);
        Dec(RemSize,FragSize);
      end;
      FreeMem(Buf,BufSize);
      Modified:=true;
    end;
  AddResourceEntryFromStream:=OK;
end;

function TResourceFile.DeleteResourceEntry(const ResName: string; ALangID: longint): boolean;
var E: PResourceEntry;
    P: PResource;
    OK: boolean;
begin
  P:=FindResource(ResName);
  OK:=P<>nil;
  if OK then E:=P^.Items^.SearchEntryForLang(ALangID);
  OK:=OK and (E<>nil);
  if OK then
    begin
      OK:=DeleteArea(E^.DataOfs,E^.DataLen,GetTotalSize(false));
      if OK then begin RemoveResEntryPtr(P,E); Dispose(E, Done); end;
      Modified:=true;
    end;
  DeleteResourceEntry:=OK;
end;

function TResourceFile.DeleteResource(const ResName: string): boolean;
var P: PResource;
    E: PResourceEntry;
    OK: boolean;
begin
  P:=FindResource(ResName);
  OK:=P<>nil;
  if P<>nil then
  begin
    while OK and (P^.Items^.Count>0) do
      begin
        E:=P^.Items^.At(P^.Items^.Count-1);
        OK:=OK and DeleteResourceEntry(ResName,E^.LangID);
      end;
    Modified:=true;
  end;
  if OK then Resources^.Free(P);
  DeleteResource:=OK;
end;

function TResourceFile.ReadResourceEntry(const ResName: string; ALangID: longint; var Buf; BufSize: sw_word): boolean;
var E: PResourceEntry;
    P: PResource;
    OK: boolean;
    CurOfs,CurFrag: sw_word;
    TempBuf: pointer;
const TempBufSize = 4096;
begin
  E:=nil;
  P:=FindResource(ResName);
  OK:=P<>nil;
  if OK then E:=P^.Items^.SearchEntryForLang(ALangID);
  OK:=OK and (E<>nil);
  OK:=OK and (E^.DataLen<=BufSize);
  if OK then
    begin
      GetMem(TempBuf,TempBufSize);
      S^.Reset;
      S^.Seek(BaseOfs+E^.DataOfs);
      OK:=(S^.Status=stOK);
      CurOfs:=0;

      while OK and (CurOfs<E^.DataLen) do
      begin
        CurFrag:=Min(E^.DataLen-CurOfs,TempBufSize);
        S^.Read(TempBuf^,CurFrag);
        OK:=OK and (S^.Status=stOK);
        if OK then
          Move(TempBuf^,PByteArray(@Buf)^[CurOfs],CurFrag);
        Inc(CurOfs,CurFrag);
      end;

      FreeMem(TempBuf,TempBufSize);
    end;
  ReadResourceEntry:=OK;
end;

function TResourceFile.ReadResourceEntryToStream(const ResName: string; ALangID: longint; var DestS: TStream): boolean;
var E: PResourceEntry;
    P: PResource;
    OK: boolean;
    CurOfs,CurFrag: sw_word;
    TempBuf: pointer;
const TempBufSize = 4096;
begin
  P:=FindResource(ResName);
  OK:=P<>nil;
  if OK then E:=P^.Items^.SearchEntryForLang(ALangID);
  OK:=OK and (E<>nil);
  if OK then
    begin
      GetMem(TempBuf,TempBufSize);
      S^.Reset;
      S^.Seek(BaseOfs+E^.DataOfs);
      OK:=(S^.Status=stOK);
      CurOfs:=0;
      { this results sometimes in endless loops
      when the resource are changed PM }
      if E^.DataLen<0 then
        OK:=false;
      while OK and (CurOfs<E^.DataLen) do
      begin
        CurFrag:=Min(E^.DataLen-CurOfs,TempBufSize);
        S^.Read(TempBuf^,CurFrag);
        OK:=OK and (S^.Status=stOK);
        if OK then
          DestS.Write(TempBuf^,CurFrag);
        OK:=OK and (DestS.Status=stOK);
        Inc(CurOfs,CurFrag);
      end;

      FreeMem(TempBuf,TempBufSize);
    end;
  ReadResourceEntryToStream:=OK;
end;

function TResourceFile.FindResource(const ResName: string): PResource;
begin
  FindResource:=Resources^.SearchResourceByName(ResName);
end;

function TResourceFile.FindResourceEntry(const ResName: string; ALangID: longint): PResourceEntry;
var P: PResource;
    E: PResourceEntry;
begin
  E:=nil;
  P:=FindResource(ResName);
  if P<>nil then
    E:=P^.Items^.SearchEntryForLang(ALangID);
  FindResourceEntry:=E;
end;

procedure TResourceFile.Flush;
begin
  if Modified=false then Exit;
  BuildFileHeader;
  S^.Seek(BaseOfs);
  WriteHeader;
  S^.Seek(BaseOfs+Header.TableOfs);
  WriteResourceTable;
  S^.Truncate;
  Modified:=false;
end;

procedure TResourceFile.BuildFileHeader;
begin
  FillChar(Header,SizeOf(Header),0);
  with Header do
  begin
    Signature:=TPDataBlockSignature;
    InfoType:=ResourceBlockSignature;
    InfoSize:=GetTotalSize(true);
    TableOfs:=GetTotalSize(false);
  end;
end;

procedure TResourceFile.WriteHeader;
begin
  S^.Write(Header,SizeOf(Header));
end;

procedure TResourceFile.WriteResourceTable;
var RH: TResourceHeader;
    REH: TResourceEntryHeader;
procedure WriteResource(P: PResource);
procedure WriteResourceEntry(P: PResourceEntry);
begin
  P^.BuildHeader(REH);
  S^.Write(REH,SizeOf(REH));
end;
var N: string;
begin
  if P^.Items^.Count=0 then Exit; { do not store resources with no entries }
  P^.BuildHeader(RH);
  S^.Write(RH,SizeOf(RH));
  N:=P^.GetName;
  S^.Write(N[1],length(N));
  P^.ForEachEntry(@WriteResourceEntry);
end;
begin
  ForEachResource(@WriteResource);
end;

procedure TResourceFile.UpdateBlockDatas;
begin
  CalcSizes(false,true);
end;

function TResourceFile.GetTotalSize(IncludeHeaders: boolean): longint;
begin
  GetTotalSize:=CalcSizes(IncludeHeaders,false);
end;

function TResourceFile.CalcSizes(IncludeHeaders, UpdatePosData: boolean): longint;
var RH  : TResourceHeader;
    REH : TResourceEntryHeader;
    Size: longint;
    NamesSize: longint;
procedure AddResourceEntrySize(P: PResourceEntry);
begin
  if UpdatePosData then P^.DataOfs:=Size;
  P^.BuildHeader(REH);
  Inc(Size,REH.DataLen);
end;
procedure AddResourceSize(P: PResource);
var RH: TResourceHeader;
begin
  P^.BuildHeader(RH);
  Inc(NamesSize,RH.NameLen);
end;
begin
  Size:=0; NamesSize:=0;
  Inc(Size,SizeOf(Header)); { this is on start so we always include it }
  ForEachResourceEntry(@AddResourceEntrySize);
  if IncludeHeaders then
    begin
      ForEachResource(@AddResourceSize);
      Inc(Size,SizeOf(RH)*Resources^.Count);
      Inc(Size,SizeOf(REH)*Entries^.Count);
      Inc(Size,NamesSize);
    end;
  CalcSizes:=Size;
end;

function TResourceFile.DeleteArea(AreaStart, AreaSize, TotalSize: longint): boolean;
const BufSize = 4096;
var RemSize,FragSize,CurOfs: longint;
    Buf: pointer;
    OK: boolean;
begin
  GetMem(Buf,BufSize);
  RemSize:=TotalSize-(AreaStart+AreaSize); CurOfs:=0;
  OK:=RemSize>=0;
  while (RemSize>0) do
    begin
      FragSize:=Min(RemSize,BufSize);
      S^.Seek(BaseOfs+AreaStart+AreaSize+CurOfs);
      S^.Read(Buf^,BufSize);
      OK:=OK and (S^.Status=stOK);
      if OK then
      begin
        S^.Seek(BaseOfs+AreaStart+CurOfs);
        S^.Write(Buf^,BufSize);
        OK:=OK and (S^.Status=stOK);
      end;
      Inc(CurOfs,FragSize); Dec(RemSize,FragSize);
    end;
  FreeMem(Buf,BufSize);
  DeleteArea:=OK;
end;

procedure TResourceFile.AddResEntryPtr(P: PResource; E: PResourceEntry);
begin
  if (P=nil) or (E=nil) then Exit;
  P^.Items^.Insert(E);
  Entries^.Insert(E);
end;

procedure TResourceFile.RemoveResEntryPtr(P: PResource; E: PResourceEntry);
begin
  if (P=nil) or (E=nil) then Exit;
  Entries^.Delete(E);
  P^.Items^.Delete(E);
end;

function TResourceFile.GetNextEntryID: longint;
var ID: longint;
begin
  if Entries^.Count=0 then ID:=1 else
    ID:=Entries^.At(Entries^.Count-1)^.ID+1;
  GetNextEntryID:=ID;
end;

destructor TResourceFile.Done;
begin
  Flush;
  inherited Done;
{  if assigned(S) then dispose(S,Done); S:=nil;}
  if Resources<>nil then Dispose(Resources, Done); Resources:=nil;
  if Entries<>nil then
    begin Entries^.DeleteAll; Dispose(Entries, Done); Entries:=nil; end;
  if MyStream and Assigned(S) then
    Dispose(S, Done);
end;

constructor TResourceFile.CreateFile(AFileName: string);
var B: PFastBufStream;
begin
  New(B, Init(AFileName, stCreate, 4096));
  if (B<>nil) and (B^.Status<>stOK) then
    begin Dispose(B, Done); B:=nil; end;
  if B=nil then Fail;
  if Create(B^)=false then
    Begin
      Dispose(B,Done);
      Fail;
    End;
  MyStream:=true;
  {$ifdef HASAMIGA}
  Flush;
  {$endif}
end;

constructor TResourceFile.LoadFile(AFileName: string);
var B: PFastBufStream;
begin
  New(B, Init(AFileName, stOpen, 4096));
  if (B<>nil) and (B^.Status<>stOK) then
    begin Dispose(B, Done); B:=nil; end;
  if B=nil then Fail;
  if Load(B^)=false then
    Begin
      Dispose(B,Done);
      Fail;
    End;
  MyStream:=true;
end;

END.
