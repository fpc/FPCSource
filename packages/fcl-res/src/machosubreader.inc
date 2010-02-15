{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource reader for Mach-O files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type

  { _TMachOSubReader_ }

  _TMachOSubReader_ = class(TAbstractMachOSubReader)
  private
    fSection : _TSection_;
    fResHdr : _TResHdr_;
    function FindResSection(aStream : TStream) : boolean;
    procedure ReadResHeader(aStream : TStream);
  protected
    procedure Load(aResources : TResources; aStream : TStream); override;
    procedure ReadNode(aStream : TStream; aParent : TResourceTreeNode;
      aResources : TResources; named : boolean); override;
  public
    constructor Create(aParent : TMachOResourceReader; const aHeader : TMachHdr;
      const aOppositeEndianess : boolean); override;
  end;

{ _TMachOSubReader_ }

procedure _TMachOSubReader_.ReadNode(aStream: TStream;
  aParent: TResourceTreeNode; aResources: TResources; named: boolean);
var infonode : _TResInfoNode_;
    aNode : TResourceTreeNode;
    i : integer;
    oldpos : int64;
    desc : TResourceDesc;
begin
  aStream.ReadBuffer(infonode,sizeof(infonode));
  oldpos:=aStream.Position;
  if fOppositeEndianess then
  begin
    infonode.nameid:=SwapEndian(infonode.nameid);
    infonode.ncount:=SwapEndian(infonode.ncount);
    infonode.idcountsize:=SwapEndian(infonode.idcountsize);
    infonode.subptr:=SwapEndian(infonode.subptr);
  end;
  if aParent=nil then aNode:=fRoot
  else
  begin
    desc:=TResourceDesc.Create;
    try
      if named then desc.Name:=ReadString(aStream,
        fSection.offset+infonode.nameid-fSection.addr)
      else desc.ID:=infonode.nameid;
      aNode:=aParent.CreateSubNode(desc);
    finally
      desc.Free;
    end;
  end;
  aStream.Position:=fSection.offset+infonode.subptr-fSection.addr;
  if aNode.IsLeaf then
    ReadResData(aStream,aNode,aResources,infonode.idcountsize)
  else
  begin
    for i:=1 to infonode.ncount do
      ReadNode(aStream,aNode,aResources,true);
    for i:=1 to infonode.idcountsize do
      ReadNode(aStream,aNode,aResources,false);
  end;
  aStream.Position:=oldpos;
end;

function _TMachOSubReader_.FindResSection(aStream: TStream) : boolean;
var i,j : integer;
    lc : TLoadCommand;
    seg : _TSegmentCommand_;
    nextpos : int64;
begin
  for i:=1 to fHeader.ncmds do
  begin
    aStream.ReadBuffer(lc,sizeof(lc));
    if fOppositeEndianess then
    begin
      lc.cmd:=SwapEndian(lc.cmd);
      lc.cmdsize:=SwapEndian(lc.cmdsize);
    end;
    nextpos:=aStream.Position-sizeof(lc)+lc.cmdsize;
    if lc.cmdsize=0 then break;
    if lc.cmd=fSegType then
    begin
      aStream.ReadBuffer(seg,sizeof(seg));
      if fOppositeEndianess then
        seg.nsects:=SwapEndian(seg.nsects);
      for j:=1 to seg.nsects do
      begin
        aStream.ReadBuffer(fSection,sizeof(fSection));
        if (fSection.segname=DataSegName) and (fSection.sectname=RsrcSectName) then
        begin
          if fOppositeEndianess then
          begin
            fSection.addr:=SwapEndian(fSection.addr);
            fSection.size:=SwapEndian(fSection.size);
            fSection.offset:=SwapEndian(fSection.offset);
            fSection.align:=SwapEndian(fSection.align);
            fSection.reloff:=SwapEndian(fSection.reloff);
            fSection.nreloc:=SwapEndian(fSection.nreloc);
            fSection.flags:=SwapEndian(fSection.flags);
            fSection.reserved1:=SwapEndian(fSection.reserved1);
            fSection.reserved2:=SwapEndian(fSection.reserved2);
          end;
          Result:=true;
          exit;
        end;
      end;
    end;
    aStream.Position:=nextpos;
  end;
  //nothing found
  Result:=false;
  FillByte(fSection,sizeof(fSection),0);
end;

procedure _TMachOSubReader_.ReadResHeader(aStream: TStream);
begin
  aStream.Position:=fSection.offset;
  aStream.ReadBuffer(fResHdr,sizeof(fResHdr));
  if fOppositeEndianess then
  begin
    fResHdr.rootptr:=SwapEndian(fResHdr.rootptr);
    fResHdr.count:=SwapEndian(fResHdr.count);
  end;
  aStream.Position:=fSection.offset+fResHdr.rootptr-fSection.addr;
end;

procedure _TMachOSubReader_.Load(aResources: TResources; aStream: TStream);
begin
  if not FindResSection(aStream) then exit;
  ReadResHeader(aStream);
  if fResHdr.count=0 then exit; //no resources in this file
  LoadResources(aResources,aStream);
end;

constructor _TMachOSubReader_.Create(aParent : TMachOResourceReader;
  const aHeader : TMachHdr; const aOppositeEndianess : boolean);
begin
  inherited Create(aParent,aHeader,aOppositeEndianess);
  {$IF _TMachOSubReader_=TMachO32SubReader}
  fSegType:=LC_SEGMENT;
  {$ELSE}
  fSegType:=LC_SEGMENT_64;
  {$ENDIF}
end;

