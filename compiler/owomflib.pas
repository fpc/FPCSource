{
    Copyright (c) 2015 by Nikolay Nikolov

    Contains the stuff for writing Relocatable Object Module Format (OMF)
    libraries directly. This is the object format used on the i8086-msdos
    platform (also known as .lib files in the dos world, even though Free
    Pascal uses the extension .a).

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit owomflib;

{$i fpcdefs.inc}

interface

uses
  cclasses,
  globtype,
  owbase;

type

  { TOmfLibDictionaryEntry }

  TOmfLibDictionaryEntry=class(TFPHashObject)
  private
    FPageNum: Word;
  public
    constructor Create(HashObjectList:TFPHashObjectList;const aName:TSymStr;aPageNum:Word);
    property PageNum: Word read FPageNum write FPageNum;
  end;

  { TOmfLibObjectWriter }

  TOmfLibObjectWriter=class(TObjectWriter)
  private
    FPageSize: Integer;
    FLibName: string;
    FLibData: TDynamicArray;
    FObjFileName: string;
    FObjData: TDynamicArray;
    FObjStartPage: Word;
    FDictionary: TFPHashObjectList;

    procedure WriteHeader(DictStart: DWord; DictBlocks: Word);
    procedure WriteFooter;
    procedure WriteLib;
    function WriteDictionary: byte;
    function TryWriteDictionaryWithSize(nblocks: Byte): Boolean;
  public
    constructor createAr(const Aarfn:string);override;
    destructor  destroy;override;
    function  createfile(const fn:string):boolean;override;
    procedure closefile;override;
    procedure writesym(const sym:string);override;
    procedure write(const b;len:longword);override;
  end;

  { TOmfLibObjectReader }

  TOmfLibObjectReader=class(TObjectReader)
  private
    islib: boolean;
    CurrMemberPos : longint;
    FPageSize: Integer;
    procedure ReadLibrary;
  public
    constructor create(const Aarfn:string;allow_nonar:boolean=false);
    destructor  destroy;override;
    function  openfile(const fn:string):boolean;override;
    procedure closefile;override;
    procedure seek(len:longint);override;
    property isarchive: boolean read islib;
  end;

implementation

    uses
      SysUtils,
      cstreams,
      globals,
      verbose,
      omfbase;

    const
      libbufsize = 65536;
      objbufsize = 65536;

{*****************************************************************************
                                   Helpers
*****************************************************************************}

    function ModName2DictEntry(const modnm: string): string;
      begin
        if Copy(modnm,Length(modnm)-1,2)='.o' then
          Result:=Copy(modnm,1,Length(modnm)-2)+'!'
        else
          Result:=modnm;
      end;

{*****************************************************************************
                                TOmfLibDictionaryEntry
*****************************************************************************}

    constructor TOmfLibDictionaryEntry.Create(HashObjectList: TFPHashObjectList; const aName: TSymStr; aPageNum: Word);
      begin
        inherited Create(HashObjectList,aName);
        PageNum:=aPageNum;
      end;

{*****************************************************************************
                                TOmfLibObjectWriter
*****************************************************************************}

    constructor TOmfLibObjectWriter.createAr(const Aarfn: string);
      begin
        FPageSize:=512;
        FLibName:=Aarfn;
        FLibData:=TDynamicArray.Create(libbufsize);
        FDictionary:=TFPHashObjectList.Create;
        { header is at page 0, so first module starts at page 1 }
        FObjStartPage:=1;
      end;


    destructor TOmfLibObjectWriter.destroy;
      begin
        if Errorcount=0 then
          WriteLib;
        FLibData.Free;
        FObjData.Free;
        FDictionary.Free;
        inherited destroy;
      end;


    function TOmfLibObjectWriter.createfile(const fn: string): boolean;
      begin
        FObjFileName:=fn;
        FreeAndNil(FObjData);
        FObjData:=TDynamicArray.Create(objbufsize);
        createfile:=true;
        fobjsize:=0;
      end;


    procedure TOmfLibObjectWriter.closefile;
      var
        RawRec: TOmfRawRecord;
        ObjHeader: TOmfRecord_THEADR;
      begin
        FLibData.seek(FObjStartPage*FPageSize);
        FObjData.seek(0);
        RawRec:=TOmfRawRecord.Create;
        repeat
          RawRec.ReadFrom(FObjData);
          if RawRec.RecordType=RT_THEADR then
            begin
              ObjHeader:=TOmfRecord_THEADR.Create;
              ObjHeader.DecodeFrom(RawRec);
              { create a dictionary entry with the module name }
              TOmfLibDictionaryEntry.Create(FDictionary,ModName2DictEntry(ObjHeader.ModuleName),FObjStartPage);
              ObjHeader.Free;
            end;
          RawRec.WriteTo(FLibData);
        until RawRec.RecordType in [RT_MODEND,RT_MODEND32];
        RawRec.Free;
        { calculate start page of next module }
        FObjStartPage:=(FLibData.Pos+FPageSize-1) div FPageSize;
        fobjsize:=0;
      end;


    procedure TOmfLibObjectWriter.writesym(const sym: string);
      begin
        TOmfLibDictionaryEntry.Create(FDictionary,sym,FObjStartPage);
      end;


    procedure TOmfLibObjectWriter.write(const b; len: longword);
      begin
        inc(fobjsize,len);
        inc(fsize,len);
        FObjData.write(b,len);
      end;

    procedure TOmfLibObjectWriter.WriteHeader(DictStart: DWord; DictBlocks: Word);
      var
        Header: TOmfRecord_LIBHEAD;
        RawRec: TOmfRawRecord;
      begin
        { set header properties }
        Header:=TOmfRecord_LIBHEAD.Create;
        Header.PageSize:=FPageSize;
        Header.DictionaryOffset:=DictStart;
        Header.DictionarySizeInBlocks:=DictBlocks;
        Header.CaseSensitive:=true;

        { write header }
        RawRec:=TOmfRawRecord.Create;
        Header.EncodeTo(RawRec);
        FLibData.seek(0);
        RawRec.WriteTo(FLibData);
        Header.Free;
        RawRec.Free;
      end;

    procedure TOmfLibObjectWriter.WriteFooter;
      var
        Footer: TOmfRecord_LIBEND;
        RawRec: TOmfRawRecord;
      begin
        FLibData.seek(FObjStartPage*FPageSize);
        Footer:=TOmfRecord_LIBEND.Create;
        Footer.CalculatePaddingBytes(FLibData.Pos);
        RawRec:=TOmfRawRecord.Create;
        Footer.EncodeTo(RawRec);
        RawRec.WriteTo(FLibData);
        Footer.Free;
        RawRec.Free;
      end;

    procedure TOmfLibObjectWriter.WriteLib;
      var
        libf: TCCustomFileStream;
        DictStart: LongWord;
        DictBlocks: Byte;
      begin
        libf:=CFileStreamClass.Create(FLibName,fmCreate);
        if CStreamError<>0 then
          begin
            Message1(exec_e_cant_create_archivefile,FLibName);
            exit;
          end;
        WriteFooter;
        DictStart:=FLibData.Pos;
        DictBlocks:=WriteDictionary;
        WriteHeader(DictStart,DictBlocks);
        FLibData.WriteStream(libf);
        libf.Free;
      end;

    function TOmfLibObjectWriter.WriteDictionary: Byte;
      var
        nb: Byte;
      begin
        for nb in OmfLibDictionaryBlockCounts do
          if TryWriteDictionaryWithSize(nb) then
            exit(nb);
        { could not write dictionary, even with the largest number of blocks }
        internalerror(2015042201);
      end;

    function TOmfLibObjectWriter.TryWriteDictionaryWithSize(nblocks: Byte): Boolean;
      const
        nbuckets=37;
        freespace=nbuckets;
      type
        PBlock=^TBlock;
        TBlock=array[0..511] of byte;
      var
        blocks: array of TBlock;
        i: Integer;
        N: TSymStr;
        length_of_string: Integer;
        h: TOmfLibHash;
        start_block,start_bucket: Integer;
        space_required: Integer;
        pb: PBlock;
        success: Boolean;
        store_at: Integer;
        PageNum: Word;
      begin
        SetLength(blocks,nblocks);
        for i:=0 to nblocks-1 do
          begin
            FillChar(blocks[i],SizeOf(blocks[i]),0);
            blocks[i][freespace]:=(freespace+1) div 2;
          end;

        for i:=0 to FDictionary.Count-1 do
          begin
            N:=TOmfLibDictionaryEntry(FDictionary[i]).Name;
            PageNum:=TOmfLibDictionaryEntry(FDictionary[i]).PageNum;
            length_of_string:=Length(N);
            h:=compute_omf_lib_hash(N,nblocks);
            start_block:=h.block_x;
            start_bucket:=h.bucket_x;
            space_required:=1+length_of_string+2;
            if odd(space_required) then
              Inc(space_required);
            repeat
              pb:=@blocks[h.block_x];
              success:=false;
              repeat
                if pb^[h.bucket_x]=0 then
                  begin
                    if (512-pb^[freespace]*2)<space_required then
                      break;
                    pb^[h.bucket_x]:=pb^[freespace];
                    store_at:=2*pb^[h.bucket_x];
                    pb^[store_at]:=length_of_string;
                    Move(N[1],pb^[store_at+1],length_of_string);
                    pb^[store_at+1+length_of_string]:=Byte(PageNum);
                    pb^[store_at+1+length_of_string+1]:=Byte(PageNum shr 8);
                    Inc(pb^[freespace],space_required div 2);
                    if pb^[freespace]=0 then
                      pb^[freespace]:=255;
                    success:=true;
                    break;
                  end;
                h.bucket_x:=(h.bucket_x+h.bucket_d) mod nbuckets;
              until h.bucket_x=start_bucket;
              if not success then
                begin
                  h.block_x:=(h.block_x+h.block_d) mod nblocks;
                  if h.block_x=start_block then
                    exit(false); // not enough blocks
                  pb^[freespace]:=255;
                end;
            until success;
          end;
        FLibData.write(blocks[0],nblocks*SizeOf(TBlock));
        Result:=true;
      end;

{*****************************************************************************
                                TOmfLibObjectReader
*****************************************************************************}

  procedure TOmfLibObjectReader.ReadLibrary;
    var
      RawRecord: TOmfRawRecord;
      Header: TOmfRecord_LIBHEAD;
    begin
      RawRecord:=TOmfRawRecord.Create;
      RawRecord.ReadFrom(Self);
      Header:=TOmfRecord_LIBHEAD.Create;
      Header.DecodeFrom(RawRecord);
      FPageSize:=Header.PageSize;
    end;

  constructor TOmfLibObjectReader.create(const Aarfn: string; allow_nonar: boolean);
    var
      RecType: Byte;
    begin
      inherited Create;
      CurrMemberPos:=0;
      if inherited openfile(Aarfn) then
        begin
          Read(RecType,1);
          Seek(0);
          islib:=RecType=RT_LIBHEAD;
          if islib then
            ReadLibrary
          else if (not allow_nonar) then
            Comment(V_Error,'Not an OMF library file, illegal magic: '+filename);
        end;
    end;

  destructor TOmfLibObjectReader.destroy;
    begin
      inherited closefile;
      inherited Destroy;
    end;

  function TOmfLibObjectReader.openfile(const fn: string): boolean;
    begin
      Result:=inherited openfile(fn);
    end;

  procedure TOmfLibObjectReader.closefile;
    begin
      CurrMemberPos:=0;
    end;

  procedure TOmfLibObjectReader.seek(len: longint);
    begin
      inherited Seek(CurrMemberPos+len);
    end;

end.
