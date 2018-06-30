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

  { TOmfLibObjectWriter }

  TOmfLibObjectWriter=class(TObjectWriter)
  strict private
    type

      { TOmfLibObjectModule }

      TOmfLibObjectModule=class
      strict private
        FObjFileName: string;
        FObjData: TDynamicArray;
        FPageNum: Word;
      public
        constructor Create(const fn:string);
        destructor Destroy; override;

        property ObjData: TDynamicArray read FObjData;
        property PageNum: Word read FPageNum write FPageNum;
      end;

      { TOmfLibDictionaryEntry }

      TOmfLibDictionaryEntry=class(TFPHashObject)
      strict private
        FModuleIndex: Integer;
      public
        constructor Create(HashObjectList:TFPHashObjectList;const aName:TSymStr;aModuleIndex:Integer);
        property ModuleIndex: Integer read FModuleIndex write FModuleIndex;
      end;
  strict private
    FPageSize: Integer;
    FLibName: string;
    FLibData: TDynamicArray;
    FFooterPos: LongWord;
    FDictionary: TFPHashObjectList;
    FObjectModules: TFPObjectList;
    FCurrentModule: TOmfLibObjectModule;
    FCurrentModuleIndex: Integer;

    procedure WriteHeader(DictStart: DWord; DictBlocks: Word);
    procedure WriteFooter;
    function TryPageSize(aPageSize: Integer): Boolean;
    procedure DeterminePageSize;
    procedure WriteLib;
    function WriteDictionary: Word;
    function TryWriteDictionaryWithSize(nblocks: Word): Boolean;
  public
    constructor createAr(const Aarfn:string);override;
    constructor createAr(const Aarfn:string;PageSize:Integer);
    destructor  destroy;override;
    function  createfile(const fn:string):boolean;override;
    procedure closefile;override;
    procedure writesym(const sym:string);override;
    procedure write(const b;len:longword);override;
  end;

  { TOmfLibObjectReader }

  TOmfLibObjectReader=class(TObjectReader)
  strict private
    type

      { TOmfLibDictionaryEntry }

      TOmfLibDictionaryEntry=class(TFPHashObject)
      strict private
        FPageNum: Word;
      public
        constructor Create(HashObjectList:TFPHashObjectList;const aName:TSymStr;aPageNum:Word);
        property PageNum: Word read FPageNum write FPageNum;
      end;
  strict private
    LibSymbols : TFPHashObjectList;
    islib: boolean;
    CurrMemberPos : longint;
    CurrMemberName : string;
    FPageSize: Integer;
    FIsCaseSensitive: Boolean;
    procedure ReadLibrary;
    procedure ReadDictionary(DictionaryOffset: DWord; DictionarySizeInBlocks: Word);
  protected
    function getfilename:string;override;
    function GetPos: longint;override;
    function GetIsArchive: boolean;override;
  public
    constructor createAr(const Aarfn:string;allow_nonar:boolean=false);override;
    destructor  destroy;override;
    function  openfile(const fn:string):boolean;override;
    procedure closefile;override;
    procedure seek(len:longint);override;
    property IsCaseSensitive: Boolean read FIsCaseSensitive;
  end;

implementation

    uses
      SysUtils,
      cstreams,cutils,
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
                 TOmfLibObjectWriter.TOmfLibObjectModule
*****************************************************************************}

    constructor TOmfLibObjectWriter.TOmfLibObjectModule.Create(const fn: string);
      begin
        FObjFileName:=fn;
        FObjData:=TDynamicArray.Create(objbufsize);
      end;

    destructor TOmfLibObjectWriter.TOmfLibObjectModule.Destroy;
      begin
        FObjData.Free;
        inherited Destroy;
      end;

{*****************************************************************************
                 TOmfLibObjectWriter.TOmfLibDictionaryEntry
*****************************************************************************}

    constructor TOmfLibObjectWriter.TOmfLibDictionaryEntry.Create(
        HashObjectList: TFPHashObjectList; const aName: TSymStr; aModuleIndex:Integer);
      begin
        inherited Create(HashObjectList,aName);
        ModuleIndex:=aModuleIndex;
      end;

{*****************************************************************************
                                TOmfLibObjectWriter
*****************************************************************************}

    constructor TOmfLibObjectWriter.createAr(const Aarfn: string);
      begin
        createAr(Aarfn,-1);
      end;

    constructor TOmfLibObjectWriter.createAr(const Aarfn: string;PageSize: Integer);
      begin
        FPageSize:=PageSize;
        FLibName:=Aarfn;
        FLibData:=TDynamicArray.Create(libbufsize);
        FDictionary:=TFPHashObjectList.Create;
        FObjectModules:=TFPObjectList.Create(True);
        FCurrentModule:=nil;
      end;


    destructor TOmfLibObjectWriter.destroy;
      begin
        if Errorcount=0 then
          WriteLib;
        FLibData.Free;
        FObjectModules.Free;
        FDictionary.Free;
        inherited destroy;
      end;


    function TOmfLibObjectWriter.createfile(const fn: string): boolean;
      begin
        FCurrentModule:=TOmfLibObjectModule.Create(fn);
        FCurrentModuleIndex:=FObjectModules.Add(FCurrentModule);
        createfile:=true;
        fobjsize:=0;
      end;


    procedure TOmfLibObjectWriter.closefile;
      var
        RawRec: TOmfRawRecord;
        ObjHeader: TOmfRecord_THEADR;
      begin
        FCurrentModule.ObjData.seek(0);
        RawRec:=TOmfRawRecord.Create;
        RawRec.ReadFrom(FCurrentModule.ObjData);
        if RawRec.RecordType<>RT_THEADR then
          begin
            RawRec.Free;
            InternalError(2018060801);
          end;
        ObjHeader:=TOmfRecord_THEADR.Create;
        ObjHeader.DecodeFrom(RawRec);
        { create a dictionary entry with the module name }
        TOmfLibDictionaryEntry.Create(FDictionary,ModName2DictEntry(ObjHeader.ModuleName),FCurrentModuleIndex);
        ObjHeader.Free;
        RawRec.Free;
        fobjsize:=0;
      end;


    procedure TOmfLibObjectWriter.writesym(const sym: string);
      begin
        TOmfLibDictionaryEntry.Create(FDictionary,sym,FCurrentModuleIndex);
      end;


    procedure TOmfLibObjectWriter.write(const b; len: longword);
      begin
        inc(fobjsize,len);
        inc(fsize,len);
        FCurrentModule.ObjData.write(b,len);
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
        FLibData.seek(FFooterPos);
        Footer:=TOmfRecord_LIBEND.Create;
        Footer.CalculatePaddingBytes(FLibData.Pos);
        RawRec:=TOmfRawRecord.Create;
        Footer.EncodeTo(RawRec);
        RawRec.WriteTo(FLibData);
        Footer.Free;
        RawRec.Free;
      end;

    function TOmfLibObjectWriter.TryPageSize(aPageSize: Integer): Boolean;
      var
        I: Integer;
        CurrentPage: Integer;
        CurrentPos: LongWord;
        pow: longint;
      begin
        if not IsPowerOf2(aPageSize,pow) then
          internalerror(2018060701);
        if (pow<4) or (pow>15) then
          internalerror(2018060702);
        FPageSize:=aPageSize;
        { header is at page 0, so first module starts at page 1 }
        CurrentPage:=1;
        for I:=0 to FObjectModules.Count-1 do
          with TOmfLibObjectModule(FObjectModules[I]) do
            begin
              if CurrentPage>high(word) then
                exit(False);
              PageNum:=CurrentPage;
              { calculate next page }
              CurrentPos:=CurrentPage*FPageSize+ObjData.Size;
              CurrentPage:=(CurrentPos+FPageSize-1) div FPageSize;
            end;
        FFooterPos:=CurrentPage*FPageSize;
        Result:=True;
      end;

    procedure TOmfLibObjectWriter.DeterminePageSize;
      var
        I: Integer;
      begin
        if (FPageSize<>-1) and TryPageSize(FPageSize) then
          { success }
          exit;
        for I:=4 to 15 do
          if TryPageSize(1 shl I) then
            exit;
        internalerror(2018060703);
      end;

    procedure TOmfLibObjectWriter.WriteLib;
      var
        libf: TCCustomFileStream;
        DictStart, bytes: LongWord;
        DictBlocks: Word;
        I: Integer;
        buf: array [0..1023] of Byte;
      begin
        DeterminePageSize;
        libf:=CFileStreamClass.Create(FLibName,fmCreate);
        if CStreamError<>0 then
          begin
            Message1(exec_e_cant_create_archivefile,FLibName);
            exit;
          end;
        for I:=0 to FObjectModules.Count-1 do
          with TOmfLibObjectModule(FObjectModules[I]) do
            begin
              FLibData.seek(PageNum*FPageSize);
              ObjData.seek(0);
              while ObjData.Pos<ObjData.size do
                begin
                  bytes:=ObjData.read(buf,Min(SizeOf(buf),ObjData.size-ObjData.Pos));
                  FLibData.write(buf,bytes);
                end;
            end;
        WriteFooter;
        DictStart:=FLibData.Pos;
        DictBlocks:=WriteDictionary;
        WriteHeader(DictStart,DictBlocks);
        FLibData.WriteStream(libf);
        libf.Free;
      end;

    function TOmfLibObjectWriter.WriteDictionary: Word;
      var
        nb: Word;
      begin
        for nb in OmfLibDictionaryBlockCounts do
          if TryWriteDictionaryWithSize(nb) then
            exit(nb);
        { could not write dictionary, even with the largest number of blocks }
        internalerror(2015042202);
      end;

        function TOmfLibObjectWriter.TryWriteDictionaryWithSize(nblocks: Word
      ): Boolean;
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
        blocks:=nil;
        SetLength(blocks,nblocks);
        for i:=0 to nblocks-1 do
          begin
            FillChar(blocks[i],SizeOf(blocks[i]),0);
            blocks[i][freespace]:=(freespace+1) div 2;
          end;

        for i:=0 to FDictionary.Count-1 do
          begin
            N:=TOmfLibDictionaryEntry(FDictionary[i]).Name;
            PageNum:=TOmfLibObjectModule(FObjectModules[TOmfLibDictionaryEntry(FDictionary[i]).ModuleIndex]).PageNum;
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
                 TOmfLibObjectReader.TOmfLibDictionaryEntry
*****************************************************************************}

  constructor TOmfLibObjectReader.TOmfLibDictionaryEntry.Create(
      HashObjectList: TFPHashObjectList; const aName: TSymStr; aPageNum: Word);
    begin
      inherited Create(HashObjectList,aName);
      PageNum:=aPageNum;
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
      FIsCaseSensitive:=Header.CaseSensitive;
      ReadDictionary(Header.DictionaryOffset, Header.DictionarySizeInBlocks);
      Header.Free;
      RawRecord.Free;
    end;

  procedure TOmfLibObjectReader.ReadDictionary(DictionaryOffset: DWord; DictionarySizeInBlocks: Word);
    const
      nbuckets=37;
      freespace=nbuckets;
    type
      PBlock=^TBlock;
      TBlock=array[0..511] of byte;
    var
      blocks: array of TBlock;
      blocknr: Integer;
      block: PBlock;
      ofs: Integer;
      bucket: Integer;
      length_of_string: Byte;
      name: string;
      PageNum: Integer;
    begin
      blocks:=nil;
      name:='';
      seek(DictionaryOffset);
      SetLength(blocks,DictionarySizeInBlocks);
      read(blocks[0],DictionarySizeInBlocks*SizeOf(TBlock));
      for blocknr:=0 to DictionarySizeInBlocks-1 do
        begin
          block:=@(blocks[blocknr]);
          for bucket:=0 to nbuckets-1 do
            if block^[bucket]<>0 then
              begin
                ofs:=2*block^[bucket];
                length_of_string:=block^[ofs];
                if (ofs+1+length_of_string+1)>High(TBlock) then
                  begin
                    Comment(V_Error,'OMF dictionary entry goes beyond end of block');
                    continue;
                  end;
                SetLength(name,length_of_string);
                Move(block^[ofs+1],name[1],length_of_string);
                PageNum:=block^[ofs+1+length_of_string]+
                         block^[ofs+1+length_of_string+1] shl 8;
                TOmfLibDictionaryEntry.create(LibSymbols,name,PageNum);
              end;
        end;
    end;

  function TOmfLibObjectReader.getfilename: string;
    begin
      Result:=inherited getfilename;
      if CurrMemberName<>'' then
        result:=result+'('+CurrMemberName+')';
    end;

  function TOmfLibObjectReader.GetPos: longint;
    begin
      result:=inherited GetPos-CurrMemberPos;
    end;

  function TOmfLibObjectReader.GetIsArchive: boolean;
    begin
      result:=islib;
    end;

  constructor TOmfLibObjectReader.createAr(const Aarfn: string; allow_nonar: boolean);
    var
      RecType: Byte;
    begin
      inherited Create;
      LibSymbols:=TFPHashObjectList.Create(true);
      CurrMemberPos:=0;
      CurrMemberName:='';
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
      LibSymbols.Free;
      inherited Destroy;
    end;

  function TOmfLibObjectReader.openfile(const fn: string): boolean;
    var
      libsym: TOmfLibDictionaryEntry;
      RawRec: TOmfRawRecord;
      Header: TOmfRecord_THEADR;
    begin
      result:=false;
      libsym:=TOmfLibDictionaryEntry(LibSymbols.Find(ModName2DictEntry(fn)));
      if not assigned(libsym) then
        exit;
      CurrMemberPos:=libsym.PageNum*FPageSize;
      inherited Seek(CurrMemberPos);

      { read the header, to obtain the module name }
      RawRec:=TOmfRawRecord.Create;
      RawRec.ReadFrom(self);
      Header:=TOmfRecord_THEADR.Create;
      Header.DecodeFrom(RawRec);
      CurrMemberName:=Header.ModuleName;
      Header.Free;
      RawRec.Free;

      { go back to the beginning of the file }
      inherited Seek(CurrMemberPos);
      result:=true;
    end;

  procedure TOmfLibObjectReader.closefile;
    begin
      CurrMemberPos:=0;
      CurrMemberName:='';
    end;

  procedure TOmfLibObjectReader.seek(len: longint);
    begin
      inherited Seek(CurrMemberPos+len);
    end;

end.
