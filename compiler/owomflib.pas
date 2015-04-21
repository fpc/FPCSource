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
  owbase;

type

  { TOmfLibObjectWriter }

  TOmfLibObjectWriter=class(TObjectWriter)
  private
    FPageSize: Integer;
    FLibName: string;
    FLibData: TDynamicArray;
    FObjFileName: string;
    FObjData: TDynamicArray;
    FObjStartPage: Word;

    procedure WriteHeader(DictStart: DWord; DictBlocks: Word);
    procedure WriteFooter;
    procedure WriteLib;
  public
    constructor createAr(const Aarfn:string);override;
    destructor  destroy;override;
    function  createfile(const fn:string):boolean;override;
    procedure closefile;override;
    procedure writesym(const sym:string);override;
    procedure write(const b;len:longword);override;
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
                                TOmfLibObjectWriter
*****************************************************************************}

    constructor TOmfLibObjectWriter.createAr(const Aarfn: string);
      begin
        FPageSize:=512;
        FLibName:=Aarfn;
        FLibData:=TDynamicArray.Create(libbufsize);
        { header is at page 0, so first module starts at page 1 }
        FObjStartPage:=1;
      end;


    destructor TOmfLibObjectWriter.destroy;
      begin
        if Errorcount=0 then
          WriteLib;
        FLibData.Free;
        FObjData.Free;
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
      begin
        FLibData.seek(FObjStartPage*FPageSize);
        FObjData.seek(0);
        RawRec:=TOmfRawRecord.Create;
        repeat
          RawRec.ReadFrom(FObjData);
          RawRec.WriteTo(FLibData);
        until RawRec.RecordType in [RT_MODEND,RT_MODEND32];
        RawRec.Free;
        { calculate start page of next module }
        FObjStartPage:=(FLibData.Pos+FPageSize-1) div FPageSize;
        fobjsize:=0;
      end;


    procedure TOmfLibObjectWriter.writesym(const sym: string);
      begin
        inherited writesym(sym);
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
      begin
        libf:=CFileStreamClass.Create(FLibName,fmCreate);
        if CStreamError<>0 then
          begin
            Message1(exec_e_cant_create_archivefile,FLibName);
            exit;
          end;
        WriteFooter;
        WriteHeader(FLibData.Pos,2);
        FLibData.WriteStream(libf);
        libf.Free;
      end;

end.
