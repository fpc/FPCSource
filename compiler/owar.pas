{
    Copyright (c) 1998-2002 by Peter Vreman

    Contains the stuff for writing .a files directly

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
unit owar;

{$i fpcdefs.inc}

interface

uses
  cclasses,
  owbase;

type
  tarhdr=packed record
    name : array[0..15] of char;
    date : array[0..11] of char;
    uid  : array[0..5] of char;
    gid  : array[0..5] of char;
    mode : array[0..7] of char;
    size : array[0..9] of char;
    fmag : array[0..1] of char;
  end;

  tarobjectwriter=class(tobjectwriter)
    constructor createAr(const Aarfn:string);override;
    destructor  destroy;override;
    function  createfile(const fn:string):boolean;override;
    procedure closefile;override;
    procedure writesym(const sym:string);override;
    procedure write(const b;len:longword);override;
  private
    arfn        : string;
    arhdr       : tarhdr;
    symreloc,
    symstr,
    lfnstr,
    ardata      : TDynamicArray;
    objpos      : longint;
    objfn       : string;
    timestamp   : string[12];
    procedure createarhdr(fn:string;asize:longint;const gid,uid,mode:string);
    procedure writear;
  end;

  tarobjectreader=class(tobjectreader)
  private
    ArSymbols : TFPHashObjectList;
    LFNStrs   : PChar;
    LFNSize   : longint;
    CurrMemberPos,
    CurrMemberSize : longint;
    CurrMemberName : string;
    isar: boolean;
    function  DecodeMemberName(ahdr:TArHdr):string;
    function  DecodeMemberSize(ahdr:TArHdr):longint;
    procedure ReadArchive;
  protected
    function getfilename:string;override;
  public
    constructor create(const Aarfn:string;allow_nonar:boolean=false);
    destructor  destroy;override;
    function  openfile(const fn:string):boolean;override;
    procedure closefile;override;
    procedure seek(len:longint);override;
    property isarchive: boolean read isar;
  end;


implementation

    uses
      SysUtils,
      cstreams,
      systems,
      globals,
      verbose;

    const
      symrelocbufsize = 4096;
      symstrbufsize = 8192;
      lfnstrbufsize = 4096;
      arbufsize  = 65536;

      armagic:array[1..8] of char='!<arch>'#10;

    type
      TArSymbol = class(TFPHashObject)
        MemberPos : longint;
      end;


{*****************************************************************************
                                   Helpers
*****************************************************************************}

      const
        C1970=2440588;
        D0=1461;
        D1=146097;
        D2=1721119;
    Function Gregorian2Julian(DT:TSystemTime):LongInt;
      Var
        Century,XYear,Month : LongInt;
      Begin
        Month:=DT.Month;
        If Month<=2 Then
         Begin
           Dec(DT.Year);
           Inc(Month,12);
         End;
        Dec(Month,3);
        Century:=(longint(DT.Year Div 100)*D1) shr 2;
        XYear:=(longint(DT.Year Mod 100)*D0) shr 2;
        Gregorian2Julian:=((((Month*153)+2) div 5)+DT.Day)+D2+XYear+Century;
      End;


    function DT2Unix(DT:TSystemTime):LongInt;
      Begin
        DT2Unix:=(Gregorian2Julian(DT)-C1970)*86400+(LongInt(DT.Hour)*3600)+(DT.Minute*60)+DT.Second;
      end;


    function lsb2msb(l:longint):longint;
      type
        bytearr=array[0..3] of byte;
      begin
{$ifndef FPC_BIG_ENDIAN}
        bytearr(result)[0]:=bytearr(l)[3];
        bytearr(result)[1]:=bytearr(l)[2];
        bytearr(result)[2]:=bytearr(l)[1];
        bytearr(result)[3]:=bytearr(l)[0];
{$else}
        result:=l;
{$endif}
      end;


{*****************************************************************************
                                TArObjectWriter
*****************************************************************************}

    constructor tarobjectwriter.createAr(const Aarfn:string);
      var
        time  : TSystemTime;
      begin
        arfn:=Aarfn;
        ardata:=TDynamicArray.Create(arbufsize);
        symreloc:=TDynamicArray.Create(symrelocbufsize);
        symstr:=TDynamicArray.Create(symstrbufsize);
        lfnstr:=TDynamicArray.Create(lfnstrbufsize);
        { create timestamp }
        GetLocalTime(time);
        Str(DT2Unix(time),timestamp);
      end;


    destructor tarobjectwriter.destroy;
      begin
        if Errorcount=0 then
         writear;
        arData.Free;
        symreloc.Free;
        symstr.Free;
        lfnstr.Free;
      end;


    procedure tarobjectwriter.createarhdr(fn:string;asize:longint;const gid,uid,mode:string);
      var
        tmp : string[9];
        hfn : string;
      begin
        { create ar header }
        fillchar(arhdr,sizeof(tarhdr),' ');
        { win32 will change names starting with .\ to ./ when using lfn, corrupting
          the sort order required for the idata sections. To prevent this strip
          always the path from the filename. (PFV) }
        hfn:=ExtractFileName(fn);
        if hfn='' then
          hfn:=fn;
        fn:=hfn+'/';
        if length(fn)>16 then
         begin
           arhdr.name[0]:='/';
           str(lfnstr.size,tmp);
           move(tmp[1],arhdr.name[1],length(tmp));
           fn:=fn+#10;
           lfnstr.write(fn[1],length(fn));
         end
        else
         move(fn[1],arhdr.name,length(fn));
        { don't write a date if also no gid/uid/mode is specified }
        if gid<>'' then
          move(timestamp[1],arhdr.date,length(timestamp));
        str(asize,tmp);
        move(tmp[1],arhdr.size,length(tmp));
        move(gid[1],arhdr.gid,length(gid));
        move(uid[1],arhdr.uid,length(uid));
        move(mode[1],arhdr.mode,length(mode));
        arhdr.fmag:='`'#10;
      end;


    function tarobjectwriter.createfile(const fn:string):boolean;
      begin
        objfn:=fn;
        objpos:=ardata.size;
        ardata.seek(objpos + sizeof(tarhdr));
        createfile:=true;
        fobjsize:=0;
      end;


    procedure tarobjectwriter.closefile;
      begin
        ardata.align(2);
        { fix the size in the header }
        createarhdr(objfn,ardata.size-objpos-sizeof(tarhdr),'42','42','644');
        { write the header }
        ardata.seek(objpos);
        ardata.write(arhdr,sizeof(tarhdr));
        fobjsize:=0;
      end;


    procedure tarobjectwriter.writesym(const sym:string);
      var
        c : char;
      begin
        c:=#0;
        symreloc.write(objpos,4);
        symstr.write(sym[1],length(sym));
        symstr.write(c,1);
      end;


    procedure tarobjectwriter.write(const b;len:longword);
      begin
        inc(fobjsize,len);
        inc(fsize,len);
        ardata.write(b,len);
      end;


    procedure tarobjectwriter.writear;
      var
        arf      : TCCustomFileStream;
        fixup,l,
        relocs,i : longint;
      begin
        arf:=CFileStreamClass.Create(arfn,fmCreate);
        if CStreamError<>0 then
          begin
             Message1(exec_e_cant_create_archivefile,arfn);
             exit;
          end;
        arf.Write(armagic,sizeof(armagic));
        { align first, because we need the size for the fixups of the symbol reloc }
        if lfnstr.size>0 then
         lfnstr.align(2);
        if symreloc.size>0 then
         begin
           symstr.align(2);
           fixup:=12+sizeof(tarhdr)+symreloc.size+symstr.size;
           if lfnstr.size>0 then
            inc(fixup,lfnstr.size+sizeof(tarhdr));
           relocs:=symreloc.size div 4;
           { fixup relocs }
           for i:=0to relocs-1 do
            begin
              symreloc.seek(i*4);
              symreloc.read(l,4);
              symreloc.seek(i*4);
              l:=lsb2msb(l+fixup);
              symreloc.write(l,4);
            end;
           createarhdr('',4+symreloc.size+symstr.size,'0','0','0');
           arf.Write(arhdr,sizeof(tarhdr));
           relocs:=lsb2msb(relocs);
           arf.Write(relocs,4);
           symreloc.WriteStream(arf);
           symstr.WriteStream(arf);
         end;
        if lfnstr.size>0 then
         begin
           createarhdr('/',lfnstr.size,'','','');
           arf.Write(arhdr,sizeof(tarhdr));
           lfnstr.WriteStream(arf);
         end;
        ardata.WriteStream(arf);
        Arf.Free;
      end;


{*****************************************************************************
                                TArObjectReader
*****************************************************************************}


    constructor tarobjectreader.create(const Aarfn:string;allow_nonar:boolean);
      var
        magic:array[0..sizeof(armagic)-1] of char;
      begin
        inherited Create;
        ArSymbols:=TFPHashObjectList.Create(true);
        CurrMemberPos:=0;
        CurrMemberSize:=0;
        CurrMemberName:='';
        if inherited openfile(Aarfn) then
          begin
            Read(magic,sizeof(armagic));
            isar:=(CompareByte(magic,armagic,sizeof(armagic))=0);
            if isar then
              ReadArchive
            else if (not allow_nonar) then
              Comment(V_Error,'Not a ar file, illegal magic: '+filename);
            Seek(0);
          end;
      end;


    destructor  tarobjectreader.destroy;
      begin
        inherited closefile;
        ArSymbols.destroy;
        if assigned(LFNStrs) then
          FreeMem(LFNStrs);
        inherited Destroy;
      end;


    function tarobjectreader.getfilename : string;
      begin
        result:=inherited getfilename;
        if CurrMemberName<>'' then
          result:=result+'('+CurrMemberName+')';
      end;


    function tarobjectreader.DecodeMemberName(ahdr:TArHdr):string;
      var
        hs : string;
        code : integer;
        hsp,
        p : pchar;
        lfnidx : longint;
      begin
        result:='';
        p:=@ahdr.name[0];
        hsp:=@hs[1];
        while (p^<>' ') and (hsp-@hs[1]<16) do
          begin
            hsp^:=p^;
            inc(p);
            inc(hsp);
          end;
        hs[0]:=chr(hsp-@hs[1]);
        if (hs[1]='/') and (hs[2] in ['0'..'9']) then
          begin
            Delete(hs,1,1);
            val(hs,lfnidx,code);
            if (lfnidx<0) or (lfnidx>=LFNSize) then
              begin
                Comment(V_Error,'Invalid ar member lfn name index in '+filename);
                exit;
              end;
            p:=@LFNStrs[lfnidx];
            hsp:=@result[1];
            while p^<>#10 do
              begin
                hsp^:=p^;
                inc(p);
                inc(hsp);
              end;
            result[0]:=chr(hsp-@result[1]);
          end
        else
          result:=hs;
        { Strip ending / }
        if result[length(result)]='/' then
         dec(result[0]);
      end;


    function tarobjectreader.DecodeMemberSize(ahdr:TArHdr):longint;
      var
        hs : string;
        code : integer;
        hsp,
        p : pchar;
      begin
        p:=@ahdr.size[0];
        hsp:=@hs[1];
        while p^<>' ' do
          begin
            hsp^:=p^;
            inc(p);
            inc(hsp);
          end;
        hs[0]:=chr(hsp-@hs[1]);
        val(hs,result,code);
        if result<=0 then
          Comment(V_Error,'Invalid ar member size in '+filename);
      end;


    procedure tarobjectreader.ReadArchive;
      var
        currarhdr   : tarhdr;
        nrelocs,
        relocidx,
        currfilesize,
        relocsize,
        symsize     : longint;
        arsym       : TArSymbol;
        s           : string;
        syms,
        currp,
        endp,
        startp      : pchar;
        relocs      : plongint;
      begin
        Read(currarhdr,sizeof(currarhdr));
        { Read number of relocs }
        Read(nrelocs,sizeof(nrelocs));
        nrelocs:=lsb2msb(nrelocs);
        { Calculate sizes }
        currfilesize:=DecodeMemberSize(currarhdr);
        relocsize:=nrelocs*4;
        symsize:=currfilesize-relocsize-4;
        if symsize<0 then
          begin
            Comment(V_Error,'Illegal symtable in ar file '+filename);
            exit;
          end;
        { Read relocs }
        getmem(Relocs,relocsize);
        Read(relocs^,relocsize);
        { Read symbols, force terminating #0 to prevent overflow }
        getmem(syms,symsize+1);
        syms[symsize]:=#0;
        Read(syms^,symsize);
        { Parse symbols }
        relocidx:=0;
        currp:=syms;
        endp:=syms+symsize;
        for relocidx:=0 to nrelocs-1 do
          begin
            startp:=currp;
            while (currp^<>#0) do
              inc(currp);
            s[0]:=chr(currp-startp);
            move(startp^,s[1],byte(s[0]));
            arsym:=TArSymbol.create(ArSymbols,s);
            arsym.MemberPos:=lsb2msb(relocs[relocidx]);
            inc(currp);
            if currp>endp then
              begin
                Comment(V_Error,'Illegal symtable in ar file '+filename);
                break;
              end;
          end;
        freemem(relocs);
        freemem(syms);
        { LFN names }
        Read(currarhdr,sizeof(currarhdr));
        if DecodeMemberName(currarhdr)='/' then
          begin
            lfnsize:=DecodeMemberSize(currarhdr);
            getmem(lfnstrs,lfnsize);
            Read(lfnstrs^,lfnsize);
          end;
      end;


    function  tarobjectreader.openfile(const fn:string):boolean;
      var
        arsym : TArSymbol;
        arhdr : TArHdr;
      begin
        result:=false;
        arsym:=TArSymbol(ArSymbols.Find(fn));
        if not assigned(arsym) then
          exit;
        inherited Seek(arsym.MemberPos);
        Read(arhdr,sizeof(arhdr));
        CurrMemberName:=DecodeMemberName(arhdr);
        CurrMemberSize:=DecodeMemberSize(arhdr);
        CurrMemberPos:=arsym.MemberPos+sizeof(arhdr);
        result:=true;
      end;


    procedure tarobjectreader.closefile;
      begin
        CurrMemberPos:=0;
        CurrMemberSize:=0;
        CurrMemberName:='';
      end;


    procedure tarobjectreader.seek(len:longint);
      begin
        inherited Seek(CurrMemberPos+len);
      end;

end.
