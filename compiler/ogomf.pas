{
    Copyright (c) 2015 by Nikolay Nikolov

    Contains the binary Relocatable Object Module Format (OMF) reader and writer
    This is the object format used on the i8086-msdos platform.

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
unit ogomf;

{$i fpcdefs.inc}

interface

    uses
       { common }
       cclasses,globtype,
       { target }
       systems,
       { assembler }
       cpuinfo,cpubase,aasmbase,assemble,link,
       { OMF definitions }
       omfbase,
       { output }
       ogbase,
       owbase;

    type

      { TOmfRelocation }

      TOmfRelocation = class(TObjRelocation)
      private
        FOmfFixup: TOmfSubRecord_FIXUP;
        function GetGroupIndex(const groupname: string): Integer;
      public
        constructor CreateSection(ADataOffset:aword;aobjsec:TObjSection;Atyp:TObjRelocationType);
        destructor Destroy; override;

        procedure BuildOmfFixup;

        property OmfFixup: TOmfSubRecord_FIXUP read FOmfFixup;
      end;

      { TOmfObjSection }

      TOmfObjSection = class(TObjSection)
      private
        FClassName: string;
        FOverlayName: string;
        FCombination: TOmfSegmentCombination;
        FUse: TOmfSegmentUse;
        FPrimaryGroup: string;
        function GetOmfAlignment: TOmfSegmentAlignment;
      public
        constructor create(AList:TFPHashObjectList;const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);override;
        property ClassName: string read FClassName;
        property OverlayName: string read FOverlayName;
        property OmfAlignment: TOmfSegmentAlignment read GetOmfAlignment;
        property Combination: TOmfSegmentCombination read FCombination;
        property Use: TOmfSegmentUse read FUse;
        property PrimaryGroup: string read FPrimaryGroup;
      end;

      { TOmfObjData }

      TOmfObjData = class(TObjData)
      private
        class function CodeSectionName(const aname:string): string;
      public
        constructor create(const n:string);override;
        function sectiontype2align(atype:TAsmSectiontype):shortint;override;
        function sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;override;
        procedure writeReloc(Data:aint;len:aword;p:TObjSymbol;Reloctype:TObjRelocationType);override;
      end;

      { TOmfObjOutput }

      TOmfObjOutput = class(tObjOutput)
      private
        FLNames: TOmfOrderedNameCollection;
        FSegments: TFPHashObjectList;
        FGroups: TFPHashObjectList;
        procedure AddSegment(const name,segclass,ovlname: string;
          Alignment: TOmfSegmentAlignment; Combination: TOmfSegmentCombination;
          Use: TOmfSegmentUse; Size: aword);
        procedure AddGroup(const groupname: string; seglist: array of const);
        procedure AddGroup(const groupname: string; seglist: TSegmentList);
        procedure WriteSections(Data:TObjData);
        procedure WriteSectionContentAndFixups(sec: TObjSection);

        procedure section_count_sections(p:TObject;arg:pointer);
        procedure WritePUBDEFs(Data: TObjData);
        procedure WriteEXTDEFs(Data: TObjData);

        property LNames: TOmfOrderedNameCollection read FLNames;
        property Segments: TFPHashObjectList read FSegments;
        property Groups: TFPHashObjectList read FGroups;
      protected
        function writeData(Data:TObjData):boolean;override;
      public
        constructor create(AWriter:TObjectWriter);override;
        destructor Destroy;override;
      end;

      { TOmfObjInput }

      TOmfObjInput = class(TObjInput)
      private
        FLNames: TOmfOrderedNameCollection;
        FExtDefs: TFPHashObjectList;
        FPubDefs: TFPHashObjectList;
        FRawRecord: TOmfRawRecord;

        function PeekNextRecordType: Byte;

        function ReadLNames(RawRec: TOmfRawRecord): Boolean;
        function ReadSegDef(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;
        function ReadGrpDef(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;
        function ReadExtDef(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;
        function ReadPubDef(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;
        function ReadLEDataAndFixups(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;

        property LNames: TOmfOrderedNameCollection read FLNames;
        property ExtDefs: TFPHashObjectList read FExtDefs;
        property PubDefs: TFPHashObjectList read FPubDefs;
      public
        constructor create;override;
        destructor destroy;override;
        class function CanReadObjData(AReader:TObjectreader):boolean;override;
        function ReadObjData(AReader:TObjectreader;out objdata:TObjData):boolean;override;
      end;

      { TMZExeRelocation }

      TMZExeRelocation = record
        offset: Word;
        segment: Word;
      end;
      TMZExeRelocations = array of TMZExeRelocation;
      TMZExeExtraHeaderData = array of Byte;

      { TMZExeHeader }

      TMZExeHeader = class
      private
        FChecksum: Word;
        FExtraHeaderData: TMZExeExtraHeaderData;
        FHeaderSizeAlignment: Integer;
        FInitialCS: Word;
        FInitialIP: Word;
        FInitialSP: Word;
        FInitialSS: Word;
        FLoadableImageSize: DWord;
        FMaxExtraParagraphs: Word;
        FMinExtraParagraphs: Word;
        FOverlayNumber: Word;
        FRelocations: TMZExeRelocations;
        procedure SetHeaderSizeAlignment(AValue: Integer);
      public
        constructor Create;
        procedure WriteTo(aWriter: TObjectWriter);
        property HeaderSizeAlignment: Integer read FHeaderSizeAlignment write SetHeaderSizeAlignment; {default=16, must be multiple of 16}
        property Relocations: TMZExeRelocations read FRelocations write FRelocations;
        property ExtraHeaderData: TMZExeExtraHeaderData read FExtraHeaderData write FExtraHeaderData;
        property LoadableImageSize: DWord read FLoadableImageSize write FLoadableImageSize;
        property MinExtraParagraphs: Word read FMinExtraParagraphs write FMinExtraParagraphs;
        property MaxExtraParagraphs: Word read FMaxExtraParagraphs write FMaxExtraParagraphs;
        property InitialSS: Word read FInitialSS write FInitialSS;
        property InitialSP: Word read FInitialSP write FInitialSP;
        property Checksum: Word read FChecksum write FChecksum;
        property InitialIP: Word read FInitialIP write FInitialIP;
        property InitialCS: Word read FInitialCS write FInitialCS;
        property OverlayNumber: Word read FOverlayNumber write FOverlayNumber;
      end;

      { TMZExeOutput }

      TMZExeOutput = class(TExeOutput)
      protected
        function writeData:boolean;override;
      public
        constructor create;override;
      end;

      TOmfAssembler = class(tinternalassembler)
        constructor create(smart:boolean);override;
      end;

implementation

    uses
       SysUtils,
       cutils,verbose,globals,
       fmodule,aasmtai,aasmdata,
       ogmap,owomflib,
       version
       ;

{****************************************************************************
                                TOmfRelocation
****************************************************************************}

    function TOmfRelocation.GetGroupIndex(const groupname: string): Integer;
      begin
        if groupname='dgroup' then
          Result:=1
        else
          internalerror(2014040703);
      end;

    constructor TOmfRelocation.CreateSection(ADataOffset: aword; aobjsec: TObjSection; Atyp: TObjRelocationType);
      begin
        if not (Atyp in [RELOC_DGROUP,RELOC_DGROUPREL]) and not assigned(aobjsec) then
          internalerror(200603036);
        DataOffset:=ADataOffset;
        Symbol:=nil;
        OrgSize:=0;
        ObjSection:=aobjsec;
        ftype:=ord(Atyp);
      end;

    destructor TOmfRelocation.Destroy;
      begin
        FOmfFixup.Free;
        inherited Destroy;
      end;

    procedure TOmfRelocation.BuildOmfFixup;
      begin
        FreeAndNil(FOmfFixup);
        FOmfFixup:=TOmfSubRecord_FIXUP.Create;
        if ObjSection<>nil then
          begin
            FOmfFixup.LocationOffset:=DataOffset;
            if typ in [RELOC_ABSOLUTE,RELOC_RELATIVE] then
              FOmfFixup.LocationType:=fltOffset
            else if typ in [RELOC_SEG,RELOC_SEGREL] then
              FOmfFixup.LocationType:=fltBase
            else
              internalerror(2015041501);
            FOmfFixup.FrameDeterminedByThread:=False;
            FOmfFixup.TargetDeterminedByThread:=False;
            if typ in [RELOC_ABSOLUTE,RELOC_SEG] then
              FOmfFixup.Mode:=fmSegmentRelative
            else if typ in [RELOC_RELATIVE,RELOC_SEGREL] then
              FOmfFixup.Mode:=fmSelfRelative
            else
              internalerror(2015041401);
            if typ in [RELOC_ABSOLUTE,RELOC_RELATIVE] then
              begin
                FOmfFixup.TargetMethod:=ftmSegmentIndexNoDisp;
                FOmfFixup.TargetDatum:=ObjSection.Index;
                if TOmfObjSection(ObjSection).PrimaryGroup<>'' then
                  begin
                    FOmfFixup.FrameMethod:=ffmGroupIndex;
                    FOmfFixup.FrameDatum:=GetGroupIndex(TOmfObjSection(ObjSection).PrimaryGroup);
                  end
                else
                  FOmfFixup.FrameMethod:=ffmTarget;
              end
            else
              begin
                FOmfFixup.FrameMethod:=ffmTarget;
                if TOmfObjSection(ObjSection).PrimaryGroup<>'' then
                  begin
                    FOmfFixup.TargetMethod:=ftmGroupIndexNoDisp;
                    FOmfFixup.TargetDatum:=GetGroupIndex(TOmfObjSection(ObjSection).PrimaryGroup);
                  end
                else
                  begin
                    FOmfFixup.TargetMethod:=ftmSegmentIndexNoDisp;
                    FOmfFixup.TargetDatum:=ObjSection.Index;
                  end;
              end;
          end
        else if symbol<>nil then
          begin
            FOmfFixup.LocationOffset:=DataOffset;
            if typ in [RELOC_ABSOLUTE,RELOC_RELATIVE] then
              FOmfFixup.LocationType:=fltOffset
            else if typ in [RELOC_SEG,RELOC_SEGREL] then
              FOmfFixup.LocationType:=fltBase
            else
              internalerror(2015041501);
            FOmfFixup.FrameDeterminedByThread:=False;
            FOmfFixup.TargetDeterminedByThread:=False;
            if typ in [RELOC_ABSOLUTE,RELOC_SEG] then
              FOmfFixup.Mode:=fmSegmentRelative
            else if typ in [RELOC_RELATIVE,RELOC_SEGREL] then
              FOmfFixup.Mode:=fmSelfRelative
            else
              internalerror(2015041401);
            FOmfFixup.TargetMethod:=ftmExternalIndexNoDisp;
            FOmfFixup.TargetDatum:=symbol.symidx;
            FOmfFixup.FrameMethod:=ffmTarget;
          end
        else if typ in [RELOC_DGROUP,RELOC_DGROUPREL] then
          begin
            FOmfFixup.LocationOffset:=DataOffset;
            FOmfFixup.LocationType:=fltBase;
            FOmfFixup.FrameDeterminedByThread:=False;
            FOmfFixup.TargetDeterminedByThread:=False;
            if typ=RELOC_DGROUP then
              FOmfFixup.Mode:=fmSegmentRelative
            else if typ=RELOC_DGROUPREL then
              FOmfFixup.Mode:=fmSelfRelative
            else
              internalerror(2015041401);
            FOmfFixup.FrameMethod:=ffmTarget;
            FOmfFixup.TargetMethod:=ftmGroupIndexNoDisp;
            FOmfFixup.TargetDatum:=GetGroupIndex('dgroup');
          end
        else
         internalerror(2015040702);
      end;

{****************************************************************************
                                TOmfObjSection
****************************************************************************}

    function TOmfObjSection.GetOmfAlignment: TOmfSegmentAlignment;
      begin
        case SecAlign of
          1:
            result:=saRelocatableByteAligned;
          2:
            result:=saRelocatableWordAligned;
          4:
            result:=saRelocatableDWordAligned;
          16:
            result:=saRelocatableParaAligned;
          else
            internalerror(2015041504);
        end;
      end;

    constructor TOmfObjSection.create(AList: TFPHashObjectList;
          const Aname: string; Aalign: shortint; Aoptions: TObjSectionOptions);
      var
        dgroup: Boolean;
      begin
        inherited create(AList, Aname, Aalign, Aoptions);
        FCombination:=scPublic;
        FUse:=suUse16;
        if oso_executable in Aoptions then
          begin
            FClassName:='code';
            dgroup:=(current_settings.x86memorymodel=mm_tiny);
          end
        else if Aname='stack' then
          begin
            FClassName:='stack';
            FCombination:=scStack;
            dgroup:=current_settings.x86memorymodel in (x86_near_data_models-[mm_tiny]);
          end
        else if Aname='heap' then
          begin
            FClassName:='heap';
            dgroup:=current_settings.x86memorymodel in x86_near_data_models;
          end
        else if Aname='bss' then
          begin
            FClassName:='bss';
            dgroup:=true;
          end
        else if Aname='data' then
          begin
            FClassName:='data';
            dgroup:=true;
          end
        else if (Aname='debug_frame') or
                (Aname='debug_info') or
                (Aname='debug_line') or
                (Aname='debug_abbrev') then
          begin
            FClassName:='DWARF';
            FUse:=suUse32;
            dgroup:=false;
          end
        else
          begin
            FClassName:='data';
            dgroup:=true;
          end;
        if dgroup then
          FPrimaryGroup:='dgroup'
        else
          FPrimaryGroup:='';
      end;

{****************************************************************************
                                TOmfObjData
****************************************************************************}

    class function TOmfObjData.CodeSectionName(const aname: string): string;
      begin
{$ifdef i8086}
        if current_settings.x86memorymodel in x86_far_code_models then
          begin
            if cs_huge_code in current_settings.moduleswitches then
              result:=aname + '_TEXT'
            else
              result:=current_module.modulename^ + '_TEXT';
          end
        else
{$endif}
          result:='text';
      end;

    constructor TOmfObjData.create(const n: string);
      begin
        inherited create(n);
        CObjSection:=TOmfObjSection;
      end;

    function TOmfObjData.sectiontype2align(atype: TAsmSectiontype): shortint;
      begin
        case atype of
          sec_stabstr:
            result:=1;
          sec_code:
            result:=1;
          sec_data,
          sec_rodata,
          sec_rodata_norel,
          sec_bss:
            result:=2;
          { For idata (at least idata2) it must be 4 bytes, because
            an entry is always (also in win64) 20 bytes and aligning
            on 8 bytes will insert 4 bytes between the entries resulting
            in a corrupt idata section.
            Same story with .pdata, it has 4-byte elements which should
            be packed without gaps. }
          sec_idata2,sec_idata4,sec_idata5,sec_idata6,sec_idata7,sec_pdata:
            result:=4;
          sec_debug_frame,sec_debug_info,sec_debug_line,sec_debug_abbrev:
            result:=4;
          sec_stack,
          sec_heap:
            result:=16;
          else
            result:=1;
        end;
      end;

    function TOmfObjData.sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;
      const
        secnames : array[TAsmSectiontype] of string[length('__DATA, __datacoal_nt,coalesced')] = ('','',
          'text',
          'data',
          'data',
          'rodata',
          'bss',
          'tbss',
          'pdata',
          'text','data','data','data','data',
          'stab',
          'stabstr',
          'idata2','idata4','idata5','idata6','idata7','edata',
          'eh_frame',
          'debug_frame','debug_info','debug_line','debug_abbrev',
          'fpc',
          '',
          'init',
          'fini',
          'objc_class',
          'objc_meta_class',
          'objc_cat_cls_meth',
          'objc_cat_inst_meth',
          'objc_protocol',
          'objc_string_object',
          'objc_cls_meth',
          'objc_inst_meth',
          'objc_cls_refs',
          'objc_message_refs',
          'objc_symbols',
          'objc_category',
          'objc_class_vars',
          'objc_instance_vars',
          'objc_module_info',
          'objc_class_names',
          'objc_meth_var_types',
          'objc_meth_var_names',
          'objc_selector_strs',
          'objc_protocol_ext',
          'objc_class_ext',
          'objc_property',
          'objc_image_info',
          'objc_cstring_object',
          'objc_sel_fixup',
          '__DATA,__objc_data',
          '__DATA,__objc_const',
          'objc_superrefs',
          '__DATA, __datacoal_nt,coalesced',
          'objc_classlist',
          'objc_nlclasslist',
          'objc_catlist',
          'obcj_nlcatlist',
          'objc_protolist',
          'stack',
          'heap'
        );
      begin
        if (atype=sec_user) then
          Result:=aname
        else if secnames[atype]='text' then
          Result:=CodeSectionName(aname)
        else
          Result:=secnames[atype];
      end;

    procedure TOmfObjData.writeReloc(Data:aint;len:aword;p:TObjSymbol;Reloctype:TObjRelocationType);
      var
        objreloc: TOmfRelocation;
        symaddr: AWord;
      begin
        { RELOC_FARPTR = RELOC_ABSOLUTE+RELOC_SEG }
        if Reloctype=RELOC_FARPTR then
          begin
            if len<>4 then
              internalerror(2015041502);
            writeReloc(Data,2,p,RELOC_ABSOLUTE);
            writeReloc(0,2,p,RELOC_SEG);
            exit;
          end;

        if CurrObjSec=nil then
          internalerror(200403072);
        objreloc:=nil;
        if assigned(p) then
          begin
            { real address of the symbol }
            symaddr:=p.address;

            if p.bind=AB_EXTERNAL then
              begin
                objreloc:=TOmfRelocation.CreateSymbol(CurrObjSec.Size,p,Reloctype);
                CurrObjSec.ObjRelocations.Add(objreloc);
              end
            { relative relocations within the same section can be calculated directly,
              without the need to emit a relocation entry }
            else if (p.objsection=CurrObjSec) and
                    (p.bind<>AB_COMMON) and
                    (Reloctype=RELOC_RELATIVE) then
              begin
                data:=data+symaddr-len-CurrObjSec.Size;
              end
            else
              begin
                objreloc:=TOmfRelocation.CreateSection(CurrObjSec.Size,p.objsection,Reloctype);
                CurrObjSec.ObjRelocations.Add(objreloc);
                if not (Reloctype in [RELOC_SEG,RELOC_SEGREL]) then
                  inc(data,symaddr);
              end;
          end
        else if Reloctype in [RELOC_DGROUP,RELOC_DGROUPREL] then
            begin
              objreloc:=TOmfRelocation.CreateSection(CurrObjSec.Size,nil,Reloctype);
              CurrObjSec.ObjRelocations.Add(objreloc);
            end;
        CurrObjSec.write(data,len);
      end;

{****************************************************************************
                                TOmfObjOutput
****************************************************************************}

    procedure TOmfObjOutput.AddSegment(const name, segclass, ovlname: string;
        Alignment: TOmfSegmentAlignment; Combination: TOmfSegmentCombination;
        Use: TOmfSegmentUse; Size: aword);
      var
        s: TOmfRecord_SEGDEF;
      begin
        s:=TOmfRecord_SEGDEF.Create;
        Segments.Add(name,s);
        s.SegmentNameIndex:=LNames.Add(name);
        s.ClassNameIndex:=LNames.Add(segclass);
        s.OverlayNameIndex:=LNames.Add(ovlname);
        s.Alignment:=Alignment;
        s.Combination:=Combination;
        s.Use:=Use;
        s.SegmentLength:=Size;
      end;

    procedure TOmfObjOutput.AddGroup(const groupname: string; seglist: array of const);
      var
        g: TOmfRecord_GRPDEF;
        I: Integer;
        SegListStr: TSegmentList;
      begin
        g:=TOmfRecord_GRPDEF.Create;
        Groups.Add(groupname,g);
        g.GroupNameIndex:=LNames.Add(groupname);
        SetLength(SegListStr,Length(seglist));
        for I:=0 to High(seglist) do
          begin
            case seglist[I].VType of
              vtString:
                SegListStr[I]:=Segments.FindIndexOf(seglist[I].VString^);
              vtAnsiString:
                SegListStr[I]:=Segments.FindIndexOf(AnsiString(seglist[I].VAnsiString));
              vtWideString:
                SegListStr[I]:=Segments.FindIndexOf(AnsiString(WideString(seglist[I].VWideString)));
              vtUnicodeString:
                SegListStr[I]:=Segments.FindIndexOf(AnsiString(UnicodeString(seglist[I].VUnicodeString)));
              else
                internalerror(2015040402);
            end;
          end;
        g.SegmentList:=SegListStr;
      end;

    procedure TOmfObjOutput.AddGroup(const groupname: string; seglist: TSegmentList);
      var
        g: TOmfRecord_GRPDEF;
      begin
        g:=TOmfRecord_GRPDEF.Create;
        Groups.Add(groupname,g);
        g.GroupNameIndex:=LNames.Add(groupname);
        g.SegmentList:=Copy(seglist);
      end;

    procedure TOmfObjOutput.WriteSections(Data: TObjData);
      var
        i:longint;
        sec:TObjSection;
      begin
        for i:=0 to Data.ObjSectionList.Count-1 do
          begin
            sec:=TObjSection(Data.ObjSectionList[i]);
            WriteSectionContentAndFixups(sec);
          end;
      end;

    procedure TOmfObjOutput.WriteSectionContentAndFixups(sec: TObjSection);
      const
        MaxChunkSize=$3fa;
      var
        RawRecord: TOmfRawRecord;
        ChunkStart,ChunkLen: DWord;
        ChunkFixupStart,ChunkFixupEnd: Integer;
        SegIndex: Integer;
        NextOfs: Integer;
        I: Integer;
      begin
        if (oso_data in sec.SecOptions) then
          begin
            if sec.Data=nil then
              internalerror(200403073);
            for I:=0 to sec.ObjRelocations.Count-1 do
              TOmfRelocation(sec.ObjRelocations[I]).BuildOmfFixup;
            SegIndex:=Segments.FindIndexOf(sec.Name);
            RawRecord:=TOmfRawRecord.Create;
            sec.data.seek(0);
            ChunkFixupStart:=0;
            ChunkFixupEnd:=-1;
            ChunkStart:=0;
            ChunkLen:=Min(MaxChunkSize, sec.Data.size-ChunkStart);
            while ChunkLen>0 do
            begin
              { find last fixup in the chunk }
              while (ChunkFixupEnd<(sec.ObjRelocations.Count-1)) and
                    (TOmfRelocation(sec.ObjRelocations[ChunkFixupEnd+1]).DataOffset<(ChunkStart+ChunkLen)) do
                inc(ChunkFixupEnd);
              { check if last chunk is crossing the chunk boundary, and trim ChunkLen if necessary }
              if (ChunkFixupEnd>=ChunkFixupStart) and
                ((TOmfRelocation(sec.ObjRelocations[ChunkFixupEnd]).DataOffset+
                  TOmfRelocation(sec.ObjRelocations[ChunkFixupEnd]).OmfFixup.LocationSize)>(ChunkStart+ChunkLen)) then
                begin
                  ChunkLen:=TOmfRelocation(sec.ObjRelocations[ChunkFixupEnd]).DataOffset-ChunkStart;
                  Dec(ChunkFixupEnd);
                end;
              { write LEDATA record }
              RawRecord.RecordType:=RT_LEDATA;
              NextOfs:=RawRecord.WriteIndexedRef(0,SegIndex);
              RawRecord.RawData[NextOfs]:=Byte(ChunkStart);
              RawRecord.RawData[NextOfs+1]:=Byte(ChunkStart shr 8);
              Inc(NextOfs,2);
              sec.data.read(RawRecord.RawData[NextOfs], ChunkLen);
              Inc(NextOfs, ChunkLen);
              RawRecord.RecordLength:=NextOfs+1;
              RawRecord.CalculateChecksumByte;
              RawRecord.WriteTo(FWriter);
              { write FIXUPP record }
              if ChunkFixupEnd>=ChunkFixupStart then
                begin
                  RawRecord.RecordType:=RT_FIXUPP;
                  NextOfs:=0;
                  for I:=ChunkFixupStart to ChunkFixupEnd do
                    begin
                      TOmfRelocation(sec.ObjRelocations[I]).OmfFixup.DataRecordStartOffset:=ChunkStart;
                      NextOfs:=TOmfRelocation(sec.ObjRelocations[I]).OmfFixup.WriteAt(RawRecord,NextOfs);
                    end;
                  RawRecord.RecordLength:=NextOfs+1;
                  RawRecord.CalculateChecksumByte;
                  RawRecord.WriteTo(FWriter);
                end;
              { prepare next chunk }
              Inc(ChunkStart, ChunkLen);
              ChunkLen:=Min(MaxChunkSize, sec.Data.size-ChunkStart);
              ChunkFixupStart:=ChunkFixupEnd+1;
            end;
            RawRecord.Free;
          end;
      end;

    procedure TOmfObjOutput.section_count_sections(p: TObject; arg: pointer);
      begin
        TOmfObjSection(p).index:=pinteger(arg)^;
        inc(pinteger(arg)^);
      end;

    procedure TOmfObjOutput.WritePUBDEFs(Data: TObjData);
      var
        PubNamesForSection: array of TFPHashObjectList;
        i: Integer;
        objsym: TObjSymbol;
        PublicNameElem: TOmfPublicNameElement;
        RawRecord: TOmfRawRecord;
        PubDefRec: TOmfRecord_PUBDEF;
        PrimaryGroupName: string;
      begin
        RawRecord:=TOmfRawRecord.Create;
        SetLength(PubNamesForSection,Data.ObjSectionList.Count);
        for i:=0 to Data.ObjSectionList.Count-1 do
          PubNamesForSection[i]:=TFPHashObjectList.Create;

        for i:=0 to Data.ObjSymbolList.Count-1 do
          begin
            objsym:=TObjSymbol(Data.ObjSymbolList[i]);
            if objsym.bind=AB_GLOBAL then
              begin
                PublicNameElem:=TOmfPublicNameElement.Create(PubNamesForSection[objsym.objsection.index-1],objsym.Name);
                PublicNameElem.PublicOffset:=objsym.offset;
              end;
          end;

        for i:=0 to Data.ObjSectionList.Count-1 do
          if PubNamesForSection[i].Count>0 then
            begin
              PubDefRec:=TOmfRecord_PUBDEF.Create;
              PubDefRec.BaseSegmentIndex:=i+1;
              PrimaryGroupName:=TOmfObjSection(Data.ObjSectionList[i]).PrimaryGroup;
              if PrimaryGroupName<>'' then
                PubDefRec.BaseGroupIndex:=Groups.FindIndexOf(PrimaryGroupName)
              else
                PubDefRec.BaseGroupIndex:=0;
              PubDefRec.PublicNames:=PubNamesForSection[i];
              while PubDefRec.NextIndex<PubDefRec.PublicNames.Count do
                begin
                  PubDefRec.EncodeTo(RawRecord);
                  RawRecord.WriteTo(FWriter);
                end;
              PubDefRec.Free;
            end;

        for i:=0 to Data.ObjSectionList.Count-1 do
          FreeAndNil(PubNamesForSection[i]);
        RawRecord.Free;
      end;

    procedure TOmfObjOutput.WriteEXTDEFs(Data: TObjData);
      var
        ExtNames: TFPHashObjectList;
        RawRecord: TOmfRawRecord;
        i,idx: Integer;
        objsym: TObjSymbol;
        ExternalNameElem: TOmfExternalNameElement;
        ExtDefRec: TOmfRecord_EXTDEF;
      begin
        ExtNames:=TFPHashObjectList.Create;
        RawRecord:=TOmfRawRecord.Create;

        idx:=1;
        for i:=0 to Data.ObjSymbolList.Count-1 do
          begin
            objsym:=TObjSymbol(Data.ObjSymbolList[i]);
            if objsym.bind=AB_EXTERNAL then
              begin
                ExternalNameElem:=TOmfExternalNameElement.Create(ExtNames,objsym.Name);
                objsym.symidx:=idx;
                Inc(idx);
              end;
          end;

        if ExtNames.Count>0 then
          begin
            ExtDefRec:=TOmfRecord_EXTDEF.Create;
            ExtDefRec.ExternalNames:=ExtNames;
            while ExtDefRec.NextIndex<ExtDefRec.ExternalNames.Count do
              begin
                ExtDefRec.EncodeTo(RawRecord);
                RawRecord.WriteTo(FWriter);
              end;
            ExtDefRec.Free;
          end;

        ExtNames.Free;
        RawRecord.Free;
      end;

    function TOmfObjOutput.writeData(Data:TObjData):boolean;
      var
        RawRecord: TOmfRawRecord;
        Header: TOmfRecord_THEADR;
        Translator_COMENT: TOmfRecord_COMENT;
        LinkPassSeparator_COMENT: TOmfRecord_COMENT;
        LNamesRec: TOmfRecord_LNAMES;
        ModEnd: TOmfRecord_MODEND;
        I: Integer;
        SegDef: TOmfRecord_SEGDEF;
        GrpDef: TOmfRecord_GRPDEF;
        DGroupSegments: TSegmentList;
        nsections: Integer;
      begin
        { calc amount of sections we have and set their index, starting with 1 }
        nsections:=1;
        data.ObjSectionList.ForEachCall(@section_count_sections,@nsections);
        { maximum amount of sections supported in the omf format is $7fff }
        if (nsections-1)>$7fff then
          internalerror(2015040701);

        { write header record }
        RawRecord:=TOmfRawRecord.Create;
        Header:=TOmfRecord_THEADR.Create;
        Header.ModuleName:=Data.Name;
        Header.EncodeTo(RawRecord);
        RawRecord.WriteTo(FWriter);
        Header.Free;

        { write translator COMENT header }
        Translator_COMENT:=TOmfRecord_COMENT.Create;
        Translator_COMENT.CommentClass:=CC_Translator;
        Translator_COMENT.CommentString:='FPC '+full_version_string+
        ' ['+date_string+'] for '+target_cpu_string+' - '+target_info.shortname;
        Translator_COMENT.EncodeTo(RawRecord);
        RawRecord.WriteTo(FWriter);
        Translator_COMENT.Free;

        LNames.Clear;
        LNames.Add('');  { insert an empty string, which has index 1 }
        FSegments.Clear;
        FSegments.Add('',nil);
        FGroups.Clear;
        FGroups.Add('',nil);

        for i:=0 to Data.ObjSectionList.Count-1 do
          with TOmfObjSection(Data.ObjSectionList[I]) do
            AddSegment(Name,ClassName,OverlayName,OmfAlignment,Combination,Use,Size);


        { create group "dgroup" }
        SetLength(DGroupSegments,0);
        for i:=0 to Data.ObjSectionList.Count-1 do
          with TOmfObjSection(Data.ObjSectionList[I]) do
            if PrimaryGroup='dgroup' then
              begin
                SetLength(DGroupSegments,Length(DGroupSegments)+1);
                DGroupSegments[High(DGroupSegments)]:=index;
              end;
        AddGroup('dgroup',DGroupSegments);

        { write LNAMES record(s) }
        LNamesRec:=TOmfRecord_LNAMES.Create;
        LNamesRec.Names:=LNames;
        while LNamesRec.NextIndex<=LNames.Count do
          begin
            LNamesRec.EncodeTo(RawRecord);
            RawRecord.WriteTo(FWriter);
          end;
        LNamesRec.Free;

        { write SEGDEF record(s) }
        for I:=1 to Segments.Count-1 do
          begin
            SegDef:=TOmfRecord_SEGDEF(Segments[I]);
            SegDef.EncodeTo(RawRecord);
            RawRecord.WriteTo(FWriter);
          end;

        { write GRPDEF record(s) }
        for I:=1 to Groups.Count-1 do
          begin
            GrpDef:=TOmfRecord_GRPDEF(Groups[I]);
            GrpDef.EncodeTo(RawRecord);
            RawRecord.WriteTo(FWriter);
          end;

        { write PUBDEF record(s) }
        WritePUBDEFs(Data);

        { write EXTDEF record(s) }
        WriteEXTDEFs(Data);

        { write link pass separator }
        LinkPassSeparator_COMENT:=TOmfRecord_COMENT.Create;
        LinkPassSeparator_COMENT.CommentClass:=CC_LinkPassSeparator;
        LinkPassSeparator_COMENT.CommentString:=#1;
        LinkPassSeparator_COMENT.NoList:=True;
        LinkPassSeparator_COMENT.EncodeTo(RawRecord);
        RawRecord.WriteTo(FWriter);
        LinkPassSeparator_COMENT.Free;

        { write section content, interleaved with fixups }
        WriteSections(Data);

        { write MODEND record }
        ModEnd:=TOmfRecord_MODEND.Create;
        ModEnd.EncodeTo(RawRecord);
        RawRecord.WriteTo(FWriter);
        ModEnd.Free;

        RawRecord.Free;
        result:=true;
      end;

    constructor TOmfObjOutput.create(AWriter:TObjectWriter);
      begin
        inherited create(AWriter);
        cobjdata:=TOmfObjData;
        FLNames:=TOmfOrderedNameCollection.Create;
        FSegments:=TFPHashObjectList.Create;
        FSegments.Add('',nil);
        FGroups:=TFPHashObjectList.Create;
        FGroups.Add('',nil);
      end;

    destructor TOmfObjOutput.Destroy;
      begin
        FGroups.Free;
        FSegments.Free;
        FLNames.Free;
        inherited Destroy;
      end;

{****************************************************************************
                               TOmfObjInput
****************************************************************************}

    function TOmfObjInput.PeekNextRecordType: Byte;
      var
        OldPos: LongInt;
      begin
        OldPos:=FReader.Pos;
        if not FReader.read(Result, 1) then
          begin
            InputError('Unexpected end of file');
            Result:=0;
            exit;
          end;
        FReader.seek(OldPos);
      end;

    function TOmfObjInput.ReadLNames(RawRec: TOmfRawRecord): Boolean;
      var
        LNamesRec: TOmfRecord_LNAMES;
      begin
        Result:=False;
        LNamesRec:=TOmfRecord_LNAMES.Create;
        LNamesRec.Names:=LNames;
        LNamesRec.DecodeFrom(RawRec);
        LNamesRec.Free;
        Result:=True;
      end;

    function TOmfObjInput.ReadSegDef(RawRec: TOmfRawRecord; objdata: TObjData): Boolean;
      var
        SegDefRec: TOmfRecord_SEGDEF;
        SegmentName,SegClassName,OverlayName: string;
        SecAlign: ShortInt;
        secoptions: TObjSectionOptions;
        objsec: TOmfObjSection;
      begin
        Result:=False;
        SegDefRec:=TOmfRecord_SEGDEF.Create;
        SegDefRec.DecodeFrom(RawRec);
        if (SegDefRec.SegmentNameIndex<1) or (SegDefRec.SegmentNameIndex>LNames.Count) then
          begin
            InputError('Segment name index out of range');
            SegDefRec.Free;
            exit;
          end;
        SegmentName:=LNames[SegDefRec.SegmentNameIndex];
        if (SegDefRec.ClassNameIndex<1) or (SegDefRec.ClassNameIndex>LNames.Count) then
          begin
            InputError('Segment class name index out of range');
            SegDefRec.Free;
            exit;
          end;
        SegClassName:=LNames[SegDefRec.ClassNameIndex];
        if (SegDefRec.OverlayNameIndex<1) or (SegDefRec.OverlayNameIndex>LNames.Count) then
          begin
            InputError('Segment overlay name index out of range');
            SegDefRec.Free;
            exit;
          end;
        OverlayName:=LNames[SegDefRec.OverlayNameIndex];
        SecAlign:=1; // otherwise warning prohibits compilation
        case SegDefRec.Alignment of
          saRelocatableByteAligned:
            SecAlign:=1;
          saRelocatableWordAligned:
            SecAlign:=2;
          saRelocatableParaAligned:
            SecAlign:=16;
          saRelocatableDWordAligned:
            SecAlign:=4;
          saRelocatablePageAligned:
            begin
              InputError('Page segment alignment not supported');
              SegDefRec.Free;
              exit;
            end;
          saAbsolute:
            begin
              InputError('Absolute segment alignment not supported');
              SegDefRec.Free;
              exit;
            end;
          saNotSupported,
          saNotDefined:
            begin
              InputError('Invalid (unsupported/undefined) OMF segment alignment');
              SegDefRec.Free;
              exit;
            end;
        end;
        secoptions:=[];
        objsec:=TOmfObjSection(objdata.createsection(SegmentName,SecAlign,secoptions,false));
        objsec.FClassName:=SegClassName;
        objsec.FOverlayName:=OverlayName;
        objsec.FCombination:=SegDefRec.Combination;
        objsec.FUse:=SegDefRec.Use;
        if SegDefRec.SegmentLength>High(objsec.Size) then
          begin
            InputError('Segment too large');
            SegDefRec.Free;
            exit;
          end;
        objsec.Size:=SegDefRec.SegmentLength;
        SegDefRec.Free;
        Result:=True;
      end;

    function TOmfObjInput.ReadGrpDef(RawRec: TOmfRawRecord; objdata: TObjData): Boolean;
      var
        GrpDefRec: TOmfRecord_GRPDEF;
        GroupName: string;
        SecGroup: TObjSectionGroup;
        i,SegIndex: Integer;
      begin
        Result:=False;
        GrpDefRec:=TOmfRecord_GRPDEF.Create;
        GrpDefRec.DecodeFrom(RawRec);
        if (GrpDefRec.GroupNameIndex<1) or (GrpDefRec.GroupNameIndex>LNames.Count) then
          begin
            InputError('Group name index out of range');
            GrpDefRec.Free;
            exit;
          end;
        GroupName:=LNames[GrpDefRec.GroupNameIndex];
        SecGroup:=objdata.createsectiongroup(GroupName);
        SetLength(SecGroup.members,Length(GrpDefRec.SegmentList));
        for i:=0 to Length(GrpDefRec.SegmentList)-1 do
          begin
            SegIndex:=GrpDefRec.SegmentList[i];
            if (SegIndex<1) or (SegIndex>objdata.ObjSectionList.Count) then
              begin
                InputError('Segment name index out of range in group definition');
                GrpDefRec.Free;
                exit;
              end;
            SecGroup.members[i]:=TOmfObjSection(objdata.ObjSectionList[SegIndex-1]);
          end;
        GrpDefRec.Free;
        Result:=True;
      end;

    function TOmfObjInput.ReadExtDef(RawRec: TOmfRawRecord; objdata: TObjData): Boolean;
      var
        ExtDefRec: TOmfRecord_EXTDEF;
        ExtDefElem: TOmfExternalNameElement;
        OldCount,NewCount,i: Integer;
        objsym: TObjSymbol;
      begin
        Result:=False;
        ExtDefRec:=TOmfRecord_EXTDEF.Create;
        ExtDefRec.ExternalNames:=ExtDefs;
        OldCount:=ExtDefs.Count;
        ExtDefRec.DecodeFrom(RawRec);
        NewCount:=ExtDefs.Count;
        for i:=OldCount to NewCount-1 do
          begin
            ExtDefElem:=TOmfExternalNameElement(ExtDefs[i]);
            objsym:=objdata.CreateSymbol(ExtDefElem.Name);
            objsym.bind:=AB_EXTERNAL;
            objsym.typ:=AT_FUNCTION;
            objsym.objsection:=nil;
            objsym.offset:=0;
            objsym.size:=0;
          end;
        ExtDefRec.Free;
        Result:=True;
      end;

    function TOmfObjInput.ReadPubDef(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;
      var
        PubDefRec: TOmfRecord_PUBDEF;
        PubDefElem: TOmfPublicNameElement;
        OldCount,NewCount,i: Integer;
        basegroup: TObjSectionGroup;
        objsym: TObjSymbol;
        objsec: TOmfObjSection;
      begin
        Result:=False;
        PubDefRec:=TOmfRecord_PUBDEF.Create;
        PubDefRec.PublicNames:=PubDefs;
        OldCount:=PubDefs.Count;
        PubDefRec.DecodeFrom(RawRec);
        NewCount:=PubDefs.Count;
        if (PubDefRec.BaseGroupIndex<0) or (PubDefRec.BaseGroupIndex>objdata.GroupsList.Count) then
          begin
            InputError('Public symbol''s group name index out of range');
            PubDefRec.Free;
            exit;
          end;
        if PubDefRec.BaseGroupIndex<>0 then
          basegroup:=TObjSectionGroup(objdata.GroupsList[PubDefRec.BaseGroupIndex-1])
        else
          basegroup:=nil;
        if (PubDefRec.BaseSegmentIndex<0) or (PubDefRec.BaseSegmentIndex>objdata.ObjSectionList.Count) then
          begin
            InputError('Public symbol''s segment name index out of range');
            PubDefRec.Free;
            exit;
          end;
        if PubDefRec.BaseSegmentIndex=0 then
          begin
            InputError('Public symbol uses absolute addressing, which is not supported by this linker');
            PubDefRec.Free;
            exit;
          end;
        objsec:=TOmfObjSection(objdata.ObjSectionList[PubDefRec.BaseSegmentIndex-1]);
        for i:=OldCount to NewCount-1 do
          begin
            PubDefElem:=TOmfPublicNameElement(PubDefs[i]);
            objsym:=objdata.CreateSymbol(PubDefElem.Name);
            objsym.bind:=AB_GLOBAL;
            objsym.typ:=AT_FUNCTION;
            objsym.group:=basegroup;
            objsym.objsection:=objsec;
            objsym.offset:=PubDefElem.PublicOffset;
            objsym.size:=0;
          end;
        PubDefRec.Free;
        Result:=True;
      end;

    function TOmfObjInput.ReadLEDataAndFixups(RawRec: TOmfRawRecord; objdata: TObjData): Boolean;
      var
        Is32Bit: Boolean;
        NextOfs: Integer;
        SegmentIndex: Integer;
        EnumeratedDataOffset: DWord;
        BlockLength: Integer;
        objsec: TOmfObjSection;
        FixupRawRec: TOmfRawRecord;
        Fixup: TOmfSubRecord_FIXUP;
      begin
        Result:=False;
        if not (RawRec.RecordType in [RT_LEDATA,RT_LEDATA32]) then
          internalerror(2015040301);
        Is32Bit:=RawRec.RecordType=RT_LEDATA32;
        NextOfs:=RawRec.ReadIndexedRef(0,SegmentIndex);
        if Is32Bit then
          begin
            if (NextOfs+3)>=RawRec.RecordLength then
              internalerror(2015040504);
            EnumeratedDataOffset := RawRec.RawData[NextOfs]+
                                   (RawRec.RawData[NextOfs+1] shl 8)+
                                   (RawRec.RawData[NextOfs+2] shl 16)+
                                   (RawRec.RawData[NextOfs+3] shl 24);
            Inc(NextOfs,4);
          end
        else
          begin
            if (NextOfs+1)>=RawRec.RecordLength then
              internalerror(2015040504);
            EnumeratedDataOffset := RawRec.RawData[NextOfs]+
                                   (RawRec.RawData[NextOfs+1] shl 8);
            Inc(NextOfs,2);
          end;
        BlockLength:=RawRec.RecordLength-NextOfs-1;
        if BlockLength<0 then
          internalerror(2015060501);
        if BlockLength>1024 then
          begin
            InputError('LEDATA contains more than 1024 bytes of data');
            exit;
          end;

        if (SegmentIndex<1) or (SegmentIndex>objdata.ObjSectionList.Count) then
          begin
            InputError('Segment index in LEDATA field is out of range');
            exit;
          end;
        objsec:=TOmfObjSection(objdata.ObjSectionList[SegmentIndex-1]);

        objsec.SecOptions:=objsec.SecOptions+[oso_Data];
        if (objsec.Data.Size<>EnumeratedDataOffset) then
          begin
            InputError('LEDATA enumerated data offset field out of sequence');
            exit;
          end;
        if (EnumeratedDataOffset+BlockLength)>objsec.Size then
          begin
            InputError('LEDATA goes beyond the segment size declared in the SEGDEF record');
            exit;
          end;
        objsec.Data.write(RawRec.RawData[NextOfs],BlockLength);

        { also read all the FIXUPP records that may follow }
        while PeekNextRecordType in [RT_FIXUPP,RT_FIXUPP32] do
          begin
            FixupRawRec:=TOmfRawRecord.Create;
            FixupRawRec.ReadFrom(FReader);
            if not FRawRecord.VerifyChecksumByte then
              begin
                InputError('Invalid checksum in OMF record');
                FixupRawRec.Free;
                exit;
              end;
            NextOfs:=0;
            Fixup:=TOmfSubRecord_FIXUP.Create;
            Fixup.Is32Bit:=FixupRawRec.RecordType=RT_FIXUPP32;
            while NextOfs<(FixupRawRec.RecordLength-1) do
              begin
                NextOfs:=Fixup.ReadAt(FixupRawRec,NextOfs);
                if Fixup.FrameDeterminedByThread or Fixup.TargetDeterminedByThread then
                  begin
                    InputError('Fixups determined by thread not supported');
                    Fixup.Free;
                    FixupRawRec.Free;
                    exit;
                  end;
                {todo: convert the fixup to a TOmfRelocation }
              end;
            Fixup.Free;
            FixupRawRec.Free;
          end;
        Result:=True;
      end;

    constructor TOmfObjInput.create;
      begin
        inherited create;
        cobjdata:=TOmfObjData;
        FLNames:=TOmfOrderedNameCollection.Create;
        FExtDefs:=TFPHashObjectList.Create;
        FPubDefs:=TFPHashObjectList.Create;
        FRawRecord:=TOmfRawRecord.Create;
      end;

    destructor TOmfObjInput.destroy;
      begin
        FRawRecord.Free;
        FPubDefs.Free;
        FExtDefs.Free;
        FLNames.Free;
        inherited destroy;
      end;

    class function TOmfObjInput.CanReadObjData(AReader: TObjectreader): boolean;
      var
        b: Byte;
      begin
        result:=false;
        if AReader.Read(b,sizeof(b)) then
          begin
            if b=RT_THEADR then
            { TODO: check additional fields }
              result:=true;
          end;
        AReader.Seek(0);
      end;

    function TOmfObjInput.ReadObjData(AReader: TObjectreader; out objdata: TObjData): boolean;
      begin
        FReader:=AReader;
        InputFileName:=AReader.FileName;
        objdata:=CObjData.Create(InputFileName);
        result:=false;
        LNames.Clear;
        ExtDefs.Clear;
        FRawRecord.ReadFrom(FReader);
        if not FRawRecord.VerifyChecksumByte then
          begin
            InputError('Invalid checksum in OMF record');
            exit;
          end;
        if FRawRecord.RecordType<>RT_THEADR then
          begin
            InputError('Can''t read OMF header');
            exit;
          end;
        repeat
          FRawRecord.ReadFrom(FReader);
          if not FRawRecord.VerifyChecksumByte then
            begin
              InputError('Invalid checksum in OMF record');
              exit;
            end;
          case FRawRecord.RecordType of
            RT_LNAMES:
              if not ReadLNames(FRawRecord) then
                exit;
            RT_SEGDEF,RT_SEGDEF32:
              if not ReadSegDef(FRawRecord,objdata) then
                exit;
            RT_GRPDEF:
              if not ReadGrpDef(FRawRecord,objdata) then
                exit;
            RT_COMENT:
              begin
                {todo}
              end;
            RT_EXTDEF:
              if not ReadExtDef(FRawRecord,objdata) then
                exit;
            RT_PUBDEF,RT_PUBDEF32:
              if not ReadPubDef(FRawRecord,objdata) then
                exit;
            RT_LEDATA,RT_LEDATA32:
              if not ReadLEDataAndFixups(FRawRecord,objdata) then
                exit;
            RT_LIDATA,RT_LIDATA32:
              begin
                InputError('LIDATA records are not supported');
                exit;
              end;
            RT_FIXUPP,RT_FIXUPP32:
              begin
                InputError('FIXUPP record is invalid, because it does not follow a LEDATA or LIDATA record');
                exit;
              end;
            RT_MODEND,RT_MODEND32:
              begin
                {todo}
              end;
            else
              begin
                InputError('Unsupported OMF record type $'+HexStr(FRawRecord.RecordType,2));
                exit;
              end;
          end;
        until FRawRecord.RecordType in [RT_MODEND,RT_MODEND32];
      end;

{****************************************************************************
                               TMZExeHeader
****************************************************************************}

    procedure TMZExeHeader.SetHeaderSizeAlignment(AValue: Integer);
      begin
        if (AValue<16) or ((AValue mod 16) <> 0) then
          Internalerror(2015060601);
        FHeaderSizeAlignment:=AValue;
      end;

    constructor TMZExeHeader.Create;
      begin
        FHeaderSizeAlignment:=16;
      end;

    procedure TMZExeHeader.WriteTo(aWriter: TObjectWriter);
      var
        NumRelocs: Word;
        HeaderSizeInBytes: DWord;
        HeaderParagraphs: Word;
        RelocTableOffset: Word;
        BytesInLastBlock: Word;
        BlocksInFile: Word;
        HeaderBytes: array [0..$1B] of Byte;
        RelocBytes: array [0..3] of Byte;
        TotalExeSize: DWord;
        i: Integer;
      begin
        NumRelocs:=Length(Relocations);
        RelocTableOffset:=$1C+Length(ExtraHeaderData);
        HeaderSizeInBytes:=Align(RelocTableOffset+4*NumRelocs,16);
        HeaderParagraphs:=HeaderSizeInBytes div 16;
        TotalExeSize:=HeaderSizeInBytes+LoadableImageSize;
        BlocksInFile:=(TotalExeSize+511) div 512;
        BytesInLastBlock:=TotalExeSize mod 512;

        HeaderBytes[$00]:=$4D;  { 'M' }
        HeaderBytes[$01]:=$5A;  { 'Z' }
        HeaderBytes[$02]:=Byte(BytesInLastBlock);
        HeaderBytes[$03]:=Byte(BytesInLastBlock shr 8);
        HeaderBytes[$04]:=Byte(BlocksInFile);
        HeaderBytes[$05]:=Byte(BlocksInFile shr 8);
        HeaderBytes[$06]:=Byte(NumRelocs);
        HeaderBytes[$07]:=Byte(NumRelocs shr 8);
        HeaderBytes[$08]:=Byte(HeaderParagraphs);
        HeaderBytes[$09]:=Byte(HeaderParagraphs shr 8);
        HeaderBytes[$0A]:=Byte(MinExtraParagraphs);
        HeaderBytes[$0B]:=Byte(MinExtraParagraphs shr 8);
        HeaderBytes[$0C]:=Byte(MaxExtraParagraphs);
        HeaderBytes[$0D]:=Byte(MaxExtraParagraphs shr 8);
        HeaderBytes[$0E]:=Byte(InitialSS);
        HeaderBytes[$0F]:=Byte(InitialSS shr 8);
        HeaderBytes[$10]:=Byte(InitialSP);
        HeaderBytes[$11]:=Byte(InitialSP shr 8);
        HeaderBytes[$12]:=Byte(Checksum);
        HeaderBytes[$13]:=Byte(Checksum shr 8);
        HeaderBytes[$14]:=Byte(InitialIP);
        HeaderBytes[$15]:=Byte(InitialIP shr 8);
        HeaderBytes[$16]:=Byte(InitialCS);
        HeaderBytes[$17]:=Byte(InitialCS shr 8);
        HeaderBytes[$18]:=Byte(RelocTableOffset);
        HeaderBytes[$19]:=Byte(RelocTableOffset shr 8);
        HeaderBytes[$1A]:=Byte(OverlayNumber);
        HeaderBytes[$1B]:=Byte(OverlayNumber shr 8);
        aWriter.write(HeaderBytes[0],$1C);
        aWriter.write(ExtraHeaderData[0],Length(ExtraHeaderData));
        for i:=0 to NumRelocs-1 do
          with Relocations[i] do
            begin
              RelocBytes[0]:=Byte(offset);
              RelocBytes[1]:=Byte(offset shr 8);
              RelocBytes[2]:=Byte(segment);
              RelocBytes[3]:=Byte(segment shr 8);
              aWriter.write(RelocBytes[0],4);
            end;
        { pad with zeros until the end of header (paragraph aligned) }
        aWriter.WriteZeros(HeaderSizeInBytes-aWriter.Size);
      end;

{****************************************************************************
                               TMZExeOutput
****************************************************************************}

    function TMZExeOutput.writeData: boolean;
      var
        Header: TMZExeHeader;
      begin
        Result:=False;
        Header:=TMZExeHeader.Create;
        {todo: fill header data}
        Header.WriteTo(FWriter);
        Header.Free;
        Result:=True;
      end;

    constructor TMZExeOutput.create;
      begin
        inherited create;
        CObjData:=TOmfObjData;
      end;

{****************************************************************************
                               TOmfAssembler
****************************************************************************}

    constructor TOmfAssembler.Create(smart:boolean);
      begin
        inherited Create(smart);
        CObjOutput:=TOmfObjOutput;
        CInternalAr:=TOmfLibObjectWriter;
      end;

{*****************************************************************************
                                  Initialize
*****************************************************************************}
{$ifdef i8086}
    const
       as_i8086_omf_info : tasminfo =
          (
            id     : as_i8086_omf;
            idtxt  : 'OMF';
            asmbin : '';
            asmcmd : '';
            supported_targets : [system_i8086_msdos];
            flags : [af_outputbinary,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );
{$endif i8086}

initialization
{$ifdef i8086}
  RegisterAssembler(as_i8086_omf_info,TOmfAssembler);
{$endif i8086}
end.
