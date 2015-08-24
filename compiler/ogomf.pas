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
        FFrameGroup: string;
        FOmfFixup: TOmfSubRecord_FIXUP;
        function GetGroupIndex(const groupname: string): Integer;
      public
        constructor CreateSection(ADataOffset:aword;aobjsec:TObjSection;Atyp:TObjRelocationType);
        destructor Destroy; override;

        procedure BuildOmfFixup;

        property FrameGroup: string read FFrameGroup write FFrameGroup;
        property OmfFixup: TOmfSubRecord_FIXUP read FOmfFixup;
      end;

      TMZExeUnifiedLogicalSegment=class;

      { TOmfObjSection }

      TOmfObjSection = class(TObjSection)
      private
        FClassName: string;
        FOverlayName: string;
        FCombination: TOmfSegmentCombination;
        FUse: TOmfSegmentUse;
        FPrimaryGroup: string;
        FSortOrder: Integer;
        FMZExeUnifiedLogicalSegment: TMZExeUnifiedLogicalSegment;
        function GetOmfAlignment: TOmfSegmentAlignment;
      public
        constructor create(AList:TFPHashObjectList;const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);override;
        function MemPosStr(AImageBase: qword): string;override;
        property ClassName: string read FClassName;
        property OverlayName: string read FOverlayName;
        property OmfAlignment: TOmfSegmentAlignment read GetOmfAlignment;
        property Combination: TOmfSegmentCombination read FCombination;
        property Use: TOmfSegmentUse read FUse;
        property PrimaryGroup: string read FPrimaryGroup;
        property SortOrder: Integer read FSortOrder write FSortOrder;
        property MZExeUnifiedLogicalSegment: TMZExeUnifiedLogicalSegment read FMZExeUnifiedLogicalSegment write FMZExeUnifiedLogicalSegment;
      end;

      { TOmfObjSectionGroup }

      TOmfObjSectionGroup = class(TObjSectionGroup)
      public
        Size,
        MemPos: qword;
        procedure CalcMemPos;
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
        FCaseSensitive: Boolean;

        function PeekNextRecordType: Byte;

        function ReadLNames(RawRec: TOmfRawRecord): Boolean;
        function ReadSegDef(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;
        function ReadGrpDef(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;
        function ReadExtDef(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;
        function ReadPubDef(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;
        function ReadModEnd(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;
        function ReadLEDataAndFixups(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;
        function ImportOmfFixup(objdata: TObjData; objsec: TOmfObjSection; Fixup: TOmfSubRecord_FIXUP): Boolean;

        property LNames: TOmfOrderedNameCollection read FLNames;
        property ExtDefs: TFPHashObjectList read FExtDefs;
        property PubDefs: TFPHashObjectList read FPubDefs;

        { Specifies whether we're case sensitive in regards to segment, class, overlay and group names.
          Symbols (in EXTDEF and PUBDEF records) are always case sensitive, regardless of the value of this property. }
        property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
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

      { TMZExeSection }

      TMZExeSection=class(TExeSection)
      public
        procedure AddObjSection(objsec:TObjSection;ignoreprops:boolean=false);override;
      end;

      { TMZExeUnifiedLogicalSegment }

      TMZExeUnifiedLogicalSegment=class(TFPHashObject)
      private
        FObjSectionList: TFPObjectList;
        FSegName: TSymStr;
        FSegClass: TSymStr;
        FPrimaryGroup: string;
      public
        Size,
        MemPos,
        MemBasePos: qword;
        constructor create(HashObjectList:TFPHashObjectList;const s:TSymStr);
        destructor destroy;override;
        procedure AddObjSection(ObjSec: TOmfObjSection);
        procedure CalcMemPos;
        function MemPosStr:string;
        property ObjSectionList: TFPObjectList read FObjSectionList;
        property SegName: TSymStr read FSegName;
        property SegClass: TSymStr read FSegClass;
        property PrimaryGroup: string read FPrimaryGroup write FPrimaryGroup;
      end;

      { TMZExeUnifiedLogicalGroup }

      TMZExeUnifiedLogicalGroup=class(TFPHashObject)
      private
        FSegmentList: TFPHashObjectList;
      public
        Size,
        MemPos: qword;
        constructor create(HashObjectList:TFPHashObjectList;const s:TSymStr);
        destructor destroy;override;
        procedure CalcMemPos;
        function MemPosStr:string;
        procedure AddSegment(UniSeg: TMZExeUnifiedLogicalSegment);
        property SegmentList: TFPHashObjectList read FSegmentList;
      end;

      { TMZExeOutput }

      TMZExeOutput = class(TExeOutput)
      private
        FMZFlatContentSection: TMZExeSection;
        FExeUnifiedLogicalSegments: TFPHashObjectList;
        FExeUnifiedLogicalGroups: TFPHashObjectList;
        FHeader: TMZExeHeader;
        function GetMZFlatContentSection: TMZExeSection;
        procedure CalcExeUnifiedLogicalSegments;
        procedure CalcExeGroups;
        procedure CalcSegments_MemBasePos;
        procedure WriteMap_SegmentsAndGroups;
        procedure WriteMap_HeaderData;
        procedure FillLoadableImageSize;
        procedure FillStartAddress;
        procedure FillHeaderData;
        function writeExe:boolean;
        function writeCom:boolean;
        property ExeUnifiedLogicalSegments: TFPHashObjectList read FExeUnifiedLogicalSegments;
        property ExeUnifiedLogicalGroups: TFPHashObjectList read FExeUnifiedLogicalGroups;
        property MZFlatContentSection: TMZExeSection read GetMZFlatContentSection;
        property Header: TMZExeHeader read FHeader;
      protected
        procedure Load_Symbol(const aname:string);override;
        procedure DoRelocationFixup(objsec:TObjSection);override;
        procedure Order_ObjSectionList(ObjSectionList : TFPObjectList;const aPattern:string);override;
        procedure MemPos_EndExeSection;override;
        function writeData:boolean;override;
      public
        constructor create;override;
        destructor destroy;override;
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
        if groupname='DGROUP' then
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
            FOmfFixup.TargetDatum:=GetGroupIndex('DGROUP');
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
            FClassName:='CODE';
            dgroup:=(current_settings.x86memorymodel=mm_tiny);
          end
        else if Aname='stack' then
          begin
            FClassName:='STACK';
            FCombination:=scStack;
            dgroup:=current_settings.x86memorymodel in (x86_near_data_models-[mm_tiny]);
          end
        else if Aname='heap' then
          begin
            FClassName:='HEAP';
            dgroup:=current_settings.x86memorymodel in x86_near_data_models;
          end
        else if Aname='bss' then
          begin
            FClassName:='BSS';
            dgroup:=true;
          end
        else if Aname='data' then
          begin
            FClassName:='DATA';
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
            FClassName:='DATA';
            dgroup:=true;
          end;
        if dgroup then
          FPrimaryGroup:='DGROUP'
        else
          FPrimaryGroup:='';
      end;

    function TOmfObjSection.MemPosStr(AImageBase: qword): string;
      begin
        Result:=HexStr(MZExeUnifiedLogicalSegment.MemBasePos shr 4,4)+':'+
          HexStr(MemPos-MZExeUnifiedLogicalSegment.MemBasePos,4);
      end;

{****************************************************************************
                             TOmfObjSectionGroup
****************************************************************************}

    procedure TOmfObjSectionGroup.CalcMemPos;
      var
        MinMemPos: qword=high(qword);
        MaxMemPos: qword=0;
        objsec: TOmfObjSection;
        i: Integer;
      begin
        if Length(members)=0 then
          internalerror(2015082201);
        for i:=low(members) to high(members) do
          begin
            objsec:=TOmfObjSection(members[i]);
            if objsec.MemPos<MinMemPos then
              MinMemPos:=objsec.MemPos;
            if (objsec.MemPos+objsec.Size)>MaxMemPos then
              MaxMemPos:=objsec.MemPos+objsec.Size;
          end;
        { align *down* on a paragraph boundary }
        MemPos:=(MinMemPos shr 4) shl 4;
        Size:=MaxMemPos-MemPos;
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
        CObjSectionGroup:=TOmfObjSectionGroup;
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


        { create group "DGROUP" }
        SetLength(DGroupSegments,0);
        for i:=0 to Data.ObjSectionList.Count-1 do
          with TOmfObjSection(Data.ObjSectionList[I]) do
            if PrimaryGroup='DGROUP' then
              begin
                SetLength(DGroupSegments,Length(DGroupSegments)+1);
                DGroupSegments[High(DGroupSegments)]:=index;
              end;
        AddGroup('DGROUP',DGroupSegments);

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
        if not CaseSensitive then
          begin
            SegmentName:=UpCase(SegmentName);
            SegClassName:=UpCase(SegClassName);
            OverlayName:=UpCase(OverlayName);
          end;
        secoptions:=[];
        objsec:=TOmfObjSection(objdata.createsection(SegmentName+'||'+SegClassName,SecAlign,secoptions,false));
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
        if not CaseSensitive then
          GroupName:=UpCase(GroupName);
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

    function TOmfObjInput.ReadModEnd(RawRec: TOmfRawRecord; objdata:TObjData): Boolean;
      var
        ModEndRec: TOmfRecord_MODEND;
        objsym: TObjSymbol;
        objsec: TOmfObjSection;
        basegroup: TObjSectionGroup;
      begin
        Result:=False;
        ModEndRec:=TOmfRecord_MODEND.Create;
        ModEndRec.DecodeFrom(RawRec);
        if ModEndRec.HasStartAddress then
          begin
            if not ModEndRec.LogicalStartAddress then
              begin
                InputError('Physical start address not supported');
                ModEndRec.Free;
                exit;
              end;
            if not (ModEndRec.TargetMethod in [ftmSegmentIndex,ftmSegmentIndexNoDisp]) then
              begin
                InputError('Target method for start address other than "Segment Index" is not supported');
                ModEndRec.Free;
                exit;
              end;
            if (ModEndRec.TargetDatum<1) or (ModEndRec.TargetDatum>objdata.ObjSectionList.Count) then
              begin
                InputError('Segment name index for start address out of range');
                ModEndRec.Free;
                exit;
              end;
            case ModEndRec.FrameMethod of
              ffmSegmentIndex:
                begin
                  if (ModEndRec.FrameDatum<1) or (ModEndRec.FrameDatum>objdata.ObjSectionList.Count) then
                    begin
                      InputError('Frame segment name index for start address out of range');
                      ModEndRec.Free;
                      exit;
                    end;
                  if ModEndRec.FrameDatum<>ModEndRec.TargetDatum then
                    begin
                      InputError('Frame segment different than target segment is not supported supported for start address');
                      ModEndRec.Free;
                      exit;
                    end;
                  basegroup:=nil;
                end;
              ffmGroupIndex:
                begin
                  if (ModEndRec.FrameDatum<1) or (ModEndRec.FrameDatum>objdata.GroupsList.Count) then
                    begin
                      InputError('Frame group name index for start address out of range');
                      ModEndRec.Free;
                      exit;
                    end;
                  basegroup:=TObjSectionGroup(objdata.GroupsList[ModEndRec.FrameDatum-1]);
                end;
              else
                begin
                  InputError('Frame method for start address other than "Segment Index" or "Group Index" is not supported');
                  ModEndRec.Free;
                  exit;
                end;
            end;
            objsec:=TOmfObjSection(objdata.ObjSectionList[ModEndRec.TargetDatum-1]);

            objsym:=objdata.CreateSymbol('..start');
            objsym.bind:=AB_GLOBAL;
            objsym.typ:=AT_FUNCTION;
            objsym.group:=basegroup;
            objsym.objsection:=objsec;
            objsym.offset:=ModEndRec.TargetDisplacement;
            objsym.size:=0;
          end;
        ModEndRec.Free;
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
        if (objsec.Data.Size>EnumeratedDataOffset) then
          begin
            InputError('LEDATA enumerated data offset field out of sequence');
            exit;
          end;
        if (EnumeratedDataOffset+BlockLength)>objsec.Size then
          begin
            InputError('LEDATA goes beyond the segment size declared in the SEGDEF record');
            exit;
          end;
        objsec.Data.seek(EnumeratedDataOffset);
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
            Fixup.DataRecordStartOffset:=EnumeratedDataOffset;
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
                ImportOmfFixup(objdata,objsec,Fixup);
              end;
            Fixup.Free;
            FixupRawRec.Free;
          end;
        Result:=True;
      end;

    function TOmfObjInput.ImportOmfFixup(objdata: TObjData; objsec: TOmfObjSection; Fixup: TOmfSubRecord_FIXUP): Boolean;
      var
        reloc: TOmfRelocation;
        sym: TObjSymbol;
        RelocType: TObjRelocationType;
      begin
        Result:=False;

        { range check location }
        if (Fixup.LocationOffset+Fixup.LocationSize)>objsec.Size then
          begin
            InputError('Fixup location exceeds the current segment boundary');
            exit;
          end;

        { range check target datum }
        case Fixup.TargetMethod of
          ftmSegmentIndex:
            if (Fixup.TargetDatum<1) or (Fixup.TargetDatum>objdata.ObjSectionList.Count) then
              begin
                InputError('Segment name index in SI(<segment name>),<displacement> fixup target is out of range');
                exit;
              end;
          ftmSegmentIndexNoDisp:
            if (Fixup.TargetDatum<1) or (Fixup.TargetDatum>objdata.ObjSectionList.Count) then
              begin
                InputError('Segment name index in SI(<segment name>) fixup target is out of range');
                exit;
              end;
          ftmGroupIndex:
            if (Fixup.TargetDatum<1) or (Fixup.TargetDatum>objdata.GroupsList.Count) then
              begin
                InputError('Group name index in GI(<group name>),<displacement> fixup target is out of range');
                exit;
              end;
          ftmGroupIndexNoDisp:
            if (Fixup.TargetDatum<1) or (Fixup.TargetDatum>objdata.GroupsList.Count) then
              begin
                InputError('Group name index in GI(<group name>) fixup target is out of range');
                exit;
              end;
          ftmExternalIndex:
            if (Fixup.TargetDatum<1) or (Fixup.TargetDatum>ExtDefs.Count) then
              begin
                InputError('External symbol name index in EI(<symbol name>),<displacement> fixup target is out of range');
                exit;
              end;
          ftmExternalIndexNoDisp:
            if (Fixup.TargetDatum<1) or (Fixup.TargetDatum>ExtDefs.Count) then
              begin
                InputError('External symbol name index in EI(<symbol name>) fixup target is out of range');
                exit;
              end;
        end;

        { range check frame datum }
        case Fixup.FrameMethod of
          ffmSegmentIndex:
            if (Fixup.FrameDatum<1) or (Fixup.FrameDatum>objdata.ObjSectionList.Count) then
              begin
                InputError('Segment name index in SI(<segment name>) fixup frame is out of range');
                exit;
              end;
          ffmGroupIndex:
            if (Fixup.FrameDatum<1) or (Fixup.FrameDatum>objdata.GroupsList.Count) then
              begin
                InputError('Group name index in GI(<group name>) fixup frame is out of range');
                exit;
              end;
          ffmExternalIndex:
            if (Fixup.TargetDatum<1) or (Fixup.TargetDatum>ExtDefs.Count) then
              begin
                InputError('External symbol name index in EI(<symbol name>) fixup frame is out of range');
                exit;
              end;
        end;

        if Fixup.TargetMethod in [ftmExternalIndex,ftmExternalIndexNoDisp] then
          begin
            sym:=objdata.symbolref(TOmfExternalNameElement(ExtDefs[Fixup.TargetDatum-1]).Name);
            case Fixup.LocationType of
              fltOffset:
                case Fixup.Mode of
                  fmSegmentRelative:
                    RelocType:=RELOC_ABSOLUTE;
                  fmSelfRelative:
                    RelocType:=RELOC_RELATIVE;
                end;
              fltBase:
                case Fixup.Mode of
                  fmSegmentRelative:
                    RelocType:=RELOC_SEG;
                  fmSelfRelative:
                    RelocType:=RELOC_SEGREL;
                end;
              else
                begin
                  InputError('Unsupported fixup location type '+IntToStr(Ord(Fixup.LocationType))+' in external reference to '+sym.Name);
                  exit;
                end;
            end;
            reloc:=TOmfRelocation.CreateSymbol(Fixup.LocationOffset,sym,RelocType);
            objsec.ObjRelocations.Add(reloc);
            case Fixup.FrameMethod of
              ffmTarget:
                {nothing};
              ffmGroupIndex:
                reloc.FrameGroup:=TObjSectionGroup(objdata.GroupsList[Fixup.FrameDatum-1]).Name;
              else
                begin
                  InputError('Unsupported frame method '+IntToStr(Ord(Fixup.FrameMethod))+' in external reference to '+sym.Name);
                  exit;
                end;
            end;
            if Fixup.TargetDisplacement<>0 then
              begin
                InputError('Unsupported nonzero target displacement '+IntToStr(Fixup.TargetDisplacement)+' in external reference to '+sym.Name);
                exit;
              end;
          end;
        {todo: convert other fixup types as well }

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
        CaseSensitive:=False;
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
              if not ReadModEnd(FRawRecord,objdata) then
                exit;
            else
              begin
                InputError('Unsupported OMF record type $'+HexStr(FRawRecord.RecordType,2));
                exit;
              end;
          end;
        until FRawRecord.RecordType in [RT_MODEND,RT_MODEND32];
        result:=true;
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
                               TMZExeSection
****************************************************************************}

    procedure TMZExeSection.AddObjSection(objsec: TObjSection; ignoreprops: boolean);
      begin
        { allow mixing initialized and uninitialized data in the same section
          => set ignoreprops=true }
        inherited AddObjSection(objsec,true);
      end;

{****************************************************************************
                         TMZExeUnifiedLogicalSegment
****************************************************************************}

    constructor TMZExeUnifiedLogicalSegment.create(HashObjectList: TFPHashObjectList; const s: TSymStr);
      var
        Separator: SizeInt;
      begin
        inherited create(HashObjectList,s);
        FObjSectionList:=TFPObjectList.Create(false);
        { name format is 'SegName||ClassName' }
        Separator:=Pos('||',s);
        if Separator>0 then
          begin
            FSegName:=Copy(s,1,Separator-1);
            FSegClass:=Copy(s,Separator+2,Length(s)-Separator-1);
          end
        else
          begin
            FSegName:=Name;
            FSegClass:='';
          end;
      end;

    destructor TMZExeUnifiedLogicalSegment.destroy;
      begin
        FObjSectionList.Free;
        inherited destroy;
      end;

    procedure TMZExeUnifiedLogicalSegment.AddObjSection(ObjSec: TOmfObjSection);
      begin
        ObjSectionList.Add(ObjSec);
        ObjSec.MZExeUnifiedLogicalSegment:=self;
      end;

    procedure TMZExeUnifiedLogicalSegment.CalcMemPos;
      var
        MinMemPos: qword=high(qword);
        MaxMemPos: qword=0;
        objsec: TOmfObjSection;
        i: Integer;
      begin
        if ObjSectionList.Count=0 then
          internalerror(2015082201);
        for i:=0 to ObjSectionList.Count-1 do
          begin
            objsec:=TOmfObjSection(ObjSectionList[i]);
            if objsec.MemPos<MinMemPos then
              MinMemPos:=objsec.MemPos;
            if (objsec.MemPos+objsec.Size)>MaxMemPos then
              MaxMemPos:=objsec.MemPos+objsec.Size;
          end;
        { align *down* on a paragraph boundary }
        MemPos:={(MinMemPos shr 4) shl 4}MinMemPos;
        Size:=MaxMemPos-MemPos;
      end;

    function TMZExeUnifiedLogicalSegment.MemPosStr: string;
      begin
        Result:=HexStr(MemBasePos shr 4,4)+':'+HexStr((MemPos-MemBasePos),4);
      end;

{****************************************************************************
                         TMZExeUnifiedLogicalGroup
****************************************************************************}

    constructor TMZExeUnifiedLogicalGroup.create(HashObjectList: TFPHashObjectList; const s: TSymStr);
      begin
        inherited create(HashObjectList,s);
        FSegmentList:=TFPHashObjectList.Create(false);
      end;

    destructor TMZExeUnifiedLogicalGroup.destroy;
      begin
        FSegmentList.Free;
        inherited destroy;
      end;

    procedure TMZExeUnifiedLogicalGroup.CalcMemPos;
      var
        MinMemPos: qword=high(qword);
        MaxMemPos: qword=0;
        UniSeg: TMZExeUnifiedLogicalSegment;
        i: Integer;
      begin
        if SegmentList.Count=0 then
          internalerror(2015082201);
        for i:=0 to SegmentList.Count-1 do
          begin
            UniSeg:=TMZExeUnifiedLogicalSegment(SegmentList[i]);
            if UniSeg.MemPos<MinMemPos then
              MinMemPos:=UniSeg.MemPos;
            if (UniSeg.MemPos+UniSeg.Size)>MaxMemPos then
              MaxMemPos:=UniSeg.MemPos+UniSeg.Size;
          end;
        { align *down* on a paragraph boundary }
        MemPos:=(MinMemPos shr 4) shl 4;
        Size:=MaxMemPos-MemPos;
      end;

    function TMZExeUnifiedLogicalGroup.MemPosStr: string;
      begin
        Result:=HexStr(MemPos shr 4,4)+':'+HexStr(MemPos and $f,4);
      end;

    procedure TMZExeUnifiedLogicalGroup.AddSegment(UniSeg: TMZExeUnifiedLogicalSegment);
      begin
        SegmentList.Add(UniSeg.Name,UniSeg);
        if UniSeg.PrimaryGroup='' then
          UniSeg.PrimaryGroup:=Name;
      end;

{****************************************************************************
                               TMZExeOutput
****************************************************************************}

    function TMZExeOutput.GetMZFlatContentSection: TMZExeSection;
      begin
        if not assigned(FMZFlatContentSection) then
          FMZFlatContentSection:=TMZExeSection(FindExeSection('.MZ_flat_content'));
        result:=FMZFlatContentSection;
      end;

    procedure TMZExeOutput.CalcExeUnifiedLogicalSegments;
      var
        ExeSec: TMZExeSection;
        ObjSec: TOmfObjSection;
        UniSeg: TMZExeUnifiedLogicalSegment;
        i: Integer;
      begin
        ExeSec:=MZFlatContentSection;
        for i:=0 to ExeSec.ObjSectionList.Count-1 do
          begin
            ObjSec:=TOmfObjSection(ExeSec.ObjSectionList[i]);
            UniSeg:=TMZExeUnifiedLogicalSegment(ExeUnifiedLogicalSegments.Find(ObjSec.Name));
            if not assigned(UniSeg) then
              UniSeg:=TMZExeUnifiedLogicalSegment.Create(ExeUnifiedLogicalSegments,ObjSec.Name);
            UniSeg.AddObjSection(ObjSec);
          end;
        for i:=0 to ExeUnifiedLogicalSegments.Count-1 do
          begin
            UniSeg:=TMZExeUnifiedLogicalSegment(ExeUnifiedLogicalSegments[i]);
            UniSeg.CalcMemPos;
          end;
      end;

    procedure TMZExeOutput.CalcExeGroups;

        procedure AddToGroup(UniSeg:TMZExeUnifiedLogicalSegment;GroupName:TSymStr);
          var
            Group: TMZExeUnifiedLogicalGroup;
          begin
            Group:=TMZExeUnifiedLogicalGroup(ExeUnifiedLogicalGroups.Find(GroupName));
            if not assigned(Group) then
              Group:=TMZExeUnifiedLogicalGroup.Create(ExeUnifiedLogicalGroups,GroupName);
            Group.AddSegment(UniSeg);
          end;

      var
        objdataidx,groupidx,secidx: Integer;
        ObjData: TObjData;
        ObjGroup: TOmfObjSectionGroup;
        ObjSec: TOmfObjSection;
        UniGrp: TMZExeUnifiedLogicalGroup;
      begin
        for objdataidx:=0 to ObjDataList.Count-1 do
          begin
            ObjData:=TObjData(ObjDataList[objdataidx]);
            if assigned(ObjData.GroupsList) then
              for groupidx:=0 to ObjData.GroupsList.Count-1 do
                begin
                  ObjGroup:=TOmfObjSectionGroup(ObjData.GroupsList[groupidx]);
                  for secidx:=low(ObjGroup.members) to high(ObjGroup.members) do
                    begin
                      ObjSec:=TOmfObjSection(ObjGroup.members[secidx]);
                      if assigned(ObjSec.MZExeUnifiedLogicalSegment) then
                        AddToGroup(ObjSec.MZExeUnifiedLogicalSegment,ObjGroup.Name);
                    end;
                end;
          end;
        for groupidx:=0 to ExeUnifiedLogicalGroups.Count-1 do
          begin
            UniGrp:=TMZExeUnifiedLogicalGroup(ExeUnifiedLogicalGroups[groupidx]);
            UniGrp.CalcMemPos;
          end;
      end;

    procedure TMZExeOutput.CalcSegments_MemBasePos;
      var
        lastbase:qword=0;
        i: Integer;
        UniSeg: TMZExeUnifiedLogicalSegment;
      begin
        for i:=0 to ExeUnifiedLogicalSegments.Count-1 do
          begin
            UniSeg:=TMZExeUnifiedLogicalSegment(ExeUnifiedLogicalSegments[i]);
            if UniSeg.PrimaryGroup<>'' then
              lastbase:=(UniSeg.MemPos shr 4) shl 4
            else
              begin
                while ((UniSeg.MemPos+UniSeg.Size-1)-lastbase)>$ffff do
                  Inc(lastbase,$10000);
              end;
            UniSeg.MemBasePos:=lastbase;
          end;
      end;

    procedure TMZExeOutput.WriteMap_SegmentsAndGroups;
      var
        i: Integer;
        UniSeg: TMZExeUnifiedLogicalSegment;
        UniGrp: TMZExeUnifiedLogicalGroup;
      begin
        exemap.AddHeader('Groups list');
        exemap.Add('');
        exemap.Add(PadSpace('Group',32)+PadSpace('Address',21)+'Size');
        exemap.Add(PadSpace('=====',32)+PadSpace('=======',21)+'====');
        exemap.Add('');
        for i:=0 to ExeUnifiedLogicalGroups.Count-1 do
          begin
            UniGrp:=TMZExeUnifiedLogicalGroup(ExeUnifiedLogicalGroups[i]);
            exemap.Add(PadSpace(UniGrp.Name,32)+PadSpace(UniGrp.MemPosStr,21)+HexStr(UniGrp.Size,8));
          end;
        exemap.Add('');
        exemap.AddHeader('Segments list');
        exemap.Add('');
        exemap.Add(PadSpace('Segment',23)+PadSpace('Class',15)+PadSpace('Group',15)+PadSpace('Address',16)+'Size');
        exemap.Add(PadSpace('=======',23)+PadSpace('=====',15)+PadSpace('=====',15)+PadSpace('=======',16)+'====');
        exemap.Add('');
        for i:=0 to ExeUnifiedLogicalSegments.Count-1 do
          begin
            UniSeg:=TMZExeUnifiedLogicalSegment(ExeUnifiedLogicalSegments[i]);
            exemap.Add(PadSpace(UniSeg.SegName,23)+PadSpace(UniSeg.SegClass,15)+PadSpace(UniSeg.PrimaryGroup,15)+PadSpace(UniSeg.MemPosStr,16)+HexStr(UniSeg.Size,8));
          end;
        exemap.Add('');
      end;

    procedure TMZExeOutput.WriteMap_HeaderData;
      begin
        exemap.AddHeader('Header data');
        exemap.Add('Loadable image size: '+HexStr(Header.LoadableImageSize,8));
        exemap.Add('Entry point address: '+HexStr(Header.InitialCS,4)+':'+HexStr(Header.InitialIP,4));
      end;

    procedure TMZExeOutput.FillLoadableImageSize;
      var
        i: Integer;
        ExeSec: TMZExeSection;
        ObjSec: TOmfObjSection;
        StartDataPos: LongWord;
        buf: array [0..1023] of byte;
        bytesread: LongWord;
      begin
        Header.LoadableImageSize:=0;
        ExeSec:=MZFlatContentSection;
        for i:=0 to ExeSec.ObjSectionList.Count-1 do
          begin
            ObjSec:=TOmfObjSection(ExeSec.ObjSectionList[i]);
            if (ObjSec.Size>0) and assigned(ObjSec.Data) then
              if (ObjSec.MemPos+ObjSec.Size)>Header.LoadableImageSize then
                Header.LoadableImageSize:=ObjSec.MemPos+ObjSec.Size;
          end;
      end;

    procedure TMZExeOutput.FillStartAddress;
      var
        EntryMemPos: qword;
        EntryMemBasePos: qword;
      begin
        EntryMemPos:=EntrySym.address;
        if assigned(EntrySym.group) then
          EntryMemBasePos:=TMZExeUnifiedLogicalGroup(ExeUnifiedLogicalGroups.Find(EntrySym.group.Name)).MemPos
        else
          EntryMemBasePos:=TOmfObjSection(EntrySym.objsection).MZExeUnifiedLogicalSegment.MemBasePos;
        Header.InitialIP:=EntryMemPos-EntryMemBasePos;
        Header.InitialCS:=EntryMemBasePos shr 4;
      end;

    procedure TMZExeOutput.FillHeaderData;
      begin
        Header.MaxExtraParagraphs:=$FFFF;
        FillLoadableImageSize;
        FillStartAddress;
        if assigned(exemap) then
          WriteMap_HeaderData;
      end;

    function TMZExeOutput.writeExe: boolean;
      var
        ExeSec: TMZExeSection;
        i: Integer;
        ObjSec: TOmfObjSection;
      begin
        Result:=False;
        FillHeaderData;
        Header.WriteTo(FWriter);

        ExeSec:=MZFlatContentSection;
        ExeSec.DataPos:=FWriter.Size;
        for i:=0 to ExeSec.ObjSectionList.Count-1 do
          begin
            ObjSec:=TOmfObjSection(ExeSec.ObjSectionList[i]);
            if ObjSec.MemPos<Header.LoadableImageSize then
              begin
                FWriter.WriteZeros(max(0,ObjSec.MemPos-FWriter.Size+ExeSec.DataPos));
                if assigned(ObjSec.Data) then
                  FWriter.writearray(ObjSec.Data);
              end;
          end;
        Result:=True;
      end;

    function TMZExeOutput.writeCom: boolean;
      const
        ComFileOffset=$100;
      var
        i: Integer;
        ExeSec: TMZExeSection;
        ObjSec: TOmfObjSection;
        StartDataPos: LongWord;
        buf: array [0..1023] of byte;
        bytesread: LongWord;
      begin
        FillHeaderData;
        ExeSec:=MZFlatContentSection;
        for i:=0 to ExeSec.ObjSectionList.Count-1 do
          begin
            ObjSec:=TOmfObjSection(ExeSec.ObjSectionList[i]);
            if ObjSec.MemPos<Header.LoadableImageSize then
              begin
                FWriter.WriteZeros(max(0,ObjSec.MemPos-ComFileOffset-FWriter.Size));
                if assigned(ObjSec.Data) then
                  begin
                    if ObjSec.MemPos<ComFileOffset then
                      begin
                        ObjSec.Data.seek(ComFileOffset-ObjSec.MemPos);
                        repeat
                          bytesread:=ObjSec.Data.read(buf,sizeof(buf));
                          if bytesread<>0 then
                            FWriter.write(buf,bytesread);
                        until bytesread=0;
                      end
                    else
                      FWriter.writearray(ObjSec.Data);
                  end;
              end;
          end;
        Result:=True;
      end;

    procedure TMZExeOutput.Load_Symbol(const aname: string);
      var
        dgroup: TObjSectionGroup;
        sym: TObjSymbol;
      begin
        { special handling for the '_edata' and '_end' symbols, which are
          internally added by the linker }
        if (aname='_edata') or (aname='_end') then
          begin
            { create an internal segment with the 'BSS' class }
            internalObjData.createsection('*'+aname+'||BSS',0,[]);
            { add to group 'DGROUP' }
            dgroup:=nil;
            if assigned(internalObjData.GroupsList) then
              dgroup:=TObjSectionGroup(internalObjData.GroupsList.Find('DGROUP'));
            if dgroup=nil then
              dgroup:=internalObjData.createsectiongroup('DGROUP');
            SetLength(dgroup.members,Length(dgroup.members)+1);
            dgroup.members[Length(dgroup.members)-1]:=internalObjData.CurrObjSec;
            { define the symbol itself }
            sym:=internalObjData.SymbolDefine(aname,AB_GLOBAL,AT_DATA);
            sym.group:=dgroup;
          end
        else
          inherited;
      end;

    procedure TMZExeOutput.DoRelocationFixup(objsec: TObjSection);
      var
        i: Integer;
        omfsec: TOmfObjSection absolute objsec;
        objreloc: TOmfRelocation;
        target: DWord;
        framebase: DWord;
        fixupamount: Integer;
        w: Word;
      begin
        for i:=0 to objsec.ObjRelocations.Count-1 do
          begin
            objreloc:=TOmfRelocation(objsec.ObjRelocations[i]);
            if assigned(objreloc.symbol) then
              begin
                target:=objreloc.symbol.address;
                if objreloc.FrameGroup<>'' then
                  framebase:=TMZExeUnifiedLogicalGroup(ExeUnifiedLogicalGroups.Find(objreloc.FrameGroup)).MemPos
                else if assigned(objreloc.symbol.group) then
                  framebase:=TMZExeUnifiedLogicalGroup(ExeUnifiedLogicalGroups.Find(objreloc.symbol.group.Name)).MemPos
                else
                  framebase:=TOmfObjSection(objreloc.symbol.objsection).MZExeUnifiedLogicalSegment.MemBasePos;
                case objreloc.typ of
                  RELOC_ABSOLUTE:
                    fixupamount:=target-framebase;
                  RELOC_RELATIVE:
                    fixupamount:=target-(omfsec.MemPos+objreloc.DataOffset)-2;
                  else
                    internalerror(2015082402);
                end;
                case objreloc.typ of
                  RELOC_ABSOLUTE,
                  RELOC_RELATIVE:
                    begin
                      omfsec.Data.seek(objreloc.DataOffset);
                      omfsec.Data.read(w,2);
                      w:=LEtoN(w);
                      Inc(w,fixupamount);
                      w:=LEtoN(w);
                      omfsec.Data.seek(objreloc.DataOffset);
                      omfsec.Data.write(w,2);
                    end;
                  else
                    internalerror(2015082403);
                end;
              end
            else
              {todo};
          end;
      end;

    function IOmfObjSectionClassNameCompare(Item1, Item2: Pointer): Integer;
      var
        I1 : TOmfObjSection absolute Item1;
        I2 : TOmfObjSection absolute Item2;
      begin
        Result:=CompareStr(I1.ClassName,I2.ClassName);
        if Result=0 then
          Result:=CompareStr(I1.Name,I2.Name);
        if Result=0 then
          Result:=I1.SortOrder-I2.SortOrder;
      end;

    procedure TMZExeOutput.Order_ObjSectionList(ObjSectionList: TFPObjectList; const aPattern: string);
      var
        i: Integer;
      begin
        for i:=0 to ObjSectionList.Count-1 do
          TOmfObjSection(ObjSectionList[i]).SortOrder:=i;
        ObjSectionList.Sort(@IOmfObjSectionClassNameCompare);
      end;

    procedure TMZExeOutput.MemPos_EndExeSection;
      var
        SecName: TSymStr='';
      begin
        if assigned(CurrExeSec) then
          SecName:=CurrExeSec.Name;
        inherited MemPos_EndExeSection;
        if SecName='.MZ_flat_content' then
          begin
            CalcExeUnifiedLogicalSegments;
            CalcExeGroups;
            CalcSegments_MemBasePos;
            if assigned(exemap) then
              WriteMap_SegmentsAndGroups;
          end;
      end;

    function TMZExeOutput.writeData: boolean;
      begin
        if apptype=app_com then
          Result:=WriteCom
        else
          Result:=WriteExe;
      end;

    constructor TMZExeOutput.create;
      begin
        inherited create;
        CExeSection:=TMZExeSection;
        CObjData:=TOmfObjData;
        { "640K ought to be enough for anybody" :) }
        MaxMemPos:=$9FFFF;
        FExeUnifiedLogicalSegments:=TFPHashObjectList.Create;
        FExeUnifiedLogicalGroups:=TFPHashObjectList.Create;
        FHeader:=TMZExeHeader.Create;
      end;

    destructor TMZExeOutput.destroy;
      begin
        FHeader.Free;
        FExeUnifiedLogicalGroups.Free;
        FExeUnifiedLogicalSegments.Free;
        inherited destroy;
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
