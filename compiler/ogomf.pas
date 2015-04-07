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
      public
        constructor CreateSymbol(ADataOffset:aword;s:TObjSymbol;Atyp:TObjRelocationType);
        destructor Destroy; override;

        property OmfFixup: TOmfSubRecord_FIXUP read FOmfFixup;
      end;

      { TOmfObjSection }

      TOmfObjSection = class(TObjSection)
      private
        FClassName: string;
        FOverlayName: string;
        FOmfAlignment: TOmfSegmentAlignment;
        FCombination: TOmfSegmentCombination;
        FUse: TOmfSegmentUse;
        FPrimaryGroup: string;
      public
        constructor create(AList:TFPHashObjectList;const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);override;
        property ClassName: string read FClassName;
        property OverlayName: string read FOverlayName;
        property OmfAlignment: TOmfSegmentAlignment read FOmfAlignment;
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
        procedure WriteSections(Data:TObjData);
        procedure WriteSectionContentAndFixups(sec: TObjSection);

        procedure section_count_sections(p:TObject;arg:pointer);

        property LNames: TOmfOrderedNameCollection read FLNames;
        property Segments: TFPHashObjectList read FSegments;
        property Groups: TFPHashObjectList read FGroups;
      protected
        function writeData(Data:TObjData):boolean;override;
      public
        constructor create(AWriter:TObjectWriter);override;
        destructor Destroy;override;
      end;

      TOmfAssembler = class(tinternalassembler)
        constructor create(smart:boolean);override;
      end;

implementation

    uses
       SysUtils,
       cutils,verbose,globals,
       fmodule,aasmtai,aasmdata,
       ogmap,
       version
       ;

{****************************************************************************
                                TOmfRelocation
****************************************************************************}

    constructor TOmfRelocation.CreateSymbol(ADataOffset: aword; s: TObjSymbol; Atyp: TObjRelocationType);
      begin
        inherited CreateSymbol(ADataOffset,s,Atyp);
        FOmfFixup:=TOmfSubRecord_FIXUP.Create;
        { dummy data, TODO: fix }
        FOmfFixup.LocationOffset:=ADataOffset;
        FOmfFixup.LocationType:=fltOffset;
        FOmfFixup.FrameDeterminedByThread:=False;
        FOmfFixup.TargetDeterminedByThread:=False;
        FOmfFixup.Mode:=fmSegmentRelative;
        FOmfFixup.TargetMethod:=ftmSegmentIndexNoDisp;
        FOmfFixup.TargetDatum:=3;
        FOmfFixup.FrameMethod:=ffmGroupIndex;
        FOmfFixup.FrameDatum:=1;
      end;

    destructor TOmfRelocation.Destroy;
      begin
        FOmfFixup.Free;
        inherited Destroy;
      end;

{****************************************************************************
                                TOmfObjSection
****************************************************************************}

    constructor TOmfObjSection.create(AList: TFPHashObjectList;
          const Aname: string; Aalign: shortint; Aoptions: TObjSectionOptions);
      var
        dgroup: Boolean;
      begin
        inherited create(AList, Aname, Aalign, Aoptions);
        FCombination:=scPublic;
        FUse:=suUse16;
        FOmfAlignment:=saRelocatableByteAligned;
        if oso_executable in Aoptions then
          begin
            FClassName:='code';
            dgroup:=(current_settings.x86memorymodel=mm_tiny);
          end
        else if Aname='stack' then
          begin
            FClassName:='stack';
            FCombination:=scStack;
            FOmfAlignment:=saRelocatableParaAligned;
            dgroup:=current_settings.x86memorymodel in (x86_near_data_models-[mm_tiny]);
          end
        else if Aname='heap' then
          begin
            FClassName:='heap';
            FOmfAlignment:=saRelocatableParaAligned;
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
            FOmfAlignment:=saRelocatableWordAligned;
            dgroup:=true;
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
{        Write('writeReloc(', data, ',', len, ',');
        if p<>nil then
          write(p.Name)
        else
          write('nil');
        Writeln(',',Reloctype,')');}

        if CurrObjSec=nil then
          internalerror(200403072);
        objreloc:=nil;
        if assigned(p) then
          begin
            { real address of the symbol }
            symaddr:=p.address;

            objreloc:=TOmfRelocation.CreateSymbol(CurrObjSec.Size,p,Reloctype);
            CurrObjSec.ObjRelocations.Add(objreloc);
            inc(data,symaddr);
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
                SegListStr[I]:=Segments.FindIndexOf(WideString(seglist[I].VWideString));
              vtUnicodeString:
                SegListStr[I]:=Segments.FindIndexOf(UnicodeString(seglist[I].VUnicodeString));
              else
                internalerror(2015040402);
            end;
          end;
        g.SegmentList:=SegListStr;
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
            SegIndex:=Segments.FindIndexOf(sec.Name);
            RawRecord:=TOmfRawRecord.Create;
            sec.data.seek(0);
            ChunkFixupStart:=0;
            ChunkFixupEnd:=-1;
            ChunkStart:=0;
            ChunkLen:=Min(MaxChunkSize, sec.Data.size-ChunkStart);
            while ChunkLen>0 do
            begin
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
              while (ChunkFixupEnd<(sec.ObjRelocations.Count-1)) and
                    (TOmfRelocation(sec.ObjRelocations[ChunkFixupEnd+1]).OmfFixup.LocationOffset<(ChunkStart+ChunkLen)) do
                inc(ChunkFixupEnd);
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

        for i:=0 to Data.ObjSectionList.Count-1 do
          with TOmfObjSection(Data.ObjSectionList[I]) do
            AddSegment(Name,ClassName,OverlayName,OmfAlignment,Combination,Use,Size);

{        if current_settings.x86memorymodel=mm_tiny then
          AddGroup('dgroup',['text','rodata','data','fpc','bss','heap'])
        else if current_settings.x86memorymodel in x86_near_data_models then
          AddGroup('dgroup',['rodata','data','fpc','bss','stack','heap'])
        else
          AddGroup('dgroup',['rodata','data','fpc','bss']);}

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
                               TOmfAssembler
****************************************************************************}

    constructor TOmfAssembler.Create(smart:boolean);
      begin
        inherited Create(smart);
        CObjOutput:=TOmfObjOutput;
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
            flags : [af_outputbinary,af_needar,af_no_debug];
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
