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

      { TOmfObjData }

      TOmfObjData = class(TObjData)
      private
        class function CodeSectionName(const aname:string): string;
      public
        function sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;override;
        procedure writeReloc(Data:aint;len:aword;p:TObjSymbol;Reloctype:TObjRelocationType);override;
      end;

      { TOmfObjOutput }

      TOmfObjOutput = class(tObjOutput)
      private
        FLNames: TOmfOrderedNameCollection;
        FSegments: TFPHashObjectList;
        FGroups: TFPHashObjectList;
        procedure AddSegment(const name,segclass: string;
          Alignment: TOmfSegmentAlignment; Combination: TOmfSegmentCombination;
          Use: TOmfSegmentUse);
        procedure AddGroup(const groupname: string; seglist: array of const);
        procedure WriteSections(Data:TObjData);
        procedure WriteSectionContentAndFixups(sec: TObjSection);

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
      begin
      end;

{****************************************************************************
                                TOmfObjOutput
****************************************************************************}

    procedure TOmfObjOutput.AddSegment(const name, segclass: string;
        Alignment: TOmfSegmentAlignment; Combination: TOmfSegmentCombination;
        Use: TOmfSegmentUse);
      var
        s: TOmfRecord_SEGDEF;
      begin
        s:=TOmfRecord_SEGDEF.Create;
        Segments.Add(name,s);
        s.SegmentNameIndex:=LNames.Add(name);
        s.ClassNameIndex:=LNames.Add(segclass);
        s.OverlayNameIndex:=1;
        s.Alignment:=Alignment;
        s.Combination:=Combination;
        s.Use:=Use;
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
        SegIndex: Integer;
        NextOfs: Integer;
      begin
        if (oso_data in sec.SecOptions) then
          begin
            if sec.Data=nil then
              internalerror(200403073);
            SegIndex:=Segments.FindIndexOf(sec.Name);
            RawRecord:=TOmfRawRecord.Create;
            sec.data.seek(0);
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
              { TODO: write fixups }
              { prepare next chunk }
              Inc(ChunkStart, ChunkLen);
              ChunkLen:=Min(1024, sec.Data.size-ChunkStart);
            end;
            RawRecord.Free;
          end;
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
      begin
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

        if not (cs_huge_code in current_settings.moduleswitches) then
          AddSegment(TOmfObjData.CodeSectionName(current_module.modulename^),'code',saRelocatableByteAligned,scPublic,suUse16);
        AddSegment('rodata','data',saRelocatableByteAligned,scPublic,suUse16);
        AddSegment('data','data',saRelocatableWordAligned,scPublic,suUse16);
        AddSegment('fpc','data',saRelocatableByteAligned,scPublic,suUse16);
        AddSegment('bss','bss',saRelocatableByteAligned,scPublic,suUse16);
        AddSegment('stack','stack',saRelocatableParaAligned,scStack,suUse16);
        AddSegment('heap','heap',saRelocatableParaAligned,scPublic,suUse16);

        if current_settings.x86memorymodel=mm_tiny then
          AddGroup('dgroup',['text','rodata','data','fpc','bss','heap'])
        else if current_settings.x86memorymodel in x86_near_data_models then
          AddGroup('dgroup',['rodata','data','fpc','bss','stack','heap'])
        else
          AddGroup('dgroup',['rodata','data','fpc','bss']);

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
