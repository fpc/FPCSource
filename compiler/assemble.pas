{
    Copyright (c) 1998-2004 by Peter Vreman

    This unit handles the assemblerfile write and assembler calls of FPC

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
{# @abstract(This unit handles the assembler file write and assembler calls of FPC)
   Handles the calls to the actual external assemblers, as well as the generation
   of object files for smart linking. Also contains the base class for writing
   the assembler statements to file.
}
unit assemble;

{$i fpcdefs.inc}

interface


    uses
{$IFDEF USE_SYSUTILS}
      sysutils,
{$ELSE USE_SYSUTILS}
      strings,
      dos,
{$ENDIF USE_SYSUTILS}
      systems,globtype,globals,aasmbase,aasmtai,ogbase;

    const
       { maximum of aasmoutput lists there will be }
       maxoutputlists = 10;
       { buffer size for writing the .s file }
       AsmOutSize=32768;

    type
      TAssembler=class(TAbstractAssembler)
      public
      {filenames}
        path     : pathstr;
        name     : namestr;
        asmfile,         { current .s and .o file }
        objfile  : string;
        ppufilename : string;
        asmprefix : string;
        SmartAsm : boolean;
        SmartFilesCount,
        SmartHeaderCount : longint;
        Constructor Create(smart:boolean);virtual;
        Destructor Destroy;override;
        procedure NextSmartName(place:tcutplace);
        procedure MakeObject;virtual;abstract;
      end;

      {# This is the base class which should be overriden for each each
         assembler writer. It is used to actually assembler a file,
         and write the output to the assembler file.
      }
      TExternalAssembler=class(TAssembler)
      private
        procedure CreateSmartLinkPath(const s:string);
      protected
      {outfile}
        AsmSize,
        AsmStartSize,
        outcnt   : longint;
        outbuf   : array[0..AsmOutSize-1] of char;
        outfile  : file;
        ioerror : boolean;
      public
        {# Returns the complete path and executable name of the assembler
           program.

           It first tries looking in the UTIL directory if specified,
           otherwise it searches in the free pascal binary directory, in
           the current working directory and then in the  directories
           in the $PATH environment.}
        Function  FindAssembler:string;

        {# Actually does the call to the assembler file. Returns false
           if the assembling of the file failed.}
        Function  CallAssembler(const command:string; const para:TCmdStr):Boolean;

        Function  DoAssemble:boolean;virtual;
        Procedure RemoveAsm;
        Procedure AsmFlush;
        Procedure AsmClear;

        {# Write a string to the assembler file }
        Procedure AsmWrite(const s:string);

        {# Write a string to the assembler file }
        Procedure AsmWritePChar(p:pchar);

        {# Write a string to the assembler file followed by a new line }
        Procedure AsmWriteLn(const s:string);

        {# Write a new line to the assembler file }
        Procedure AsmLn;

        procedure AsmCreate(Aplace:tcutplace);
        procedure AsmClose;

        {# This routine should be overriden for each assembler, it is used
           to actually write the abstract assembler stream to file.}
        procedure WriteTree(p:TAAsmoutput);virtual;

        {# This routine should be overriden for each assembler, it is used
           to actually write all the different abstract assembler streams
           by calling for each stream type, the @var(WriteTree) method.}
        procedure WriteAsmList;virtual;

        {# Constructs the command line for calling the assembler }
        function MakeCmdLine: TCmdStr;
      public
        Constructor Create(smart:boolean);override;
        procedure MakeObject;override;
      end;

      TInternalAssembler=class(TAssembler)
      public
        constructor create(smart:boolean);override;
        destructor  destroy;override;
        procedure MakeObject;override;
      protected
        objectdata   : TAsmObjectData;
        objectoutput : tobjectoutput;
      private
        { the aasmoutput lists that need to be processed }
        lists        : byte;
        list         : array[1..maxoutputlists] of TAAsmoutput;
        { current processing }
        currlistidx  : byte;
        currlist     : TAAsmoutput;
        currpass     : byte;
{$ifdef GDB}
        n_line       : byte;     { different types of source lines }
        linecount,
        includecount : longint;
        funcname     : tasmsymbol;
        stabslastfileinfo : tfileposinfo;
        procedure convertstabs(p:pchar);
        procedure emitlineinfostabs(nidx,line : longint);
        procedure emitstabs(s:string);
        procedure WriteFileLineInfo(var fileinfo : tfileposinfo);
        procedure StartFileLineInfo;
        procedure EndFileLineInfo;
{$endif}
        function  MaybeNextList(var hp:Tai):boolean;
        function  TreePass0(hp:Tai):Tai;
        function  TreePass1(hp:Tai):Tai;
        function  TreePass2(hp:Tai):Tai;
        procedure writetree;
        procedure writetreesmart;
      end;

    TAssemblerClass = class of TAssembler;

    Procedure GenerateAsm(smart:boolean);
    Procedure OnlyAsm;

    procedure RegisterAssembler(const r:tasminfo;c:TAssemblerClass);
    procedure InitAssembler;
    procedure DoneAssembler;


Implementation

    uses
{$ifdef hasunix}
  {$ifdef havelinuxrtl10}
      linux,
  {$else}
      unix,
  {$endif}
{$endif}
      cutils,script,fmodule,verbose,
{$ifdef memdebug}
      cclasses,
{$endif memdebug}
{$ifdef GDB}
      finput,
      gdb,
{$endif GDB}
{$ifdef m68k}
      cpuinfo,
{$endif m68k}
      aasmcpu
      ;

    var
      CAssembler : array[tasm] of TAssemblerClass;


{*****************************************************************************
                                   TAssembler
*****************************************************************************}

    Constructor TAssembler.Create(smart:boolean);
      begin
      { load start values }
        asmfile:=current_module.get_asmfilename;
        objfile:=current_module.objfilename^;
        name:=Lower(current_module.modulename^);
        path:=current_module.outputpath^;
        asmprefix := current_module.asmprefix^;
        if not assigned(current_module.outputpath) then
          ppufilename := ''
        else
          ppufilename := current_module.ppufilename^;
        SmartAsm:=smart;
        SmartFilesCount:=0;
        SmartHeaderCount:=0;
        SmartLinkOFiles.Clear;
      end;


    Destructor TAssembler.Destroy;
      begin
      end;


    procedure TAssembler.NextSmartName(place:tcutplace);
      var
        s : string;
      begin
        inc(SmartFilesCount);
        if SmartFilesCount>999999 then
         Message(asmw_f_too_many_asm_files);
        case place of
          cut_begin :
            begin
              inc(SmartHeaderCount);
              s:=asmprefix+tostr(SmartHeaderCount)+'h';
            end;
          cut_normal :
            s:=asmprefix+tostr(SmartHeaderCount)+'s';
          cut_end :
            s:=asmprefix+tostr(SmartHeaderCount)+'t';
        end;
        AsmFile:=Path+FixFileName(s+tostr(SmartFilesCount)+target_info.asmext);
        ObjFile:=Path+FixFileName(s+tostr(SmartFilesCount)+target_info.objext);
        { insert in container so it can be cleared after the linking }
        SmartLinkOFiles.Insert(Objfile);
      end;


{*****************************************************************************
                                 TExternalAssembler
*****************************************************************************}

    Function DoPipe:boolean;
      begin
        DoPipe:=(cs_asm_pipe in aktglobalswitches) and
                (([cs_asm_leave,cs_link_on_target] * aktglobalswitches) = []) and
                ((target_asm.id in [as_gas,as_darwin]));
      end;


    Constructor TExternalAssembler.Create(smart:boolean);
      begin
        inherited Create(smart);
        if SmartAsm then
         begin
           path:=FixPath(path+FixFileName(name)+target_info.smartext,false);
           CreateSmartLinkPath(path);
         end;
        Outcnt:=0;
      end;


    procedure TExternalAssembler.CreateSmartLinkPath(const s:string);
      var
{$IFDEF USE_SYSUTILS}
        dir : TSearchRec;
{$ELSE USE_SYSUTILS}
        dir : searchrec;
{$ENDIF USE_SYSUTILS}
        hs  : string;
      begin
        if PathExists(s) then
         begin
           { the path exists, now we clean only all the .o and .s files }
           { .o files }
{$IFDEF USE_SYSUTILS}
           if findfirst(s+source_info.dirsep+'*'+target_info.objext,faAnyFile,dir) = 0
           then repeat
              RemoveFile(s+source_info.dirsep+dir.name);
           until findnext(dir) <> 0;
{$ELSE USE_SYSUTILS}
           findfirst(s+source_info.dirsep+'*'+target_info.objext,anyfile,dir);
           while (doserror=0) do
            begin
              RemoveFile(s+source_info.dirsep+dir.name);
              findnext(dir);
            end;
{$ENDIF USE_SYSUTILS}
           findclose(dir);
           { .s files }
{$IFDEF USE_SYSUTILS}
           if findfirst(s+source_info.dirsep+'*'+target_info.asmext,faAnyFile,dir) = 0
           then repeat
             RemoveFile(s+source_info.dirsep+dir.name);
           until findnext(dir) <> 0;
{$ELSE USE_SYSUTILS}
           findfirst(s+source_info.dirsep+'*'+target_info.asmext,anyfile,dir);
           while (doserror=0) do
            begin
              RemoveFile(s+source_info.dirsep+dir.name);
              findnext(dir);
            end;
{$ENDIF USE_SYSUTILS}
           findclose(dir);
         end
        else
         begin
           hs:=s;
           if hs[length(hs)] in ['/','\'] then
            delete(hs,length(hs),1);
           {$I-}
            mkdir(hs);
           {$I+}
           if ioresult<>0 then;
         end;
      end;


    const
      lastas  : byte=255;
    var
      LastASBin : pathstr;
    Function TExternalAssembler.FindAssembler:string;
      var
        asfound : boolean;
        UtilExe  : string;
      begin
        asfound:=false;
        if cs_link_on_target in aktglobalswitches then
         begin
           { If linking on target, don't add any path PM }
           FindAssembler:=utilsprefix+AddExtension(target_asm.asmbin,target_info.exeext);
           exit;
         end
        else
         UtilExe:=utilsprefix+AddExtension(target_asm.asmbin,source_info.exeext);
        if lastas<>ord(target_asm.id) then
         begin
           lastas:=ord(target_asm.id);
           { is an assembler passed ? }
           if utilsdirectory<>'' then
             asfound:=FindFile(UtilExe,utilsdirectory,LastASBin);
           if not AsFound then
             asfound:=FindExe(UtilExe,LastASBin);
           if (not asfound) and not(cs_asm_extern in aktglobalswitches) then
            begin
              Message1(exec_e_assembler_not_found,LastASBin);
              aktglobalswitches:=aktglobalswitches+[cs_asm_extern];
            end;
           if asfound then
            Message1(exec_t_using_assembler,LastASBin);
         end;
        FindAssembler:=LastASBin;
      end;


    Function TExternalAssembler.CallAssembler(const command:string; const para:TCmdStr):Boolean;
{$IFDEF USE_SYSUTILS}
      var
        DosExitCode:Integer;
{$ENDIF USE_SYSUTILS}
      begin
        callassembler:=true;
        if not(cs_asm_extern in aktglobalswitches) then
{$IFDEF USE_SYSUTILS}
        try
          DosExitCode := ExecuteProcess(command,para);
          if DosExitCode <>0
          then begin
            Message1(exec_e_error_while_assembling,tostr(dosexitcode));
            callassembler:=false;
          end;
        except on E:EOSError do
          begin
            Message1(exec_e_cant_call_assembler,tostr(E.ErrorCode));
            aktglobalswitches:=aktglobalswitches+[cs_asm_extern];
            callassembler:=false;
          end
        end
{$ELSE USE_SYSUTILS}
         begin
           swapvectors;
           exec(maybequoted(command),para);
           swapvectors;
           if (doserror<>0) then
            begin
              Message1(exec_e_cant_call_assembler,tostr(doserror));
              aktglobalswitches:=aktglobalswitches+[cs_asm_extern];
              callassembler:=false;
            end
           else
            if (dosexitcode<>0) then
             begin
              Message1(exec_e_error_while_assembling,tostr(dosexitcode));
              callassembler:=false;
             end;
         end
{$ENDIF USE_SYSUTILS}
        else
         AsmRes.AddAsmCommand(command,para,name);
      end;


    procedure TExternalAssembler.RemoveAsm;
      var
        g : file;
      begin
        if cs_asm_leave in aktglobalswitches then
         exit;
        if cs_asm_extern in aktglobalswitches then
         AsmRes.AddDeleteCommand(AsmFile)
        else
         begin
           assign(g,AsmFile);
           {$I-}
            erase(g);
           {$I+}
           if ioresult<>0 then;
         end;
      end;


    Function TExternalAssembler.DoAssemble:boolean;
      var
        s : TCmdStr;
      begin
        DoAssemble:=true;
        if DoPipe then
         exit;
        if not(cs_asm_extern in aktglobalswitches) then
         begin
           if SmartAsm then
            begin
              if (SmartFilesCount<=1) then
               Message1(exec_i_assembling_smart,name);
            end
           else
           Message1(exec_i_assembling,name);
         end;
        
        if CallAssembler(FindAssembler,MakeCmdLine) then
         RemoveAsm
        else
         begin
            DoAssemble:=false;
            GenerateError;
         end;
      end;


    Procedure TExternalAssembler.AsmFlush;
      begin
        if outcnt>0 then
         begin
           { suppress i/o error }
           {$i-}
           BlockWrite(outfile,outbuf,outcnt);
           {$i+}
           ioerror:=ioerror or (ioresult<>0);
           outcnt:=0;
         end;
      end;


    Procedure TExternalAssembler.AsmClear;
      begin
        outcnt:=0;
      end;


    Procedure TExternalAssembler.AsmWrite(const s:string);
      begin
        if OutCnt+length(s)>=AsmOutSize then
         AsmFlush;
        Move(s[1],OutBuf[OutCnt],length(s));
        inc(OutCnt,length(s));
        inc(AsmSize,length(s));
      end;


    Procedure TExternalAssembler.AsmWriteLn(const s:string);
      begin
        AsmWrite(s);
        AsmLn;
      end;


    Procedure TExternalAssembler.AsmWritePChar(p:pchar);
      var
        i,j : longint;
      begin
        i:=StrLen(p);
        j:=i;
        while j>0 do
         begin
           i:=min(j,AsmOutSize);
           if OutCnt+i>=AsmOutSize then
            AsmFlush;
           Move(p[0],OutBuf[OutCnt],i);
           inc(OutCnt,i);
           inc(AsmSize,i);
           dec(j,i);
           p:=pchar(@p[i]);
         end;
      end;


    Procedure TExternalAssembler.AsmLn;
      begin
        if OutCnt>=AsmOutSize-2 then
         AsmFlush;
        if (cs_link_on_target in aktglobalswitches) then
          begin
            OutBuf[OutCnt]:=target_info.newline[1];
            inc(OutCnt);
            inc(AsmSize);
            if length(target_info.newline)>1 then
             begin
               OutBuf[OutCnt]:=target_info.newline[2];
               inc(OutCnt);
               inc(AsmSize);
             end;
          end
        else
          begin
            OutBuf[OutCnt]:=source_info.newline[1];
            inc(OutCnt);
            inc(AsmSize);
            if length(source_info.newline)>1 then
             begin
               OutBuf[OutCnt]:=source_info.newline[2];
               inc(OutCnt);
               inc(AsmSize);
             end;
          end;
      end;


    function TExternalAssembler.MakeCmdLine: TCmdStr;
      begin
        result:=target_asm.asmcmd;
{$ifdef m68k}
        if aktoptprocessor = MC68020 then
          result:='-m68020 '+result
        else
          result:='-m68000 '+result;
{$endif}
        if (cs_link_on_target in aktglobalswitches) then
         begin
           Replace(result,'$ASM',maybequoted(ScriptFixFileName(AsmFile)));
           Replace(result,'$OBJ',maybequoted(ScriptFixFileName(ObjFile)));
         end
        else
         begin
{$ifdef hasunix}
          if DoPipe then
            Replace(result,'$ASM','')
          else
{$endif}
             Replace(result,'$ASM',maybequoted(AsmFile));
           Replace(result,'$OBJ',maybequoted(ObjFile));
         end;
      end;


    procedure TExternalAssembler.AsmCreate(Aplace:tcutplace);
      begin
        if SmartAsm then
         NextSmartName(Aplace);
{$ifdef hasunix}
        if DoPipe then
         begin
           Message1(exec_i_assembling_pipe,asmfile);
           POpen(outfile,FindAssembler+' '+MakeCmdLine,'W');
         end
        else
{$endif}
         begin
           Assign(outfile,asmfile);
           {$I-}
           Rewrite(outfile,1);
           {$I+}
           if ioresult<>0 then
             begin
               ioerror:=true;
               Message1(exec_d_cant_create_asmfile,asmfile);
             end;
         end;
        outcnt:=0;
        AsmSize:=0;
        AsmStartSize:=0;
      end;


    procedure TExternalAssembler.AsmClose;
      var
        f : file;
        FileAge : longint;
      begin
        AsmFlush;
{$ifdef hasunix}
        if DoPipe then
          begin
            if PClose(outfile) <> 0 then
              GenerateError;
          end
        else
{$endif}
         begin
         {Touch Assembler time to ppu time is there is a ppufilename}
           if ppufilename<>'' then
            begin
              Assign(f,ppufilename);
              {$I-}
              reset(f,1);
              {$I+}
              if ioresult=0 then
               begin
{$IFDEF USE_SYSUTILS}
                 FileAge := FileGetDate(GetFileHandle(f));
{$ELSE USE_SYSUTILS}
                 GetFTime(f, FileAge);
{$ENDIF USE_SYSUTILS}
                 close(f);
                 reset(outfile,1);
{$IFDEF USE_SYSUTILS}
                 FileSetDate(GetFileHandle(outFile),FileAge);
{$ELSE USE_SYSUTILS}
                 SetFTime(f, FileAge);
{$ENDIF USE_SYSUTILS}
               end;
            end;
           close(outfile);
         end;
      end;


    procedure TExternalAssembler.WriteTree(p:TAAsmoutput);
      begin
      end;


    procedure TExternalAssembler.WriteAsmList;
      begin
      end;


    procedure TExternalAssembler.MakeObject;
      begin
        AsmCreate(cut_normal);
        WriteAsmList;
        AsmClose;
        if not(ioerror) then
          DoAssemble;
      end;


{*****************************************************************************
                                  TInternalAssembler
*****************************************************************************}

    constructor TInternalAssembler.create(smart:boolean);
      begin
        inherited create(smart);
        objectoutput:=nil;
        objectdata:=nil;
        SmartAsm:=smart;
        currpass:=0;
      end;


   destructor TInternalAssembler.destroy;
{$ifdef MEMDEBUG}
      var
        d : tmemdebug;
{$endif}
      begin
{$ifdef MEMDEBUG}
        d := tmemdebug.create(name+' - agbin');
{$endif}
        objectdata.free;
        objectoutput.free;
{$ifdef MEMDEBUG}
        d.free;
{$endif}
      end;


{$ifdef GDB}
    procedure TInternalAssembler.convertstabs(p:pchar);
      var
        ofs,
        nidx,nother,ii,i,line,j : longint;
        code : integer;
        hp : pchar;
        reloc : boolean;
        ps : tasmsymbol;
        s : string;
      begin
        ofs:=0;
        reloc:=true;
        ps:=nil;
        if p[0]='"' then
         begin
           i:=1;
           { we can have \" inside the string !! PM }
           while not ((p[i]='"') and (p[i-1]<>'\')) do
            inc(i);
           p[i]:=#0;
           ii:=i;
           hp:=@p[1];
           s:=StrPas(@P[i+2]);
         end
        else
         begin
           hp:=nil;
           s:=StrPas(P);
           i:=-2; {needed below (PM) }
         end;
      { When in pass 1 then only alloc and leave }
        if currpass=1 then
         begin
           objectdata.allocstabs(hp);
           if assigned(hp) then
            p[i]:='"';
           exit;
         end;
      { Parse the rest of the stabs }
        if s='' then
         internalerror(33000);
        j:=pos(',',s);
        if j=0 then
         internalerror(33001);
        Val(Copy(s,1,j-1),nidx,code);
        if code<>0 then
         internalerror(33002);
        i:=i+2+j;
        Delete(s,1,j);
        j:=pos(',',s);
        if (j=0) then
         internalerror(33003);
        Val(Copy(s,1,j-1),nother,code);
        if code<>0 then
         internalerror(33004);
        i:=i+j;
        Delete(s,1,j);
        j:=pos(',',s);
        if j=0 then
         begin
           j:=256;
           ofs:=-1;
         end;
        Val(Copy(s,1,j-1),line,code);
        if code<>0 then
          internalerror(33005);
        if ofs=0 then
          begin
            Delete(s,1,j);
            i:=i+j;
            Val(s,ofs,code);
            if code=0 then
              reloc:=false
            else
              begin
                ofs:=0;
                s:=strpas(@p[i]);
                { handle asmsymbol or
                    asmsymbol - asmsymbol }
                j:=pos(' ',s);
                if j=0 then
                  j:=pos('-',s);
                { also try to handle
                  asmsymbol + constant
                  or
                  asmsymbol - constant }
                if j=0 then
                  j:=pos('+',s);

                if j<>0 then
                  begin
                    Val(Copy(s,j+1,255),ofs,code);
                    if code<>0 then
                      ofs:=0
                    else
                    { constant reading successful,
                      avoid further treatment by
                      setting s[j] to '+' }
                      s[j]:='+';
                  end
                else
                { single asmsymbol }
                  j:=256;
                { the symbol can be external
                  so we must use newasmsymbol and
                  not getasmsymbol !! PM }
                ps:=objectlibrary.newasmsymbol(copy(s,1,j-1),AB_EXTERNAL,AT_NONE);
                if not assigned(ps) then
                  internalerror(33006)
                else
                  begin
                    ofs:=ofs+ps.address;
                    reloc:=true;
                    objectlibrary.UsedAsmSymbolListInsert(ps);
                  end;
                if (j<256) and (s[j]<>'+') then
                  begin
                    i:=i+j;
                    s:=strpas(@p[i]);
                    if (s<>'') and (s[1]=' ') then
                      begin
                         j:=0;
                         while (s[j+1]=' ') do
                           inc(j);
                         i:=i+j;
                         s:=strpas(@p[i]);
                      end;
                    ps:=objectlibrary.getasmsymbol(s);
                    if not assigned(ps) then
                      internalerror(33007)
                    else
                      begin
                        if ps.section<>objectdata.currsec then
                          internalerror(33008);
                        ofs:=ofs-ps.address;
                        reloc:=false;
                        objectlibrary.UsedAsmSymbolListInsert(ps);
                      end;
                  end;
              end;
          end;
        { External references (AB_EXTERNAL and AB_COMMON) need a symbol relocation }
        if assigned(ps) and (ps.currbind in [AB_EXTERNAL,AB_COMMON]) then
          begin
            if currpass=2 then
              begin
                objectdata.writesymbol(ps);
                objectoutput.exportsymbol(ps);
              end;
            objectdata.writeSymStabs(ofs,hp,ps,nidx,nother,line,reloc)
          end
        else
          objectdata.writeStabs(ofs,hp,nidx,nother,line,reloc);
        if assigned(hp) then
         p[ii]:='"';
      end;


    procedure TInternalAssembler.emitlineinfostabs(nidx,line : longint);
      begin
        if currpass=1 then
          begin
            objectdata.allocstabs(nil);
            exit;
          end;

        if (nidx=n_textline) and assigned(funcname) and
           (target_info.use_function_relative_addresses) then
          objectdata.writeStabs(objectdata.currsec.datasize-funcname.address,nil,nidx,0,line,false)
        else
          objectdata.writeStabs(objectdata.currsec.datasize,nil,nidx,0,line,true);
      end;


    procedure TInternalAssembler.emitstabs(s:string);
      begin
        s:=s+#0;
        ConvertStabs(@s[1]);
      end;


    procedure TInternalAssembler.WriteFileLineInfo(var fileinfo : tfileposinfo);
      var
        curr_n : byte;
        hp : tasmsymbol;
        infile : tinputfile;
      begin
        if not ((cs_debuginfo in aktmoduleswitches) or
           (cs_gdb_lineinfo in aktglobalswitches)) then
         exit;

        { file changed ? (must be before line info) }
        if (fileinfo.fileindex<>0) and
           (stabslastfileinfo.fileindex<>fileinfo.fileindex) then
         begin
           infile:=current_module.sourcefiles.get_file(fileinfo.fileindex);
           if assigned(infile) then
            begin
              if includecount=0 then
               curr_n:=n_sourcefile
              else
               curr_n:=n_includefile;
              { get symbol for this includefile }
              hp:=objectlibrary.newasmsymbol('Ltext'+ToStr(IncludeCount),AB_LOCAL,AT_FUNCTION);
              if currpass=1 then
                begin
                  objectdata.allocsymbol(currpass,hp,0);
                  objectlibrary.UsedAsmSymbolListInsert(hp);
                end
              else
                objectdata.writesymbol(hp);
              { emit stabs }
              if (infile.path^<>'') then
                EmitStabs('"'+BsToSlash(FixPath(infile.path^,false))+'",'+tostr(curr_n)+
                          ',0,0,Ltext'+ToStr(IncludeCount));
              EmitStabs('"'+FixFileName(infile.name^)+'",'+tostr(curr_n)+
                        ',0,0,Ltext'+ToStr(IncludeCount));
              inc(includecount);
              { force new line info }
              stabslastfileinfo.line:=-1;
            end;
         end;

        { line changed ? }
        if (stabslastfileinfo.line<>fileinfo.line) and (fileinfo.line<>0) then
          emitlineinfostabs(n_line,fileinfo.line);
        stabslastfileinfo:=fileinfo;
      end;


    procedure TInternalAssembler.StartFileLineInfo;
      var
        fileinfo : tfileposinfo;
      begin
        FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
        n_line:=n_bssline;
        funcname:=nil;
        linecount:=1;
        includecount:=0;
        fileinfo.fileindex:=1;
        fileinfo.line:=1;
        WriteFileLineInfo(fileinfo);
      end;


    procedure TInternalAssembler.EndFileLineInfo;
      var
        hp : tasmsymbol;
      begin
          if not ((cs_debuginfo in aktmoduleswitches) or
             (cs_gdb_lineinfo in aktglobalswitches)) then
           exit;
        objectdata.createsection(sec_code,'',0,[]);
        hp:=objectlibrary.newasmsymbol('Letext',AB_LOCAL,AT_FUNCTION);
        if currpass=1 then
          begin
            objectdata.allocsymbol(currpass,hp,0);
            objectlibrary.UsedAsmSymbolListInsert(hp);
          end
        else
          objectdata.writesymbol(hp);
        EmitStabs('"",'+tostr(n_sourcefile)+',0,0,Letext');
      end;
{$endif GDB}


    function TInternalAssembler.MaybeNextList(var hp:Tai):boolean;
      begin
        { maybe end of list }
        while not assigned(hp) do
         begin
           if currlistidx<lists then
            begin
              inc(currlistidx);
              currlist:=list[currlistidx];
              hp:=Tai(currList.first);
            end
           else
            begin
              MaybeNextList:=false;
              exit;
            end;
         end;
        MaybeNextList:=true;
      end;


    function TInternalAssembler.TreePass0(hp:Tai):Tai;
      var
        l : longint;
      begin
        while assigned(hp) do
         begin
           case hp.typ of
             ait_align :
               begin
                 { always use the maximum fillsize in this pass to avoid possible
                   short jumps to become out of range }
                 Tai_align_abstract(hp).fillsize:=Tai_align_abstract(hp).aligntype;
                 objectdata.alloc(Tai_align_abstract(hp).fillsize);
               end;
             ait_datablock :
               begin
                 l:=used_align(size_2_align(Tai_datablock(hp).size),0,objectdata.currsec.addralign);
                 if SmartAsm or (not Tai_datablock(hp).is_global) then
                   begin
                     objectdata.allocalign(l);
                     objectdata.alloc(Tai_datablock(hp).size);
                   end;
               end;
             ait_real_80bit :
               objectdata.alloc(10);
             ait_real_64bit :
               objectdata.alloc(8);
             ait_real_32bit :
               objectdata.alloc(4);
             ait_comp_64bit :
               objectdata.alloc(8);
             ait_const_64bit,
             ait_const_32bit,
             ait_const_16bit,
             ait_const_8bit,
             ait_const_rva_symbol,
             ait_const_indirect_symbol :
               objectdata.alloc(tai_const(hp).size);
             ait_section:
               begin
                 objectdata.CreateSection(Tai_section(hp).sectype,Tai_section(hp).name^,Tai_section(hp).secalign,[]);
                 Tai_section(hp).sec:=objectdata.CurrSec;
               end;
             ait_symbol :
               objectdata.allocsymbol(currpass,Tai_symbol(hp).sym,0);
             ait_label :
               objectdata.allocsymbol(currpass,Tai_label(hp).l,0);
             ait_string :
               objectdata.alloc(Tai_string(hp).len);
             ait_instruction :
               begin
{$ifdef i386}
{$ifndef NOAG386BIN}
                 { reset instructions which could change in pass 2 }
                 Taicpu(hp).resetpass2;
                 objectdata.alloc(Taicpu(hp).Pass1(objectdata.currsec.datasize));
{$endif NOAG386BIN}
{$endif i386}
               end;
             ait_cutobject :
               if SmartAsm then
                break;
           end;
           hp:=Tai(hp.next);
         end;
        TreePass0:=hp;
      end;


    function TInternalAssembler.TreePass1(hp:Tai):Tai;
      var
        InlineLevel,
        l : longint;
{$ifdef i386}
{$ifndef NOAG386BIN}
        i : longint;
{$endif NOAG386BIN}
{$endif i386}
      begin
        inlinelevel:=0;
        while assigned(hp) do
         begin
{$ifdef GDB}
           { write stabs, no line info for inlined code }
           if (inlinelevel=0) and
              ((cs_debuginfo in aktmoduleswitches) or
               (cs_gdb_lineinfo in aktglobalswitches)) then
            begin
              if (objectdata.currsec<>nil) and
                 not(hp.typ in SkipLineInfo) then
               WriteFileLineInfo(tailineinfo(hp).fileinfo);
            end;
{$endif GDB}
           case hp.typ of
             ait_align :
               begin
                 { here we must determine the fillsize which is used in pass2 }
                 Tai_align_abstract(hp).fillsize:=align(objectdata.currsec.datasize,Tai_align_abstract(hp).aligntype)-
                   objectdata.currsec.datasize;
                 objectdata.alloc(Tai_align_abstract(hp).fillsize);
               end;
             ait_datablock :
               begin
                 if objectdata.currsec.sectype<>sec_bss then
                   Message(asmw_e_alloc_data_only_in_bss);
                 l:=used_align(size_2_align(Tai_datablock(hp).size),0,objectdata.currsec.addralign);
                 if Tai_datablock(hp).is_global and
                    not SmartAsm then
                  begin
                    objectdata.allocsymbol(currpass,Tai_datablock(hp).sym,Tai_datablock(hp).size);
                    { force to be common/external, must be after setaddress as that would
                      set it to AB_GLOBAL }
                    Tai_datablock(hp).sym.currbind:=AB_COMMON;
                  end
                 else
                  begin
                    objectdata.allocalign(l);
                    objectdata.allocsymbol(currpass,Tai_datablock(hp).sym,Tai_datablock(hp).size);
                    objectdata.alloc(Tai_datablock(hp).size);
                  end;
                 objectlibrary.UsedAsmSymbolListInsert(Tai_datablock(hp).sym);
               end;
             ait_real_80bit :
               objectdata.alloc(10);
             ait_real_64bit :
               objectdata.alloc(8);
             ait_real_32bit :
               objectdata.alloc(4);
             ait_comp_64bit :
               objectdata.alloc(8);
             ait_const_64bit,
             ait_const_32bit,
             ait_const_16bit,
             ait_const_8bit,
             ait_const_rva_symbol,
             ait_const_indirect_symbol :
               begin
                 objectdata.alloc(tai_const(hp).size);
                 if assigned(Tai_const(hp).sym) then
                   objectlibrary.UsedAsmSymbolListInsert(Tai_const(hp).sym);
                 if assigned(Tai_const(hp).endsym) then
                   objectlibrary.UsedAsmSymbolListInsert(Tai_const(hp).endsym);
               end;
             ait_section:
               begin
                 { use cached value }
                 objectdata.setsection(Tai_section(hp).sec);
{$ifdef GDB}
                 case Tai_section(hp).sectype of
                   sec_code :
                     n_line:=n_textline;
                   sec_data :
                     n_line:=n_dataline;
                   sec_bss :
                     n_line:=n_bssline;
                   else
                     n_line:=n_dataline;
                 end;
                 stabslastfileinfo.line:=-1;
{$endif GDB}
               end;
{$ifdef GDB}
             ait_stabn :
               begin
                 if assigned(Tai_stabn(hp).str) then
                   convertstabs(Tai_stabn(hp).str);
               end;
             ait_stabs :
               begin
                 if assigned(Tai_stabs(hp).str) then
                   convertstabs(Tai_stabs(hp).str);
               end;
             ait_stab_function_name :
               begin
                 if assigned(Tai_stab_function_name(hp).str) then
                  begin
                    funcname:=objectlibrary.getasmsymbol(strpas(Tai_stab_function_name(hp).str));
                    objectlibrary.UsedAsmSymbolListInsert(funcname);
                  end
                 else
                  funcname:=nil;
               end;
             ait_force_line :
               stabslastfileinfo.line:=0;
{$endif}
             ait_symbol :
               begin
                 objectdata.allocsymbol(currpass,Tai_symbol(hp).sym,0);
                 objectlibrary.UsedAsmSymbolListInsert(Tai_symbol(hp).sym);
               end;
             ait_symbol_end :
               begin
                 if target_info.system in [system_i386_linux,system_i386_beos] then
                  begin
                    Tai_symbol_end(hp).sym.size:=objectdata.currsec.datasize-Tai_symbol_end(hp).sym.address;
                    objectlibrary.UsedAsmSymbolListInsert(Tai_symbol_end(hp).sym);
                  end;
                end;
             ait_label :
               begin
                 objectdata.allocsymbol(currpass,Tai_label(hp).l,0);
                 objectlibrary.UsedAsmSymbolListInsert(Tai_label(hp).l);
               end;
             ait_string :
               objectdata.alloc(Tai_string(hp).len);
             ait_instruction :
               begin
{$ifdef i386}
{$ifndef NOAG386BIN}
                 objectdata.alloc(Taicpu(hp).Pass1(objectdata.currsec.datasize));
                 { fixup the references }
                 for i:=1 to Taicpu(hp).ops do
                  begin
                    with Taicpu(hp).oper[i-1]^ do
                     begin
                       case typ of
                         top_ref :
                           begin
                             if assigned(ref^.symbol) then
                              objectlibrary.UsedAsmSymbolListInsert(ref^.symbol);
                             if assigned(ref^.relsymbol) then
                              objectlibrary.UsedAsmSymbolListInsert(ref^.symbol);
                           end;
                       end;
                     end;
                  end;
{$endif NOAG386BIN}
{$endif i386}
               end;
             ait_direct :
               Message(asmw_f_direct_not_supported);
             ait_cutobject :
               if SmartAsm then
                break;
             ait_marker :
               if tai_marker(hp).kind=InlineStart then
                 inc(InlineLevel)
               else if tai_marker(hp).kind=InlineEnd then
                 dec(InlineLevel);
           end;
           hp:=Tai(hp.next);
         end;
        TreePass1:=hp;
      end;


    function TInternalAssembler.TreePass2(hp:Tai):Tai;
      var
        fillbuffer : tfillbuffer;
        InlineLevel,
        l  : longint;
        v  : int64;
{$ifdef x86}
        co : comp;
{$endif x86}
      begin
        inlinelevel:=0;
        { main loop }
        while assigned(hp) do
         begin
{$ifdef GDB}
           { write stabs, no line info for inlined code }
           if (inlinelevel=0) and
              ((cs_debuginfo in aktmoduleswitches) or
               (cs_gdb_lineinfo in aktglobalswitches)) then
            begin
              if (objectdata.currsec<>nil) and
                 not(hp.typ in SkipLineInfo) then
               WriteFileLineInfo(tailineinfo(hp).fileinfo);
            end;
{$endif GDB}
           case hp.typ of
             ait_align :
               begin
                 if objectdata.currsec.sectype=sec_bss then
                   objectdata.alloc(Tai_align_abstract(hp).fillsize)
                 else
                   objectdata.writebytes(Tai_align_abstract(hp).calculatefillbuf(fillbuffer)^,Tai_align_abstract(hp).fillsize);
               end;
             ait_section :
               begin
                 { use cached value }
                 objectdata.setsection(Tai_section(hp).sec);
{$ifdef GDB}
                 case Tai_section(hp).sectype of
                  sec_code : n_line:=n_textline;
                  sec_data : n_line:=n_dataline;
                   sec_bss : n_line:=n_bssline;
                 else
                  n_line:=n_dataline;
                 end;
                 stabslastfileinfo.line:=-1;
{$endif GDB}
               end;
             ait_symbol :
               begin
                 objectdata.writesymbol(Tai_symbol(hp).sym);
                 objectoutput.exportsymbol(Tai_symbol(hp).sym);
               end;
             ait_datablock :
               begin
                 l:=used_align(size_2_align(Tai_datablock(hp).size),0,objectdata.currsec.addralign);
                 objectdata.writesymbol(Tai_datablock(hp).sym);
                 objectoutput.exportsymbol(Tai_datablock(hp).sym);
                 if SmartAsm or (not Tai_datablock(hp).is_global) then
                   begin
                     objectdata.allocalign(l);
                     objectdata.alloc(Tai_datablock(hp).size);
                   end;
               end;
             ait_real_80bit :
               objectdata.writebytes(Tai_real_80bit(hp).value,10);
             ait_real_64bit :
               objectdata.writebytes(Tai_real_64bit(hp).value,8);
             ait_real_32bit :
               objectdata.writebytes(Tai_real_32bit(hp).value,4);
             ait_comp_64bit :
               begin
{$ifdef x86}
{$ifdef FPC}
                 co:=comp(Tai_comp_64bit(hp).value);
{$else}
                 co:=Tai_comp_64bit(hp).value;
{$endif}
                 objectdata.writebytes(co,8);
{$endif x86}
               end;
             ait_string :
               objectdata.writebytes(Tai_string(hp).str^,Tai_string(hp).len);
             ait_const_64bit,
             ait_const_32bit,
             ait_const_16bit,
             ait_const_8bit :
               begin
                 if assigned(tai_const(hp).sym) then
                   begin
                     if assigned(tai_const(hp).endsym) then
                       begin
                         if tai_const(hp).endsym.section<>tai_const(hp).sym.section then
                           internalerror(200404124);
                         v:=tai_const(hp).endsym.address-tai_const(hp).sym.address+Tai_const(hp).value;
                         objectdata.writebytes(v,tai_const(hp).size);
                       end
                     else
                       objectdata.writereloc(Tai_const(hp).value,Tai_const(hp).size,Tai_const(hp).sym,RELOC_ABSOLUTE);
                   end
                 else
                   objectdata.writebytes(Tai_const(hp).value,tai_const(hp).size);
               end;
             ait_const_rva_symbol :
               objectdata.writereloc(Tai_const(hp).value,sizeof(aint),Tai_const(hp).sym,RELOC_RVA);
             ait_label :
               begin
                 objectdata.writesymbol(Tai_label(hp).l);
                 { exporting shouldn't be necessary as labels are local,
                   but it's better to be on the safe side (PFV) }
                 objectoutput.exportsymbol(Tai_label(hp).l);
               end;
{$ifdef i386}
{$ifndef NOAG386BIN}
             ait_instruction :
               Taicpu(hp).Pass2(objectdata);
{$endif NOAG386BIN}
{$endif i386}
{$ifdef GDB}
             ait_stabn :
               convertstabs(Tai_stabn(hp).str);
             ait_stabs :
               convertstabs(Tai_stabs(hp).str);
             ait_stab_function_name :
               if assigned(Tai_stab_function_name(hp).str) then
                 funcname:=objectlibrary.getasmsymbol(strpas(Tai_stab_function_name(hp).str))
               else
                 funcname:=nil;
             ait_force_line :
               stabslastfileinfo.line:=0;
{$endif}
             ait_cutobject :
               if SmartAsm then
                break;
             ait_marker :
               if tai_marker(hp).kind=InlineStart then
                 inc(InlineLevel)
               else if tai_marker(hp).kind=InlineEnd then
                 dec(InlineLevel);
           end;
           hp:=Tai(hp.next);
         end;
        TreePass2:=hp;
      end;


    procedure TInternalAssembler.writetree;
      var
        hp : Tai;
      label
        doexit;
      begin
        objectdata:=objectoutput.newobjectdata(Objfile);
        { reset the asmsymbol list }
        objectlibrary.CreateUsedAsmsymbolList;

      { Pass 0 }
        currpass:=0;
        objectdata.createsection(sec_code,'',0,[]);
        objectdata.beforealloc;
        { start with list 1 }
        currlistidx:=1;
        currlist:=list[currlistidx];
        hp:=Tai(currList.first);
        while assigned(hp) do
         begin
           hp:=TreePass0(hp);
           MaybeNextList(hp);
         end;
        objectdata.afteralloc;
        { leave if errors have occured }
        if errorcount>0 then
         goto doexit;

      { Pass 1 }
        currpass:=1;
        objectdata.resetsections;
        objectdata.beforealloc;
        objectdata.createsection(sec_code,'',0,[]);
{$ifdef GDB}
        StartFileLineInfo;
{$endif GDB}
        { start with list 1 }
        currlistidx:=1;
        currlist:=list[currlistidx];
        hp:=Tai(currList.first);
        while assigned(hp) do
         begin
           hp:=TreePass1(hp);
           MaybeNextList(hp);
         end;
{$ifdef GDB}
        EndFileLineInfo;
{$endif GDB}
        objectdata.afteralloc;
        { check for undefined labels and reset }
        objectlibrary.UsedAsmSymbolListCheckUndefined;

        { leave if errors have occured }
        if errorcount>0 then
         goto doexit;

      { Pass 2 }
        currpass:=2;
        objectdata.resetsections;
        objectdata.beforewrite;
        objectdata.createsection(sec_code,'',0,[]);
{$ifdef GDB}
        StartFileLineInfo;
{$endif GDB}
        { start with list 1 }
        currlistidx:=1;
        currlist:=list[currlistidx];
        hp:=Tai(currList.first);
        while assigned(hp) do
         begin
           hp:=TreePass2(hp);
           MaybeNextList(hp);
         end;
{$ifdef GDB}
        EndFileLineInfo;
{$endif GDB}
        objectdata.afterwrite;

        { don't write the .o file if errors have occured }
        if errorcount=0 then
         begin
           { write objectfile }
           objectoutput.startobjectfile(ObjFile);
           objectoutput.writeobjectfile(objectdata);
           objectdata.free;
           objectdata:=nil;
         end;

      doexit:
        { reset the used symbols back, must be after the .o has been
          written }
        objectlibrary.UsedAsmsymbolListReset;
        objectlibrary.DestroyUsedAsmsymbolList;
      end;


    procedure TInternalAssembler.writetreesmart;
      var
        hp : Tai;
        startsectype : TAsmSectionType;
        place: tcutplace;
      begin
        NextSmartName(cut_normal);
        objectdata:=objectoutput.newobjectdata(Objfile);
        startsectype:=sec_code;

        { start with list 1 }
        currlistidx:=1;
        currlist:=list[currlistidx];
        hp:=Tai(currList.first);
        while assigned(hp) do
         begin
         { reset the asmsymbol list }
           objectlibrary.CreateUsedAsmSymbolList;

         { Pass 0 }
           currpass:=0;
           objectdata.resetsections;
           objectdata.beforealloc;
           objectdata.createsection(startsectype,'',0,[]);
           TreePass0(hp);
           objectdata.afteralloc;
           { leave if errors have occured }
           if errorcount>0 then
            exit;

         { Pass 1 }
           currpass:=1;
           objectdata.resetsections;
           objectdata.beforealloc;
           objectdata.createsection(startsectype,'',0,[]);
{$ifdef GDB}
           StartFileLineInfo;
{$endif GDB}
           TreePass1(hp);
{$ifdef GDB}
           EndFileLineInfo;
{$endif GDB}
           objectdata.afteralloc;
           { check for undefined labels }
           objectlibrary.UsedAsmSymbolListCheckUndefined;

           { leave if errors have occured }
           if errorcount>0 then
            exit;

         { Pass 2 }
           currpass:=2;
           objectoutput.startobjectfile(Objfile);
           objectdata.resetsections;
           objectdata.beforewrite;
           objectdata.createsection(startsectype,'',0,[]);
{$ifdef GDB}
           StartFileLineInfo;
{$endif GDB}
           hp:=TreePass2(hp);
           { save section type for next loop, must be done before EndFileLineInfo
             because that changes the section to sec_code }
           startsectype:=objectdata.currsec.sectype;
{$ifdef GDB}
           EndFileLineInfo;
{$endif GDB}
           objectdata.afterwrite;
           { leave if errors have occured }
           if errorcount>0 then
            exit;

           { write the current objectfile }
           objectoutput.writeobjectfile(objectdata);
           objectdata.free;
           objectdata:=nil;

           { reset the used symbols back, must be after the .o has been
             written }
           objectlibrary.UsedAsmsymbolListReset;
           objectlibrary.DestroyUsedAsmsymbolList;

           { end of lists? }
           if not MaybeNextList(hp) then
            break;

           { we will start a new objectfile so reset everything }
           { The place can still change in the next while loop, so don't init }
           { the writer yet (JM)                                              }
           if (hp.typ=ait_cutobject) then
            place := Tai_cutobject(hp).place
           else
            place := cut_normal;

           { avoid empty files }
           while assigned(hp) and
                 (Tai(hp).typ in [ait_marker,ait_comment,ait_section,ait_cutobject]) do
            begin
              if Tai(hp).typ=ait_section then
               startsectype:=Tai_section(hp).sectype
              else if (Tai(hp).typ=ait_cutobject) then
               place:=Tai_cutobject(hp).place;
              hp:=Tai(hp.next);
            end;
           { there is a problem if startsectype is sec_none !! PM }
           if startsectype=sec_none then
             startsectype:=sec_code;

           if not MaybeNextList(hp) then
             break;

           { start next objectfile }
           NextSmartName(place);
           objectdata:=objectoutput.newobjectdata(Objfile);
         end;
      end;


    procedure TInternalAssembler.MakeObject;

        procedure addlist(p:TAAsmoutput);
        begin
          inc(lists);
          list[lists]:=p;
        end;

      begin
        if cs_debuginfo in aktmoduleswitches then
          addlist(debuglist);
        addlist(codesegment);
        addlist(datasegment);
        addlist(consts);
        addlist(rttilist);
        addlist(picdata);
        if assigned(resourcestringlist) then
          addlist(resourcestringlist);
        addlist(bsssegment);
        if assigned(importssection) then
          addlist(importssection);
        if assigned(exportssection) and not UseDeffileForExports then
          addlist(exportssection);
        if assigned(resourcesection) then
          addlist(resourcesection);
{$warning TODO internal writer support for dwarf}
        {if assigned(dwarflist) then
          addlist(dwarflist);}

        if SmartAsm then
          writetreesmart
        else
          writetree;
      end;


{*****************************************************************************
                     Generate Assembler Files Main Procedure
*****************************************************************************}

    Procedure GenerateAsm(smart:boolean);
      var
        a : TAssembler;
      begin
        if not assigned(CAssembler[target_asm.id]) then
          Message(asmw_f_assembler_output_not_supported);
        a:=CAssembler[target_asm.id].Create(smart);
        a.MakeObject;
        a.Free;
      end;


    Procedure OnlyAsm;
      var
        a : TExternalAssembler;
      begin
        a:=TExternalAssembler.Create(false);
        a.DoAssemble;
        a.Free;
      end;


{*****************************************************************************
                                 Init/Done
*****************************************************************************}

    procedure RegisterAssembler(const r:tasminfo;c:TAssemblerClass);
      var
        t : tasm;
      begin
        t:=r.id;
        if assigned(asminfos[t]) then
          writeln('Warning: Assembler is already registered!')
        else
          Getmem(asminfos[t],sizeof(tasminfo));
        asminfos[t]^:=r;
        CAssembler[t]:=c;
      end;


    procedure InitAssembler;
      begin
        { target_asm is already set by readarguments }
        initoutputformat:=target_asm.id;
        aktoutputformat:=target_asm.id;
      end;


    procedure DoneAssembler;
      begin
      end;

end.
