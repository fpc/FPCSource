{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements some support functions and global variables

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
unit globals;

{$i fpcdefs.inc}

{ Use the internal linker by default }
{ define INTERNALLINKER}

interface

    uses
{$ifdef win32}
      windows,
{$endif}
{$ifdef hasunix}
  {$ifdef ver1_0}
      linux,
  {$else}
      Baseunix,unix,
  {$endif}
{$endif}
{$ifdef Delphi}
      SysUtils,
      dmisc,
{$else}
      strings,
      dos,
{$endif}
      cutils,cclasses,
      globtype,version,systems,cpuinfo;

    const
{$ifdef Splitheap}
       testsplit : boolean = false;
{$endif Splitheap}

       delphimodeswitches : tmodeswitches=
         [m_delphi,m_all,m_class,m_objpas,m_result,m_string_pchar,
          m_pointer_2_procedure,m_autoderef,m_tp_procvar,m_initfinal,m_default_ansistring,
          m_out,m_default_para,m_duplicate_names,m_hintdirective,m_add_pointer];
       fpcmodeswitches    : tmodeswitches=
         [m_fpc,m_all,m_string_pchar,m_nested_comment,m_repeat_forward,
          m_cvar_support,m_initfinal,m_add_pointer,m_hintdirective];
       objfpcmodeswitches : tmodeswitches=
         [m_objfpc,m_fpc,m_all,m_class,m_objpas,m_result,m_string_pchar,m_nested_comment,
          m_repeat_forward,m_cvar_support,m_initfinal,m_add_pointer,m_out,m_default_para,m_hintdirective];
       tpmodeswitches     : tmodeswitches=
         [m_tp7,m_all,m_tp_procvar,m_duplicate_names];
       gpcmodeswitches    : tmodeswitches=
         [m_gpc,m_all];
       macmodeswitches : tmodeswitches=
         [m_mac,m_all,m_class,m_result,m_repeat_forward];


       { maximum number of locals in bytes before warning is emitted }
       maxlocalsize = high(smallint);
       { maximum number of paras in bytes before warning is emitted }
       maxparasize = high(word);
       { maximum nesting of routines }
       maxnesting = 32;
       { maximum of units which are supported for a compilation }
       maxunits = 1024;

       treelogfilename = 'tree.log';

       { I don't know if this endian dependend }
       MathQNaN : array[0..7] of byte = (0,0,0,0,0,0,252,255);
       MathInf : array[0..7] of byte = (0,0,0,0,0,0,240,127);
       MathNegInf : array[0..7] of byte = (0,0,0,0,0,0,240,255);


    type
       TFPUException = (exInvalidOp, exDenormalized, exZeroDivide,
                        exOverflow, exUnderflow, exPrecision);
       TFPUExceptionMask = set of TFPUException;

       pfileposinfo = ^tfileposinfo;
       tfileposinfo = record
         line      : longint;
         column    : word;
         fileindex : word;
         { moduleindex : word; }
       end;

       TSearchPathList = class(TStringList)
         procedure AddPath(s:string;addfirst:boolean);overload;
         procedure AddPath(SrcPath,s:string;addfirst:boolean);overload;
         procedure AddList(list:TSearchPathList;addfirst:boolean);
         function  FindFile(const f : string;var foundfile:string):boolean;
       end;

       tcodepagestring = string[20];

       { the ordinal type used when evaluating constant integer expressions }
       TConstExprInt = int64;
       { ... the same unsigned }
       TConstExprUInt = {$ifdef fpc}qword{$else}int64{$endif};

    var
       { specified inputfile }
       inputdir       : dirstr;
       inputfile      : namestr;
       inputextension : extstr;
       { specified outputfile with -o parameter }
       outputfile     : namestr;
       { specified with -FE or -FU }
       outputexedir   : dirstr;
       outputunitdir  : dirstr;

       { things specified with parameters }
       paralinkoptions,
       paradynamiclinker : string;
       paraprintnodetree : byte;
       parapreprocess    : boolean;
       printnodefile     : text;

       { directory where the utils can be found (options -FD) }
       utilsdirectory : dirstr;
       { targetname specific prefix used by these utils (options -XP<path>) }
       utilsprefix    : dirstr;

       { some flags for global compiler switches }
       do_build,
       do_release,
       do_make       : boolean;
       { path for searching units, different paths can be seperated by ; }
       exepath            : dirstr;  { Path to ppc }
       librarysearchpath,
       unitsearchpath,
       objectsearchpath,
       includesearchpath  : TSearchPathList;

       { deffile }
       usewindowapi  : boolean;
       description   : string;
       dllversion    : string;
       dllmajor,dllminor,dllrevision : word;  { revision only for netware }

       akttokenpos,                  { position of the last token }
       aktfilepos : tfileposinfo;    { current position }

       nwscreenname : string;
       nwthreadname : string;
       nwcopyright  : string;

       codegenerror : boolean;           { true if there is an error reported }

       block_type : tblock_type;         { type of currently parsed block }

       parsing_para_level : integer;     { parameter level, used to convert
                                           proc calls to proc loads in firstcalln }
       compile_level : word;
       make_ref : boolean;
       resolving_forward : boolean;      { used to add forward reference as second ref }
       inlining_procedure : boolean;     { are we inlining a procedure }
       exceptblockcounter    : integer;  { each except block gets a unique number check gotos      }
       aktexceptblock        : integer;  { the exceptblock number of the current block (0 if none) }

     { commandline values }
       initdefines        : tstringlist;
       initglobalswitches : tglobalswitches;
       initmoduleswitches : tmoduleswitches;
       initlocalswitches  : tlocalswitches;
       initmodeswitches   : tmodeswitches;
       {$IFDEF testvarsets}
        Initsetalloc,                            {0=fixed, 1 =var}
       {$ENDIF}
       initpackenum       : longint;
       initalignment      : talignmentinfo;
       initoptprocessor,
       initspecificoptprocessor : tprocessors;
       initfputype        : tfputype;
       initasmmode        : tasmmode;
       initinterfacetype  : tinterfacetypes;
       initoutputformat   : tasm;
       initdefproccall    : tproccalloption;
       initsourcecodepage : tcodepagestring;

     { current state values }
       aktglobalswitches  : tglobalswitches;
       aktmoduleswitches  : tmoduleswitches;
       aktlocalswitches   : tlocalswitches;
       nextaktlocalswitches : tlocalswitches;
       localswitcheschanged : boolean;
       aktmodeswitches    : tmodeswitches;
       {$IFDEF testvarsets}
        aktsetalloc,
       {$ENDIF}
       aktpackenum        : longint;
       aktmaxfpuregisters : longint;
       aktalignment       : talignmentinfo;
       aktoptprocessor,
       aktspecificoptprocessor : tprocessors;
       aktfputype        : tfputype;
       aktasmmode         : tasmmode;
       aktinterfacetype   : tinterfacetypes;
       aktoutputformat    : tasm;
       aktdefproccall     : tproccalloption;
       aktsourcecodepage : tcodepagestring;

     { Memory sizes }
       heapsize,
       stacksize    : longint;

{$Ifdef EXTDEBUG}
     { parameter switches }
       debugstop : boolean;
{$EndIf EXTDEBUG}
       { windows / OS/2 application type }
       apptype : tapptype;

    const
       RelocSection : boolean = true;
       RelocSectionSetExplicitly : boolean = false;
       LinkTypeSetExplicitly : boolean = false;
       DLLsource : boolean = false;
       DLLImageBase : pstring = nil;
       UseDeffileForExport : boolean = true;
       ForceDeffileForExport : boolean = false;

       { used to set all registers used for each global function
         this should dramatically decrease the number of
         recompilations needed PM }
       simplify_ppu : boolean = true;

       { should we allow non static members ? }
       allow_only_static : boolean = false;

       Inside_asm_statement : boolean = false;

       global_unit_count : word = 0;

       { for error info in pp.pas }
       parser_current_file : string = '';

{$ifdef m68k}
       { PalmOS resources }
       palmos_applicationname : string = 'FPC Application';
       palmos_applicationid : string[4] = 'FPCA';
{$endif m68k}


    procedure abstract;

    function bstoslash(const s : string) : string;

    function getdatestr:string;
    function gettimestr:string;
    function filetimestring( t : longint) : string;

    procedure DefaultReplacements(var s:string);
    function  GetCurrentDir:string;
    function  path_absolute(const s : string) : boolean;
    Function  PathExists ( F : String) : Boolean;
    Function  FileExists ( Const F : String) : Boolean;
    Function  RemoveFile(const f:string):boolean;
    Function  RemoveDir(d:string):boolean;
    Function  GetFileTime ( Var F : File) : Longint;
    Function  GetNamedFileTime ( Const F : String) : Longint;
    Function  SplitPath(const s:string):string;
    Function  SplitFileName(const s:string):string;
    Function  SplitName(const s:string):string;
    Function  SplitExtension(Const HStr:String):String;
    Function  AddExtension(Const HStr,ext:String):String;
    Function  ForceExtension(Const HStr,ext:String):String;
    Function  FixPath(s:string;allowdot:boolean):string;
    function  FixFileName(const s:string):string;
    function  TargetFixPath(s:string;allowdot:boolean):string;
    function  TargetFixFileName(const s:string):string;
    procedure SplitBinCmd(const s:string;var bstr,cstr:string);
    function  FindFile(const f : string;path : string;var foundfile:string):boolean;
    function  FindFilePchar(const f : string;path : pchar;var foundfile:string):boolean;
    function  FindExe(const bin:string;var foundfile:string):boolean;
    function  GetShortName(const n:string):string;

    Procedure Shell(const command:string);
    function  GetEnvPChar(const envname:string):pchar;
    procedure FreeEnvPChar(p:pchar);

    function SetFPUExceptionMask(const Mask : TFPUExceptionMask) : TFPUExceptionMask;
    function is_number_float(d : double) : boolean;

    Function SetCompileMode(const s:string; changeInit: boolean):boolean;
    function SetAktProcCall(const s:string; changeInit: boolean):boolean;

    procedure InitGlobals;
    procedure DoneGlobals;

    function  string2guid(const s: string; var GUID: TGUID): boolean;
    function  guid2string(const GUID: TGUID): string;
    procedure swap_qword(var q : qword);

    function UpdateAlignmentStr(s:string;var a:talignmentinfo):boolean;

    {# Routine to get the required alignment for size of data, which will
       be placed in bss segment, according to the current alignment requirements }
    function var_align(siz: longint): longint;
    {# Routine to get the required alignment for size of data, which will
       be placed in data/const segment, according to the current alignment requirements }
    function const_align(siz: longint): longint;


implementation

    uses
      comphook;

    procedure abstract;
      begin
        do_internalerror(255);
      end;


    procedure WarnNonExistingPath(const path : string);
      begin
        if assigned({$ifndef FPCPROCVAR}@{$endif}do_comment) then
          do_comment(V_Hint,'Path "'+path+'" not found');
      end;


    function bstoslash(const s : string) : string;
    {
      return string s with all \ changed into /
    }
      var
         i : longint;
      begin
        for i:=1to length(s) do
         if s[i]='\' then
          bstoslash[i]:='/'
         else
          bstoslash[i]:=s[i];
         bstoslash[0]:=s[0];
      end;


{****************************************************************************
                               Time Handling
****************************************************************************}

    Function L0(l:longint):string;
    {
      return the string of value l, if l<10 then insert a zero, so
      the string is always at least 2 chars '01','02',etc
    }
      var
        s : string;
      begin
        Str(l,s);
        if l<10 then
         s:='0'+s;
        L0:=s;
      end;


   function gettimestr:string;
   {
     get the current time in a string HH:MM:SS
   }
      var
        hour,min,sec,hsec : word;
      begin
{$ifdef delphi}
        dmisc.gettime(hour,min,sec,hsec);
{$else delphi}
        dos.gettime(hour,min,sec,hsec);
{$endif delphi}
        gettimestr:=L0(Hour)+':'+L0(min)+':'+L0(sec);
      end;


   function getdatestr:string;
   {
     get the current date in a string YY/MM/DD
   }
      var
        Year,Month,Day,Wday : Word;
      begin
{$ifdef delphi}
        dmisc.getdate(year,month,day,wday);
{$else}
        dos.getdate(year,month,day,wday);
{$endif}
        getdatestr:=L0(Year)+'/'+L0(Month)+'/'+L0(Day);
      end;


   function  filetimestring( t : longint) : string;
   {
     convert dos datetime t to a string YY/MM/DD HH:MM:SS
   }
     var
       DT : DateTime;
     begin
       if t=-1 then
        begin
          FileTimeString:='Not Found';
          exit;
        end;
       unpacktime(t,DT);
       filetimestring:=L0(dt.Year)+'/'+L0(dt.Month)+'/'+L0(dt.Day)+' '+L0(dt.Hour)+':'+L0(dt.min)+':'+L0(dt.sec);
     end;


{****************************************************************************
                          Default Macro Handling
****************************************************************************}

     procedure DefaultReplacements(var s:string);
       begin
         { Replace some macro's }
         Replace(s,'$FPCVER',version_string);
         Replace(s,'$VERSION',version_string);
         Replace(s,'$FULLVERSION',full_version_string);
         Replace(s,'$FPCDATE',date_string);
         Replace(s,'$FPCTARGET',target_cpu_string);
         Replace(s,'$FPCCPU',target_cpu_string);
         Replace(s,'$TARGET',target_path);
         Replace(s,'$FPCOS',target_path);
       end;


{****************************************************************************
                               File Handling
****************************************************************************}

   function GetCurrentDir:string;
     var
       CurrentDir : string;
     begin
       GetDir(0,CurrentDir);
       GetCurrentDir:=FixPath(CurrentDir,false);
     end;


   function path_absolute(const s : string) : boolean;
   {
     is path s an absolute path?
   }
     begin
        path_absolute:=false;
{$ifdef unix}
        if (length(s)>0) and (s[1]='/') then
          path_absolute:=true;
{$else unix}
  {$ifdef amiga}
        if ((length(s)>0) and ((s[1]='\') or (s[1]='/'))) or (Pos(':',s) = length(s)) then
          path_absolute:=true;
  {$else}
        if ((length(s)>0) and ((s[1]='\') or (s[1]='/'))) or
           ((length(s)>2) and (s[2]=':') and ((s[3]='\') or (s[3]='/'))) then
          path_absolute:=true;
  {$endif amiga}
{$endif unix}
     end;

{$ifndef FPC}
    Procedure FindClose(var Info : SearchRec);
      Begin
      End;
{$endif not FPC}


    Function FileExists ( Const F : String) : Boolean;
      Var
         res : boolean;
{$ifndef delphi}
         Info : SearchRec;
{$endif}
      begin
{$ifdef delphi}
        res:=sysutils.FileExists(f);
{$else}
        findfirst(F,readonly+archive+hidden,info);
        res:=(doserror=0);
        findclose(Info);
{$endif delphi}
        if assigned({$ifndef FPCPROVCAR}@{$endif}do_comment) then
         begin
           if res then
             do_comment(V_Tried,'Searching file '+F+'... found')
           else
             do_comment(V_Tried,'Searching file '+F+'... not found');
         end;
        FileExists:=res;
      end;


    Function PathExists ( F : String) : Boolean;
      Var
        Info : SearchRec;
        disk : byte;
      begin
        { these operating systems have dos type drives }
        if source_info.system in [system_m68k_atari,system_i386_go32v2,
                                  system_i386_win32,system_i386_os2,
                                  system_i386_emx,system_i386_wdosx] then
        Begin
          if (Length(f)=3) and (F[2]=':') and (F[3] in ['/','\']) then
            begin
              if F[1] in ['A'..'Z'] then
                disk:=ord(F[1])-ord('A')+1
              else if F[1] in ['a'..'z'] then
                disk:=ord(F[1])-ord('a')+1
              else
                disk:=255;
              if disk=255 then
                PathExists:=false
              else
                PathExists:=(DiskSize(disk)<>-1);
              exit;
            end;
        end;
        if F[Length(f)] in ['/','\'] then
         Delete(f,length(f),1);
        findfirst(F,readonly+archive+hidden+directory,info);
        PathExists:=(doserror=0) and ((info.attr and directory)=directory);
        findclose(Info);
      end;


    Function RemoveFile(const f:string):boolean;
      var
        g : file;
      begin
        assign(g,f);
        {$I-}
         erase(g);
        {$I+}
        RemoveFile:=(ioresult=0);
      end;


    Function RemoveDir(d:string):boolean;
      begin
        if d[length(d)]=source_info.DirSep then
         Delete(d,length(d),1);
        {$I-}
         rmdir(d);
        {$I+}
        RemoveDir:=(ioresult=0);
      end;


    Function SplitPath(const s:string):string;
      var
        i : longint;
      begin
        i:=Length(s);
        while (i>0) and not(s[i] in ['/','\']) do
         dec(i);
        SplitPath:=Copy(s,1,i);
      end;


    Function SplitFileName(const s:string):string;
      var
        p : dirstr;
        n : namestr;
        e : extstr;
      begin
        FSplit(s,p,n,e);
        SplitFileName:=n+e;
      end;


    Function SplitName(const s:string):string;
      var
        i,j : longint;
      begin
        i:=Length(s);
        j:=Length(s);
        while (i>0) and not(s[i] in ['/','\']) do
         dec(i);
        while (j>0) and (s[j]<>'.') do
         dec(j);
        if j<=i then
         j:=255;
        SplitName:=Copy(s,i+1,j-(i+1));
      end;


    Function SplitExtension(Const HStr:String):String;
      var
        j : longint;
      begin
        j:=length(Hstr);
        while (j>0) and (Hstr[j]<>'.') do
         begin
           if hstr[j]=source_info.DirSep then
            j:=0
           else
            dec(j);
         end;
        if j=0 then
         j:=254;
        SplitExtension:=Copy(Hstr,j,255);
      end;


    Function AddExtension(Const HStr,ext:String):String;
      begin
        if (Ext<>'') and (SplitExtension(HStr)='') then
         AddExtension:=Hstr+Ext
        else
         AddExtension:=Hstr;
      end;


    Function ForceExtension(Const HStr,ext:String):String;
      var
        j : longint;
      begin
        j:=length(Hstr);
        while (j>0) and (Hstr[j]<>'.') do
         dec(j);
        if j=0 then
         j:=255;
        ForceExtension:=Copy(Hstr,1,j-1)+Ext;
      end;


    Function FixPath(s:string;allowdot:boolean):string;
      var
        i : longint;
      begin
        { Fix separator }
        for i:=1 to length(s) do
         if s[i] in ['/','\'] then
          s[i]:=source_info.DirSep;
        { Fix ending / }
        if (length(s)>0) and (s[length(s)]<>source_info.DirSep) and
           (s[length(s)]<>':') then
         s:=s+source_info.DirSep;
        { Remove ./ }
        if (not allowdot) and (s='.'+source_info.DirSep) then
         s:='';
        { return }
        if source_info.files_case_relevent then
         FixPath:=s
        else
         FixPath:=Lower(s);
      end;


   function FixFileName(const s:string):string;
     var
       i      : longint;
     begin
       if source_info.files_case_relevent then
        begin
          for i:=1 to length(s) do
           begin
             case s[i] of
               '/','\' :
                 FixFileName[i]:=source_info.dirsep;
               else
                 FixFileName[i]:=s[i];
             end;
           end;
        end
       else
        begin
          for i:=1 to length(s) do
           begin
             case s[i] of
               '/','\' :
                  FixFileName[i]:=source_info.dirsep;
               'A'..'Z' :
                  FixFileName[i]:=char(byte(s[i])+32);
                else
                  FixFileName[i]:=s[i];
             end;
           end;
        end;
       FixFileName[0]:=s[0];
     end;


    Function TargetFixPath(s:string;allowdot:boolean):string;
      var
        i : longint;
      begin
        { Fix separator }
        for i:=1 to length(s) do
         if s[i] in ['/','\'] then
          s[i]:=target_info.DirSep;
        { Fix ending / }
        if (length(s)>0) and (s[length(s)]<>target_info.DirSep) and
           (s[length(s)]<>':') then
         s:=s+target_info.DirSep;
        { Remove ./ }
        if (not allowdot) and (s='.'+target_info.DirSep) then
         s:='';
        { return }
        if target_info.files_case_relevent then
         TargetFixPath:=s
        else
         TargetFixPath:=Lower(s);
      end;


   function TargetFixFileName(const s:string):string;
     var
       i      : longint;
     begin
       if target_info.files_case_relevent then
        begin
          for i:=1 to length(s) do
           begin
             case s[i] of
               '/','\' :
                 TargetFixFileName[i]:=target_info.dirsep;
               else
                 TargetFixFileName[i]:=s[i];
             end;
           end;
        end
       else
        begin
          for i:=1 to length(s) do
           begin
             case s[i] of
               '/','\' :
                  TargetFixFileName[i]:=target_info.dirsep;
               'A'..'Z' :
                  TargetFixFileName[i]:=char(byte(s[i])+32);
                else
                  TargetFixFileName[i]:=s[i];
             end;
           end;
        end;
       TargetFixFileName[0]:=s[0];
     end;


   procedure SplitBinCmd(const s:string;var bstr,cstr:string);
     var
       i : longint;
     begin
       i:=pos(' ',s);
       if i>0 then
        begin
          bstr:=Copy(s,1,i-1);
          cstr:=Copy(s,i+1,length(s)-i);
        end
       else
        begin
          bstr:=s;
          cstr:='';
        end;
     end;

  procedure TSearchPathList.AddPath(s:string;addfirst:boolean);
    begin
      AddPath('',s,AddFirst);
    end;

   procedure TSearchPathList.AddPath(SrcPath,s:string;addfirst:boolean);
     var
       j        : longint;
       hs,hsd,
       CurrentDir,
       CurrPath : string;
       subdirfound : boolean;
       dir      : searchrec;
       hp       : TStringListItem;

       procedure addcurrpath;
       begin
         if addfirst then
          begin
            Remove(currPath);
            Insert(currPath);
          end
         else
          begin
            { Check if already in path, then we don't add it }
            hp:=Find(currPath);
            if not assigned(hp) then
             Concat(currPath);
          end;
       end;

     begin
       if s='' then
        exit;
     { Support default macro's }
       DefaultReplacements(s);
     { get current dir }
       CurrentDir:=GetCurrentDir;
       repeat
         { get currpath }
         if addfirst then
          begin
            j:=length(s);
            while (j>0) and (s[j]<>';') do
             dec(j);
            CurrPath:=FixPath(Copy(s,j+1,length(s)-j),false);
            if j=0 then
             s:=''
            else
             System.Delete(s,j,length(s)-j+1);
          end
         else
          begin
            j:=Pos(';',s);
            if j=0 then
             j:=255;
            CurrPath:=SrcPath+FixPath(Copy(s,1,j-1),false);
            System.Delete(s,1,j);
          end;
         { fix pathname }
         if CurrPath='' then
          CurrPath:='.'+source_info.DirSep
         else
          begin
            CurrPath:=FixPath(FExpand(CurrPath),false);
            if (CurrentDir<>'') and (Copy(CurrPath,1,length(CurrentDir))=CurrentDir) then
             begin
{$ifdef AMIGA}
               CurrPath:=CurrentDir+Copy(CurrPath,length(CurrentDir)+1,255);
{$else}
               CurrPath:='.'+source_info.DirSep+Copy(CurrPath,length(CurrentDir)+1,255);
{$endif}
             end;
          end;
         { wildcard adding ? }
         if pos('*',currpath)>0 then
          begin
            if currpath[length(currpath)]=source_info.dirsep then
             hs:=Copy(currpath,1,length(CurrPath)-1)
            else
             hs:=currpath;
            hsd:=SplitPath(hs);
            findfirst(hs,directory,dir);
            subdirfound:=false;
            while doserror=0 do
             begin
               if (dir.name<>'.') and
                  (dir.name<>'..') and
                  ((dir.attr and directory)<>0) then
                begin
                  subdirfound:=true;
                  currpath:=hsd+dir.name+source_info.dirsep;
                  hp:=Find(currPath);
                  if not assigned(hp) then
                   AddCurrPath;
                end;
               findnext(dir);
               if not subdirfound then
                 WarnNonExistingPath(currpath);
             end;
            FindClose(dir);
          end
         else
          begin
            if PathExists(currpath) then
             addcurrpath
            else
             WarnNonExistingPath(currpath);
          end;
       until (s='');
     end;


   procedure TSearchPathList.AddList(list:TSearchPathList;addfirst:boolean);
     var
       s : string;
       hl : TSearchPathList;
       hp,hp2 : TStringListItem;
     begin
       if list.empty then
        exit;
       { create temp and reverse the list }
       if addfirst then
        begin
          hl:=TSearchPathList.Create;
          hp:=TStringListItem(list.first);
          while assigned(hp) do
           begin
             hl.insert(hp.Str);
             hp:=TStringListItem(hp.next);
           end;
          while not hl.empty do
           begin
             s:=hl.GetFirst;
             Remove(s);
             Insert(s);
           end;
          hl.Free;
        end
       else
        begin
          hp:=TStringListItem(list.first);
          while assigned(hp) do
           begin
             hp2:=Find(hp.Str);
             { Check if already in path, then we don't add it }
             if not assigned(hp2) then
              Concat(hp.Str);
             hp:=TStringListItem(hp.next);
           end;
        end;
     end;


   function TSearchPathList.FindFile(const f : string;var foundfile:string):boolean;
     Var
       p : TStringListItem;
     begin
       FindFile:=false;
       p:=TStringListItem(first);
       while assigned(p) do
        begin
          {
            Search order for case sensitive systems:
             1. lowercase
             2. NormalCase
             3. UPPERCASE
            None case sensitive only lowercase
          }
          FoundFile:=p.Str+Lower(f);
          If FileExists(FoundFile) then
           begin
             FindFile:=true;
             exit;
           end;
{$ifdef UNIX}
          FoundFile:=p.Str+f;
          If FileExists(FoundFile) then
           begin
             FindFile:=true;
             exit;
           end;
          FoundFile:=p.Str+Upper(f);
          If FileExists(FoundFile) then
           begin
             FindFile:=true;
             exit;
           end;
{$endif UNIX}
          p:=TStringListItem(p.next);
        end;
       { Return original filename if not found }
       FoundFile:=f;
     end;


   Function GetFileTime ( Var F : File) : Longint;
     Var
     {$ifdef hasunix}
        info: Stat;
     {$endif}
       L : longint;
     begin
     {$ifdef hasunix}
       {$IFDEF VER1_0}
        FStat (F,Info);
        {$ifdef BSD}
        L:=Info.st_Mtime;
        {$else}
        L:=Info.Mtime;
        {$endif}
       {$ELSE}
        FPFStat (F,Info);
        L:=Info.st_Mtime;
       {$ENDIF}
     {$else}
       GetFTime(f,l);
     {$endif}
       GetFileTime:=L;
     end;


   Function GetNamedFileTime (Const F : String) : Longint;
     begin
       GetNamedFileTime:=do_getnamedfiletime(F);
     end;


   function FindFile(const f : string;path : string;var foundfile:string):boolean;
      Var
        singlepathstring : string;
        i : longint;
     begin
{$ifdef Unix}
       for i:=1 to length(path) do
        if path[i]=':' then
         path[i]:=';';
{$endif Unix}
       FindFile:=false;
       repeat
          i:=pos(';',path);
          if i=0 then
           i:=256;
          singlepathstring:=FixPath(copy(path,1,i-1),false);
          delete(path,1,i);
          {
            Search order for case sensitive systems:
             1. lowercase
             2. NormalCase
             3. UPPERCASE
            None case sensitive only lowercase
          }
          FoundFile:=singlepathstring+Lower(f);
          If FileExists(FoundFile) then
           begin
             FindFile:=true;
             exit;
           end;
{$ifdef UNIX}
          FoundFile:=singlepathstring+f;
          If FileExists(FoundFile) then
           begin
             FindFile:=true;
             exit;
           end;
          FoundFile:=singlepathstring+Upper(f);
          If FileExists(FoundFile) then
           begin
             FindFile:=true;
             exit;
           end;
{$endif UNIX}
       until path='';
       FoundFile:=f;
     end;


   function FindFilePchar(const f : string;path : pchar;var foundfile:string):boolean;
      Var
        singlepathstring : string;
        startpc,pc : pchar;
        sepch : char;
     begin
{$ifdef Unix}
       sepch:=':';
{$else}
       sepch:=';';
{$endif Unix}
       FindFilePchar:=false;
       pc:=path;
       repeat
          startpc:=pc;
          while (pc^<>sepch) and (pc^<>';') and (pc^<>#0) do
           inc(pc);
          move(startpc^,singlepathstring[1],pc-startpc);
          singlepathstring[0]:=char(longint(pc-startpc));
          singlepathstring:=FixPath(singlepathstring,false);
          {
            Search order for case sensitive systems:
             1. lowercase
             2. NormalCase
             3. UPPERCASE
            None case sensitive only lowercase
          }
          FoundFile:=singlepathstring+Lower(f);
          If FileExists(FoundFile) then
           begin
             FindFilePchar:=true;
             exit;
           end;
{$ifdef UNIX}
          FoundFile:=singlepathstring+f;
          If FileExists(FoundFile) then
           begin
             FindFilePchar:=true;
             exit;
           end;
          FoundFile:=singlepathstring+Upper(f);
          If FileExists(FoundFile) then
           begin
             FindFilePchar:=true;
             exit;
           end;
{$endif UNIX}
          if (pc^=#0) then
           break;
          inc(pc);
       until false;
     end;


   function  FindExe(const bin:string;var foundfile:string):boolean;
     var
       p : pchar;
       found : boolean;
     begin
       found:=FindFile(FixFileName(AddExtension(bin,source_info.exeext)),'.;'+exepath,foundfile);
       if not found then
        begin
          p:=GetEnvPchar('PATH');
          found:=FindFilePChar(FixFileName(AddExtension(bin,source_info.exeext)),p,foundfile);
          FreeEnvPChar(p);
        end;
       FindExe:=found;
     end;


    function GetShortName(const n:string):string;
{$ifdef win32}
      var
        hs,hs2 : string;
        i : longint;
{$endif}
{$ifdef go32v2}
      var
        hs : string;
{$endif}
{$ifdef watcom}
      var
        hs : string;
{$endif}
      begin
        GetShortName:=n;
{$ifdef win32}
        hs:=n+#0;
        i:=Windows.GetShortPathName(@hs[1],@hs2[1],high(hs2));
        if (i>0) and (i<=high(hs2)) then
          begin
            hs2[0]:=chr(strlen(@hs2[1]));
            GetShortName:=hs2;
          end;
{$endif}
{$ifdef go32v2}
        hs:=n;
        if Dos.GetShortName(hs) then
         GetShortName:=hs;
{$endif}
{$ifdef watcom}
        hs:=n;
        if Dos.GetShortName(hs) then
         GetShortName:=hs;
{$endif}
      end;


 {****************************************************************************
                               OS Dependent things
 ****************************************************************************}

    function GetEnvPChar(const envname:string):pchar;
      {$ifdef win32}
      var
        s     : string;
        i,len : longint;
        hp,p,p2 : pchar;
      {$endif}
      begin
      {$ifdef hasunix}
        GetEnvPchar:={$ifdef ver1_0}Linux.getenv{$else}BaseUnix.fpGetEnv{$endif}(envname);
        {$define GETENVOK}
      {$endif}
      {$ifdef win32}
        GetEnvPchar:=nil;
        p:=GetEnvironmentStrings;
        hp:=p;
        while hp^<>#0 do
         begin
           s:=strpas(hp);
           i:=pos('=',s);
           len:=strlen(hp);
           if upper(copy(s,1,i-1))=upper(envname) then
            begin
              GetMem(p2,len-length(envname));
              Move(hp[i],p2^,len-length(envname));
              GetEnvPchar:=p2;
              break;
            end;
           { next string entry}
           hp:=hp+len+1;
         end;
        FreeEnvironmentStrings(p);
        {$define GETENVOK}
      {$endif}
      {$ifdef os2}
        GetEnvPChar := Dos.GetEnvPChar (EnvName);
        {$define GETENVOK}
      {$endif}
      {$ifdef GETENVOK}
        {$undef GETENVOK}
      {$else}
        GetEnvPchar:=StrPNew({$ifdef delphi}DMisc{$else}Dos{$endif}.Getenv(envname));
      {$endif}
      end;


    procedure FreeEnvPChar(p:pchar);
      begin
      {$ifndef hasunix}
       {$ifndef os2}
        StrDispose(p);
       {$endif}
      {$endif}
      end;


    Procedure Shell(const command:string);
      { This is already defined in the linux.ppu for linux, need for the *
        expansion under linux }
      {$ifdef hasunix}
      begin
        {$ifdef ver1_0}Linux{$else}Unix{$endif}.Shell(command);
      end;
      {$else}
      {$ifdef amiga}
      begin
        exec('',command);
      end;
      {$else}
      var
        comspec : string;
      begin
        comspec:=getenv('COMSPEC');
        Exec(comspec,' /C '+command);
      end;
      {$endif}
      {$endif}


{$ifdef CPUI386}
      { later, this should be replaced by the math unit }
      const
        Default8087CW : word = $1332;
{$ASMMODE ATT}
      procedure Set8087CW(cw:word);assembler;
        asm
          movw cw,%ax
          movw %ax,default8087cw
          fnclex
          fldcw default8087cw
        end;


      function Get8087CW:word;assembler;
        asm
          pushl $0
          fnstcw (%esp)
          popl %eax
        end;


      function SetFPUExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
        var
          CtlWord: Word;
        begin
          CtlWord:=Get8087CW;
          Set8087CW( (CtlWord and $FFC0) or Byte(Longint(Mask)) );
          Result:=TFPUExceptionMask(CtlWord and $3F);
        end;
{$else CPUI386}
      function SetFPUExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
        begin
        end;
{$endif CPUI386}

      function is_number_float(d : double) : boolean;
        var
           bytearray : array[0..7] of byte;
        begin
          move(d,bytearray,8);
          { only 1.1 save, 1.0.x will use always little endian }
{$ifdef FPC_BIG_ENDIAN}
          result:=((bytearray[0] and $7f)<>$7f) or ((bytearray[1] and $f0)<>$f0);
{$else FPC_BIG_ENDIAN}
          result:=((bytearray[7] and $7f)<>$7f) or ((bytearray[6] and $f0)<>$f0);
{$endif FPC_BIG_ENDIAN}
        end;


      Function SetCompileMode(const s:string; changeInit: boolean):boolean;
      var
        b : boolean;
      begin
        b:=true;
        if s='DEFAULT' then
          aktmodeswitches:=initmodeswitches
        else
         if s='DELPHI' then
          aktmodeswitches:=delphimodeswitches
        else
         if s='TP' then
          aktmodeswitches:=tpmodeswitches
        else
         if s='FPC' then
          aktmodeswitches:=fpcmodeswitches
        else
         if s='OBJFPC' then
          aktmodeswitches:=objfpcmodeswitches
        else
         if s='GPC' then
          aktmodeswitches:=gpcmodeswitches
        else
         if s='MAC' then
          aktmodeswitches:=macmodeswitches
        else
         b:=false;

        if b and changeInit then
          initmodeswitches := aktmodeswitches;

        if b then
         begin
           { turn ansistrings on by default ? }
           if (m_delphi in aktmodeswitches) then
            begin
              include(aktlocalswitches,cs_ansistrings);
              if changeinit then
               include(initlocalswitches,cs_ansistrings);
            end
           else
            begin
              exclude(aktlocalswitches,cs_ansistrings);
              if changeinit then
               exclude(initlocalswitches,cs_ansistrings);
            end;
           { enum packing }
           if (m_tp7 in aktmodeswitches) then
             aktpackenum:=1
           else
             aktpackenum:=4;
           if changeinit then
             initpackenum:=aktpackenum;
{$ifdef i386}
           { Default to intel assembler for delphi/tp7 on i386 }
           if (m_delphi in aktmodeswitches) or
              (m_tp7 in aktmodeswitches) then
             aktasmmode:=asmmode_i386_intel;
           if changeinit then
             initasmmode:=aktasmmode;
{$endif i386}
         end;

        SetCompileMode:=b;
      end;


    function SetAktProcCall(const s:string; changeInit:boolean):boolean;
      const
        DefProcCallName : array[tproccalloption] of string[12] = ('',
         'CDECL',
         'CPPDECL',
         '', { compilerproc }
         'FAR16',
         'FPCCALL',
         'INLINE',
         '', { internproc }
         '', { palmossyscall }
         'PASCAL',
         'REGISTER',
         'SAFECALL',
         'STDCALL'
        );
      var
        t : tproccalloption;
      begin
        SetAktProcCall:=false;
        for t:=low(tproccalloption) to high(tproccalloption) do
         if DefProcCallName[t]=s then
          begin
            AktDefProcCall:=t;
            SetAktProcCall:=true;
            break;
          end;
        if changeinit then
         InitDefProcCall:=AktDefProcCall;
      end;



    { '('D1:'00000000-'D2:'0000-'D3:'0000-'D4:'0000-000000000000)' }
    function string2guid(const s: string; var GUID: TGUID): boolean;
        function ishexstr(const hs: string): boolean;
          var
            i: integer;
          begin
            ishexstr:=false;
            for i:=1 to Length(hs) do begin
              if not (hs[i] in ['0'..'9','A'..'F','a'..'f']) then
                exit;
            end;
            ishexstr:=true;
          end;
        function hexstr2longint(const hexs: string): longint;
          var
            i: integer;
            rl: longint;
          begin
            rl:=0;
            for i:=1 to length(hexs) do begin
              rl:=rl shl 4;
              case hexs[i] of
                '0'..'9' : inc(rl,ord(hexs[i])-ord('0'));
                'A'..'F' : inc(rl,ord(hexs[i])-ord('A')+10);
                'a'..'f' : inc(rl,ord(hexs[i])-ord('a')+10);
              end
            end;
            hexstr2longint:=rl;
          end;
      var
        i: integer;
      begin
        if (Length(s)=38) and (s[1]='{') and (s[38]='}') and
           (s[10]='-') and (s[15]='-') and (s[20]='-') and (s[25]='-') and
           ishexstr(copy(s,2,8)) and ishexstr(copy(s,11,4)) and
           ishexstr(copy(s,16,4)) and ishexstr(copy(s,21,4)) and
           ishexstr(copy(s,26,12)) then begin
          GUID.D1:=dword(hexstr2longint(copy(s,2,8)));
          { these values are arealdy in the correct range (4 chars = word) }
          GUID.D2:=word(hexstr2longint(copy(s,11,4)));
          GUID.D3:=word(hexstr2longint(copy(s,16,4)));
          for i:=0 to 1 do
            GUID.D4[i]:=byte(hexstr2longint(copy(s,21+i*2,2)));
          for i:=2 to 7 do
            GUID.D4[i]:=byte(hexstr2longint(copy(s,22+i*2,2)));
          string2guid:=true;
        end
        else
          string2guid:=false;
      end;

    function guid2string(const GUID: TGUID): string;
        function long2hex(l, len: longint): string;
          const
            hextbl: array[0..15] of char = '0123456789ABCDEF';
          var
            rs: string;
            i: integer;
          begin
            rs[0]:=chr(len);
            for i:=len downto 1 do begin
              rs[i]:=hextbl[l and $F];
              l:=l shr 4;
            end;
            long2hex:=rs;
          end;
      begin
        guid2string:=
          '{'+long2hex(GUID.D1,8)+
          '-'+long2hex(GUID.D2,4)+
          '-'+long2hex(GUID.D3,4)+
          '-'+long2hex(GUID.D4[0],2)+long2hex(GUID.D4[1],2)+
          '-'+long2hex(GUID.D4[2],2)+long2hex(GUID.D4[3],2)+
              long2hex(GUID.D4[4],2)+long2hex(GUID.D4[5],2)+
              long2hex(GUID.D4[6],2)+long2hex(GUID.D4[7],2)+
          '}';
      end;


    procedure swap_qword(var q : qword);
      begin
         q:=(qword(lo(q)) shl 32) or hi(q);
      end;

    function UpdateAlignmentStr(s:string;var a:talignmentinfo):boolean;
      var
        tok  : string;
        vstr : string;
        l    : longint;
        code : integer;
        b    : talignmentinfo;
      begin
        UpdateAlignmentStr:=true;
        uppervar(s);
        fillchar(b,sizeof(b),0);
        repeat
          tok:=GetToken(s,'=');
          if tok='' then
           break;
          vstr:=GetToken(s,',');
          val(vstr,l,code);
          if tok='PROC' then
           b.procalign:=l
          else if tok='JUMP' then
           b.jumpalign:=l
          else if tok='LOOP' then
           b.loopalign:=l
          else if tok='CONSTMIN' then
           b.constalignmin:=l
          else if tok='CONSTMAX' then
           b.constalignmax:=l
          else if tok='VARMIN' then
           b.varalignmin:=l
          else if tok='VARMAX' then
           b.varalignmax:=l
          else if tok='LOCALMIN' then
           b.localalignmin:=l
          else if tok='LOCALMAX' then
           b.localalignmax:=l
          else if tok='RECORDMIN' then
           b.recordalignmin:=l
          else if tok='RECORDMAX' then
           b.recordalignmax:=l
          else { Error }
           UpdateAlignmentStr:=false;
        until false;
        UpdateAlignment(a,b);
      end;


    function var_align(siz: longint): longint;
      begin
        siz := size_2_align(siz);
        var_align := used_align(siz,aktalignment.varalignmin,aktalignment.varalignmax);
      end;


    function const_align(siz: longint): longint;
      begin
        siz := size_2_align(siz);
        const_align := used_align(siz,aktalignment.constalignmin,aktalignment.constalignmax);
      end;


{****************************************************************************
                                    Init
****************************************************************************}

{$ifdef unix}
  {$define need_path_search}
{$endif unix}
{$ifdef os2}
  {$define need_path_search}
{$endif os2}

   procedure get_exepath;
     var
       hs1 : namestr;
       hs2 : extstr;
       p   : pchar;
     begin
{$ifdef delphi}
       exepath:=dmisc.getenv('PPC_EXEC_PATH');
{$else delphi}
       exepath:=dos.getenv('PPC_EXEC_PATH');
{$endif delphi}
       if exepath='' then
        fsplit(FixFileName(system.paramstr(0)),exepath,hs1,hs2);
{$ifdef need_path_search}
       if exepath='' then
        begin
          if pos(source_info.exeext,hs1) <>
               (length(hs1) - length(source_info.exeext)+1) then
            hs1 := hs1 + source_info.exeext;
          p:=GetEnvPchar('PATH');
          FindFilePChar(hs1,p,exepath);
          FreeEnvPChar(p);
          exepath:=SplitPath(exepath);
        end;
{$endif need_path_search}
       exepath:=FixPath(exepath,false);
     end;



   procedure DoneGlobals;
     begin
       initdefines.free;
       if assigned(DLLImageBase) then
         StringDispose(DLLImageBase);
       RelocSection:=true;
       RelocSectionSetExplicitly:=false;
       UseDeffileForExport:=true;
       librarysearchpath.Free;
       unitsearchpath.Free;
       objectsearchpath.Free;
       includesearchpath.Free;
     end;

   procedure InitGlobals;
     begin
        get_exepath;

      { reset globals }
        do_build:=false;
        do_release:=false;
        do_make:=true;
        compile_level:=0;
        DLLsource:=false;
        inlining_procedure:=false;
        resolving_forward:=false;
        make_ref:=false;

      { Output }
        OutputFile:='';
        OutputExeDir:='';
        OutputUnitDir:='';

      { Utils directory }
        utilsdirectory:='';
        utilsprefix:='';

      { Search Paths }
        librarysearchpath:=TSearchPathList.Create;
        unitsearchpath:=TSearchPathList.Create;
        includesearchpath:=TSearchPathList.Create;
        objectsearchpath:=TSearchPathList.Create;

      { Def file }
        usewindowapi:=false;
        description:='Compiled by FPC '+version_string+' - '+target_cpu_string;
        dllversion:='';
        nwscreenname := '';
        nwthreadname := '';
        nwcopyright  := '';

      { Init values }
        initmodeswitches:=fpcmodeswitches;
        initlocalswitches:=[cs_check_io,cs_typed_const_writable];
        initmoduleswitches:=[cs_extsyntax,cs_implicit_exceptions];
        initsourcecodepage:='8859-1';
        initglobalswitches:=[cs_check_unit_name,cs_link_static{$ifdef INTERNALLINKER},cs_link_internal,cs_link_map{$endif}];
        initoutputformat:=target_asm.id;
        fillchar(initalignment,sizeof(talignmentinfo),0);
{$ifdef i386}
        initoptprocessor:=Class386;
        initspecificoptprocessor:=Class386;

        initfputype:=fpu_x87;

        initpackenum:=4;
        {$IFDEF testvarsets}
        initsetalloc:=0;
        {$ENDIF}
        initasmmode:=asmmode_i386_att;
{$endif i386}
{$ifdef m68k}
        initoptprocessor:=MC68020;
        initpackenum:=4;
        {$IFDEF testvarsets}
         initsetalloc:=0;
        {$ENDIF}
        initasmmode:=asmmode_standard;
{$endif m68k}
{$ifdef powerpc}
        initoptprocessor:=PPC604;
        initpackenum:=4;
        {$IFDEF testvarsets}
         initsetalloc:=0;
        {$ENDIF}
        initasmmode:=asmmode_direct;
        initfputype:=fpu_standard;
{$endif powerpc}
{$ifdef sparc}
        initoptprocessor:=SPARC_V8;
        initpackenum:=4;
        {$IFDEF testvarsets}
         initsetalloc:=0;
        {$ENDIF}
        initasmmode:=asmmode_direct;
{$endif sparc}
{$ifdef arm}
        initpackenum:=4;
        {$IFDEF testvarsets}
        initsetalloc:=0;
        {$ENDIF}
        initasmmode:=asmmode_direct;
        initfputype:=fpu_fpa;
{$endif arm}
{$ifdef x86_64}
        initoptprocessor:=ClassDefault;
        initspecificoptprocessor:=ClassDefault;

        initfputype:=fpu_standard;

        initpackenum:=4;
        {$IFDEF testvarsets}
        initsetalloc:=0;
        {$ENDIF}
        initasmmode:=asmmode_direct;
{$endif x86_64}
        initinterfacetype:=it_interfacecom;
        initdefproccall:=pocall_default;
        initdefines:=TStringList.Create;

      { memory sizes, will be overriden by parameter or default for target
        in options or init_parser }
        stacksize:=0;
        heapsize:=0;

        apptype:=app_cui;
     end;

end.
{
  $Log$
  Revision 1.108  2003-10-08 14:10:16  mazen
  + ASMMODE ATT added to bypass fpc.cfg defaults

  Revision 1.107  2003/10/03 22:00:33  peter
    * parameter alignment fixes

  Revision 1.106  2003/10/03 14:16:48  marco
   * -XP<prefix> support

  Revision 1.105  2003/10/02 21:16:18  peter
    * delphi and tp7 mode use intel asm by default

  Revision 1.104  2003/10/01 20:34:48  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.103  2003/09/24 17:13:22  florian
    + processor type intialization for x86-64 added

  Revision 1.102  2003/09/18 15:38:17  marco
   * BSD 1.0.x still uses st_ prefixes.

  Revision 1.101  2003/09/16 16:17:01  peter
    * varspez in calls to push_addr_param

  Revision 1.100  2003/09/15 20:11:06  marco
   * fixed

  Revision 1.99  2003/09/14 20:26:18  marco
   * Unix reform

  Revision 1.98  2003/09/14 12:55:06  jonas
    * fixed compilation for non-x86

  Revision 1.97  2003/09/07 22:09:34  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.96  2003/09/06 16:47:24  florian
    + support of NaN and Inf in the compiler as values of real constants

  Revision 1.95  2003/09/05 17:41:12  florian
    * merged Wiktor's Watcom patches in 1.1

  Revision 1.94  2003/09/04 21:37:29  olle
    + added new lagnuage mode: MAC

  Revision 1.93  2003/09/03 11:18:36  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.92  2003/05/23 22:33:48  florian
    * fix some small flaws which prevent sparc linux system unit from compiling
    * some reformatting done

  Revision 1.91  2003/05/23 15:03:31  peter
    * fix previous commit for unix

  Revision 1.90  2003/05/23 14:39:56  peter
    * FindFilePChar added to allow PATH variables > 256 chars

  Revision 1.89  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.88  2003/04/27 11:21:32  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.87  2003/04/27 07:29:50  peter
    * current_procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.86  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.85  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.84  2003/03/23 23:21:42  hajny
    + emx target added

  Revision 1.83  2003/01/30 21:45:53  peter
    * amiga path fix (merged)

  Revision 1.82  2003/01/12 15:42:23  peter
    * m68k pathexist update from 1.0.x
    * palmos res update from 1.0.x

  Revision 1.81  2003/01/10 21:49:00  marco
   * more hasunix fixes

  Revision 1.80  2003/01/04 16:20:44  hajny
    * modified to make use of the common GetEnv code under OS/2

  Revision 1.79  2002/12/25 01:26:17  peter
    * delphi also has pointer archimetic

  Revision 1.78  2002/12/07 14:27:07  carl
    * 3% memory optimization
    * changed some types
    + added type checking with different size for call node and for
       parameters

  Revision 1.77  2002/12/06 17:50:00  peter
    * amiga fixes merged

  Revision 1.76  2002/12/01 22:07:41  carl
    * warning of portabilitiy problems with parasize / localsize
    + some added documentation

  Revision 1.75  2002/11/30 23:13:48  carl
    - cs_fp_emulation is no longer automatic for m68k target

  Revision 1.74  2002/11/30 11:08:46  carl
    * fix bug n last commit (this bug was only detected gby 1.0.x, fixed)

  Revision 1.73  2002/11/30 00:34:20  pierre
   * remove double in delphimodeswitches to allow compilation with -Cr option

  Revision 1.72  2002/11/29 22:31:19  carl
    + unimplemented hint directive added
    * hint directive parsing implemented
    * warning on these directives

  Revision 1.71  2002/11/20 10:11:46  mazen
  + TSearchPathList.AddPath supports a local path in addition to file name

  Revision 1.70  2002/11/16 14:49:12  carl
    - browser information is off by default

  Revision 1.69  2002/11/15 01:58:47  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.68  2002/11/09 15:38:39  carl
    + added var_align/const_align routines

  Revision 1.67  2002/10/16 19:01:43  peter
    + $IMPLICITEXCEPTIONS switch to turn on/off generation of the
      implicit exception frames for procedures with initialized variables
      and for constructors. The default is on for compatibility

  Revision 1.66  2002/09/05 19:28:29  peter
    * removed repetitive pass counting
    * display heapsize also for extdebug

  Revision 1.65  2002/08/19 19:36:42  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.64  2002/08/12 15:08:39  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.63  2002/08/10 14:46:29  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.62  2002/07/28 20:45:22  florian
    + added direct assembler reader for PowerPC

  Revision 1.61  2002/07/20 17:12:42  florian
    + source code page support

  Revision 1.60  2002/07/01 18:46:22  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.59  2002/07/01 16:23:52  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.58  2002/05/18 13:34:08  peter
    * readded missing revisions

  Revision 1.57  2002/05/16 19:46:36  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.55  2002/04/20 21:32:23  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.54  2002/04/07 13:24:30  carl
  + moved constant from cpuinfo, these are NOT cpu specific
    constant, but more related to compiler functionality.

  Revision 1.53  2002/04/02 17:11:28  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.52  2002/03/28 16:07:52  armin
  + initialize threadvars defined local in units

  Revision 1.51  2002/01/24 18:25:48  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

}
