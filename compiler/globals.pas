{
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

interface

    uses
{$ifdef win32}
      windows,
{$endif}
{$ifdef hasunix}
  {$ifdef havelinuxrtl10}
      linux,
  {$else}
      Baseunix,unix,
  {$endif}
{$endif}
      { comphook pulls in sysutils anyways }
      SysUtils,
{$IFDEF USE_SYSUTILS}
{$ELSE USE_SYSUTILS}
      strings,
      dos,
{$ENDIF USE_SYSUTILS}
      cutils,cclasses,
      cpuinfo,
      globtype,version,systems;

    const
       delphimodeswitches : tmodeswitches=
         [m_delphi,m_all,m_class,m_objpas,m_result,m_string_pchar,
          m_pointer_2_procedure,m_autoderef,m_tp_procvar,m_initfinal,m_default_ansistring,
          m_out,m_default_para,m_duplicate_names,m_hintdirective,m_add_pointer,
          m_property];
       fpcmodeswitches    : tmodeswitches=
         [m_fpc,m_all,m_string_pchar,m_nested_comment,m_repeat_forward,
          m_cvar_support,m_initfinal,m_add_pointer,m_hintdirective,
          m_property];
       objfpcmodeswitches : tmodeswitches=
         [m_objfpc,m_fpc,m_all,m_class,m_objpas,m_result,m_string_pchar,m_nested_comment,
          m_repeat_forward,m_cvar_support,m_initfinal,m_add_pointer,m_out,m_default_para,m_hintdirective,
          m_property];
       tpmodeswitches     : tmodeswitches=
         [m_tp7,m_all,m_tp_procvar,m_duplicate_names];
       gpcmodeswitches    : tmodeswitches=
         [m_gpc,m_all,m_tp_procvar];
       macmodeswitches : tmodeswitches=
         [m_mac,m_all,m_result,m_cvar_support,m_mac_procvar];


       { maximum nesting of routines }
       maxnesting = 32;

       { Filenames and extensions }
       sourceext  = '.pp';
       pasext     = '.pas';
       pext       = '.p';

       treelogfilename = 'tree.log';

{$if defined(CPUARM) and defined(FPUFPA)}
       MathQNaN : tdoublerec = (bytes : (0,0,252,255,0,0,0,0));
       MathInf : tdoublerec = (bytes : (0,0,240,127,0,0,0,0));
       MathNegInf : tdoublerec = (bytes : (0,0,240,255,0,0,0,0));
       MathPi : tdoublerec =  (bytes : (251,33,9,64,24,45,68,84));
{$else}
{$ifdef FPC_LITTLE_ENDIAN}
       MathQNaN : tdoublerec = (bytes : (0,0,0,0,0,0,252,255));
       MathInf : tdoublerec = (bytes : (0,0,0,0,0,0,240,127));
       MathNegInf : tdoublerec = (bytes : (0,0,0,0,0,0,240,255));
       MathPi : tdoublerec = (bytes : (24,45,68,84,251,33,9,64));
       MathPiExtended : textendedrec = (bytes : (53,194,104,33,162,218,15,201,0,64));
{$else FPC_LITTLE_ENDIAN}
       MathQNaN : tdoublerec = (bytes : (255,252,0,0,0,0,0,0));
       MathInf : tdoublerec = (bytes : (127,240,0,0,0,0,0,0));
       MathNegInf : tdoublerec = (bytes : (255,240,0,0,0,0,0,0));
       MathPi : tdoublerec =  (bytes : (64,9,33,251,84,68,45,24));
       MathPiExtended : textendedrec = (bytes : (64,0,201,15,218,162,33,104,194,53));
{$endif FPC_LITTLE_ENDIAN}
{$endif}

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

    var
       { specified inputfile }
       inputdir          : dirstr;
       inputfile         : namestr;
       inputextension    : extstr;
       { specified outputfile with -o parameter }
       outputfile        : namestr;
       outputprefix      : pstring;
       outputsuffix      : pstring;
       outputextension   : namestr;
       { specified with -FE or -FU }
       outputexedir      : dirstr;
       outputunitdir     : dirstr;

       { things specified with parameters }
       paratarget        : tsystem;
       paratargetdbg     : tdbg;
       paratargetasm     : tasm;
       paralinkoptions,
       paradynamiclinker : string;
       paraprintnodetree : byte;
       parapreprocess    : boolean;
       printnodefile     : text;

       {  typical cross compiling params}

       { directory where the utils can be found (options -FD) }
       utilsdirectory : dirstr;
       { targetname specific prefix used by these utils (options -XP<path>) }
       utilsprefix    : dirstr;
       cshared        : boolean;        { pass --shared to ld to link C libs shared}
       Dontlinkstdlibpath: Boolean;     { Don't add std paths to linkpath}
       rlinkpath      : dirstr;         { rpath-link linkdir override}

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
       autoloadunits      : string;

       { linking }
       usewindowapi  : boolean;
       description   : string;
       SetPEFlagsSetExplicity,
       ImageBaseSetExplicity,
       MinStackSizeSetExplicity,
       MaxStackSizeSetExplicity,
       DescriptionSetExplicity : boolean;
       dllversion    : string;
       dllmajor,
       dllminor,
       dllrevision   : word;  { revision only for netware }
       { win pe  }
       peflags : longint;
       minstacksize,
       maxstacksize,
       imagebase : aword;
       UseDeffileForExports    : boolean;
       UseDeffileForExportsSetExplicitly : boolean;
       GenerateImportSection,
       GenerateImportSectionSetExplicitly,
       RelocSection : boolean;
       RelocSectionSetExplicitly : boolean;
       LinkTypeSetExplicitly : boolean;

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
       LinkLibraryAliases : TLinkStrMap;
       LinkLibraryOrder   : TLinkStrMap;


     { commandline values }
       initglobalswitches : tglobalswitches;
       initmoduleswitches : tmoduleswitches;
       initlocalswitches  : tlocalswitches;
       initmodeswitches   : tmodeswitches;
       initoptimizerswitches : toptimizerswitches;
       {$IFDEF testvarsets}
        Initsetalloc,                            {0=fixed, 1 =var}
       {$ENDIF}
       initpackenum       : shortint;
       initalignment      : talignmentinfo;
       initcputype,
       initoptimizecputype : tcputype;
       initfputype        : tfputype;
       initasmmode        : tasmmode;
       initinterfacetype  : tinterfacetypes;
       initdefproccall    : tproccalloption;
       initsourcecodepage : tcodepagestring;

     { current state values }
       aktglobalswitches  : tglobalswitches;
       aktmoduleswitches  : tmoduleswitches;
       aktlocalswitches   : tlocalswitches;
       aktoptimizerswitches : toptimizerswitches;
       nextaktlocalswitches : tlocalswitches;
       localswitcheschanged : boolean;
       aktmodeswitches    : tmodeswitches;
       {$IFDEF testvarsets}
        aktsetalloc,
       {$ENDIF}
       aktpackrecords,
       aktpackenum        : shortint;
       aktmaxfpuregisters : longint;
       aktalignment       : talignmentinfo;
       aktcputype,
       aktoptimizecputype      : tcputype;
       aktfputype         : tfputype;
       aktasmmode         : tasmmode;
       aktinterfacetype   : tinterfacetypes;
       aktdefproccall     : tproccalloption;
       aktsourcecodepage  : tcodepagestring;

     { Memory sizes }
       heapsize,
       stacksize,
       jmp_buf_size : longint;

{$Ifdef EXTDEBUG}
     { parameter switches }
       debugstop : boolean;
{$EndIf EXTDEBUG}
       { windows / OS/2 application type }
       apptype : tapptype;

    const
       DLLsource : boolean = false;
       DLLImageBase : pstring = nil;

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

{$if defined(m68k) or defined(arm)}
       { PalmOS resources }
       palmos_applicationname : string = 'FPC Application';
       palmos_applicationid : string[4] = 'FPCA';
{$endif defined(m68k) or defined(arm)}

{$ifdef powerpc}
       { default calling convention used on MorphOS }
       syscall_convention : string = 'LEGACY';
{$endif powerpc}

       { default name of the C-style "main" procedure of the library/program }
       { (this will be prefixed with the target_info.cprefix)                }
       mainaliasname : string = 'main';

    procedure abstract;

    function bstoslash(const s : string) : string;

    function getdatestr:string;
    function gettimestr:string;
    function filetimestring( t : longint) : string;

    procedure DefaultReplacements(var s:string);
    {Gives the absolute path to the current directory}
    function  GetCurrentDir:string;
    {Gives the relative path to the current directory,
     with a trailing dir separator. E. g. on unix ./ }
    function CurDirRelPath(systeminfo: tsysteminfo): string;
    function  path_absolute(const s : string) : boolean;
    Function  PathExists ( F : String) : Boolean;
    Function  FileExists ( Const F : String) : Boolean;
    Function  DirectoryExists ( Const F : String) : Boolean;
    function  FileExistsNonCase(const path,fn:string;var foundfile:string):boolean;
    Function  RemoveFile(const f:string):boolean;
    Function  RemoveDir(d:string):boolean;
    Function  GetFileTime ( Var F : File) : Longint;
    Function  GetNamedFileTime ( Const F : String) : Longint;
    {Extracts the path without its filename, from a path.}
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
    procedure SplitBinCmd(const s:string;var bstr: String;var cstr:TCmdStr);
    function  FindFile(const f : string;path : string;var foundfile:string):boolean;
    function  FindFilePchar(const f : string;path : pchar;var foundfile:string):boolean;
    function  FindExe(const bin:string;var foundfile:string):boolean;
    function  GetShortName(const n:string):string;
    function  cleanpath(const s:string):String;

    function Shell(const command:string): longint;
    function  GetEnvPChar(const envname:string):pchar;
    procedure FreeEnvPChar(p:pchar);

    procedure SetFPUExceptionMask(const Mask: TFPUExceptionMask);
    function is_number_float(d : double) : boolean;
    { discern +0.0 and -0.0 }
    function get_real_sign(r: bestreal): longint;

    procedure InitGlobals;
    procedure DoneGlobals;

    function  string2guid(const s: string; var GUID: TGUID): boolean;
    function  guid2string(const GUID: TGUID): string;

    function SetAktProcCall(const s:string; var a:tproccalloption):boolean;
    function Setcputype(const s:string;var a:tcputype):boolean;
    function SetFpuType(const s:string;var a:tfputype):boolean;
    function UpdateAlignmentStr(s:string;var a:talignmentinfo):boolean;
    function UpdateOptimizerStr(s:string;var a:toptimizerswitches):boolean;

    {# Routine to get the required alignment for size of data, which will
       be placed in bss segment, according to the current alignment requirements }
    function var_align(siz: shortint): shortint;
    {# Routine to get the required alignment for size of data, which will
       be placed in data/const segment, according to the current alignment requirements }
    function const_align(siz: shortint): shortint;

{$IFDEF MACOS_USE_FAKE_SYSUTILS}

{Since SysUtils is not yet available for MacOS, fake
 Exceptions classes are included here.}

type
   { exceptions }
   Exception = class(TObject);

   EExternal = class(Exception);

   { integer math exceptions }
   EInterror    = Class(EExternal);
   EDivByZero   = Class(EIntError);
   ERangeError  = Class(EIntError);
   EIntOverflow = Class(EIntError);

   { General math errors }
   EMathError  = Class(EExternal);
   EInvalidOp  = Class(EMathError);
   EZeroDivide = Class(EMathError);
   EOverflow   = Class(EMathError);
   EUnderflow  = Class(EMathError);

{$ENDIF MACOS_USE_FAKE_SYSUTILS}

implementation

    uses
{$ifdef macos}
      macutils,
{$endif}
      comphook;

    procedure abstract;
      begin
        do_internalerror(255);
      end;


    procedure WarnNonExistingPath(const path : string);
      begin
        if assigned(do_comment) then
          do_comment(V_Tried,'Path "'+path+'" not found');
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
{$IFDEF USE_SYSUTILS}
        DecodeTime(Time,hour,min,sec,hsec);
{$ELSE USE_SYSUTILS}
        dos.gettime(hour,min,sec,hsec);
{$ENDIF USE_SYSUTILS}
        gettimestr:=L0(Hour)+':'+L0(min)+':'+L0(sec);
      end;


   function getdatestr:string;
   {
     get the current date in a string YY/MM/DD
   }
      var
{$IFDEF USE_SYSUTILS}
        Year,Month,Day: Word;
{$ELSE USE_SYSUTILS}
        Year,Month,Day,Wday : Word;
{$ENDIF USE_SYSUTILS}
      begin
{$IFDEF USE_SYSUTILS}
        DecodeDate(Date,year,month,day);
{$ELSE USE_SYSUTILS}
        dos.getdate(year,month,day,wday);
{$ENDIF USE_SYSUTILS}
        getdatestr:=L0(Year)+'/'+L0(Month)+'/'+L0(Day);
      end;


   function  filetimestring( t : longint) : string;
   {
     convert dos datetime t to a string YY/MM/DD HH:MM:SS
   }
     var
{$IFDEF USE_SYSUTILS}
       DT : TDateTime;
       hsec : word;
{$ELSE USE_SYSUTILS}
       DT : DateTime;
{$ENDIF USE_SYSUTILS}
       Year,Month,Day: Word;
       hour,min,sec : word;
     begin
       if t=-1 then
        begin
          Result := 'Not Found';
          exit;
        end;
{$IFDEF USE_SYSUTILS}
       DT := FileDateToDateTime(t);
       DecodeTime(DT,hour,min,sec,hsec);
       DecodeDate(DT,year,month,day);
{$ELSE USE_SYSUTILS}
       unpacktime(t,DT);
       year := DT.year;
       month := DT.month;
       day := DT.day;
       hour := DT.hour;
       min := DT.min;
       sec := DT.sec;
{$ENDIF USE_SYSUTILS}
       Result := L0(Year)+'/'+L0(Month)+'/'+L0(Day)+' '+L0(Hour)+':'+L0(min)+':'+L0(sec);
     end;


{****************************************************************************
                          Default Macro Handling
****************************************************************************}

     procedure DefaultReplacements(var s:string);
       begin
         { Replace some macros }
         Replace(s,'$FPCVERSION',version_string);
         Replace(s,'$FPCFULLVERSION',full_version_string);
         Replace(s,'$FPCDATE',date_string);
         Replace(s,'$FPCCPU',target_cpu_string);
         Replace(s,'$FPCOS',target_os_string);
         if tf_use_8_3 in Source_Info.Flags then
           Replace(s,'$FPCTARGET',target_os_string)
         else
           Replace(s,'$FPCTARGET',target_full_string);
       end;


{****************************************************************************
                               File Handling
****************************************************************************}

     var
       CachedCurrentDir : string;

   {Gives the absolute path to the current directory}
   function GetCurrentDir:string;
     begin
       if CachedCurrentDir='' then
         begin
           GetDir(0,CachedCurrentDir);
           CachedCurrentDir:=FixPath(CachedCurrentDir,false);
         end;
       result:=CachedCurrentDir;
     end;

   {Gives the relative path to the current directory,
    with a trailing dir separator. E. g. on unix ./ }
   function CurDirRelPath(systeminfo: tsysteminfo): string;

   begin
     if systeminfo.system <> system_powerpc_macos then
       CurDirRelPath:= '.'+systeminfo.DirSep
     else
       CurDirRelPath:= ':'
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
{$ifdef macos}
        if IsMacFullPath(s) then
          path_absolute:=true;
{$else}
        if ((length(s)>0) and ((s[1]='\') or (s[1]='/'))) or
           ((length(s)>2) and (s[2]=':') and ((s[3]='\') or (s[3]='/'))) then
          path_absolute:=true;
{$endif macos}
{$endif amiga}
{$endif unix}
     end;

{$ifndef FPC}
    Procedure FindClose(var Info : SearchRec);
      Begin
      End;
{$endif not FPC}


    Function FileExists ( Const F : String) : Boolean;
{$IFDEF USE_SYSUTILS}
{$ELSE USE_SYSUTILS}
      var
         Info : SearchRec;
{$ENDIF USE_SYSUTILS}
      begin
{$IFDEF USE_SYSUTILS}
        Result:=SysUtils.FileExists(f);
{$ELSE USE_SYSUTILS}
        findfirst(F,readonly+archive+hidden,info);
        result:=(doserror=0);
        findclose(Info);
{$ENDIF USE_SYSUTILS}
        if assigned(do_comment) then
         begin
           if Result then
             do_comment(V_Tried,'Searching file '+F+'... found')
           else
             do_comment(V_Tried,'Searching file '+F+'... not found');
         end;
      end;


    Function DirectoryExists ( Const F : String) : Boolean;
      begin
        Result:=SysUtils.DirectoryExists(f);
      end;


    function FileExistsNonCase(const path,fn:string;var foundfile:string):boolean;
      var
        fn2 : string;
      begin
        result:=false;
        if tf_files_case_sensitive in source_info.flags then
          begin
            {
              Search order for case sensitive systems:
               1. NormalCase
               2. lowercase
               3. UPPERCASE
            }
            FoundFile:=path+fn;
            If FileExists(FoundFile) then
             begin
               result:=true;
               exit;
             end;
            fn2:=Lower(fn);
            if fn2<>fn then
              begin
                FoundFile:=path+fn2;
                If FileExists(FoundFile) then
                 begin
                   result:=true;
                   exit;
                 end;
              end;
            fn2:=Upper(fn);
            if fn2<>fn then
              begin
                FoundFile:=path+fn2;
                If FileExists(FoundFile) then
                 begin
                   result:=true;
                   exit;
                 end;
              end;
          end
        else
          if tf_files_case_aware in source_info.flags then
            begin
              {
                Search order for case aware systems:
                 1. NormalCase
              }
              FoundFile:=path+fn;
              If FileExists(FoundFile) then
               begin
                 result:=true;
                 exit;
               end;
           end
        else
          begin
            { None case sensitive only lowercase }
            FoundFile:=path+Lower(fn);
            If FileExists(FoundFile) then
             begin
               result:=true;
               exit;
             end;
          end;
        { Set foundfile to something usefull }
        FoundFile:=fn;
      end;


    Function PathExists ( F : String) : Boolean;
      Var
{$IFDEF USE_SYSUTILS}
{$ELSE USE_SYSUTILS}
        FF : file;
{$ENDIF USE_SYSUTILS}
        A: word;
        I: longint;
      begin
        if F = '' then
          begin
            PathExists := true;
            exit;
          end;
{$ifdef USE_SYSUTILS}
        F := ExpandFileName(F);
{$else USE_SYSUTILS}
        F := FExpand (F);
{$endif USE_SYSUTILS}
        I := Pos (DriveSeparator, F);
        if (F [Length (F)] = DirectorySeparator)
                  and (((I = 0) and (Length (F) > 1)) or (I <> Length (F) - 1))
          then
            Delete (F, Length (F), 1);
{$IFDEF USE_SYSUTILS}
        I := FileGetAttr(F);
        PathExists := (I <> -1) and (I and faDirectory = faDirectory);
{$ELSE USE_SYSUTILS}
        Assign (FF, FExpand (F));
        GetFAttr (FF, A);
        PathExists := (DosError = 0) and (A and Directory = Directory);
{$ENDIF USE_SYSUTILS}
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
{$ifdef macos}
        while (i>0) and not(s[i] in [':']) do
         dec(i);
{$else macos}
        while (i>0) and not(s[i] in ['/','\']) do
         dec(i);
{$endif macos}
        SplitPath:=Copy(s,1,i);
      end;


    Function SplitFileName(const s:string):string;
{$IFDEF USE_SYSUTILS}
{$ELSE USE_SYSUTILS}
      var
        p : dirstr;
        n : namestr;
        e : extstr;
{$ENDIF USE_SYSUTILS}
      begin
{$IFDEF USE_SYSUTILS}
        SplitFileName:=ExtractFileName(s);
{$ELSE USE_SYSUTILS}
        FSplit(s,p,n,e);
        SplitFileName:=n+e;
{$ENDIF USE_SYSUTILS}
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
        if (tf_files_case_aware in source_info.flags) or
           (tf_files_case_sensitive in source_info.flags) then
         FixPath:=s
        else
         FixPath:=Lower(s);
      end;

  {Actually the version in macutils.pp could be used,
   but that would not work for crosscompiling, so this is a slightly modified
   version of it.}
  function TranslatePathToMac (const path: string; mpw: Boolean): string;

    function GetVolumeIdentifier: string;

    begin
      GetVolumeIdentifier := '{Boot}'
      (*
      if mpw then
        GetVolumeIdentifier := '{Boot}'
      else
        GetVolumeIdentifier := macosBootVolumeName;
      *)
    end;

    var
      slashPos, oldpos, newpos, oldlen, maxpos: Longint;

  begin
    oldpos := 1;
    slashPos := Pos('/', path);
    if (slashPos <> 0) then   {its a unix path}
      begin
        if slashPos = 1 then
          begin      {its a full path}
            oldpos := 2;
            TranslatePathToMac := GetVolumeIdentifier;
          end
        else     {its a partial path}
          TranslatePathToMac := ':';
      end
    else
      begin
        slashPos := Pos('\', path);
        if (slashPos <> 0) then   {its a dos path}
          begin
            if slashPos = 1 then
              begin      {its a full path, without drive letter}
                oldpos := 2;
                TranslatePathToMac := GetVolumeIdentifier;
              end
            else if (Length(path) >= 2) and (path[2] = ':') then {its a full path, with drive letter}
              begin
                oldpos := 4;
                TranslatePathToMac := GetVolumeIdentifier;
              end
            else     {its a partial path}
              TranslatePathToMac := ':';
          end;
      end;

    if (slashPos <> 0) then   {its a unix or dos path}
      begin
        {Translate "/../" to "::" , "/./" to ":" and "/" to ":" }
        newpos := Length(TranslatePathToMac);
        oldlen := Length(path);
        SetLength(TranslatePathToMac, newpos + oldlen);  {It will be no longer than what is already}
                                                                        {prepended plus length of path.}
        maxpos := Length(TranslatePathToMac);          {Get real maxpos, can be short if String is ShortString}

        {There is never a slash in the beginning, because either it was an absolute path, and then the}
        {drive and slash was removed, or it was a relative path without a preceding slash.}
        while oldpos <= oldlen do
          begin
            {Check if special dirs, ./ or ../ }
            if path[oldPos] = '.' then
              if (oldpos + 1 <= oldlen) and (path[oldPos + 1] = '.') then
                begin
                  if (oldpos + 2 > oldlen) or (path[oldPos + 2] in ['/', '\']) then
                    begin
                      {It is "../" or ".."  translates to ":" }
                      if newPos = maxPos then
                        begin {Shouldn't actually happen, but..}
                          Exit('');
                        end;
                      newPos := newPos + 1;
                      TranslatePathToMac[newPos] := ':';
                      oldPos := oldPos + 3;
                      continue;  {Start over again}
                    end;
                end
              else if (oldpos + 1 > oldlen) or (path[oldPos + 1] in ['/', '\']) then
                begin
                  {It is "./" or "."  ignor it }
                  oldPos := oldPos + 2;
                  continue;  {Start over again}
                end;

            {Collect file or dir name}
            while (oldpos <= oldlen) and not (path[oldPos] in ['/', '\']) do
              begin
                if newPos = maxPos then
                  begin {Shouldn't actually happen, but..}
                    Exit('');
                  end;
                newPos := newPos + 1;
                TranslatePathToMac[newPos] := path[oldPos];
                oldPos := oldPos + 1;
              end;

            {When we come here there is either a slash or we are at the end.}
            if (oldpos <= oldlen) then
              begin
                if newPos = maxPos then
                  begin {Shouldn't actually happen, but..}
                    Exit('');
                  end;
                newPos := newPos + 1;
                TranslatePathToMac[newPos] := ':';
                oldPos := oldPos + 1;
              end;
          end;

        SetLength(TranslatePathToMac, newpos);
      end
    else if (path = '.') then
      TranslatePathToMac := ':'
    else if (path = '..') then
      TranslatePathToMac := '::'
    else
      TranslatePathToMac := path;  {its a mac path}
  end;


   function FixFileName(const s:string):string;
     var
       i      : longint;
     begin
       if source_info.system = system_powerpc_MACOS then
         FixFileName:= TranslatePathToMac(s, true)
       else
        if (tf_files_case_aware in source_info.flags) or
           (tf_files_case_sensitive in source_info.flags) then
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
          FixFileName[0]:=s[0];
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
          FixFileName[0]:=s[0];
        end;
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
        if (tf_files_case_aware in target_info.flags) or
           (tf_files_case_sensitive in target_info.flags) then
         TargetFixPath:=s
        else
         TargetFixPath:=Lower(s);
      end;


   function TargetFixFileName(const s:string):string;
     var
       i : longint;
     begin
       if target_info.system = system_powerpc_MACOS then
         TargetFixFileName:= TranslatePathToMac(s, true)
       else
        if (tf_files_case_aware in target_info.flags) or
           (tf_files_case_sensitive in target_info.flags) then
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
           TargetFixFileName[0]:=s[0];
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
           TargetFixFileName[0]:=s[0];
         end;
     end;


   procedure SplitBinCmd(const s:string;var bstr:String;var cstr:TCmdStr);
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
       staridx,
       j        : longint;
       prefix,
       suffix,
       CurrentDir,
       currPath : string;
       subdirfound : boolean;
{$IFDEF USE_SYSUTILS}
       dir      : TSearchRec;
{$ELSE USE_SYSUTILS}
       dir      : searchrec;
{$ENDIF USE_SYSUTILS}
       hp       : TStringListItem;

       procedure AddCurrPath;
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
            currPath:= TrimSpace(Copy(s,j+1,length(s)-j));
            DePascalQuote(currPath);
            currPath:=FixPath(currPath,false);
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
            currPath:= TrimSpace(Copy(s,1,j-1));
            DePascalQuote(currPath);
            currPath:=SrcPath+FixPath(currPath,false);
            System.Delete(s,1,j);
          end;

         { fix pathname }
         if currPath='' then
           currPath:= CurDirRelPath(source_info)
         else
          begin
{$ifdef USE_SYSUTILS}
            currPath:=FixPath(ExpandFileName(currpath),false);
{$else USE_SYSUTILS}
            currPath:=FixPath(FExpand(currPath),false);
{$endif USE_SYSUTILS}
            if (CurrentDir<>'') and (Copy(currPath,1,length(CurrentDir))=CurrentDir) then
             begin
{$ifdef AMIGA}
               currPath:= CurrentDir+Copy(currPath,length(CurrentDir)+1,255);
{$else}
               currPath:= CurDirRelPath(source_info)+Copy(currPath,length(CurrentDir)+1,255);
{$endif}
             end;
          end;
         { wildcard adding ? }
         staridx:=pos('*',currpath);
         if staridx>0 then
          begin
            prefix:=SplitPath(Copy(currpath,1,staridx));
            suffix:=Copy(currpath,staridx+1,length(currpath));
            subdirfound:=false;
{$IFDEF USE_SYSUTILS}
            if findfirst(prefix+'*',faDirectory,dir) = 0 then
              begin
                repeat
                  if (dir.name<>'.') and
                      (dir.name<>'..') and
                      ((dir.attr and faDirectory)<>0) then
                    begin
                      subdirfound:=true;
                      currpath:=prefix+dir.name+suffix;
                      if (suffix='') or PathExists(currpath) then
                        begin
                          hp:=Find(currPath);
                          if not assigned(hp) then
                            AddCurrPath;
                        end;
                    end;
                until findnext(dir) <> 0;
              end;
{$ELSE USE_SYSUTILS}
            findfirst(prefix+'*',directory,dir);
            while doserror=0 do
             begin
               if (dir.name<>'.') and
                  (dir.name<>'..') and
                  ((dir.attr and directory)<>0) then
                begin
                  subdirfound:=true;
                  currpath:=prefix+dir.name+suffix;
                  if (suffix='') or PathExists(currpath) then
                    begin
                      hp:=Find(currPath);
                      if not assigned(hp) then
                        AddCurrPath;
                    end;
                end;
               findnext(dir);
             end;
{$ENDIF USE_SYSUTILS}
            FindClose(dir);
            if not subdirfound then
              WarnNonExistingPath(currpath);
          end
         else
          begin
            if PathExists(currpath) then
             AddCurrPath
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
          result:=FileExistsNonCase(p.Str,f,FoundFile);
          if result then
            exit;
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
       {$IFDEF havelinuxrtl10}
        FStat (F,Info);
        L:=Info.Mtime;
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
          result:=FileExistsNonCase(singlepathstring,f,FoundFile);
          if result then
            exit;
       until path='';
       FoundFile:=f;
     end;


   function FindFilePchar(const f : string;path : pchar;var foundfile:string):boolean;
      Var
        singlepathstring : string;
        startpc,pc : pchar;
        sepch : char;
     begin
       FindFilePchar:=false;
       if Assigned (Path) then
        begin
{$ifdef Unix}
          sepch:=':';
{$else}
{$ifdef macos}
          sepch:=',';
{$else}
          sepch:=';';
{$endif macos}
{$endif Unix}
          pc:=path;
          repeat
             startpc:=pc;
             while (pc^<>sepch) and (pc^<>';') and (pc^<>#0) do
              inc(pc);
             move(startpc^,singlepathstring[1],pc-startpc);
             singlepathstring[0]:=char(longint(pc-startpc));
             singlepathstring:=FixPath(singlepathstring,false);
             result:=FileExistsNonCase(singlepathstring,f,FoundFile);
             if result then
               exit;
             if (pc^=#0) then
               break;
             inc(pc);
          until false;
        end;
       foundfile:=f;
     end;


   function  FindExe(const bin:string;var foundfile:string):boolean;
     var
       p : pchar;
       found : boolean;
     begin
       found:=FindFile(FixFileName(AddExtension(bin,source_info.exeext)),'.;'+exepath,foundfile);
       if not found then
        begin
{$ifdef macos}
          p:=GetEnvPchar('Commands');
{$else}
          p:=GetEnvPchar('PATH');
{$endif}
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


function  CleanPath(const s:string):String;
{ Wrapper that encapsulate fexpand/expandfilename}
begin
{$IFDEF USE_SYSUTILS}
 cleanpath:=ExpandFileName(s);
{$else}
 cleanpath:=fexpand(s);
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
        GetEnvPchar:={$ifdef havelinuxrtl10}Linux.getenv{$else}BaseUnix.fpGetEnv{$endif}(envname);
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
        GetEnvPchar:=StrPNew(Dos.Getenv(envname));
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

{$IFDEF MORPHOS}
{$DEFINE AMIGASHELL}
{$ENDIF}
{$IFDEF AMIGA}
{$DEFINE AMIGASHELL}
{$ENDIF}

    function Shell(const command:string): longint;
      { This is already defined in the linux.ppu for linux, need for the *
        expansion under linux }
      {$ifdef hasunix}
      begin
        result := {$ifdef havelinuxrtl10}Linux{$else}Unix{$endif}.Shell(command);
      end;
      {$else}
      {$ifdef amigashell}
      begin
{$IFDEF USE_SYSUTILS}
        result := ExecuteProcess('',command);
{$ELSE USE_SYSUTILS}
        exec('',command);
        if (doserror <> 0) then
          result := doserror
        else
          result := dosexitcode;
      end;
{$ENDIF USE_SYSUTILS}
      {$else}
      var
        comspec : string;
      begin
        comspec:=getenv('COMSPEC');
{$IFDEF USE_SYSUTILS}
        result := ExecuteProcess(comspec,' /C '+command);
{$ELSE USE_SYSUTILS}
        Exec(comspec,' /C '+command);
        if (doserror <> 0) then
          result := doserror
        else
          result := dosexitcode;
      end;
{$ENDIF USE_SYSUTILS}
      {$endif}
      {$endif}

{$UNDEF AMIGASHELL}

{$ifdef CPUI386}
{$asmmode att}
  {$define HASSETFPUEXCEPTIONMASK}
      { later, this should be replaced by the math unit }
      const
        Default8087CW : word = $1332;

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


      procedure SetFPUExceptionMask(const Mask: TFPUExceptionMask);
        var
          CtlWord: Word;
        begin
          CtlWord:=Get8087CW;
          Set8087CW( (CtlWord and $FFC0) or Byte(Longint(Mask)) );
        end;
{$endif CPUI386}

{$ifdef CPUX86_64}
  {$define HASSETFPUEXCEPTIONMASK}
      { later, this should be replaced by the math unit }
      const
        Default8087CW : word = $1332;

      procedure Set8087CW(cw:word);assembler;
        asm
          movw cw,%ax
          movw %ax,default8087cw
          fnclex
          fldcw default8087cw
        end;


      function Get8087CW:word;assembler;
        asm
          pushq $0
          fnstcw (%rsp)
          popq %rax
        end;


      procedure SetSSECSR(w : dword);
        var
          _w : dword;
        begin
          _w:=w;
          asm
            ldmxcsr _w
          end;
        end;


      function GetSSECSR : dword;
          var
            _w : dword;
          begin
            asm
              stmxcsr _w
            end;
            result:=_w;
          end;


      procedure SetFPUExceptionMask(const Mask: TFPUExceptionMask);
        var
          CtlWord: Word;
          newmask : dword;
        const
          MM_MaskInvalidOp = %0000000010000000;
          MM_MaskDenorm    = %0000000100000000;
          MM_MaskDivZero   = %0000001000000000;
          MM_MaskOverflow  = %0000010000000000;
          MM_MaskUnderflow = %0000100000000000;
          MM_MaskPrecision = %0001000000000000;
        begin
          { classic FPU }
          CtlWord:=Get8087CW;
          Set8087CW( (CtlWord and $FFC0) or Byte(Longint(Mask)) );

          { SSE }

          newmask:=GetSSECSR;

          { invalid operation }
          if (exInvalidOp in mask) then
            newmask:=newmask or MM_MaskInvalidOp
          else
            newmask:=newmask and not(MM_MaskInvalidOp);

          { denormals }
          if (exDenormalized in mask) then
            newmask:=newmask or MM_MaskDenorm
          else
            newmask:=newmask and not(MM_MaskDenorm);

          { zero divide }
          if (exZeroDivide in mask) then
            newmask:=newmask or MM_MaskDivZero
          else
            newmask:=newmask and not(MM_MaskDivZero);

          { overflow }
          if (exOverflow in mask) then
            newmask:=newmask or MM_MaskOverflow
          else
            newmask:=newmask and not(MM_MaskOverflow);

          { underflow }
          if (exUnderflow in mask) then
            newmask:=newmask or MM_MaskUnderflow
          else
            newmask:=newmask and not(MM_MaskUnderflow);

          { Precision (inexact result) }
          if (exPrecision in mask) then
            newmask:=newmask or MM_MaskPrecision
          else
            newmask:=newmask and not(MM_MaskPrecision);
          SetSSECSR(newmask);
        end;
{$endif CPUX86_64}

{$ifdef CPUPOWERPC}
  {$define HASSETFPUEXCEPTIONMASK}
      procedure SetFPUExceptionMask(const Mask: TFPUExceptionMask);
        var
          newmask: record
            case byte of
               1: (d: double);
               2: (a,b: cardinal);
            end;
        begin
          { load current control register contents }
          asm
            mffs f0
            stfd f0,newmask.d
          end;
          { invalid operation: bit 24 (big endian, bit 0 = left-most bit) }
          if (exInvalidOp in mask) then
            newmask.b := newmask.b and not(1 shl (31-24))
          else
            newmask.b := newmask.b or (1 shl (31-24));

          { denormals can not cause exceptions on the PPC }

          { zero divide: bit 27 }
          if (exZeroDivide in mask) then
            newmask.b := newmask.b and not(1 shl (31-27))
          else
            newmask.b := newmask.b or (1 shl (31-27));

          { overflow: bit 25 }
          if (exOverflow in mask) then
            newmask.b := newmask.b and not(1 shl (31-25))
          else
            newmask.b := newmask.b or (1 shl (31-25));

          { underflow: bit 26 }
          if (exUnderflow in mask) then
            newmask.b := newmask.b and not(1 shl (31-26))
          else
            newmask.b := newmask.b or (1 shl (31-26));

          { Precision (inexact result): bit 28 }
          if (exPrecision in mask) then
            newmask.b := newmask.b and not(1 shl (31-28))
          else
            newmask.b := newmask.b or (1 shl (31-28));
          { update control register contents }
          asm
            lfd   f0, newmask.d
            mtfsf 255,f0
          end;
        end;
{$endif CPUPOWERPC}

{$ifdef CPUSPARC}
  {$define HASSETFPUEXCEPTIONMASK}
      procedure SetFPUExceptionMask(const Mask: TFPUExceptionMask);
        var
          fsr : cardinal;
        begin
          { load current control register contents }
          asm
            st %fsr,fsr
          end;
          { invalid operation: bit 27 }
          if (exInvalidOp in mask) then
            fsr:=fsr and not(1 shl 27)
          else
            fsr:=fsr or (1 shl 27);

          { zero divide: bit 24 }
          if (exZeroDivide in mask) then
            fsr:=fsr and not(1 shl 24)
          else
            fsr:=fsr or (1 shl 24);

          { overflow: bit 26 }
          if (exOverflow in mask) then
            fsr:=fsr and not(1 shl 26)
          else
            fsr:=fsr or (1 shl 26);

          { underflow: bit 25 }
          if (exUnderflow in mask) then
            fsr:=fsr and not(1 shl 25)
          else
            fsr:=fsr or (1 shl 25);

          { Precision (inexact result): bit 23 }
          if (exPrecision in mask) then
            fsr:=fsr and not(1 shl 23)
          else
            fsr:=fsr or (1 shl 23);
          { update control register contents }
          asm
            ld fsr,%fsr
          end;
        end;
{$endif CPUSPARC}

{$ifndef HASSETFPUEXCEPTIONMASK}
      procedure SetFPUExceptionMask(const Mask: TFPUExceptionMask);
        begin
        end;
{$endif HASSETFPUEXCEPTIONMASK}

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

    function get_real_sign(r: bestreal): longint;
      var
        p: pbyte;
      begin
        p := @r;
{$ifdef CPU_ARM}
        inc(p,4);
{$else}
{$ifdef FPC_LITTLE_ENDIAN}
        inc(p,sizeof(r)-1);
{$endif}
{$endif}
        if (p^ and $80) = 0 then
          result := 1
        else
          result := -1;
      end;

    function convertdoublerec(d : tdoublerec) : tdoublerec;{$ifdef USEINLINE}inline;{$endif}
{$ifdef CPUARM}
      var
        i : longint;
      begin
        for i:=0 to 3 do
          begin
            result.bytes[i+4]:=d.bytes[i];
            result.bytes[i]:=d.bytes[i+4];
          end;
{$else CPUARM}
      begin
        result:=d;
{$endif CPUARM}
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


    function SetAktProcCall(const s:string; var a:tproccalloption):boolean;
      const
        DefProcCallName : array[tproccalloption] of string[12] = ('',
         'CDECL',
         'CPPDECL',
         'FAR16',
         'OLDFPCCALL',
         '', { internproc }
         '', { syscall }
         'PASCAL',
         'REGISTER',
         'SAFECALL',
         'STDCALL',
         'SOFTFLOAT',
         'MWPASCAL'
        );
      var
        t  : tproccalloption;
        hs : string;
      begin
        result:=false;
        if (s = '') then
          exit;
        hs:=upper(s);
        if (hs = 'DEFAULT') then
          begin
            a := pocall_default;
            result := true;
            exit;
          end;
        for t:=low(tproccalloption) to high(tproccalloption) do
         if DefProcCallName[t]=hs then
          begin
            a:=t;
            result:=true;
            break;
          end;
      end;


    function Setcputype(const s:string;var a:tcputype):boolean;
      var
        t  : tcputype;
        hs : string;
      begin
        result:=false;
        hs:=Upper(s);
        for t:=low(tcputype) to high(tcputype) do
          if cputypestr[t]=hs then
            begin
              a:=t;
              result:=true;
              break;
            end;
      end;


    function SetFpuType(const s:string;var a:tfputype):boolean;
      var
        t : tfputype;
      begin
        result:=false;
        for t:=low(tfputype) to high(tfputype) do
          if fputypestr[t]=s then
            begin
              a:=t;
              result:=true;
              break;
            end;
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


    function UpdateOptimizerStr(s:string;var a:toptimizerswitches):boolean;
      var
        tok   : string;
        doset,
        found : boolean;
        opt   : toptimizerswitch;
      begin
        result:=true;
        uppervar(s);
        repeat
          tok:=GetToken(s,',');
          if tok='' then
           break;
          if Copy(tok,1,2)='NO' then
            begin
              delete(tok,1,2);
              doset:=false;
            end
          else
            doset:=true;
          found:=false;
          for opt:=low(toptimizerswitch) to high(toptimizerswitch) do
            begin
              if OptimizerSwitchStr[opt]=tok then
                begin
                  found:=true;
                  break;
                end;
            end;
          if found then
            begin
              if doset then
                include(a,opt)
              else
                exclude(a,opt);
            end
          else
            result:=false;
        until false;
      end;


    function var_align(siz: shortint): shortint;
      begin
        siz := size_2_align(siz);
        var_align := used_align(siz,aktalignment.varalignmin,aktalignment.varalignmax);
      end;


    function const_align(siz: shortint): shortint;
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
{$ifdef macos}
  {$define need_path_search}
{$endif macos}

   procedure get_exepath;
     var
       hs1 : namestr;
       hs2 : extstr;
{$IFDEF USE_SYSUTILS}
       exeName:String;
{$ENDIF USE_SYSUTILS}
{$ifdef need_path_search}
       p   : pchar;
{$endif need_path_search}
     begin
{$IFDEF USE_SYSUTILS}
       exepath:=GetEnvironmentVariable('PPC_EXEC_PATH');
{$ELSE USE_SYSUTILS}
       exepath:=dos.getenv('PPC_EXEC_PATH');
{$ENDIF USE_SYSUTILS}
       if exepath='' then
{$IFDEF USE_SYSUTILS}
        exeName := FixFileName(system.paramstr(0));
        exepath := ExtractFilePath(exeName);
        hs1 := ExtractFileName(exeName);
        hs2 := ExtractFileExt(exeName);
{$ELSE USE_SYSUTILS}
        fsplit(FixFileName(system.paramstr(0)),exepath,hs1,hs2);
{$ENDIF USE_SYSUTILS}
{$ifdef need_path_search}
       if exepath='' then
        begin
          if pos(source_info.exeext,hs1) <>
               (length(hs1) - length(source_info.exeext)+1) then
            hs1 := hs1 + source_info.exeext;
{$ifdef macos}
          p:=GetEnvPchar('Commands');
{$else macos}
          p:=GetEnvPchar('PATH');
{$endif macos}
          FindFilePChar(hs1,p,exepath);
          FreeEnvPChar(p);
          exepath:=SplitPath(exepath);
        end;
{$endif need_path_search}
       exepath:=FixPath(exepath,false);
     end;



   procedure DoneGlobals;
     begin
       if assigned(DLLImageBase) then
         StringDispose(DLLImageBase);
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
        LinkTypeSetExplicitly:=false;
        paratarget:=system_none;
        paratargetasm:=as_none;
        paratargetdbg:=dbg_none;

        { Output }
        OutputFile:='';
        OutputPrefix:=Nil;
        OutputSuffix:=Nil;
        OutputExtension:='';

        OutputExeDir:='';
        OutputUnitDir:='';

        { Utils directory }
        utilsdirectory:='';
        utilsprefix:='';
        cshared:=false;
        rlinkpath:='';

        { Search Paths }
        librarysearchpath:=TSearchPathList.Create;
        unitsearchpath:=TSearchPathList.Create;
        includesearchpath:=TSearchPathList.Create;
        objectsearchpath:=TSearchPathList.Create;

        { Def file }
        usewindowapi:=false;
        description:='Compiled by FPC '+version_string+' - '+target_cpu_string;
        DescriptionSetExplicity:=false;
        SetPEFlagsSetExplicity:=false;
        ImageBaseSetExplicity:=false;
        MinStackSizeSetExplicity:=false;
        MaxStackSizeSetExplicity:=false;

        dllversion:='';
        dllmajor:=1;
        dllminor:=0;
        dllrevision:=0;
        nwscreenname := '';
        nwthreadname := '';
        nwcopyright  := '';
        UseDeffileForExports:=false;
        UseDeffileForExportsSetExplicitly:=false;
        GenerateImportSection:=false;
        RelocSection:=false;
        RelocSectionSetExplicitly:=false;
        LinkTypeSetExplicitly:=false;
        { memory sizes, will be overriden by parameter or default for target
          in options or init_parser }
        stacksize:=0;
        { not initialized yet }
        jmp_buf_size:=-1;
        apptype:=app_cui;

        { Init values }
        initmodeswitches:=fpcmodeswitches;
        initlocalswitches:=[cs_check_io,cs_typed_const_writable];
        initmoduleswitches:=[cs_extsyntax,cs_implicit_exceptions];
        initglobalswitches:=[cs_check_unit_name,cs_link_static];
        initoptimizerswitches:=[];
        initsourcecodepage:='8859-1';
        initpackenum:=4;
        {$IFDEF testvarsets}
        initsetalloc:=0;
        {$ENDIF}
        fillchar(initalignment,sizeof(talignmentinfo),0);
        { might be overridden later }
        initasmmode:=asmmode_standard;
        initcputype:=cpu_none;
        initoptimizecputype:=cpu_none;
        initfputype:=fpu_none;
        initinterfacetype:=it_interfacecom;
        initdefproccall:=pocall_default;

        { Target specific defaults, these can override previous default options }
{$ifdef i386}
        initcputype:=cpu_Pentium;
        initoptimizecputype:=cpu_Pentium3;
        initfputype:=fpu_x87;
{$endif i386}
{$ifdef m68k}
        initcputype:=cpu_MC68020;
        initfputype:=fpu_soft;
{$endif m68k}
{$ifdef powerpc}
        initcputype:=cpu_PPC604;
        initfputype:=fpu_standard;
{$endif powerpc}
{$ifdef POWERPC64}
        initcputype:=cpu_PPC970;
        initfputype:=fpu_standard;
{$endif POWERPC64}
{$ifdef sparc}
        initcputype:=cpu_SPARC_V8;
        initfputype:=fpu_hard;
{$endif sparc}
{$ifdef arm}
        initcputype:=cpu_armv3;
        initfputype:=fpu_fpa;
{$endif arm}
{$ifdef x86_64}
        initcputype:=cpu_athlon64;
        initfputype:=fpu_sse64;
{$endif x86_64}
        if initoptimizecputype=cpu_none then
          initoptimizecputype:=initcputype;

        LinkLibraryAliases :=TLinkStrMap.Create;
        LinkLibraryOrder   :=TLinkStrMap.Create;

     end;

end.
