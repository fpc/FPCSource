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
{$ifdef os2}
      dos,
{$endif os2}
{$ifdef hasunix}
      Baseunix,unix,
{$endif}

{$IFNDEF USE_FAKE_SYSUTILS}
      sysutils,
{$ELSE}
      fksysutl,
{$ENDIF}

      { comphook pulls in sysutils anyways }
      cutils,cclasses,cfileutl,
      cpuinfo,
      globtype,version,systems;

    const
       delphimodeswitches =
         [m_delphi,m_all,m_class,m_objpas,m_result,m_string_pchar,
          m_pointer_2_procedure,m_autoderef,m_tp_procvar,m_initfinal,m_default_ansistring,
          m_out,m_default_para,m_duplicate_names,m_hintdirective,m_add_pointer,
          m_property,m_default_inline,m_except];
       fpcmodeswitches =
         [m_fpc,m_all,m_string_pchar,m_nested_comment,m_repeat_forward,
          m_cvar_support,m_initfinal,m_add_pointer,m_hintdirective,
          m_property,m_default_inline];
       objfpcmodeswitches =
         [m_objfpc,m_fpc,m_all,m_class,m_objpas,m_result,m_string_pchar,m_nested_comment,
          m_repeat_forward,m_cvar_support,m_initfinal,m_add_pointer,m_out,m_default_para,m_hintdirective,
          m_property,m_default_inline,m_except];
       tpmodeswitches =
         [m_tp7,m_all,m_tp_procvar,m_duplicate_names];
{$ifdef gpc_mode}
       gpcmodeswitches =
         [m_gpc,m_all,m_tp_procvar];
{$endif}
       macmodeswitches =
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
       tcodepagestring = string[20];

       tsettings = record
         globalswitches  : tglobalswitches;
         moduleswitches  : tmoduleswitches;
         localswitches   : tlocalswitches;
         modeswitches    : tmodeswitches;
         optimizerswitches : toptimizerswitches;
         { generate information necessary to perform these wpo's during a subsequent compilation }
         genwpoptimizerswitches: twpoptimizerswitches;
         { perform these wpo's using information generated during a previous compilation }
         dowpoptimizerswitches: twpoptimizerswitches;
         debugswitches   : tdebugswitches;
         { 0: old behaviour for sets <=256 elements
           >0: round to this size }
         setalloc,
         packenum        : shortint;
         alignment       : talignmentinfo;
         cputype,
         optimizecputype : tcputype;
         fputype         : tfputype;
         asmmode         : tasmmode;
         interfacetype   : tinterfacetypes;
         defproccall     : tproccalloption;
         sourcecodepage  : tcodepagestring;

         packrecords     : shortint;
         maxfpuregisters : shortint;

         minfpconstprec  : tfloattype;

        { CPU targets with microcontroller support can add a controller specific unit }
{$if defined(ARM)}
        controllertype   : tcontrollertype;
{$endif defined(ARM)}
       end;

    const
      LinkMapWeightDefault = 1000;

    type
      TLinkRec = record
        Key   : AnsiString;
        Value : AnsiString; // key expands to valuelist "value"
        Weight: longint;
      end;

      TLinkStrMap  = class
      private
        itemcnt : longint;
        fmap : Array Of TLinkRec;
        function  Lookup(key:Ansistring):longint;
        function getlinkrec(i:longint):TLinkRec;
      public
        procedure Add(key:ansistring;value:AnsiString='';weight:longint=LinkMapWeightDefault);
        procedure addseries(keys:AnsiString;weight:longint=LinkMapWeightDefault);
        function  AddDep(keyvalue:String):boolean;
        function  AddWeight(keyvalue:String):boolean;
        procedure SetValue(key:AnsiString;Weight:Integer);
        procedure SortonWeight;
        function Find(key:AnsiString):AnsiString;
        procedure Expand(src:TCmdStrList;dest: TLinkStrMap);
        procedure UpdateWeights(Weightmap:TLinkStrMap);
        constructor Create;
        property count : longint read itemcnt;
        property items[I:longint]:TLinkRec read getlinkrec; default;
      end;

      tpendingstate = record
        nextverbositystr : string;
        nextlocalswitches : tlocalswitches;
        nextverbosityfullswitch: longint;
        verbosityfullswitched,
        localswitcheschanged : boolean;
      end;


    var
       { specified inputfile }
       inputfilepath     : string;
       inputfilename     : string;
       { specified outputfile with -o parameter }
       outputfilename    : string;
       outputprefix      : pshortstring;
       outputsuffix      : pshortstring;
       { specified with -FE or -FU }
       outputexedir      : TPathStr;
       outputunitdir     : TPathStr;
       { specified with -FW and -Fw }
       wpofeedbackinput,
       wpofeedbackoutput : TPathStr;

       { things specified with parameters }
       paratarget        : tsystem;
       paratargetdbg     : tdbg;
       paratargetasm     : tasm;
       paralinkoptions   : TCmdStr;
       paradynamiclinker : string;
       paraprintnodetree : byte;
       parapreprocess    : boolean;
       printnodefile     : text;

       {  typical cross compiling params}

       { directory where the utils can be found (options -FD) }
       utilsdirectory : TPathStr;
       { targetname specific prefix used by these utils (options -XP<path>) }
       utilsprefix    : TCmdStr;
       cshared        : boolean;        { pass --shared to ld to link C libs shared}
       Dontlinkstdlibpath: Boolean;     { Don't add std paths to linkpath}
       rlinkpath      : TCmdStr;        { rpath-link linkdir override}
       sysrootpath    : TCmdStr;        { target system root to search dyn linker }

       { some flags for global compiler switches }
       do_build,
       do_release,
       do_make       : boolean;
       { path for searching units, different paths can be seperated by ; }
       exepath            : TPathStr;  { Path to ppc }
       librarysearchpath,
       unitsearchpath,
       objectsearchpath,
       includesearchpath,
       frameworksearchpath  : TSearchPathList;
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

       current_tokenpos,                  { position of the last token }
       current_filepos : tfileposinfo;    { current position }

       nwscreenname : string;
       nwthreadname : string;
       nwcopyright  : string;

       codegenerror : boolean;           { true if there is an error reported }

       block_type : tblock_type;         { type of currently parsed block }

       compile_level : word;
       exceptblockcounter    : integer;  { each except block gets a unique number check gotos      }
       current_exceptblock        : integer;  { the exceptblock number of the current block (0 if none) }
       LinkLibraryAliases : TLinkStrMap;
       LinkLibraryOrder   : TLinkStrMap;

       init_settings,
       current_settings   : tsettings;

       pendingstate       : tpendingstate;
     { Memory sizes }
       heapsize,
       stacksize,
       jmp_buf_size,
       jmp_buf_align : longint;

{$Ifdef EXTDEBUG}
     { parameter switches }
       debugstop : boolean;
{$EndIf EXTDEBUG}
       { windows / OS/2 application type }
       apptype : tapptype;

       features : tfeatures;

    const
       DLLsource : boolean = false;
       DLLImageBase : pshortstring = nil;

       { used to set all registers used for each global function
         this should dramatically decrease the number of
         recompilations needed PM }
       simplify_ppu : boolean = true;

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
       defaultmainaliasname = 'main';
       mainaliasname : string = defaultmainaliasname;

       { by default no local variable trashing }
       localvartrashing: longint = -1;
       { actual values are defined in ncgutil.pas }
       nroftrashvalues = 4;

    const
      default_settings : TSettings = (
        globalswitches : [cs_check_unit_name,cs_link_static];
        moduleswitches : [cs_extsyntax,cs_implicit_exceptions];
        localswitches : [cs_check_io,cs_typed_const_writable];
        modeswitches : fpcmodeswitches;
        optimizerswitches : [];
        genwpoptimizerswitches : [];
        dowpoptimizerswitches : [];
        debugswitches : [];
        setalloc : 0;
        packenum : 4;
        alignment : (
          procalign : 0;
          loopalign : 0;
          jumpalign : 0;
          constalignmin : 0;
          constalignmax : 0;
          varalignmin : 0;
          varalignmax : 0;
          localalignmin : 0;
          localalignmax : 0;
          recordalignmin : 0;
          recordalignmax : 0;
          maxCrecordalign : 0;
        );
{$ifdef i386}
        cputype : cpu_Pentium;
        optimizecputype : cpu_Pentium3;
        fputype : fpu_x87;
{$endif i386}
{$ifdef m68k}
        cputype : cpu_MC68020;
        optimizecputype : cpu_MC68020;
        fputype : fpu_soft;
{$endif m68k}
{$ifdef powerpc}
        cputype : cpu_PPC604;
        optimizecputype : cpu_ppc7400;
        fputype : fpu_standard;
{$endif powerpc}
{$ifdef POWERPC64}
        cputype : cpu_PPC970;
        optimizecputype : cpu_ppc970;
        fputype : fpu_standard;
{$endif POWERPC64}
{$ifdef sparc}
        cputype : cpu_SPARC_V8;
        optimizecputype : cpu_SPARC_V8;
        fputype : fpu_hard;
{$endif sparc}
{$ifdef arm}
        cputype : cpu_armv3;
        optimizecputype : cpu_armv3;
        fputype : fpu_fpa;
{$endif arm}
{$ifdef x86_64}
        cputype : cpu_athlon64;
        optimizecputype : cpu_athlon64;
        fputype : fpu_sse64;
{$endif x86_64}
{$ifdef avr}
        cputype : cpuinfo.cpu_avr;
        optimizecputype : cpuinfo.cpu_avr;
        fputype : fpu_none;
{$endif avr}
        asmmode : asmmode_standard;
        interfacetype : it_interfacecom;
        defproccall : pocall_default;
        sourcecodepage : '8859-1';
        packrecords     : 0;
        maxfpuregisters : 0;
        minfpconstprec : s32real;
      );

    var
      starttime  : real;

    function getdatestr:string;
    function gettimestr:string;
    function filetimestring( t : longint) : string;
    function getrealtime : real;

    procedure DefaultReplacements(var s:ansistring);

    function Shell(const command:ansistring): longint;
    function  GetEnvPChar(const envname:string):pchar;
    procedure FreeEnvPChar(p:pchar);

    function is_number_float(d : double) : boolean;
    { discern +0.0 and -0.0 }
    function get_real_sign(r: bestreal): longint;

    procedure InitGlobals;
    procedure DoneGlobals;

    function  string2guid(const s: string; var GUID: TGUID): boolean;
    function  guid2string(const GUID: TGUID): string;

    function SetAktProcCall(const s:string; var a:tproccalloption):boolean;
    function Setabitype(const s:string;var a:tabi):boolean;
    function Setcputype(const s:string;var a:tcputype):boolean;
    function SetFpuType(const s:string;var a:tfputype):boolean;
{$if defined(arm) or defined(avr)}
    function SetControllerType(const s:string;var a:tcontrollertype):boolean;
{$endif defined(arm) or defined(avr)}
    function UpdateAlignmentStr(s:string;var a:talignmentinfo):boolean;
    function UpdateOptimizerStr(s:string;var a:toptimizerswitches):boolean;
    function UpdateWpoStr(s: string; var a: twpoptimizerswitches): boolean;
    function UpdateDebugStr(s:string;var a:tdebugswitches):boolean;
    function IncludeFeature(const s : string) : boolean;
    function SetMinFPConstPrec(const s: string; var a: tfloattype) : boolean;

    {# Routine to get the required alignment for size of data, which will
       be placed in bss segment, according to the current alignment requirements }
    function var_align(siz: longint): shortint;
    {# Routine to get the required alignment for size of data, which will
       be placed in data/const segment, according to the current alignment requirements }
    function const_align(siz: longint): shortint;
{$ifdef ARM}
    function is_double_hilo_swapped: boolean;{$ifdef USEINLINE}inline;{$endif}
{$endif ARM}
    function floating_point_range_check_error : boolean;

implementation

    uses
{$ifdef macos}
      macutils,
{$endif}
      comphook;

{****************************************************************************
                                 TLinkStrMap
****************************************************************************}

    Constructor TLinkStrMap.create;
      begin
        inherited;
        itemcnt:=0;
      end;


    procedure TLinkStrMap.Add(key:ansistring;value:AnsiString='';weight:longint=LinkMapWeightDefault);
      begin
        if lookup(key)<>-1 Then
          exit;
        if itemcnt<=length(fmap) Then
          setlength(fmap,itemcnt+10);
        fmap[itemcnt].key:=key;
        fmap[itemcnt].value:=value;
        fmap[itemcnt].weight:=weight;
        inc(itemcnt);
      end;


    function  TLinkStrMap.AddDep(keyvalue:String):boolean;
      var
        i : Longint;
      begin
        AddDep:=false;
        i:=pos('=',keyvalue);
        if i=0 then
          exit;
        Add(Copy(KeyValue,1,i-1),Copy(KeyValue,i+1,length(KeyValue)-i));
        AddDep:=True;
      end;


    function  TLinkStrMap.AddWeight(keyvalue:String):boolean;
      var
        i,j    : Longint;
        Code : Word;
        s    : AnsiString;
      begin
        AddWeight:=false;
        i:=pos('=',keyvalue);
        if i=0 then
          exit;
        s:=Copy(KeyValue,i+1,length(KeyValue)-i);
        val(s,j,code);
        if code=0 Then
          begin
            Add(Copy(KeyValue,1,i-1),'',j);
            AddWeight:=True;
          end;
      end;


    procedure TLinkStrMap.addseries(keys:AnsiString;weight:longint);
      var
        i,j,k : longint;
      begin
       k:=length(keys);
       i:=1;
       while i<=k do
         begin
           j:=i;
           while (i<=k) and (keys[i]<>',') do
             inc(i);
           add(copy(keys,j,i-j),'',weight);
           inc(i);
         end;
      end;

    procedure TLinkStrMap.SetValue(Key:Ansistring;weight:Integer);
      var
        j : longint;
      begin
         j:=lookup(key);
         if j<>-1 then
          fmap[j].weight:=weight;
      end;


    function TLinkStrMap.find(key:Ansistring):Ansistring;
      var
        j : longint;
      begin
         find:='';
         j:=lookup(key);
         if j<>-1 then
          find:=fmap[j].value;
      end;


    function TLinkStrMap.lookup(key:Ansistring):longint;
      var
        i : longint;
      begin
         lookup:=-1;
         i:=0;
         while (i<itemcnt) and (fmap[i].key<>key) do
           inc(i);
         if i<>itemcnt then
            lookup:=i;
      end;


    procedure TLinkStrMap.SortOnWeight;
      var
        i, j : longint;
        m    : TLinkRec;
      begin
        if itemcnt <2 then exit;
        for i:=0 to itemcnt-1 do
          for j:=i+1 to itemcnt-1 do
            begin
            if fmap[i].weight>fmap[j].weight Then
              begin
                m:=fmap[i];
                fmap[i]:=fmap[j];
                fmap[j]:=m;
              end;
           end;
      end;


    function TLinkStrMap.getlinkrec(i:longint):TLinkRec;
      begin
        result:=fmap[i];
      end;


    procedure TLinkStrMap.Expand(Src:TCmdStrList;Dest:TLinkStrMap);
      // expands every thing in Src to Dest for linkorder purposes.
      var
        r  : longint;
        LibN    : TCmdStr;
      begin
        while not src.empty do
          begin
            LibN:=src.getfirst;
            r:=lookup (LibN);
            if r=-1 then
              dest.add(LibN)
            else
              dest.addseries(fmap[r].value);
          end;
      end;

    procedure TLinkStrMap.UpdateWeights(Weightmap:TLinkStrMap);
      var
        l,r : longint;
      begin
        for l := 0 to itemcnt-1 do
          begin
            r:=weightmap.lookup (fmap[l].key);
            if r<>-1 then
              fmap[l].weight:=weightmap[r].weight;
          end;
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
        DecodeTime(Time,hour,min,sec,hsec);
        gettimestr:=L0(Hour)+':'+L0(min)+':'+L0(sec);
      end;


   function getdatestr:string;
   {
     get the current date in a string YY/MM/DD
   }
      var
        Year,Month,Day: Word;
      begin
        DecodeDate(Date,year,month,day);
        getdatestr:=L0(Year)+'/'+L0(Month)+'/'+L0(Day);
      end;


   function  filetimestring( t : longint) : string;
   {
     convert dos datetime t to a string YY/MM/DD HH:MM:SS
   }
     var
       DT : TDateTime;
       hsec : word;
       Year,Month,Day: Word;
       hour,min,sec : word;
     begin
       if t=-1 then
        begin
          Result := 'Not Found';
          exit;
        end;
       DT := FileDateToDateTime(t);
       DecodeTime(DT,hour,min,sec,hsec);
       DecodeDate(DT,year,month,day);
       Result := L0(Year)+'/'+L0(Month)+'/'+L0(Day)+' '+L0(Hour)+':'+L0(min)+':'+L0(sec);
     end;


   function getrealtime : real;
     var
       h,m,s,s1000 : word;
     begin
       DecodeTime(Time,h,m,s,s1000);
       result:=h*3600.0+m*60.0+s+s1000/1000.0;
     end;

{****************************************************************************
                          Default Macro Handling
****************************************************************************}

     procedure DefaultReplacements(var s:ansistring);
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
        GetEnvPchar:=BaseUnix.fpGetEnv(envname);
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
        GetEnvPchar:=StrPNew(GetEnvironmentVariable(envname));
      {$endif}
      end;


    procedure FreeEnvPChar(p:pchar);
      begin
      {$ifndef hasunix}
       {$ifndef os2}
        freemem(p);
       {$endif}
      {$endif}
      end;

{$if defined(MORPHOS) or defined(AMIGA)}
  {$define AMIGASHELL}
{$endif}

    function Shell(const command:ansistring): longint;
      { This is already defined in the linux.ppu for linux, need for the *
        expansion under linux }
{$ifdef hasunix}
      begin
        result := Unix.fpsystem(command);
      end;
{$else hasunix}
  {$ifdef amigashell}
      begin
        result := ExecuteProcess('',command);
      end;
  {$else amigashell}
      var
        comspec : string;
      begin
        comspec:=GetEnvironmentVariable('COMSPEC');
        result := ExecuteProcess(comspec,' /C '+command);
      end;
   {$endif amigashell}
{$endif hasunix}

{$UNDEF AMIGASHELL}
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
        p := pbyte(@r);
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
        else if (length(s)=0) then
          begin
          FillChar(GUID,SizeOf(GUID),0);
          string2guid:=true;
          end
        else
          string2guid:=false;
      end;


    function guid2string(const GUID: TGUID): string;

      begin
        guid2string:=
          '{'+hexstr(GUID.D1,8)+
          '-'+hexstr(GUID.D2,4)+
          '-'+hexstr(GUID.D3,4)+
          '-'+hexstr(GUID.D4[0],2)+hexstr(GUID.D4[1],2)+
          '-'+hexstr(GUID.D4[2],2)+hexstr(GUID.D4[3],2)+
              hexstr(GUID.D4[4],2)+hexstr(GUID.D4[5],2)+
              hexstr(GUID.D4[6],2)+hexstr(GUID.D4[7],2)+
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


    function Setabitype(const s:string;var a:tabi):boolean;
      var
        t  : tabi;
        hs : string;
      begin
        result:=false;
        hs:=Upper(s);
        for t:=low(t) to high(t) do
          if abi2str[t]=hs then
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


{$if defined(arm) or defined(avr)}
    function SetControllerType(const s:string;var a:tcontrollertype):boolean;
      var
        t  : tcontrollertype;
        hs : string;
      begin
        result:=false;
        hs:=Upper(s);
        for t:=low(tcontrollertype) to high(tcontrollertype) do
          if controllertypestr[t]=hs then
            begin
              a:=t;
              result:=true;
              break;
            end;
      end;
{$endif defined(arm) or defined(avr)}


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


    function UpdateWpoStr(s: string; var a: twpoptimizerswitches): boolean;
      var
        tok   : string;
        doset,
        found : boolean;
        opt   : twpoptimizerswitch;
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
          if (tok = 'ALL') then
            begin
              for opt:=low(twpoptimizerswitch) to high(twpoptimizerswitch) do
                if doset then
                  include(a,opt)
                else
                  exclude(a,opt);
            end
          else
            begin
              for opt:=low(twpoptimizerswitch) to high(twpoptimizerswitch) do
                begin
                  if WPOptimizerSwitchStr[opt]=tok then
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
            end;
        until false;
      end;


    function UpdateDebugStr(s:string;var a:tdebugswitches):boolean;
      var
        tok   : string;
        doset,
        found : boolean;
        opt   : tdebugswitch;
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
          for opt:=low(tdebugswitch) to high(tdebugswitch) do
            begin
              if DebugSwitchStr[opt]=tok then
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


    function IncludeFeature(const s : string) : boolean;
      var
        i : tfeature;
      begin
        result:=true;
        for i:=low(tfeature) to high(tfeature) do
          if s=featurestr[i] then
            begin
              include(features,i);
              exit;
            end;
        result:=false;
      end;


    function SetMinFPConstPrec(const s: string; var a: tfloattype) : boolean;
      var
        value, error: longint;
      begin
        if (upper(s)='DEFAULT') then
          begin
            a:=s32real;
            result:=true;
            exit;
          end;
        result:=false;
        val(s,value,error);
        if (error<>0) then
          exit;
        case value of
          32: a:=s32real;
          64: a:=s64real;
          { adding support for 80 bit here is tricky, since we can't really }
          { check whether the target cpu+OS actually supports it            }
          else
            exit;
        end;
        result:=true;
      end;


    function var_align(siz: longint): shortint;
      begin
        siz := size_2_align(siz);
        var_align := used_align(siz,current_settings.alignment.varalignmin,current_settings.alignment.varalignmax);
      end;


    function const_align(siz: longint): shortint;
      begin
        siz := size_2_align(siz);
        const_align := used_align(siz,current_settings.alignment.constalignmin,current_settings.alignment.constalignmax);
      end;

{$ifdef ARM}
    function is_double_hilo_swapped: boolean;{$ifdef USEINLINE}inline;{$endif}
      begin
        result := (current_settings.fputype in [fpu_fpa,fpu_fpa10,fpu_fpa11]) and
          not(cs_fp_emulation in current_settings.moduleswitches);
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
        { inverse result if compiler was compiled with swapped hilo already }
        result := not result;
{$endif FPC_DOUBLE_HILO_SWAPPED}
      end;
{$endif ARM}


    function floating_point_range_check_error : boolean;
      begin
        result:=((([cs_check_range,cs_check_overflow]*current_settings.localswitches)<>[]) and not
                   (m_delphi in current_settings.modeswitches)
                ); // or (cs_ieee_errors in current_settings.localswitches);
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
	   localExepath : TCmdStr;
       exeName:TCmdStr;
{$ifdef need_path_search}
       hs1 : TPathStr;
{$endif need_path_search}
     begin
       localexepath:=GetEnvironmentVariable('PPC_EXEC_PATH');
       if localexepath='' then
         begin
           exeName := FixFileName(system.paramstr(0));
           localexepath := ExtractFilePath(exeName);
         end;
{$ifdef need_path_search}
       if localexepath='' then
        begin
          hs1 := ExtractFileName(exeName);
          ChangeFileExt(hs1,source_info.exeext);
{$ifdef macos}
          FindFile(hs1,GetEnvironmentVariable('Commands'),false,localExepath);
{$else macos}
          FindFile(hs1,GetEnvironmentVariable('PATH'),false,localExepath);
{$endif macos}
          localExepath:=ExtractFilePath(localExepath);
        end;
{$endif need_path_search}
       exepath:=FixPath(localExepath,false);
     end;



   procedure DoneGlobals;
     begin
       if assigned(DLLImageBase) then
         StringDispose(DLLImageBase);
       librarysearchpath.Free;
       unitsearchpath.Free;
       objectsearchpath.Free;
       includesearchpath.Free;
       frameworksearchpath.Free;
       LinkLibraryAliases.Free;
       LinkLibraryOrder.Free;
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
        paratarget:=system_none;
        paratargetasm:=as_none;
        paratargetdbg:=dbg_none;

        { Output }
        OutputFileName:='';
        OutputPrefix:=Nil;
        OutputSuffix:=Nil;

        OutputExeDir:='';
        OutputUnitDir:='';

        { Utils directory }
        utilsdirectory:='';
        utilsprefix:='';
        cshared:=false;
        rlinkpath:='';
        sysrootpath:='';

        { Search Paths }
        librarysearchpath:=TSearchPathList.Create;
        unitsearchpath:=TSearchPathList.Create;
        includesearchpath:=TSearchPathList.Create;
        objectsearchpath:=TSearchPathList.Create;
        frameworksearchpath:=TSearchPathList.Create;

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
        init_settings:=default_settings;
        if init_settings.optimizecputype=cpu_none then
          init_settings.optimizecputype:=init_settings.cputype;

        LinkLibraryAliases :=TLinkStrMap.Create;
        LinkLibraryOrder   :=TLinkStrMap.Create;

        { enable all features by default }
        features:=[low(Tfeature)..high(Tfeature)];
     end;

end.
