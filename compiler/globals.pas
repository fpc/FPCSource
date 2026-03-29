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
{$ifdef windows}
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
{$if defined(LLVM) or defined(GENERIC_CPU)}
      llvminfo,
{$endif LLVM or GENERIC_CPU}
      globtype,version,systems,
      compilerbase;

    const
       delphimodeswitches =
         [m_delphi,m_class,m_objpas,m_result,m_string_pchar,
          m_pointer_2_procedure,m_autoderef,m_tp_procvar,m_initfinal,m_default_ansistring,
          m_out,m_default_para,m_duplicate_names,m_hintdirective,
          m_property,m_default_inline,m_except,m_advanced_records,
          m_array_operators,m_prefixed_attributes,m_underscoreisseparator,
          m_function_references,m_anonymous_functions,m_multiline_strings];
       delphiunicodemodeswitches = delphimodeswitches + [m_systemcodepage,m_default_unicodestring];
       fpcmodeswitches =
         [m_fpc,m_string_pchar,m_nested_comment,m_repeat_forward,
          m_cvar_support,m_initfinal,m_hintdirective,
          m_property,m_default_inline];
       objfpcmodeswitches =
         [m_objfpc,m_fpc,m_class,m_objpas,m_result,m_string_pchar,m_nested_comment,
          m_repeat_forward,m_cvar_support,m_initfinal,m_out,m_default_para,m_hintdirective,
          m_property,m_default_inline,m_except,m_multiline_strings];
       tpmodeswitches =
         [m_tp7,m_tp_procvar,m_duplicate_names];
{$ifdef gpc_mode}
       gpcmodeswitches =
         [m_gpc,m_tp_procvar];
{$endif}
       macmodeswitches =
         [m_mac,m_cvar_support,m_mac_procvar,m_nested_procvars,m_non_local_goto,m_isolike_unary_minus,m_default_inline];
       isomodeswitches =
         [m_iso,m_tp_procvar,m_duplicate_names,m_nested_procvars,m_non_local_goto,m_isolike_unary_minus,m_isolike_io,
          m_isolike_program_para,
          m_isolike_mod];
       extpasmodeswitches =
         [m_extpas,m_tp_procvar,m_duplicate_names,m_nested_procvars,m_non_local_goto,m_isolike_unary_minus,m_isolike_io,
          m_isolike_program_para,
          m_isolike_mod];

       { maximum nesting of routines }
       maxnesting = 32;

       { Filenames and extensions }
       sourceext  = '.pp';
       pasext     = '.pas';
       pext       = '.p';

       treelogfilename = 'tree.log';

{$if defined(CPUARM) and defined(FPUFPA)}
       MathQNaN : tcompdoublerec = (bytes : (0,0,252,255,0,0,0,0));
       MathInf : tcompdoublerec = (bytes : (0,0,240,127,0,0,0,0));
       MathNegInf : tcompdoublerec = (bytes : (0,0,240,255,0,0,0,0));
       MathPi : tcompdoublerec =  (bytes : (251,33,9,64,24,45,68,84));
{$else}
{$ifdef FPC_LITTLE_ENDIAN}
       MathQNaN : tcompdoublerec = (bytes : (0,0,0,0,0,0,252,255));
       MathInf : tcompdoublerec = (bytes : (0,0,0,0,0,0,240,127));
       MathNegInf : tcompdoublerec = (bytes : (0,0,0,0,0,0,240,255));
       MathPi : tcompdoublerec = (bytes : (24,45,68,84,251,33,9,64));
       MathPiExtended : tcompextendedrec = (bytes : (53,194,104,33,162,218,15,201,0,64));
{$else FPC_LITTLE_ENDIAN}
       MathQNaN : tcompdoublerec = (bytes : (255,252,0,0,0,0,0,0));
       MathInf : tcompdoublerec = (bytes : (127,240,0,0,0,0,0,0));
       MathNegInf : tcompdoublerec = (bytes : (255,240,0,0,0,0,0,0));
       MathPi : tcompdoublerec =  (bytes : (64,9,33,251,84,68,45,24));
       MathPiExtended : tcompextendedrec = (bytes : (64,0,201,15,218,162,33,104,194,53));
{$endif FPC_LITTLE_ENDIAN}
{$endif}

       CP_UTF8 = 65001;
       CP_UTF16LE = 1200;
       CP_UTF16BE = 1201;
       CP_NONE  = 65535;

       { by default no local variable trashing }
       localvartrashing: longint = -1;

       nroftrashvalues = 4;
       trashintvalues: array[0..nroftrashvalues-1] of int64 = ($5555555555555555,$AAAAAAAAAAAAAAAA,$EFEFEFEFEFEFEFEF,0);

{ Verbosity constants }
Const
  { Levels }
  V_None         = $0;
  V_Fatal        = $1;
  V_Error        = $2;
  V_Normal       = $4; { doesn't show a text like Error: }
  V_Warning      = $8;
  V_Note         = $10;
  V_Hint         = $20;
  V_LineInfoMask = $fff;
  { From here by default no line info }
  V_Info         = $1000;
  V_Status       = $2000;
  V_Used         = $4000;
  V_Tried        = $8000;
  V_Conditional  = $10000;
  V_Debug        = $20000;
  V_Executable   = $40000;
  V_TimeStamps   = $80000;
  V_LevelMask    = $ffffff;
  V_All          = V_LevelMask;
  V_Default      = V_Fatal + V_Error + V_Normal;
  { Flags }
  V_LineInfo     = $10000000;
  V_Parallel     = $20000000;


    type
       { this is written to ppus during token recording for generics,
         it used to required to be packed,
         but this requirement is now obsolete,
         as the fields are written one by one. PM 2012-06-13 }
       tsettings = record
         alignment       : talignmentinfo;
         globalswitches  : tglobalswitches;
         targetswitches  : ttargetswitches;
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

         packrecords     : shortint;
         maxfpuregisters : shortint;

         verbosity       : longint;

         cputype,
         optimizecputype,
         asmcputype      : tcputype;
         fputype         : tfputype;
         asmmode         : tasmmode;
         interfacetype   : tinterfacetypes;
         defproccall     : tproccalloption;
         sourcecodepage  : tstringencoding;

         minfpconstprec  : tfloattype;

         disabledircache : boolean;

         tlsmodel : ttlsmodel;

        { CPU targets with microcontroller support can add a controller specific unit }
         controllertype   : tcontrollertype;

         { WARNING: pmessage cannot be written as such in record token
              pmessage is the top of a stack of message/verbosity changes
              RestoreLocalVerbosity applies the current stack. }
         pmessage : pmessagestaterecord;

         lineendingtype : tlineendingtype;

         whitespacetrimcount : word;

         whitespacetrimauto : boolean;

{$if defined(generic_cpu)}
         case byte of
{$endif}
{$if defined(i8086) or defined(generic_cpu)}
   {$ifdef generic_cpu} 1:({$endif}
           x86memorymodel  : tx86memorymodel;
   {$ifdef generic_cpu}   );{$endif}
{$endif defined(i8086) or defined(generic_cpu)}

{$if defined(ARM) or defined(generic_cpu)}
   {$ifdef generic_cpu} 2:({$endif}
         instructionset : tinstructionset;
   {$ifdef generic_cpu}   );{$endif}
{$endif defined(ARM) or defined(generic_cpu)}

{$if defined(LLVM) or defined(GENERIC_CPU)}
   {$ifdef generic_cpu} 3:({$endif}
         llvmversion: tllvmversion;
   {$ifdef generic_cpu}   );{$endif}
{$endif defined(LLVM) or defined(GENERIC_CPU)}
       end;

       { TReadOnlySettings }

       TReadOnlySettings = class
       protected
         FAlignment: TReadOnlyAlignmentInfo;
         Fglobalswitches: tglobalswitches;
         Ftargetswitches: ttargetswitches;
         Fmoduleswitches: tmoduleswitches;
         Flocalswitches: tlocalswitches;
         Fmodeswitches: tmodeswitches;
         Foptimizerswitches: toptimizerswitches;
         Fgenwpoptimizerswitches: twpoptimizerswitches;
         Fdowpoptimizerswitches: twpoptimizerswitches;
         Fdebugswitches: tdebugswitches;
         Fsetalloc: shortint;
         Fpackenum: shortint;
         Fpackrecords: shortint;
         Fmaxfpuregisters: shortint;
         Fverbosity: longint;
         Fcputype: tcputype;
         Foptimizecputype: tcputype;
         Fasmcputype: tcputype;
         Ffputype: tfputype;
         Fasmmode: tasmmode;
         Finterfacetype: tinterfacetypes;
         Fdefproccall: tproccalloption;
         Fsourcecodepage: tstringencoding;
         Fminfpconstprec: tfloattype;
         Fdisabledircache: boolean;
         Ftlsmodel: ttlsmodel;
         Fcontrollertype: tcontrollertype;
         Fpmessage: pmessagestaterecord;
         Flineendingtype: tlineendingtype;
         Fwhitespacetrimcount: word;
         Fwhitespacetrimauto: boolean;

{$if defined(i8086) or defined(generic_cpu)}
         Fx86memorymodel: tx86memorymodel;
{$endif defined(i8086) or defined(generic_cpu)}

{$if defined(ARM) or defined(generic_cpu)}
         Finstructionset: tinstructionset;
{$endif defined(ARM) or defined(generic_cpu)}

{$if defined(LLVM) or defined(GENERIC_CPU)}
         Fllvmversion: tllvmversion;
{$endif defined(LLVM) or defined(GENERIC_CPU)}
       public
         function Equals(Obj: TObject): boolean; override;

         function ToRecord: tsettings;

         property alignment: TReadOnlyAlignmentInfo read FAlignment;
         property globalswitches: tglobalswitches read Fglobalswitches;
         property targetswitches: ttargetswitches read Ftargetswitches;
         property moduleswitches: tmoduleswitches read Fmoduleswitches;
         property localswitches: tlocalswitches read Flocalswitches;
         property modeswitches: tmodeswitches read Fmodeswitches;
         property optimizerswitches: toptimizerswitches read Foptimizerswitches;
         property genwpoptimizerswitches: twpoptimizerswitches read Fgenwpoptimizerswitches;
         property dowpoptimizerswitches: twpoptimizerswitches read Fdowpoptimizerswitches;
         property debugswitches: tdebugswitches read Fdebugswitches;
         property setalloc: shortint read Fsetalloc;
         property packenum: shortint read Fpackenum;
         property packrecords: shortint read Fpackrecords;
         property maxfpuregisters: shortint read Fmaxfpuregisters;
         property verbosity: longint read Fverbosity;
         property cputype: tcputype read Fcputype;
         property optimizecputype: tcputype read Foptimizecputype;
         property asmcputype: tcputype read Fasmcputype;
         property fputype: tfputype read Ffputype;
         property asmmode: tasmmode read Fasmmode;
         property interfacetype: tinterfacetypes read Finterfacetype;
         property defproccall: tproccalloption read Fdefproccall;
         property sourcecodepage: tstringencoding read Fsourcecodepage;
         property minfpconstprec: tfloattype read Fminfpconstprec;
         property disabledircache: boolean read Fdisabledircache;
         property tlsmodel: ttlsmodel read Ftlsmodel;
         property controllertype: tcontrollertype read Fcontrollertype;
         property pmessage: pmessagestaterecord read Fpmessage;
         property lineendingtype: tlineendingtype read Flineendingtype;
         property whitespacetrimcount: word read Fwhitespacetrimcount;
         property whitespacetrimauto: boolean read Fwhitespacetrimauto;

{$if defined(i8086) or defined(generic_cpu)}
         property x86memorymodel: tx86memorymodel read Fx86memorymodel;
{$endif defined(i8086) or defined(generic_cpu)}

{$if defined(ARM) or defined(generic_cpu)}
         property instructionset: tinstructionset read Finstructionset;
{$endif defined(ARM) or defined(generic_cpu)}

{$if defined(LLVM) or defined(GENERIC_CPU)}
         property llvmversion: tllvmversion read Fllvmversion;
{$endif defined(LLVM) or defined(GENERIC_CPU)}
       end;

       { TMutableSettings }

       TMutableSettings = class(TReadOnlySettings)
       private
         function GetMutableAlignment: TMutableAlignmentInfo;
       public
         constructor Create;
         constructor CreateFromRecord(const rec: tsettings);

         procedure Assign(Source: TReadOnlySettings);

         property alignment: TMutableAlignmentInfo read GetMutableAlignment;
         property globalswitches: tglobalswitches read Fglobalswitches write Fglobalswitches;
         property targetswitches: ttargetswitches read Ftargetswitches write Ftargetswitches;
         property moduleswitches: tmoduleswitches read Fmoduleswitches write Fmoduleswitches;
         property localswitches: tlocalswitches read Flocalswitches write Flocalswitches;
         property modeswitches: tmodeswitches read Fmodeswitches write Fmodeswitches;
         property optimizerswitches: toptimizerswitches read Foptimizerswitches write Foptimizerswitches;
         property genwpoptimizerswitches: twpoptimizerswitches read Fgenwpoptimizerswitches write Fgenwpoptimizerswitches;
         property dowpoptimizerswitches: twpoptimizerswitches read Fdowpoptimizerswitches write Fdowpoptimizerswitches;
         property debugswitches: tdebugswitches read Fdebugswitches write Fdebugswitches;
         property setalloc: shortint read Fsetalloc write Fsetalloc;
         property packenum: shortint read Fpackenum write Fpackenum;
         property packrecords: shortint read Fpackrecords write Fpackrecords;
         property maxfpuregisters: shortint read Fmaxfpuregisters write Fmaxfpuregisters;
         property verbosity: longint read Fverbosity write Fverbosity;
         property cputype: tcputype read Fcputype write Fcputype;
         property optimizecputype: tcputype read Foptimizecputype write Foptimizecputype;
         property asmcputype: tcputype read Fasmcputype write Fasmcputype;
         property fputype: tfputype read Ffputype write Ffputype;
         property asmmode: tasmmode read Fasmmode write Fasmmode;
         property interfacetype: tinterfacetypes read Finterfacetype write Finterfacetype;
         property defproccall: tproccalloption read Fdefproccall write Fdefproccall;
         property sourcecodepage: tstringencoding read Fsourcecodepage write Fsourcecodepage;
         property minfpconstprec: tfloattype read Fminfpconstprec write Fminfpconstprec;
         property disabledircache: boolean read Fdisabledircache write Fdisabledircache;
         property tlsmodel: ttlsmodel read Ftlsmodel write Ftlsmodel;
         property controllertype: tcontrollertype read Fcontrollertype write Fcontrollertype;
         property pmessage: pmessagestaterecord read Fpmessage write Fpmessage;
         property lineendingtype: tlineendingtype read Flineendingtype write Flineendingtype;
         property whitespacetrimcount: word read Fwhitespacetrimcount write Fwhitespacetrimcount;
         property whitespacetrimauto: boolean read Fwhitespacetrimauto write Fwhitespacetrimauto;

{$if defined(i8086) or defined(generic_cpu)}
         property x86memorymodel: tx86memorymodel read Fx86memorymodel write Fx86memorymodel;
{$endif defined(i8086) or defined(generic_cpu)}

{$if defined(ARM) or defined(generic_cpu)}
         property instructionset: tinstructionset read Finstructionset write Finstructionset;
{$endif defined(ARM) or defined(generic_cpu)}

{$if defined(LLVM) or defined(GENERIC_CPU)}
         property llvmversion: tllvmversion read Fllvmversion write Fllvmversion;
{$endif defined(LLVM) or defined(GENERIC_CPU)}
       end;


    const
      LinkMapWeightDefault = 1000;
{$ifdef CPU_BC_HAS_SIZE_LIMIT}
    {$if defined(POWERPC) or defined(POWERPC64)}
      { instructions are 4-byte long and relative jump distance
        a signed 16-bit signed integer, code as
        reduced by a small amount to avoid troubles
        as distance can be modified by optimizations. }
      BC_max_distance = ($8000 div 4) - $100;
    {$endif}
{$endif CPU_BC_HAS_SIZE_LIMIT}

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
        function  Lookup(const key:Ansistring):longint;
        function getlinkrec(i:longint):TLinkRec;
      public
        procedure Add(const key:ansistring;const value:AnsiString='';weight:longint=LinkMapWeightDefault);
        procedure addseries(const keys:AnsiString;weight:longint=LinkMapWeightDefault);
        function  AddDep(const keyvalue:String):boolean;
        function  AddWeight(const keyvalue:String):boolean;
        procedure SetValue(const key:AnsiString;Weight:Integer);
        procedure SortonWeight;
        function Find(const key:AnsiString):AnsiString;
        procedure Expand(src:TCmdStrList;dest: TLinkStrMap);
        procedure UpdateWeights(Weightmap:TLinkStrMap);
        constructor Create;
        property count : longint read itemcnt;
        property items[I:longint]:TLinkRec read getlinkrec; default;
      end;

      tpendingstateflag = (
        psf_alignment_changed,
        psf_verbosity_full_switched,
        psf_local_switches_changed,
        psf_packenum_changed,
        psf_packrecords_changed,
        psf_setalloc_changed,
        psf_asmmode_changed,
        psf_optimizerswitches_changed
      );
      tpendingstateflags = set of tpendingstateflag;

      tpendingstate = record
        nextverbositystr : shortstring;
        nextlocalswitches : tlocalswitches;
        nextverbosityfullswitch: longint;
        nextcallingstr : shortstring;
        nextmessagerecord : pmessagestaterecord;
        nextalignment : talignmentinfo;
        nextpackenum : shortint;
        nextpackrecords : shortint;
        nextsetalloc : shortint;
        nextasmmode : tasmmode;
        nextoptimizerswitches : toptimizerswitches;
        flags : tpendingstateflags;
      end;


    var
       cgbackend: tcgbackend;

    const
       { default name of the C-style "main" procedure of the library/program }
       { (this will be prefixed with the compiler.target.info.cprefix)                }
       defaultmainaliasname = 'main';

       custom_attribute_suffix = 'ATTRIBUTE';

    const
      default_settings : TSettings = (
        alignment : (
          procalign : 0;
          loopalign : 0;
          jumpalign : 0;
          jumpalignskipmax    : 0;
          coalescealign   : 0;
          coalescealignskipmax: 0;
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
        globalswitches : [cs_check_unit_name,cs_link_static];
        targetswitches : [];
        moduleswitches : [cs_extsyntax,cs_implicit_exceptions];
        localswitches : [cs_check_io,cs_typed_const_writable,cs_pointermath,cs_imported_data{$ifdef i8086},cs_force_far_calls{$endif}];
        modeswitches : fpcmodeswitches;
        optimizerswitches : [];
        genwpoptimizerswitches : [];
        dowpoptimizerswitches : [];
{$ifdef i8086}
        debugswitches : [ds_dwarf_sets,ds_dwarf_omf_linnum];
{$else i8086}
        debugswitches : [ds_dwarf_sets];
{$endif i8086}

        setalloc : 0;
        packenum : 4;

{$ifdef i8086}
        packrecords     : 1;
{$else i8086}
        packrecords     : 0;
{$endif i8086}
        maxfpuregisters : 0;

        verbosity : V_Default;

{ Note: GENERIC_CPU is used together with generic subdirectory to
  be able to compile some of the units without any real CPU.
  This is used to generate a CPU independant PPUDUMP utility. PM }
{$ifdef GENERIC_CPU}
        cputype : cpu_none;
        optimizecputype : cpu_none;
        asmcputype : cpu_none;
        fputype : fpu_none;
{$else not GENERIC_CPU}
  {$ifdef i386}
        cputype : cpu_Pentium2;
        optimizecputype : cpu_Pentium3;
        asmcputype : cpu_none;
        fputype : fpu_x87;
  {$endif i386}
  {$ifdef m68k}
        cputype : cpu_MC68020;
        optimizecputype : cpu_MC68020;
        asmcputype : cpu_none;
        fputype : fpu_soft;
  {$endif m68k}
  {$ifdef powerpc}
        cputype : cpu_PPC604;
        optimizecputype : cpu_ppc7400;
        asmcputype : cpu_none;
        fputype : fpu_standard;
  {$endif powerpc}
  {$ifdef POWERPC64}
        cputype : cpu_PPC970;
        optimizecputype : cpu_ppc970;
        asmcputype : cpu_none;
        fputype : fpu_standard;
  {$endif POWERPC64}
  {$ifdef sparc}
        cputype : cpu_SPARC_V9;
        optimizecputype : cpu_SPARC_V9;
        asmcputype : cpu_none;
        fputype : fpu_hard;
  {$endif sparc}
  {$ifdef sparc64}
        cputype : cpu_SPARC_V9;
        optimizecputype : cpu_SPARC_V9;
        asmcputype : cpu_none;
        fputype : fpu_hard;
  {$endif sparc64}
  {$ifdef arm}
        cputype : cpu_armv4;
        optimizecputype : cpu_armv4;
        asmcputype : cpu_none;
        fputype : fpu_fpa;
  {$endif arm}
  {$ifdef x86_64}
        cputype : cpu_athlon64;
        optimizecputype : cpu_athlon64;
        asmcputype : cpu_none;
        fputype : fpu_sse64;
  {$endif x86_64}
  {$ifdef avr}
        cputype : cpuinfo.cpu_avr5;
        optimizecputype : cpuinfo.cpu_avr5;
        asmcputype : cpu_none;
        fputype : fpu_none;
  {$endif avr}
  {$ifdef mips}
  {$ifdef mips64}
        cputype : cpu_mips3;
        optimizecputype : cpu_mips3;
        asmcputype : cpu_none;
        fputype : fpu_mips3;
  {$else mips64}
        cputype : cpu_mips2;
        optimizecputype : cpu_mips2;
        asmcputype : cpu_none;
        fputype : fpu_mips2;
  {$endif mips64}
  {$endif mips}
  {$ifdef jvm}
        cputype : cpu_none;
        optimizecputype : cpu_none;
        asmcputype : cpu_none;
        fputype : fpu_standard;
  {$endif jvm}
  {$ifdef aarch64}
        cputype : cpu_armv8;
        optimizecputype : cpu_armv8;
        asmcputype : cpu_none;
        fputype : fpu_vfp;
  {$endif aarch64}
  {$ifdef i8086}
        cputype : cpu_8086;
        optimizecputype : cpu_8086;
        { Use cpu_none by default,
        because using cpu_8086 by default means
        that we reject any instruction above bare 8086 instruction set
        for all assembler code PM }
        asmcputype : cpu_none;
        fputype : fpu_x87;
  {$endif i8086}
  {$ifdef riscv32}
        cputype : cpu_rv32ima;
        optimizecputype : cpu_rv32ima;
        asmcputype : cpu_none;
        fputype : fpu_soft;
  {$endif riscv32}
  {$ifdef riscv64}
        cputype : cpu_rv64imafdc;
        optimizecputype : cpu_rv64imafdc;
        asmcputype : cpu_none;
        fputype : fpu_fd;
  {$endif riscv64}
  {$ifdef xtensa}
        cputype : cpu_none;
        optimizecputype : cpu_none;
        asmcputype : cpu_none;
        fputype : fpu_none;
  {$endif xtensa}
  {$ifdef z80}
        cputype : cpu_zilog_z80;
        optimizecputype : cpu_zilog_z80;
        { Use cpu_none by default,
        because using cpu_8086 by default means
        that we reject any instruction above bare 8086 instruction set
        for all assembler code PM }
        asmcputype : cpu_none;
        fputype : fpu_soft;
  {$endif z80}
  {$ifdef wasm}
        cputype : cpu_none;
        optimizecputype : cpu_none;
        asmcputype : cpu_none;
        fputype : fpu_standard;
  {$endif wasm}
  {$ifdef loongarch64}
        cputype : cpu_3a;
        optimizecputype : cpu_3a;
        asmcputype : cpu_none;
        fputype : fpu_fd;
  {$endif loongarch64}
{$endif not GENERIC_CPU}
        asmmode : asmmode_standard;
{$ifndef jvm}
        interfacetype : it_interfacecom;
{$else jvm}
        interfacetype : it_interfacejava;
{$endif jvm}
        defproccall : pocall_default;
        sourcecodepage : 28591;
        minfpconstprec : s32real;

        disabledircache : false;

        tlsmodel : tlsm_none;
        controllertype : ct_none;
        pmessage : nil;
        lineendingtype : le_platform;
        whitespacetrimcount : 0;
        whitespacetrimauto : false;
{$if defined(i8086) or defined(GENERIC_CPU)}
        x86memorymodel : mm_small;
{$endif defined(i8086) or defined(GENERIC_CPU)}
{$if defined(ARM)}
        instructionset : is_arm;
{$endif defined(ARM)}
{$if defined(LLVM) and not defined(GENERIC_CPU)}
        llvmversion    : llvmver_7_0;
{$endif defined(LLVM) and not defined(GENERIC_CPU)}
      );

    type

      { TCompilerTime }

      TCompilerTime = class
        starttime  : real;
        startsystime : TSystemTime;
        function getdatestr:string;
        function gettimestr:string;
        function filetimestring( t : longint) : string;
        function getrealtime(const st: TSystemTime) : real;
        function getrealtime : real;
      end;

      TCompilerGlobals = class;

      TInitDoneProc = procedure(ACompilerGlobals: TCompilerGlobals);

      { TReadOnlyCompilerGlobals }

      TReadOnlyCompilerGlobals = class
      protected
        Finputfilepath: string;
        Finputfilename: string;
        Foutputfilename: string;
        Foutputprefix: pshortstring;
        Foutputsuffix: pshortstring;
        Foutputexedir: TPathStr;
        Foutputunitdir: TPathStr;
        Fwpofeedbackinput: TPathStr;
        Fwpofeedbackoutput: TPathStr;
{$if defined(XTENSA) or defined(RISCV32) or defined(ARM)}
        Fidfpath: TPathStr;
        Fidf_version: longint;
{$endif defined(XTENSA) or defined(RISCV32) or defined(ARM)}
        Fasmextraopt: string;
        Fparalinkoptions: TCmdStr;
        Fparadynamiclinker: string;
{$ifdef PREPROCWRITE}
        Fparapreprocess: boolean;
{$endif PREPROCWRITE}
        Futilsdirectory: TPathStr;
        Futilsprefix: TCmdStr;
        Fllvmutilssuffix: TCmdStr;
        Fcshared: boolean;
        FDontlinkstdlibpath: Boolean;
        Frlinkpath: TCmdStr;
        Fsysrootpath: TCmdStr;
        Fdo_build: boolean;
        Fdo_release: boolean;
        Fdo_make: boolean;
        Ftimestr: string;
        Fdatestr: string;
        Fexepath: TPathStr;
        Funicodepath: TPathStr;
        Flibrarysearchpath: TSearchPathList;
        Funitsearchpath: TSearchPathList;
        Fobjectsearchpath: TSearchPathList;
        Fincludesearchpath: TSearchPathList;
        Fframeworksearchpath: TSearchPathList;
        Fpackagesearchpath: TSearchPathList;
        Fnamespacelist: TCmdStrList;
        Fpremodule_namespacelist: TCmdStrList;
        Fcurrent_namespacelist: TCmdStrList;
        Fpackagelist: TFPHashList;
        Fautoloadunits: string;
        Fusewindowapi: boolean;
        Fdescription: string;
        FSetPEFlagsSetExplicity: boolean;
        FSetPEOptFlagsSetExplicity: boolean;
        FSetPEOSVersionSetExplicitely: boolean;
        FSetPESubSysVersionSetExplicitely: boolean;
        FSetPEUserVersionSetExplicitely: boolean;
        FImageBaseSetExplicity: boolean;
        FMinStackSizeSetExplicity: boolean;
        FMaxStackSizeSetExplicity: boolean;
        FDescriptionSetExplicity: boolean;
        Fdllversion: string;
        Fdllmajor: word;
        Fdllminor: word;
        Fdllrevision: word;
        Fpeosversionminor: word;
        Fpeosversionmajor: word;
        Fpesubsysversionminor: word;
        Fpesubsysversionmajor: word;
        Fpeuserversionminor: word;
        Fpeuserversionmajor: word;
        Fpeoptflags: longint;
        Fpeflags: longint;
        Fminstacksize: puint;
        Fmaxstacksize: puint;
        Fimagebase: puint;
        FUseDeffileForExports: boolean;
      public
        { specified inputfile }
        property inputfilepath: string read Finputfilepath;
        property inputfilename: string read Finputfilename;
        { specified outputfile with -o parameter }
        property outputfilename: string read Foutputfilename;
        property outputprefix: pshortstring read Foutputprefix;
        property outputsuffix: pshortstring read Foutputsuffix;

        { specified with -FE or -FU }
        property outputexedir: TPathStr read Foutputexedir;
        property outputunitdir: TPathStr read Foutputunitdir;
        { specified with -FW and -Fw }
        property wpofeedbackinput: TPathStr read Fwpofeedbackinput;
        property wpofeedbackoutput: TPathStr read Fwpofeedbackoutput;
{$if defined(XTENSA) or defined(RISCV32) or defined(ARM)}
        { specified with -Ff }
        property idfpath: TPathStr read Fidfpath;
        { specified with }
        property idf_version: longint read Fidf_version;
{$endif defined(XTENSA) or defined(RISCV32) or defined(ARM)}
        { external assembler extra option }
        property asmextraopt: string read Fasmextraopt;

        { things specified with parameters }
        property paralinkoptions: TCmdStr read Fparalinkoptions;
        property paradynamiclinker: string read Fparadynamiclinker;
{$ifdef PREPROCWRITE}
        property parapreprocess: boolean read Fparapreprocess;
{$endif PREPROCWRITE}

        {  typical cross compiling params}

        { directory where the utils can be found (options -FD) }
        property utilsdirectory: TPathStr read Futilsdirectory;
        { targetname specific prefix used by these utils (options -XP<path>) }
        property utilsprefix: TCmdStr read Futilsprefix;

        { Suffix for LLVM utilities, e.g. '-7' for clang-7 }
        property llvmutilssuffix: TCmdStr read Fllvmutilssuffix;

        property cshared: boolean read Fcshared;        { pass --shared to ld to link C libs shared}
        property Dontlinkstdlibpath: Boolean read FDontlinkstdlibpath;     { Don't add std paths to linkpath}
        property rlinkpath: TCmdStr read Frlinkpath;        { rpath-link linkdir override}
        property sysrootpath: TCmdStr read Fsysrootpath;        { target system root to search dyn linker }

        { some flags for global compiler switches }
        property do_build: boolean read Fdo_build;
        property do_release: boolean read Fdo_release;
        property do_make: boolean read Fdo_make;

        property timestr: string read Ftimestr;
        property datestr: string read Fdatestr;
        { Path to ppc }
        property exepath: TPathStr read Fexepath;
        { Path to unicode charmap/collation binaries }
        property unicodepath: TPathStr read Funicodepath;
        { path for searching units, different paths can be separated by ; }
        property librarysearchpath: TSearchPathList read Flibrarysearchpath;
        property unitsearchpath: TSearchPathList read Funitsearchpath;
        property objectsearchpath: TSearchPathList read Fobjectsearchpath;
        property includesearchpath: TSearchPathList read Fincludesearchpath;
        property frameworksearchpath: TSearchPathList read Fframeworksearchpath;
        property packagesearchpath: TSearchPathList read Fpackagesearchpath;

        { list of default namespaces }
        property namespacelist : TCmdStrList read Fnamespacelist;
        // During scanning/parsing, a module may not yet be available.
        // Scanner checks first current_namespacelist, then local_namespacelist
        property premodule_namespacelist: TCmdStrList read Fpremodule_namespacelist;  // always set: used as long as current_namespacelist is not correctly set.
        property current_namespacelist: TCmdStrList read Fcurrent_namespacelist;  // Set when parsing module to the current module's namespace.
        { contains tpackageentry entries }
        property packagelist: TFPHashList read Fpackagelist;
        property autoloadunits: string read Fautoloadunits;

        { linking }
        property usewindowapi: boolean read Fusewindowapi;
        property description: string read Fdescription;
        property SetPEFlagsSetExplicity: boolean read FSetPEFlagsSetExplicity;
        property SetPEOptFlagsSetExplicity: boolean read FSetPEOptFlagsSetExplicity;
        property SetPEOSVersionSetExplicitely: boolean read FSetPEOSVersionSetExplicitely;
        property SetPESubSysVersionSetExplicitely: boolean read FSetPESubSysVersionSetExplicitely;
        property SetPEUserVersionSetExplicitely: boolean read FSetPEUserVersionSetExplicitely;
        property ImageBaseSetExplicity: boolean read FImageBaseSetExplicity;
        property MinStackSizeSetExplicity: boolean read FMinStackSizeSetExplicity;
        property MaxStackSizeSetExplicity: boolean read FMaxStackSizeSetExplicity;
        property DescriptionSetExplicity: boolean read FDescriptionSetExplicity;
        property dllversion: string read Fdllversion;
        property dllmajor: word read Fdllmajor;
        property dllminor: word read Fdllminor;
        property dllrevision: word read Fdllrevision;  { revision only for netware }
        { win pe  }
        property peosversionminor: word read Fpeosversionminor;
        property peosversionmajor: word read Fpeosversionmajor;
        property pesubsysversionminor: word read Fpesubsysversionminor;
        property pesubsysversionmajor: word read Fpesubsysversionmajor;
        property peuserversionminor: word read Fpeuserversionminor;
        property peuserversionmajor: word read Fpeuserversionmajor;
        property peoptflags: longint read Fpeoptflags;
        property peflags: longint read Fpeflags;
        property minstacksize: puint read Fminstacksize;
        property maxstacksize: puint read Fmaxstacksize;
        property imagebase: puint read Fimagebase;
        property UseDeffileForExports: boolean read FUseDeffileForExports;
      end;

      { TCompilerGlobals }

      TCompilerGlobals = class(TReadOnlyCompilerGlobals)
      private
        procedure get_exepath;
        procedure callinitprocs;
        procedure calldoneprocs;
        procedure InitGlobals(ATarget: TCompilerTarget);
        procedure DoneGlobals;
      public
        UseDeffileForExportsSetExplicitly : boolean;
        GenerateImportSection,
        GenerateImportSectionSetExplicitly,
        RelocSection : boolean;

        RelocSectionSetExplicitly : boolean;

        // TODO: current_tokenpros and current_filepos should probably be moved to the scanner or the parser
        current_tokenpos,                  { position of the last token }
        current_filepos : tfileposinfo;    { current position }

        nwscreenname : string;
        nwthreadname : string;
        nwcopyright  : string;

        // TODO: exception_raised should probably be moved somewhere else (possible candidates: TVerbose, TParser, TCompiler)
        exception_raised : boolean;           { true if there is an exception reported }

        // TODO: block_type should probably be moved to the scanner or parser
        block_type : tblock_type;         { type of currently parsed block }

        // TODO: exceptblockcounter should probably be moved somewhere else
        exceptblockcounter    : integer;  { each except block gets a unique number check gotos      }
        // TODO: current_exceptblock should probably be moved somewhere else (parser?)
        current_exceptblock        : integer;  { the exceptblock number of the current block (0 if none) }
        LinkLibraryAliases : TLinkStrMap;
        LinkLibraryOrder   : TLinkStrMap;


        init_settings,
        current_settings   : TMutableSettings;

        pendingstate       : tpendingstate;
      { Memory sizes }
        heapsize,
        maxheapsize : int64;
        stacksize   : longint;

{$Ifdef EXTDEBUG}
      { parameter switches }
        debugstop : boolean;
{$EndIf EXTDEBUG}
        { Application type (platform specific)
          see globtype.pas for description }
        apptype : tapptype;

        features : tfeatures;

        { prefix added to automatically generated setters/getters. If empty,
          no getters/setters will be automatically generated except if required
          for visibility reasons (but in that case the names will be mangled so
          they are unique) }
        prop_auto_getter_prefix,
        prop_auto_setter_prefix : string;

        // TODO: Inside_asm_statement should probably be moved inside the scanner or parser
        Inside_asm_statement : boolean;

        global_unit_count : word;

        { for error info in pp.pas }
        parser_current_file : string;

{$if defined(m68k) or defined(arm)}
        { PalmOS resources }
        palmos_applicationname : string;
        palmos_applicationid : string[4];
{$endif defined(m68k) or defined(arm)}
{$if defined(m68k)}
        { Atari Specific }
        ataritos_exe_flags: dword;
        ataritos_exe_format: string;

        { Sinclair QL specific }
        sinclairql_metadata_format: string[4];
        sinclairql_vlink_experimental: boolean;
{$endif defined(m68k)}

        mainaliasname : string;

        LTOExt: TCmdStr;

        { resources (comprsrc unit) }
        ResCompiler : String;
        RCCompiler  : String;
        RCForceFPCRes : Boolean;

        constructor Create(ATarget: TCompilerTarget);
        destructor Destroy; override;

        function HandleFeature(const s : string) : boolean;

        property inputfilepath: string read Finputfilepath write Finputfilepath;
        property inputfilename: string read Finputfilename write Finputfilename;
        property outputfilename: string read Foutputfilename write Foutputfilename;
        property outputprefix: pshortstring read Foutputprefix write Foutputprefix;
        property outputsuffix: pshortstring read Foutputsuffix write Foutputsuffix;
        property outputexedir: TPathStr read Foutputexedir write Foutputexedir;
        property outputunitdir: TPathStr read Foutputunitdir write Foutputunitdir;
        property wpofeedbackinput: TPathStr read Fwpofeedbackinput write Fwpofeedbackinput;
        property wpofeedbackoutput: TPathStr read Fwpofeedbackoutput write Fwpofeedbackoutput;
{$if defined(XTENSA) or defined(RISCV32) or defined(ARM)}
        property idfpath: TPathStr read Fidfpath write Fidfpath;
        property idf_version: longint read Fidf_version write Fidf_version;
{$endif defined(XTENSA) or defined(RISCV32) or defined(ARM)}
        property asmextraopt: string read Fasmextraopt write Fasmextraopt;
        property paralinkoptions: TCmdStr read Fparalinkoptions write Fparalinkoptions;
        property paradynamiclinker: string read Fparadynamiclinker write Fparadynamiclinker;
{$ifdef PREPROCWRITE}
        property parapreprocess: boolean read Fparapreprocess write Fparapreprocess;
{$endif PREPROCWRITE}
        property utilsdirectory: TPathStr read Futilsdirectory write Futilsdirectory;
        property utilsprefix: TCmdStr read Futilsprefix write Futilsprefix;
        property llvmutilssuffix: TCmdStr read Fllvmutilssuffix write Fllvmutilssuffix;
        property cshared: boolean read Fcshared write Fcshared;
        property Dontlinkstdlibpath: Boolean read FDontlinkstdlibpath write FDontlinkstdlibpath;
        property rlinkpath: TCmdStr read Frlinkpath write Frlinkpath;
        property sysrootpath: TCmdStr read Fsysrootpath write Fsysrootpath;
        property do_build: boolean read Fdo_build write Fdo_build;
        property do_release: boolean read Fdo_release write Fdo_release;
        property do_make: boolean read Fdo_make write Fdo_make;
        property timestr: string read Ftimestr write Ftimestr;
        property datestr: string read Fdatestr write Fdatestr;
        property exepath: TPathStr read Fexepath write Fexepath;
        property unicodepath: TPathStr read Funicodepath write Funicodepath;
        // TODO: mutable and read only property librarysearchpath: TSearchPathList read Flibrarysearchpath;
        // TODO: mutable and read only property unitsearchpath: TSearchPathList read Funitsearchpath;
        // TODO: mutable and read only property objectsearchpath: TSearchPathList read Fobjectsearchpath;
        // TODO: mutable and read only property includesearchpath: TSearchPathList read Fincludesearchpath;
        // TODO: mutable and read only property frameworksearchpath: TSearchPathList read Fframeworksearchpath;
        // TODO: mutable and read only property packagesearchpath: TSearchPathList read Fpackagesearchpath;
        // TODO: mutable and read only property namespacelist : TCmdStrList read Fnamespacelist;
        // TODO: mutable and read only property premodule_namespacelist: TCmdStrList read Fpremodule_namespacelist;
        property current_namespacelist: TCmdStrList read Fcurrent_namespacelist write Fcurrent_namespacelist;
        property packagelist: TFPHashList read Fpackagelist write Fpackagelist;
        property autoloadunits: string read Fautoloadunits write Fautoloadunits;
        property usewindowapi: boolean read Fusewindowapi write Fusewindowapi;
        property description: string read Fdescription write Fdescription;
        property SetPEFlagsSetExplicity: boolean read FSetPEFlagsSetExplicity write FSetPEFlagsSetExplicity;
        property SetPEOptFlagsSetExplicity: boolean read FSetPEOptFlagsSetExplicity write FSetPEOptFlagsSetExplicity;
        property SetPEOSVersionSetExplicitely: boolean read FSetPEOSVersionSetExplicitely write FSetPEOSVersionSetExplicitely;
        property SetPESubSysVersionSetExplicitely: boolean read FSetPESubSysVersionSetExplicitely write FSetPESubSysVersionSetExplicitely;
        property SetPEUserVersionSetExplicitely: boolean read FSetPEUserVersionSetExplicitely write FSetPEUserVersionSetExplicitely;
        property ImageBaseSetExplicity: boolean read FImageBaseSetExplicity write FImageBaseSetExplicity;
        property MinStackSizeSetExplicity: boolean read FMinStackSizeSetExplicity write FMinStackSizeSetExplicity;
        property MaxStackSizeSetExplicity: boolean read FMaxStackSizeSetExplicity write FMaxStackSizeSetExplicity;
        property DescriptionSetExplicity: boolean read FDescriptionSetExplicity write FDescriptionSetExplicity;
        property dllversion: string read Fdllversion write Fdllversion;
        property dllmajor: word read Fdllmajor write Fdllmajor;
        property dllminor: word read Fdllminor write Fdllminor;
        property dllrevision: word read Fdllrevision write Fdllrevision;
        property peosversionminor: word read Fpeosversionminor write Fpeosversionminor;
        property peosversionmajor: word read Fpeosversionmajor write Fpeosversionmajor;
        property pesubsysversionminor: word read Fpesubsysversionminor write Fpesubsysversionminor;
        property pesubsysversionmajor: word read Fpesubsysversionmajor write Fpesubsysversionmajor;
        property peuserversionminor: word read Fpeuserversionminor write Fpeuserversionminor;
        property peuserversionmajor: word read Fpeuserversionmajor write Fpeuserversionmajor;
        property peoptflags: longint read Fpeoptflags write Fpeoptflags;
        property peflags: longint read Fpeflags write Fpeflags;
        property minstacksize: puint read Fminstacksize write Fminstacksize;
        property maxstacksize: puint read Fmaxstacksize write Fmaxstacksize;
        property imagebase: puint read Fimagebase write Fimagebase;
        property UseDeffileForExports: boolean read FUseDeffileForExports write FUseDeffileForExports;
      end;

    function  GetEnvPChar(const envname:ansistring):pchar;
    procedure FreeEnvPChar(p:pchar);

    function is_number_float(d : double) : boolean;
    { discern +0.0 and -0.0 }
    function get_real_sign(r: bestreal): longint;

    procedure register_initdone_proc(init,done:TInitDoneProc);

    function  string2guid(const s: string; var GUID: TGUID): boolean;
    function  guid2string(const GUID: TGUID): string;

    function SetAktProcCall(const s:string; var a:tproccalloption):boolean;
    function Setabitype(const s:string;target:TCompilerTarget;var a:tabi):boolean;
    function Setoptimizecputype(const s:string;var a:tcputype):boolean;
    function Setcputype(const s:string;a:TMutableSettings):boolean;
    function SetFpuType(const s:string;var a:tfputype):boolean;
    function SetControllerType(const s:string;var a:tcontrollertype):boolean;
    function SetMinFPConstPrec(const s: string; var a: tfloattype) : boolean;

    {# Routine to get the required alignment for size of data, which will
       be placed in bss segment, according to the current alignment requirements }
    function size_2_align(len : asizeuint) : longint;
    function var_align(want_align: longint): shortint;
    function var_align_size(siz: asizeuint): shortint;
    {# Routine to get the required alignment for size of data, which will
       be placed in data/const segment, according to the current alignment requirements }
    function const_align(want_align: longint): shortint;
    function const_align_size(siz: asizeuint): shortint;
{$ifdef ARM}
    function is_double_hilo_swapped: boolean;{$ifdef USEINLINE}inline;{$endif}
{$endif ARM}
    function floating_point_range_check_error : boolean;

  { hide Sysutils.ExecuteProcess in units using this one after SysUtils}
  const
    ExecuteProcess = 'Do not use' deprecated 'Use cfileutil.RequotedExecuteProcess instead, ExecuteProcess cannot deal with single quotes as used by Unix command lines';

  var
    AllowedFilenameTransFormations : tfilenametransformations = AllTransformations;


implementation

    uses
{$if defined(macos)}
      macutils,
{$elseif defined(mswindows)}
      windirs,
{$endif}
      compiler;

    { TReadOnlySettings }

    function TReadOnlySettings.Equals(Obj: TObject): boolean;
      var
        s: TReadOnlySettings;
      begin
        if Obj is TReadOnlySettings then
          begin
            s:=TReadOnlySettings(Obj);
            Result:=alignment.Equals(s.alignment) and
                    (globalswitches=s.globalswitches) and
                    (targetswitches=s.targetswitches) and
                    (moduleswitches=s.moduleswitches) and
                    (localswitches=s.localswitches) and
                    (modeswitches=s.modeswitches) and
                    (optimizerswitches=s.optimizerswitches) and
                    (genwpoptimizerswitches=s.genwpoptimizerswitches) and
                    (dowpoptimizerswitches=s.dowpoptimizerswitches) and
                    (debugswitches=s.debugswitches) and
                    (setalloc=s.setalloc) and
                    (packenum=s.packenum) and
                    (packrecords=s.packrecords) and
                    (maxfpuregisters=s.maxfpuregisters) and
                    (verbosity=s.verbosity) and
                    (cputype=s.cputype) and
                    (optimizecputype=s.optimizecputype) and
                    (asmcputype=s.asmcputype) and
                    (fputype=s.fputype) and
                    (asmmode=s.asmmode) and
                    (interfacetype=s.interfacetype) and
                    (defproccall=s.defproccall) and
                    (sourcecodepage=s.sourcecodepage) and
                    (minfpconstprec=s.minfpconstprec) and
                    (disabledircache=s.disabledircache) and
                    (tlsmodel=s.tlsmodel) and
                    (controllertype=s.controllertype) and
                    (pmessage=s.pmessage) and
                    (lineendingtype=s.lineendingtype) and
                    (whitespacetrimcount=s.whitespacetrimcount) and
                    (whitespacetrimauto=s.whitespacetrimauto)

{$if defined(i8086) or defined(generic_cpu)}
                and (x86memorymodel=s.x86memorymodel)
{$endif defined(i8086) or defined(generic_cpu)}

{$if defined(ARM) or defined(generic_cpu)}
                and (instructionset=s.instructionset)
{$endif defined(ARM) or defined(generic_cpu)}

{$if defined(LLVM) or defined(GENERIC_CPU)}
                and (llvmversion=s.llvmversion)
{$endif defined(LLVM) or defined(GENERIC_CPU)}
            ;
          end
        else
          Result:=inherited Equals(Obj);
      end;

    function TReadOnlySettings.ToRecord: tsettings;
      begin
        FillChar(result,sizeof(result),0);
        result.alignment:=alignment.ToRecord;
        result.globalswitches:=globalswitches;
        result.targetswitches:=targetswitches;
        result.moduleswitches:=moduleswitches;
        result.localswitches:=localswitches;
        result.modeswitches:=modeswitches;
        result.optimizerswitches:=optimizerswitches;
        result.genwpoptimizerswitches:=genwpoptimizerswitches;
        result.dowpoptimizerswitches:=dowpoptimizerswitches;
        result.debugswitches:=debugswitches;
        result.setalloc:=setalloc;
        result.packenum:=packenum;
        result.packrecords:=packrecords;
        result.maxfpuregisters:=maxfpuregisters;
        result.verbosity:=verbosity;
        result.cputype:=cputype;
        result.optimizecputype:=optimizecputype;
        result.asmcputype:=asmcputype;
        result.fputype:=fputype;
        result.asmmode:=asmmode;
        result.interfacetype:=interfacetype;
        result.defproccall:=defproccall;
        result.sourcecodepage:=sourcecodepage;
        result.minfpconstprec:=minfpconstprec;
        result.disabledircache:=disabledircache;
        result.tlsmodel:=tlsmodel;
        result.controllertype:=controllertype;
        result.pmessage:=pmessage;
        result.lineendingtype:=lineendingtype;
        result.whitespacetrimcount:=whitespacetrimcount;
        result.whitespacetrimauto:=whitespacetrimauto;

{$if defined(i8086) or defined(generic_cpu)}
        result.x86memorymodel:=x86memorymodel;
{$endif defined(i8086) or defined(generic_cpu)}

{$if defined(ARM) or defined(generic_cpu)}
        result.instructionset:=instructionset;
{$endif defined(ARM) or defined(generic_cpu)}

{$if defined(LLVM) or defined(GENERIC_CPU)}
        result.llvmversion:=llvmversion;
{$endif defined(LLVM) or defined(GENERIC_CPU)}
      end;

    { TMutableSettings }

    function TMutableSettings.GetMutableAlignment: TMutableAlignmentInfo;
      begin
        result:=TMutableAlignmentInfo(FAlignment);
      end;

    constructor TMutableSettings.Create;
      begin
        inherited;
      end;

    constructor TMutableSettings.CreateFromRecord(const rec: tsettings);
      begin
        { FreeAndNil, because this constructor can also be used like a method }
        FreeAndNil(FAlignment);
        FAlignment:=TMutableAlignmentInfo.CreateFromRecord(rec.alignment);
        globalswitches:=rec.globalswitches;
        targetswitches:=rec.targetswitches;
        moduleswitches:=rec.moduleswitches;
        localswitches:=rec.localswitches;
        modeswitches:=rec.modeswitches;
        optimizerswitches:=rec.optimizerswitches;
        genwpoptimizerswitches:=rec.genwpoptimizerswitches;
        dowpoptimizerswitches:=rec.dowpoptimizerswitches;
        debugswitches:=rec.debugswitches;
        setalloc:=rec.setalloc;
        packenum:=rec.packenum;
        packrecords:=rec.packrecords;
        maxfpuregisters:=rec.maxfpuregisters;
        verbosity:=rec.verbosity;
        cputype:=rec.cputype;
        optimizecputype:=rec.optimizecputype;
        asmcputype:=rec.asmcputype;
        fputype:=rec.fputype;
        asmmode:=rec.asmmode;
        interfacetype:=rec.interfacetype;
        defproccall:=rec.defproccall;
        sourcecodepage:=rec.sourcecodepage;
        minfpconstprec:=rec.minfpconstprec;
        disabledircache:=rec.disabledircache;
        tlsmodel:=rec.tlsmodel;
        controllertype:=rec.controllertype;
        pmessage:=rec.pmessage;
        lineendingtype:=rec.lineendingtype;
        whitespacetrimcount:=rec.whitespacetrimcount;
        whitespacetrimauto:=rec.whitespacetrimauto;

{$if defined(i8086) or defined(generic_cpu)}
        x86memorymodel:=rec.x86memorymodel;
{$endif defined(i8086) or defined(generic_cpu)}

{$if defined(ARM) or defined(generic_cpu)}
        instructionset:=rec.instructionset;
{$endif defined(ARM) or defined(generic_cpu)}

{$if defined(LLVM) or defined(GENERIC_CPU)}
        llvmversion:=rec.llvmversion;
{$endif defined(LLVM) or defined(GENERIC_CPU)}
      end;

    procedure TMutableSettings.Assign(Source: TReadOnlySettings);
      var
        r: TSettings;
      begin
        r:=Source.ToRecord;
        CreateFromRecord(r);
      end;

{****************************************************************************
                                 TLinkStrMap
****************************************************************************}

    Constructor TLinkStrMap.create;
      begin
        inherited;
        itemcnt:=0;
      end;


    procedure TLinkStrMap.Add(const key:ansistring;const value:AnsiString='';weight:longint=LinkMapWeightDefault);
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


    function  TLinkStrMap.AddDep(const keyvalue:String):boolean;
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


    function  TLinkStrMap.AddWeight(const keyvalue:String):boolean;
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


    procedure TLinkStrMap.addseries(const keys:AnsiString;weight:longint);
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

    procedure TLinkStrMap.SetValue(const Key:Ansistring;weight:Integer);
      var
        j : longint;
      begin
         j:=lookup(key);
         if j<>-1 then
          fmap[j].weight:=weight;
      end;


    function TLinkStrMap.find(const key:Ansistring):Ansistring;
      var
        j : longint;
      begin
         find:='';
         j:=lookup(key);
         if j<>-1 then
          find:=fmap[j].value;
      end;


    function TLinkStrMap.lookup(const key:Ansistring):longint;
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


   function TCompilerTime.gettimestr:string;
   {
     get the current time in a string HH:MM:SS
   }
      var
        st: TSystemTime;
      begin
        GetLocalTime(st);
        gettimestr:=L0(st.Hour)+':'+L0(st.Minute)+':'+L0(st.Second);
      end;


   function TCompilerTime.getdatestr:string;
   {
     get the current date in a string YY/MM/DD
   }
      var
        st: TSystemTime;
      begin
        GetLocalTime(st);
        getdatestr:=L0(st.Year)+'/'+L0(st.Month)+'/'+L0(st.Day);
      end;


   function  TCompilerTime.filetimestring( t : longint) : string;
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

   function TCompilerTime.getrealtime(const st: TSystemTime) : real;
     begin
       result := st.Hour*3600.0 + st.Minute*60.0 + st.Second + st.MilliSecond/1000.0;
     end;

   function TCompilerTime.getrealtime : real;
     var
       st:TSystemTime;
     begin
       GetLocalTime(st);
       result:=getrealtime(st);
     end;


 {****************************************************************************
                               OS Dependent things
 ****************************************************************************}

    function GetEnvPChar(const envname:ansistring):pchar;
      {$ifdef mswindows}
      var
        s     : string;
        i,len : longint;
        hp,p,p2 : pchar;
      {$endif}
      begin
      {$ifdef hasunix}
        GetEnvPchar:=BaseUnix.fpGetEnv(pansichar(envname));
        {$define GETENVOK}
      {$endif}
      {$ifdef mswindows}
        GetEnvPchar:=nil;
        p:=GetEnvironmentStringsA;
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
        if (length(GetEnvPChar)=0) then
          begin
            FreeEnvPChar(GetEnvPChar);
            GetEnvPChar:=nil;
          end;
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
{$ifdef FPUARM_HAS_FPA}
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

    function convertdoublerec(d : tcompdoublerec) : tcompdoublerec;{$ifdef USEINLINE}inline;{$endif}
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
          { these values are already in the correct range (4 chars = word) }
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
        DefProcCallName : array[tproccalloption] of string[16] = ('',
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
         'MWPASCAL',
         'INTERRUPT',
         'HARDFLOAT',
         'SYSV_ABI_DEFAULT',
         'SYSV_ABI_CDECL',
         'MS_ABI_DEFAULT',
         'MS_ABI_CDECL',
         'VECTORCALL'
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


    function Setabitype(const s:string;target:TCompilerTarget;var a:tabi):boolean;
      var
        t  : tabi;
        hs : string;
      begin
        result:=false;
        hs:=Upper(s);
        for t:=low(t) to high(t) do
          if abiinfo[t].supported and
             (abiinfo[t].name=hs) then
            begin
              a:=t;
              { abi_old_win32_gnu is a win32 i386 specific "feature" }
              if (t<>abi_old_win32_gnu) or (target.info.system=system_i386_win32) then
                result:=true;
              break;
            end;
      end;


    function Setoptimizecputype(const s:string;var a:tcputype):boolean;
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


    function Setcputype(const s:string;a:TMutableSettings):boolean;
      var
        t  : tcputype;
        hs : string;
      begin
        result:=false;
        hs:=Upper(s);
        for t:=low(tcputype) to high(tcputype) do
          if cputypestr[t]=hs then
            begin
              a.cputype:=t;
              result:=true;
              break;
            end;
{$ifdef arm}
        { set default instruction set for arm }
        if result then
          begin
            if a.cputype in [cpu_armv6m,cpu_armv6t2,cpu_armv7m,cpu_armv7em] then
              a.instructionset:=is_thumb
            else
              a.instructionset:=is_arm;
          end;
{$endif arm}
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


    function SetControllerType(const s:string;var a:tcontrollertype):boolean;
      var
        t  : tcontrollertype;
        hs : string;
      begin
{ The following check allows to reduce amount of code for platforms  }
{ not supporting microcontrollers due to evaluation at compile time. }
{$PUSH}
 {$WARN 6018 OFF} (* Unreachable code due to compile time evaluation *)
        if ControllerSupport then
         begin
          result:=false;
          hs:=Upper(s);
          for t:=low(tcontrollertype) to high(tcontrollertype) do
            if embedded_controllers[t].controllertypestr=hs then
              begin
                a:=t;
                result:=true;
                break;
              end;
         end
        else
         begin
          a := ct_none;
          Result := true;
         end;
{$POP}
      end;


    function TCompilerGlobals.HandleFeature(const s : string) : boolean;
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
        { Also support -Sfnoheap to exclude heap }
        if Copy(S,1,2)='NO' then
          for i:=low(tfeature) to high(tfeature) do
            if s='NO'+featurestr[i] then
              begin
                exclude(features,i);
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


    function size_2_align(len : asizeuint) : longint;
      begin
         if len>16 then
           size_2_align:=32
         else if len>8 then
           size_2_align:=16
         else if len>4 then
           size_2_align:=8
         else if len>2 then
           size_2_align:=4
         else if len>1 then
           size_2_align:=2
         else
           size_2_align:=1;
      end;


    function var_align(want_align: longint): shortint;
      var
        compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
      begin
        var_align := used_align(want_align,compiler.globals.current_settings.alignment.varalignmin,compiler.globals.current_settings.alignment.varalignmax);
      end;


    function var_align_size(siz: asizeuint): shortint;
      var
        compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
      begin
        siz := size_2_align(siz);
        var_align_size := var_align(siz);
      end;


    function const_align(want_align: longint): shortint;
      var
        compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
      begin
        const_align := used_align(want_align,compiler.globals.current_settings.alignment.constalignmin,compiler.globals.current_settings.alignment.constalignmax);
      end;


    function const_align_size(siz: asizeuint): shortint;
      begin
        siz := size_2_align(siz);
        const_align_size := const_align(siz);
      end;


{$ifdef ARM}
    function is_double_hilo_swapped: boolean;{$ifdef USEINLINE}inline;{$endif}
      var
        compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
      begin
        result := (compiler.globals.current_settings.fputype in [fpu_fpa,fpu_fpa10,fpu_fpa11]) and
          not(cs_fp_emulation in compiler.globals.current_settings.moduleswitches);
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
        { inverse result if compiler was compiled with swapped hilo already }
        result := not result;
{$endif FPC_DOUBLE_HILO_SWAPPED}
      end;
{$endif ARM}


    function floating_point_range_check_error : boolean;
      var
        compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
      begin
        result:=cs_ieee_errors in compiler.globals.current_settings.localswitches;
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

   procedure TCompilerGlobals.get_exepath;
     var
       localExepath : TCmdStr;
       exeName:TCmdStr;
{$ifdef need_path_search}
       hs1 : TPathStr;
{$endif need_path_search}
     begin
       localexepath:=GetEnvironmentVariable('PPC_EXEC_PATH');
       exeName := '';
       if localexepath='' then
         begin
           exeName := FixFileName(system.paramstr(0));
           localexepath := ExtractFilePath(exeName);
         end;
{$ifdef need_path_search}
       if localexepath='' then
        begin
          hs1 := ExtractFileName(exeName);
          hs1 := ChangeFileExt(hs1,source_info.exeext);
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



   type
     tinitdoneentry=record
       init:TInitDoneProc;
       done:TInitDoneProc;
     end;
     pinitdoneentry=^tinitdoneentry;


   const
     initdoneprocs : TFPList = nil;


   procedure register_initdone_proc(init,done:TInitDoneProc);
     var
       entry : pinitdoneentry;
     begin
       new(entry);
       entry^.init:=init;
       entry^.done:=done;
       TFPList.AddOnDemand(initdoneprocs,entry);
     end;


   procedure TCompilerGlobals.callinitprocs;
     var
       i : longint;
     begin
       if not assigned(initdoneprocs) then
         exit;
       for i:=0 to initdoneprocs.count-1 do
         with pinitdoneentry(initdoneprocs[i])^ do
           if assigned(init) then
             init(Self);
     end;


   procedure TCompilerGlobals.calldoneprocs;
     var
       i : longint;
     begin
       if not assigned(initdoneprocs) then
         exit;
       for i:=0 to initdoneprocs.count-1 do
         with pinitdoneentry(initdoneprocs[i])^ do
           if assigned(done) then
             done(Self);
     end;


   procedure TCompilerGlobals.DoneGlobals;
     begin
       calldoneprocs;
       FreeAndNil(Flibrarysearchpath);
       FreeAndNil(Funitsearchpath);
       FreeAndNil(Fobjectsearchpath);
       FreeAndNil(Fincludesearchpath);
       FreeAndNil(Fframeworksearchpath);
       FreeAndNil(LinkLibraryAliases);
       FreeAndNil(LinkLibraryOrder);
       FreeAndNil(Fpackagesearchpath);
       FreeAndNil(Fnamespacelist);
       FreeAndNil(Fpremodule_namespacelist);
       current_namespacelist:=Nil;
       FreeAndNil(current_settings);
       FreeAndNil(init_settings);
       stringdispose(Foutputprefix);
       stringdispose(Foutputsuffix);
     end;

   procedure TCompilerGlobals.InitGlobals(ATarget: TCompilerTarget);
     begin
        init_settings:=TMutableSettings.Create;
        current_settings:=TMutableSettings.Create;

        get_exepath;

        { reset globals }
        do_build:=false;
        do_release:=false;
        do_make:=true;
        global_unit_count:=0;

        { Output }
        OutputFileName:='';
        OutputPrefix:=Nil;
        OutputSuffix:=Nil;

        outputexedir:='';
        outputunitdir:='';

        { Utils directory }
        utilsdirectory:='';
        utilsprefix:='';
        llvmutilssuffix:='';
        cshared:=false;
        rlinkpath:='';
        sysrootpath:='';
{$if defined(XTENSA) or defined(RISCV32)}
        idfpath:='';
{$endif defined(XTENSA) or defined(RISCV32)}

        { Search Paths }
        unicodepath:='';
        Flibrarysearchpath:=TSearchPathList.Create;
        Funitsearchpath:=TSearchPathList.Create;
        Fincludesearchpath:=TSearchPathList.Create;
        Fobjectsearchpath:=TSearchPathList.Create;
        Fframeworksearchpath:=TSearchPathList.Create;
        Fpackagesearchpath:=TSearchPathList.Create;
        Fnamespacelist:=TCmdStrList.Create;
        Fpremodule_namespacelist:=TCmdStrList.Create;
        current_namespacelist:=Nil;
        { Def file }
        usewindowapi:=false;
        description:='Compiled by FPC '+version_string+' - '+ATarget.cpu_string;
        DescriptionSetExplicity:=false;
        SetPEFlagsSetExplicity:=false;
        SetPEOptFlagsSetExplicity:=false;
        SetPEOSVersionSetExplicitely:=false;
        SetPESubSysVersionSetExplicitely:=false;
        SetPEUserVersionSetExplicitely:=false;
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
        { memory sizes, will be overridden by parameter or default for target
          in options or init_parser }
        stacksize:=0;
        { not initialized yet }
        apptype:=app_cui;

        { Init values }
        init_settings.CreateFromRecord(default_settings);
        if init_settings.optimizecputype=cpu_none then
          init_settings.optimizecputype:=init_settings.cputype;

        LinkLibraryAliases :=TLinkStrMap.Create;
        LinkLibraryOrder   :=TLinkStrMap.Create;

        { enable all features by default }
        features:=[low(Tfeature)..high(Tfeature)];

        Inside_asm_statement:=false;
        parser_current_file:='';

{$if defined(m68k) or defined(arm)}
        palmos_applicationname := 'FPC Application';
        palmos_applicationid := 'FPCA';
{$endif defined(m68k) or defined(arm)}
{$if defined(m68k)}
        ataritos_exe_flags:=7;
        ataritos_exe_format:='ataritos';
        sinclairql_metadata_format:='QHDR';
        sinclairql_vlink_experimental:=true; { temporary }
{$endif defined(m68k)}

        mainaliasname:=defaultmainaliasname;

        LTOExt:='';

        callinitprocs;
     end;

   constructor TCompilerGlobals.Create(ATarget: TCompilerTarget);
     begin
       InitGlobals(ATarget);
     end;

   destructor TCompilerGlobals.Destroy;
     begin
       DoneGlobals;
       inherited Destroy;
     end;

initialization
{$ifdef LLVM}
  cgbackend:=cg_llvm;
{$else}
  cgbackend:=cg_fpc;
{$endif}
finalization
  tfplist.FreeAndNilDisposing(initdoneprocs,TypeInfo(tinitdoneentry));
end.
