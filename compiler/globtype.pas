{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl, Pierre Muller

    Global types

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
unit globtype;

{$i fpcdefs.inc}

interface

    const
       maxidlen = 64;

    type
{$ifdef ver1_0}
       { Bootstrapping }
       PtrInt = DWord;
       SizeInt = Longint;
{$endif ver1_0}

       { Switches which can be changed locally }
       tlocalswitch = (cs_localnone,
         { codegen }
         cs_check_overflow,cs_check_range,cs_check_object,
         cs_check_io,cs_check_stack,
         cs_omitstackframe,cs_do_assertion,cs_generate_rtti,
         cs_full_boolean_eval,cs_typed_const_writable,
         { mmx }
         cs_mmx,cs_mmx_saturation,
         { parser }
         cs_typed_addresses,cs_strict_var_strings,cs_ansistrings
       );
       tlocalswitches = set of tlocalswitch;

       { Switches which can be changed only at the beginning of a new module }
       tmoduleswitch = (cs_modulenone,
         { parser }
         cs_fp_emulation,cs_extsyntax,cs_openstring,
         { support }
         cs_support_inline,cs_support_goto,cs_support_macro,
         cs_support_c_operators,cs_static_keyword,
         { generation }
         cs_profile,cs_debuginfo,cs_browser,cs_local_browser,cs_compilesystem,
         cs_lineinfo,cs_threading,cs_implicit_exceptions,
         { linking }
         cs_create_smart,cs_create_dynamic,cs_create_pic
       );
       tmoduleswitches = set of tmoduleswitch;

       { Switches which can be changed only for a whole program/compilation,
         mostly set with commandline }
       tglobalswitch = (cs_globalnone,
         { parameter switches }
         cs_check_unit_name,cs_constructor_name,
         { units }
         cs_load_objpas_unit,
         cs_load_gpc_unit,
         { optimizer }
         cs_regvars,cs_no_regalloc,cs_uncertainopts,cs_littlesize,
         cs_optimize,cs_fastoptimize,cs_slowoptimize,cs_align,
         { browser }
         cs_browser_log,
         { debugger }
         cs_gdb_dbx,cs_gdb_gsym,cs_gdb_heaptrc,cs_gdb_lineinfo,
         cs_checkpointer,cs_gdb_valgrind,
         { assembling }
         cs_asm_leave,cs_asm_extern,cs_asm_pipe,cs_asm_source,
         cs_asm_regalloc,cs_asm_tempalloc,cs_asm_nodes,
         { linking }
         cs_link_extern,cs_link_static,cs_link_smart,cs_link_shared,cs_link_deffile,
	 cs_link_strip,cs_link_staticflag,cs_link_on_target,cs_link_internal,
	 cs_link_map,cs_link_pthread
       );
       tglobalswitches = set of tglobalswitch;

       { Switches which can be changed by a mode (fpc,tp7,delphi) }
       tmodeswitch = (m_none,m_all, { needed for keyword }
         { generic }
         m_fpc,m_objfpc,m_delphi,m_tp7,m_gpc,m_mac,
         { more specific }
         m_class,               { delphi class model }
         m_objpas,              { load objpas unit }
         m_result,              { result in functions }
         m_string_pchar,        { pchar 2 string conversion }
         m_cvar_support,        { cvar variable directive }
         m_nested_comment,      { nested comments }
         m_tp_procvar,          { tp style procvars (no @ needed) }
         m_repeat_forward,      { repeating forward declarations is needed }
         m_pointer_2_procedure, { allows the assignement of pointers to
                                  procedure variables                     }
         m_autoderef,           { does auto dereferencing of struct. vars }
         m_initfinal,           { initialization/finalization for units }
         m_add_pointer,         { allow pointer add/sub operations }
         m_default_ansistring,  { ansistring turned on by default }
         m_out,                 { support the calling convention OUT }
         m_default_para,        { support default parameters }
         m_hintdirective,       { support hint directives }
         m_duplicate_names      { allow locals/paras to have duplicate names of globals }
       );
       tmodeswitches = set of tmodeswitch;

       { Win32, OS/2 & MacOS application types }
       tapptype = (
         app_none,
         app_gui,		{ graphic user-interface application}
         app_cui,       { console application}
         app_fs,        { full-screen type application (OS/2 and EMX only) }
         app_tool       { tool application, (MPW tool for MacOS, MacOS only)}
       );

       { interface types }
       tinterfacetypes = (
         it_interfacecom,
         it_interfacecorba
       );

       { currently parsed block type }
       tblock_type = (bt_none,
         bt_general,bt_type,bt_const,bt_except,bt_body
       );

       { calling convention for tprocdef and tprocvardef }
       tproccalloption=(pocall_none,
         { procedure uses C styled calling }
         pocall_cdecl,
         { C++ calling conventions }
         pocall_cppdecl,
         { Procedure is used for internal compiler calls }
         pocall_compilerproc,
         { Far16 for OS/2 }
         pocall_far16,
         { Old style FPC default calling }
         pocall_oldfpccall,
         { Procedure is an assembler macro }
         pocall_inline,
         { Procedure has compiler magic}
         pocall_internproc,
         { procedure is a system call, applies e.g. to MorphOS and PalmOS }
         pocall_syscall,
         { pascal standard left to right }
         pocall_pascal,
         { procedure uses register (fastcall) calling }
         pocall_register,
         { safe call calling conventions }
         pocall_safecall,
         { procedure uses stdcall call }
         pocall_stdcall,
         { Special calling convention for cpus without a floating point
           unit. Floating point numbers are passed in integer registers
           instead of floating point registers. Depending on the other
           available calling conventions available for the cpu
           this replaces either pocall_fastcall or pocall_stdcall.
         }
         pocall_softfloat
       );
       tproccalloptions = set of tproccalloption;

       tprocinfoflag=(
         { procedure uses asm }
         pi_uses_asm,
         { procedure does a call }
         pi_do_call,
         { procedure has a try statement = no register optimization }
         pi_uses_exceptions,
         { procedure is declared as @var(assembler), don't optimize}
         pi_is_assembler,
         { procedure contains data which needs to be finalized }
         pi_needs_implicit_finally,
         { procedure has the implicit try..finally generated }
         pi_has_implicit_finally,
         { procedure uses fpu}
         pi_uses_fpu,
         { procedure uses GOT for PIC code }
         pi_needs_got
       );
       tprocinfoflags=set of tprocinfoflag;

{$ifdef ansistring_bits}
       Tstringbits=(sb_16,sb_32,sb_64);
{$endif}

     const
       proccalloptionStr : array[tproccalloption] of string[14]=('',
           'CDecl',
           'CPPDecl',
           'CompilerProc',
           'Far16',
           'OldFPCCall',
           'Inline',
           'InternProc',
           'SysCall',
           'Pascal',
           'Register',
           'SafeCall',
           'StdCall',
           'SoftFloat'
         );

       { Default calling convention }
{$ifdef x86}
       pocall_default = pocall_register;
{$else}
       pocall_default = pocall_stdcall;
{$endif}

    type
       stringid = string[maxidlen];

       tnormalset = set of byte; { 256 elements set }
       pnormalset = ^tnormalset;

       pboolean   = ^boolean;
       pdouble    = ^double;
       pbyte      = ^byte;
       pword      = ^word;
       plongint   = ^longint;
       plongintarray = plongint;

       Tconstant=record
            case signed:boolean of
                false:
                    (valueu:cardinal);
                true:
                    (values:longint);
       end;

{$ifndef Delphi}
  {$ifndef xFPC}
    type
      pguid = ^tguid;
      tguid = packed record
        D1: LongWord;
        D2: Word;
        D3: Word;
        D4: array[0..7] of Byte;
      end;
  {$endif}
{$endif}

    const
       { link options }
       link_none    = $0;
       link_allways = $1;
       link_static  = $2;
       link_smart   = $4;
       link_shared  = $8;

implementation

end.
{
  $Log$
  Revision 1.55  2004-05-23 14:32:17  peter
    * tprocinfoflag moved to globtype

  Revision 1.54  2004/05/02 11:48:46  peter
    * strlenint is replaced with sizeint

  Revision 1.53  2004/04/29 19:56:36  daniel
    * Prepare compiler infrastructure for multiple ansistring types

  Revision 1.52  2004/04/28 15:19:03  florian
    + syscall directive support for MorphOS added

  Revision 1.51  2004/04/04 18:46:09  olle
    + added $APPTYPE TOOL for MPW tools on MacOS

  Revision 1.50  2004/03/10 22:52:57  peter
    * more stabs fixes
    * special mode -gv for valgrind compatible stabs

  Revision 1.49  2004/02/15 16:34:18  marco
   * pthread on -CURRENT related fixes.

  Revision 1.48  2003/12/23 23:22:35  peter
    * register calling is now default for i386

  Revision 1.47  2003/12/14 20:51:17  daniel
    * Register calling disabled again

  Revision 1.46  2003/12/14 20:24:28  daniel
    * Register allocator speed optimizations
      - Worklist no longer a ringbuffer
      - No find operations are left
      - Simplify now done in constant time
      - unusedregs is now a Tsuperregisterworklist
      - Microoptimizations

  Revision 1.45  2003/12/04 23:27:32  peter
    * remove redundant calls to add_edge_used

  Revision 1.44  2003/11/07 15:58:32  florian
    * Florian's culmutative nr. 1; contains:
      - invalid calling conventions for a certain cpu are rejected
      - arm softfloat calling conventions
      - -Sp for cpu dependend code generation
      - several arm fixes
      - remaining code for value open array paras on heap

  Revision 1.43  2003/09/28 13:39:58  peter
    * default calling convention changed to stdcall

  Revision 1.42  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.41  2003/09/04 21:37:29  olle
    + added new lagnuage mode: MAC

  Revision 1.40  2003/09/03 15:55:00  peter
    * NEWRA branch merged

  Revision 1.39  2003/08/09 18:56:54  daniel
    * cs_regalloc renamed to cs_regvars to avoid confusion with register
      allocator
    * Some preventive changes to i386 spillinh code

  Revision 1.38  2003/04/27 11:21:32  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.37  2003/04/27 07:29:50  peter
    * current_procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.36  2003/04/22 23:50:22  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.35  2003/03/08 08:59:07  daniel
    + $define newra will enable new register allocator
    + getregisterint will return imaginary registers with $newra
    + -sr switch added, will skip register allocation so you can see
      the direct output of the code generator before register allocation

  Revision 1.34  2002/10/16 19:01:43  peter
    + $IMPLICITEXCEPTIONS switch to turn on/off generation of the
      implicit exception frames for procedures with initialized variables
      and for constructors. The default is on for compatibility

  Revision 1.33  2002/10/14 19:43:41  peter
    * threading switch, defines the symbol FPC_THREADING

  Revision 1.32  2002/10/05 12:43:24  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.31  2002/08/19 19:36:42  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.30  2002/08/12 15:08:39  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.29  2002/08/06 20:55:20  florian
    * first part of ppc calling conventions fix

  Revision 1.28  2002/07/04 20:43:00  florian
    * first x86-64 patches

  Revision 1.27  2002/07/01 18:46:22  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.26  2002/05/18 13:34:08  peter
    * readded missing revisions

  Revision 1.25  2002/05/16 19:46:36  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.23  2002/05/12 16:53:05  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.22  2002/04/21 19:02:03  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.21  2002/03/24 19:05:59  carl
  + patch for SPARC from Mazen NEIFER

  Revision 1.20  2002/01/24 18:25:48  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

}
