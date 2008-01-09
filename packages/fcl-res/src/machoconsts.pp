{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Constants used by Mach-O resource reader and writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit machoconsts;

{$MODE OBJFPC}

interface

uses
  machotypes;

const
  DataSegName     : TSegSectName = '__DATA'+#0+#0+#0+#0+#0+#0+#0+#0+#0+#0;
  RsrcSectName    : TSegSectName = 'fpc.resources'+#0+#0+#0;
  HandlesSectName : TSegSectName = 'fpc.reshandles'+#0+#0;
  
  //private constants used by reader and writer only, not apple-defined
  MACH_BIG_ENDIAN = 1;
  MACH_LITTLE_ENDIAN = 2;

  MACH_ERRBIT = 0;
  MACH_32BIT = 1;
  MACH_64BIT = 2;
  //end of private constants

  //Mach-O magic numbers
  MH_MAGIC    = $FEEDFACE;
  MH_MAGIC_64 = $FEEDFACF;
  MH_CIGAM    = $CEFAEDFE;
  MH_CIGAM_64 = $CFFAEDFE;

  //Cpu types
  CPU_ARCH_ABI64     = $1000000;
  CPU_TYPE_ANY       = -1;
  CPU_TYPE_I386      = 7;
  CPU_TYPE_X86_64    = CPU_TYPE_I386 or CPU_ARCH_ABI64;
//  CPU_TYPE_ARM       = 12;
  CPU_TYPE_POWERPC   = 18;
  CPU_TYPE_POWERPC64 = CPU_TYPE_POWERPC or CPU_ARCH_ABI64;

  //Cpu subtypes
  CPU_SUBTYPE_I386_ALL    = 3;
  CPU_SUBTYPE_X86_64_ALL  = CPU_SUBTYPE_I386_ALL;
  CPU_SUBTYPE_POWERPC_ALL = 0;

  //Mach-O object types
  MH_OBJECT      = $1;            // relocatable object file
  MH_EXECUTE     = $2;            // demand paged executable file
  MH_FVMLIB      = $3;            // fixed VM shared library file
  MH_CORE        = $4;            // core file
  MH_PRELOAD     = $5;            // preloaded executable file
  MH_DYLIB       = $6;            // dynamically bound shared library
  MH_DYLINKER    = $7;            // dynamic link editor
  MH_BUNDLE      = $8;            // dynamically bound bundle file
  MH_DYLIB_STUB  = $9;            // shared library stub for static
                                  //  linking only, no section contents
  //Mach-O object flags

  MH_NOUNDEFS                = $00001;
  MH_INCRLINK                = $00002;
  MH_DYLDLINK                = $00004;
  MH_BINDATLOAD              = $00008;
  MH_PREBOUND                = $00010;
  MH_SPLIT_SEGS              = $00020;
  MH_TWOLEVEL                = $00080;
  MH_FORCE_FLAT              = $00100;
  MH_NOMULTIDEFS             = $00200;
  MH_NOFIXPREBINDING         = $00400;
  MH_PREBINDABLE             = $00800;
  MH_ALLMODSBOUND            = $01000;
  MH_SUBSECTIONS_VIA_SYMBOLS = $02000;
  MH_CANONICAL               = $04000;
  MH_WEAK_DEFINES            = $08000;
  MH_BINDS_TO_WEAK           = $10000;

  //Load commands

  LC_SEGMENT        = $1;    // segment of this file to be mapped
  LC_SYMTAB         = $2;    // link-edit stab symbol table info
  LC_SYMSEG         = $3;    // link-edit gdb symbol table info (obsolete)
  LC_THREAD         = $4;    // thread
  LC_UNIXTHREAD     = $5;    // unix thread (includes a stack)
  LC_LOADFVMLIB     = $6;    // load a specified fixed VM shared library
  LC_IDFVMLIB       = $7;    // fixed VM shared library identification
  LC_DYSYMTAB       = $b;    // dynamic link-edit symbol table info
  LC_LOAD_DYLIB     = $c;    // load a dynamically linked shared library
  LC_ID_DYLIB       = $d;    // dynamically linked shared lib ident
  LC_LOAD_DYLINKER  = $e;    // load a dynamic linker
  LC_ID_DYLINKER    = $f;    // dynamic linker identification
  LC_PREBOUND_DYLIB = $10;   // modules prebound for a dynamically
                             //  linked shared library
  LC_ROUTINES       = $11;   // image routines
  LC_SUB_FRAMEWORK  = $12;   // sub framework
  LC_SUB_UMBRELLA   = $13;   // sub umbrella
  LC_SUB_CLIENT     = $14;   // sub client
  LC_SUB_LIBRARY    = $15;   // sub library
  LC_TWOLEVEL_HINTS = $16;   // two-level namespace lookup hints
  LC_PREBIND_CKSUM  = $17;   // prebind checksum
  LC_LOAD_WEAK_DYLIB= $80000018;
  LC_SEGMENT_64     = $19;   // 64-bit segment of this file to be mapped
  LC_ROUTINES_64    = $1a;   // 64-bit image routines
  LC_UUID           = $1b;   // the uuid
  
  //Segment: virtual memory protection
  VM_PROT_NONE       = $00;
  VM_PROT_READ       = $01;                // read permission
  VM_PROT_WRITE      = $02;                // write permission
  VM_PROT_EXECUTE    = $04;                // execute permission
  VM_PROT_DEFAULT    = VM_PROT_READ or VM_PROT_WRITE;
  VM_PROT_ALL        = VM_PROT_READ or VM_PROT_WRITE or VM_PROT_EXECUTE;
  VM_PROT_NO_CHANGE  = $08;
  VM_PROT_COPY       = $10;
  VM_PROT_WANTS_COPY = $10;
  
  //Segment flags
  SG_HIGHVM  = $01;
  SG_FVMLIB  = $02;
  SG_NORELOC = $04;
  
  //Section type and attributes masks
  SECTION_TYPE       = $000000ff;    // 256 section types
  SECTION_ATTRIBUTES = $ffffff00;    //  24 section attributes

  //Section types
  S_REGULAR                  = $0;
  S_ZEROFILL                 = $1;
  S_CSTRING_LITERALS         = $2;
  S_4BYTE_LITERALS           = $3;
  S_8BYTE_LITERALS           = $4;
  S_LITERAL_POINTERS         = $5;
  S_NON_LAZY_SYMBOL_POINTERS = $6;
  S_LAZY_SYMBOL_POINTERS     = $7;
  S_SYMBOL_STUBS             = $8;
  S_MOD_INIT_FUNC_POINTERS   = $9;
  S_MOD_TERM_FUNC_POINTERS   = $a;
  S_COALESCED                = $b;
  S_GB_ZEROFILL              = $c;
  S_INTERPOSING              = $d;

  //Section attributes
  SECTION_ATTRIBUTES_USR   = $ff000000;
  S_ATTR_PURE_INSTRUCTIONS = $80000000;
  S_ATTR_NO_TOC            = $40000000;
  S_ATTR_STRIP_STATIC_SYMS = $20000000;
  S_ATTR_NO_DEAD_STRIP     = $10000000;
  S_ATTR_LIVE_SUPPORT      = $08000000;
  SECTION_ATTRIBUTES_SYS   = $00ffff00;
  S_ATTR_SOME_INSTRUCTIONS = $00000400;
  S_ATTR_EXT_RELOC         = $00000200;
  S_ATTR_LOC_RELOC         = $00000100;

  //Symbols: masks for type
  N_STAB = $e0;  // if any of these bits set, a symbolic debugging entry
  N_PEXT = $10;  // private external symbol bit
  N_TYPE = $0e;  // mask for the type bits
  N_EXT  = $01;  // external symbol bit, set for external symbols

  //values for type in the N_TYPE bits
  N_UNDF = $0;             // undefined, n_sect == NO_SECT
  N_ABS  = $2;             // absolute, n_sect == NO_SECT
  N_SECT = $e;             // defined in section number n_sect
  N_PBUD = $c;             // prebound undefined (defined in a dylib)
  N_INDR = $a;             // indirect
  
  //Relocations: masks for flag
  R_SYMBOLNUM_BE = $FFFFFF00;
  R_PCREL_BE     = $00000080;
  R_LENGTH_BE    = $00000060;
  R_EXTERN_BE    = $00000010;
  R_TYPE_BE      = $0000000F;

  R_SYMBOLNUM_LE = $00FFFFFF;
  R_PCREL_LE     = $01000000;
  R_LENGTH_LE    = $06000000;
  R_EXTERN_LE    = $08000000;
  R_TYPE_LE      = $F0000000;

  //relocation types - powerpc
  PPC_RELOC_VANILLA = 0;      // generic relocation

  //relocation types - i386
  GENERIC_RELOC_VANILLA = 0;  // generic relocation

  //relocation types - x86_64
  X86_64_RELOC_UNSIGNED = 0;  // for absolute addresses


implementation

end.
