{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Constants used by COFF resource reader and writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit coffconsts;

{$MODE OBJFPC}

interface

//Machine types
const
  IMAGE_FILE_MACHINE_UNKNOWN         = 0;
  IMAGE_FILE_MACHINE_I386            = $014c;  // Intel 386.
{
  IMAGE_FILE_MACHINE_R3000           = $0162;  // MIPS little-endian, 0x160 big-endian
  IMAGE_FILE_MACHINE_R4000           = $0166;  // MIPS little-endian
  IMAGE_FILE_MACHINE_R10000          = $0168;  // MIPS little-endian
  IMAGE_FILE_MACHINE_WCEMIPSV2       = $0169;  // MIPS little-endian WCE v2
  IMAGE_FILE_MACHINE_ALPHA           = $0184;  // Alpha_AXP
  IMAGE_FILE_MACHINE_SH3             = $01a2;  // SH3 little-endian
  IMAGE_FILE_MACHINE_SH3DSP          = $01a3;
  IMAGE_FILE_MACHINE_SH3E            = $01a4;  // SH3E little-endian
  IMAGE_FILE_MACHINE_SH4             = $01a6;  // SH4 little-endian
  IMAGE_FILE_MACHINE_SH5             = $01a8;  // SH5
}
  IMAGE_FILE_MACHINE_ARM             = $01c0;  // ARM Little-Endian
{
  IMAGE_FILE_MACHINE_THUMB           = $01c2;
  IMAGE_FILE_MACHINE_AM33            = $01d3;
}
  IMAGE_FILE_MACHINE_POWERPC32_AIX   = $01DF;  // IBM AIX 32 bit PowerPC
  IMAGE_FILE_MACHINE_POWERPC64_AIX   = $01EF;  // IBM AIX 62 bit PowerPC
{
  IMAGE_FILE_MACHINE_POWERPC         = $01F0;  // IBM PowerPC Little-Endian
  IMAGE_FILE_MACHINE_POWERPCFP       = $01f1;
  IMAGE_FILE_MACHINE_IA64            = $0200;  // Intel 64
  IMAGE_FILE_MACHINE_MIPS16          = $0266;  // MIPS
  IMAGE_FILE_MACHINE_ALPHA64         = $0284;  // ALPHA64
  IMAGE_FILE_MACHINE_MIPSFPU         = $0366;  // MIPS
  IMAGE_FILE_MACHINE_MIPSFPU16       = $0466;  // MIPS
  IMAGE_FILE_MACHINE_AXP64           = IMAGE_FILE_MACHINE_ALPHA64;
  IMAGE_FILE_MACHINE_TRICORE         = $0520;  // Infineon
  IMAGE_FILE_MACHINE_CEF             = $0CEF;
  IMAGE_FILE_MACHINE_EBC             = $0EBC;  // EFI Byte Code
}
  IMAGE_FILE_MACHINE_AMD64           = $8664;  // AMD64 (K8)
  IMAGE_FILE_MACHINE_ARM64           = $aa64;  // ARM64 little endian
{
  IMAGE_FILE_MACHINE_M32R            = $9041;  // M32R little-endian
  IMAGE_FILE_MACHINE_CEE             = $C0EE;
}

//Coff header characteristics
const
  IMAGE_FILE_RELOCS_STRIPPED         = $0001;  // Relocation info stripped from file.
  IMAGE_FILE_EXECUTABLE_IMAGE        = $0002;  // File is executable  (i.e. no unresolved externel references).
  IMAGE_FILE_LINE_NUMS_STRIPPED      = $0004;  // Line nunbers stripped from file.
  IMAGE_FILE_LOCAL_SYMS_STRIPPED     = $0008;  // Local symbols stripped from file.
  IMAGE_FILE_AGGRESIVE_WS_TRIM       = $0010;  // Agressively trim working set
  IMAGE_FILE_LARGE_ADDRESS_AWARE     = $0020;  // App can handle >2gb addresses
  IMAGE_FILE_BYTES_REVERSED_LO       = $0080;  // Bytes of machine word are reversed.
  IMAGE_FILE_32BIT_MACHINE           = $0100;  // 32 bit word machine.
  IMAGE_FILE_DEBUG_STRIPPED          = $0200;  // Debugging info stripped from file in .DBG file
  IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP = $0400;  // If Image is on removable media, copy and run from the swap file.
  IMAGE_FILE_NET_RUN_FROM_SWAP       = $0800;  // If Image is on Net, copy and run from the swap file.
  IMAGE_FILE_SYSTEM                  = $1000;  // System File.
  IMAGE_FILE_DLL                     = $2000;  // File is a DLL.
  IMAGE_FILE_UP_SYSTEM_ONLY          = $4000;  // File should only be run on a UP machine
  IMAGE_FILE_BYTES_REVERSED_HI       = $8000;  // Bytes of machine word are reversed.

//Section header characteristics
const
  IMAGE_SCN_CNT_CODE                 = $00000020;  // Section contains code.
  IMAGE_SCN_CNT_INITIALIZED_DATA     = $00000040;  // Section contains initialized data.
  IMAGE_SCN_CNT_UNINITIALIZED_DATA   = $00000080;  // Section contains uninitialized data.
  IMAGE_SCN_LNK_OTHER                = $00000100;  // Reserved.
  IMAGE_SCN_LNK_INFO                 = $00000200;  // Section contains comments or some other type of information.
  IMAGE_SCN_LNK_REMOVE               = $00000800;  // Section contents will not become part of image.
  IMAGE_SCN_LNK_COMDAT               = $00001000;  // Section contents comdat.
  IMAGE_SCN_GPREL                    = $00008000;  // Section content can be accessed relative to GP
  IMAGE_SCN_ALIGN_1BYTES             = $00100000;  //
  IMAGE_SCN_ALIGN_2BYTES             = $00200000;  //
  IMAGE_SCN_ALIGN_4BYTES             = $00300000;  //
  IMAGE_SCN_ALIGN_8BYTES             = $00400000;  //
  IMAGE_SCN_ALIGN_16BYTES            = $00500000;  // Default alignment if no others are specified.
  IMAGE_SCN_ALIGN_32BYTES            = $00600000;  //
  IMAGE_SCN_ALIGN_64BYTES            = $00700000;  //
  IMAGE_SCN_ALIGN_128BYTES           = $00800000;  //
  IMAGE_SCN_ALIGN_256BYTES           = $00900000;  //
  IMAGE_SCN_ALIGN_512BYTES           = $00A00000;  //
  IMAGE_SCN_ALIGN_1024BYTES          = $00B00000;  //
  IMAGE_SCN_ALIGN_2048BYTES          = $00C00000;  //
  IMAGE_SCN_ALIGN_4096BYTES          = $00D00000;  //
  IMAGE_SCN_ALIGN_8192BYTES          = $00E00000;  //
  IMAGE_SCN_LNK_NRELOC_OVFL          = $01000000;  // Section contains extended relocations.
  IMAGE_SCN_MEM_DISCARDABLE          = $02000000;  // Section can be discarded.
  IMAGE_SCN_MEM_NOT_CACHED           = $04000000;  // Section is not cachable.
  IMAGE_SCN_MEM_NOT_PAGED            = $08000000;  // Section is not pageable.
  IMAGE_SCN_MEM_SHARED               = $10000000;  // Section is shareable.
  IMAGE_SCN_MEM_EXECUTE              = $20000000;  // Section is executable.
  IMAGE_SCN_MEM_READ                 = $40000000;  // Section is readable.
  IMAGE_SCN_MEM_WRITE                = $80000000;  // Section is writeable.

const
// I386 relocation types.
  IMAGE_REL_I386_ABSOLUTE       = $0000;  // Reference is absolute, no relocation is necessary
  IMAGE_REL_I386_DIR16          = $0001;  // Direct 16-bit reference to the symbols virtual address
  IMAGE_REL_I386_REL16          = $0002;  // PC-relative 16-bit reference to the symbols virtual address
  IMAGE_REL_I386_DIR32          = $0006;  // Direct 32-bit reference to the symbols virtual address
  IMAGE_REL_I386_DIR32NB        = $0007;  // Direct 32-bit reference to the symbols virtual address, base not included
  IMAGE_REL_I386_SEG12          = $0009;  // Direct 16-bit reference to the segment-selector bits of a 32-bit virtual address
  IMAGE_REL_I386_SECTION        = $000A;
  IMAGE_REL_I386_SECREL         = $000B;
  IMAGE_REL_I386_TOKEN          = $000C;  // clr token
  IMAGE_REL_I386_SECREL7        = $000D;  // 7 bit offset from base of section containing target
  IMAGE_REL_I386_REL32          = $0014;  // PC-relative 32-bit reference to the symbols virtual address

// ARM relocation types.
  IMAGE_REL_ARM_ABSOLUTE        = $0000;  // No relocation required
  IMAGE_REL_ARM_ADDR32          = $0001;  // 32 bit address
  IMAGE_REL_ARM_ADDR32NB        = $0002;  // 32 bit address w/o image base
  IMAGE_REL_ARM_BRANCH24        = $0003;  // 24 bit offset << 2 & sign ext.
  IMAGE_REL_ARM_BRANCH11        = $0004;  // Thumb: 2 11 bit offsets
  IMAGE_REL_ARM_TOKEN           = $0005;  // clr token
  IMAGE_REL_ARM_GPREL12         = $0006;  // GP-relative addressing (ARM)
  IMAGE_REL_ARM_GPREL7          = $0007;  // GP-relative addressing (Thumb)
  IMAGE_REL_ARM_BLX24           = $0008;
  IMAGE_REL_ARM_BLX11           = $0009;
  IMAGE_REL_ARM_SECTION         = $000E;  // Section table index
  IMAGE_REL_ARM_SECREL          = $000F;  // Offset within section

// x64 relocation types.
  IMAGE_REL_AMD64_ABSOLUTE      = $0000;  // Reference is absolute, no relocation is necessary
  IMAGE_REL_AMD64_ADDR64        = $0001;  // 64-bit address (VA).
  IMAGE_REL_AMD64_ADDR32        = $0002;  // 32-bit address (VA).
  IMAGE_REL_AMD64_ADDR32NB      = $0003;  // 32-bit address w/o image base (RVA).
  IMAGE_REL_AMD64_REL32         = $0004;  // 32-bit relative address from byte following reloc
  IMAGE_REL_AMD64_REL32_1       = $0005;  // 32-bit relative address from byte distance 1 from reloc
  IMAGE_REL_AMD64_REL32_2       = $0006;  // 32-bit relative address from byte distance 2 from reloc
  IMAGE_REL_AMD64_REL32_3       = $0007;  // 32-bit relative address from byte distance 3 from reloc
  IMAGE_REL_AMD64_REL32_4       = $0008;  // 32-bit relative address from byte distance 4 from reloc
  IMAGE_REL_AMD64_REL32_5       = $0009;  // 32-bit relative address from byte distance 5 from reloc
  IMAGE_REL_AMD64_SECTION       = $000A;  // Section index
  IMAGE_REL_AMD64_SECREL        = $000B;  // 32 bit offset from base of section containing target
  IMAGE_REL_AMD64_SECREL7       = $000C;  // 7 bit unsigned offset from base of section containing target
  IMAGE_REL_AMD64_TOKEN         = $000D;  // 32 bit metadata token
  IMAGE_REL_AMD64_SREL32        = $000E;  // 32 bit signed span-dependent value emitted into object
  IMAGE_REL_AMD64_PAIR          = $000F;
  IMAGE_REL_AMD64_SSPAN32       = $0010;  // 32 bit signed span-dependent value applied at link time

// aarch64 relocation types

  IMAGE_REL_ARM64_ABSOLUTE      = $0000;  // The relocation is ignored.
  IMAGE_REL_ARM64_ADDR32        = $0001;  // The 32-bit VA of the target.
  IMAGE_REL_ARM64_ADDR32NB      = $0002;  // The 32-bit RVA of the target.
  IMAGE_REL_ARM64_BRANCH26      = $0003;  // The 26-bit relative displacement to the target, for B and BL instructions.
  IMAGE_REL_ARM64_PAGEBASE_REL21= $0004;  // The page base of the target, for ADRP instruction.
  IMAGE_REL_ARM64_REL21         = $0005;  // The 12-bit relative displacement to the target, for instruction ADR
  IMAGE_REL_ARM64_PAGEOFFSET_12A= $0006;  // The 12-bit page offset of the target, for instructions ADD/ADDS (immediate) with zero shift.
  IMAGE_REL_ARM64_PAGEOFFSET_12L= $0007;  // The 12-bit page offset of the target, for instruction LDR (indexed, unsigned immediate).
  IMAGE_REL_ARM64_SECREL        = $0008;  // The 32-bit offset of the target from the beginning of its section. This is used to support debugging information and static thread local storage.
  IMAGE_REL_ARM64_SECREL_LOW12A = $0009;  // Bit 0:11 of section offset of the target, for instructions ADD/ADDS (immediate) with zero shift.
  IMAGE_REL_ARM64_SECREL_HIGH12A= $000A;  // Bit 12:23 of section offset of the target, for instructions ADD/ADDS (immediate) with zero shift.
  IMAGE_REL_ARM64_SECREL_LOW12L = $000B;  // Bit 0:11 of section offset of the target, for instruction LDR (indexed, unsigned immediate).
  IMAGE_REL_ARM64_TOKEN         = $000C;  // CLR token.
  IMAGE_REL_ARM64_SECTION       = $000D;  // The 16-bit section index of the section that contains the target. This is used to support debugging information.
  IMAGE_REL_ARM64_ADDR64        = $000E;  // The 64-bit VA of the relocation target.
  IMAGE_REL_ARM64_BRANCH19      = $000F;  // The 19-bit offset to the relocation target, for conditional B instruction.
  IMAGE_REL_ARM64_BRANCH14      = $0010;  // The 14-bit offset to the relocation target, for instructions TBZ and TBNZ.
  IMAGE_REL_ARM64_REL32         = $0011;  // The 32-bit relative address from the byte following the relocation.

// AIX PPC32/PPC64 relocation types.

  IMAGE_REL_PPC_POS             = $1F00;  // A(sym) Positive Relocation


const
  // storage classes
  IMAGE_SYM_CLASS_STATIC        = $0003;

  // XCOFF
  IMAGE_SYM_CLASS_EXT           = 2;
  IMAGE_SYM_CLASS_HIDEXT        = 107;

  XTY_ER = 0;
  XTY_SD = 1;
  XTY_LD = 2;
  XTY_CM = 3;

  XMC_RW = 5;

  // section types
  STYP_DATA = $40;
  STYP_BSS = $80;

const
  XCoffRsrcSectName    = 'fpc.resources';
  XCoffHandlesSectName = 'fpc.reshandles';


implementation

end.
