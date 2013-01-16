{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Constants used by ELF resource reader and writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit elfconsts;

{$MODE OBJFPC}

interface

type
  TElfMachineType = (emtnone, emtsparc, emti386, emtm68k, emtppc, emtppc64,
                     emtarm, emtarmeb, emtia64, emtx86_64, emtalpha,
                     emtmips, emtmipsel);
const
  ELFMAGIC     = chr($7f)+'ELF';

  //elf class
  ELFCLASSNONE = 0;
  ELFCLASS32   = 1;
  ELFCLASS64   = 2;

  //byte order
  ELFDATANONE  = 0;
  ELFDATA2LSB  = 1;
  ELFDATA2MSB  = 2;

  //elf version
  EV_NONE      = 0;
  EV_CURRENT   = 1;

  //OS ABI
  ELFOSABI_NONE    =  0;  // UNIX System V ABI
  ELFOSABI_LINUX   =  3;
  ELFOSABI_FREEBSD =  9;
  ELFOSABI_ARM     = 97;

  //object file type
  ET_NONE      = 0;
  ET_REL       = 1;
  ET_EXEC      = 2;
  ET_DYN       = 3;
  ET_CORE      = 4;
  ET_LOOS      = $fe00; //os-specific
  ET_HIOS      = $feff;
  ET_LOPROC    = $ff00; //processor-specific
  ET_HIPROC    = $ffff;

  //machine type
  EM_NONE        =  0;
  EM_SPARC       =  2;
  EM_386         =  3;
  EM_68K         =  4;
  EM_MIPS        =  8; // GNU readelf returns machine name: "MIPS R3000"
  EM_MIPS_RS3_LE = 10; // GNU readelf returns machine name: "MIPS R4000 big endian"!
  EM_PPC         = 20;
  EM_PPC64       = 21;
  EM_ARM         = 40;
//  EM_OLD_ALPHA       = 41;
  EM_IA_64       = 50;
  EM_MIPS_X      = 51; // GNU readelf returns machine name "Stanford MIPS-X"
  EM_X86_64      = 62;
  EM_ALPHA       = $9026; //unofficial, but used by gnu toolchain
  
  //machine-specific flags
  EF_IA_64_ABI64 = $10;  //wow, this is really a 64-bit object file!

  //section type
  SHT_NULL     =  0;
  SHT_PROGBITS =  1;
  SHT_SYMTAB   =  2;
  SHT_STRTAB   =  3;
  SHT_RELA     =  4;
  SHT_HASH     =  5;
  SHT_DYNAMIC  =  6;
  SHT_NOTE     =  7;
  SHT_NOBITS   =  8;
  SHT_REL      =  9;
  SHT_SHLIB    = 10;
  SHT_DYNSYM   = 11;
  SHT_LOPROC   = $70000000;
  SHT_HIPROC   = $7fffffff;
  SHT_LOOS     = $80000000;
  SHT_HIOS     = $ffffffff;

  //section attribute flags
  SHF_WRITE     =         1;
  SHF_ALLOC     =         2;
  SHF_EXECINSTR =         4;
  SHF_MASKOS    = $0f000000;
  SHF_MASKPROC  = $f0000000;
  
  //symbol bindings
  STB_LOCAL  =  0;
  STB_GLOBAL =  1;
  STB_WEAK   =  2;
  STB_LOOS   = 10;
  STB_HIOS   = 12;
  STB_LOPROC = 13;
  STB_HIPROC = 15;
  
  //symbol types
  STT_NOTYPE         =  0;
  STT_OBJECT         =  1;
  STT_FUNC           =  2;
  STT_SECTION        =  3;
  STT_FILE           =  4;
  STT_COMMON         =  5;
  STT_TLS            =  6;
  STT_LOOS           = 10;
  STT_HIOS           = 12;
  STT_LOPROC         = 13;
  STT_SPARC_REGISTER = 13;
  STT_HIPROC         = 15;

  //direct, natural-size relocation types
  R_386_32        =   1;
  R_x86_64_64     =   1;
  R_PPC_ADDR32    =   1;
  R_PPC64_ADDR64  =  38;
  R_ARM_ABS32     =   2;
  R_68K_32        =   1;
  R_SPARC_32      =   3;
  R_ALPHA_REFQUAD =   2;
  R_IA64_DIR64LSB = $27;
  R_MIPS_32       =   2;


  //fpc resource constants
  RsrcSectName    = 'fpc.resources';
  HandlesSectName = 'fpc.reshandles';
  
  RSRCSECT_IDX = 1;
  HANDLESECT_IDX = 2;

implementation

end.
