unit multiboot;

interface

type
 PMBModule = ^TMBModule;
 TMBModule = packed record
  mod_start,
  mod_end: PtrUInt;
  name: pchar;
  reserved: DWord;
 end;

 PMemoryMap = ^TMemoryMap;
 TMemoryMap = packed record
  size,
  base_lower,
  base_upper,
  length_lower,
  length_upper,
  mtype: DWord;
 end;

 PElfSectionHeaderTable = ^TElfSectionHeaderTable;
 TElfSectionHeaderTable = packed record
  num,
  size,
  addr,
  shndx: DWord;
 end;

 PMultibootinfo = ^TMultibootinfo;
 TMultiBootInfo = packed record
  Flags,
  MemLower, MemUpper,
  BootDevice,
  CmdLine,
  ModuleCount: DWord;
  ModuleAddress: PMBModule;
  ElfSection: TElfSectionHeaderTable;
  MMapLength: longint;
  MMapAddress: PMemoryMap;
 end;

var MultibootTable: PMultibootinfo;

implementation

{$asmmode att}

procedure MultibootHeader; nostackframe; assembler;
const
 MULTIBOOT_PAGE_ALIGN = 1; // Align modules on a 4kB boundary
 MULTIBOOT_MEMORY_INFO = 2; // Memory info table is valid
 MULTIBOOT_AOUT_KLUDGE = 1 shl 16; // A.OUT kludge follows the header
 
 MultibootSig = $1BADB002;
 MultibootFlags = MULTIBOOT_MEMORY_INFO;
 MultibootChksum = -(MultibootSig + MultibootFlags);
asm
   .init
   .long MultibootSig
   .long MultibootFlags
   .long MultibootChksum
   .text
end;

procedure haltproc; assembler; nostackframe; public name '_haltproc';
asm
.Lloop:
   jmp .Lloop
end;

procedure PASCALMAIN; external name 'PASCALMAIN';

procedure Start; assembler; nostackframe; public name '_START';
asm
   cli
   
   movl %ebx, MultibootTable
   
   jmp PASCALMAIN
end;

end.