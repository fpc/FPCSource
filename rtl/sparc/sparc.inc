procedure fpc_cpuinit;
begin
end;

{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;{assembler;}
begin{asm}
    {$warning FIX ME!}
    //    !!!!!!! depends on ABI !!!!!!!!
end;
{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;{assembler;}
begin{asm}
   {$warning FIX ME!}
    //     !!!!!!! depends on ABI !!!!!!!!
end;
{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;{assembler;}
begin{asm}
    {$warning FIX ME!}
   //     !!!!!!! depends on ABI !!!!!!!!
end;
{$define FPC_SYSTEM_HAS_SPTR}
function Sptr:Pointer;{assembler;}
begin{asm}
   {$warning FIX ME!}
end;
{$define FPC_SYSTEM_HAS_FPC_HELP_FAIL_CLASS}
function fpc_help_fail_class(_vmt:pointer;_self:pointer):pointer;{assembler;}[public,alias:'FPC_HELP_FAIL_CLASS']; {$ifdef hascompilerproc} compilerproc; {$endif}
{ a non zero class must allways be disposed
  VMT is allways at pos 0 }
begin{asm}
{$warning FIX ME!}
// !!!!!!!!!!!
end;
{
  $Log$
  Revision 1.5  2004-01-02 17:22:14  jonas
    + fpc_cpuinit procedure to allow cpu/fpu initialisation before any unit
      initialises
    + fpu exceptions for invalid operations and division by zero enabled for
      ppc

  Revision 1.4  2003/12/04 21:42:07  peter
    * register calling updates

  Revision 1.3  2003/03/17 14:30:11  peter
    * changed address parameter/return values to pointer instead
      of longint

  Revision 1.2  2003/02/05 21:48:34  mazen
  * fixing run time errors related to unimplemented abstract methods in CG
  + giving empty emplementations for some RTL functions

  Revision 1.1  2002/11/16 20:10:31  florian
    + sparc specific rtl skeleton added
}
