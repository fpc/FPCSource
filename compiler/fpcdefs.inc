{$ifdef FPC}
  {$ifdef DELPHI}
    {$mode delphi}
    {$asmmode intel}
  {$else}
    {$mode objfpc}
    {$H-}
    {$goto on}

    { This reduces the memory requirements a lot }
    {$PACKENUM 1}

    { We don't use exceptions, so turn off the implicit
      exceptions in the constructors }
    {$ifndef VER1_0}
      {$IMPLICITEXCEPTIONS OFF}
    {$endif VER1_0}

    {$define FPCPROCVAR}
    {$ifdef I386}
      {$define USEEXCEPT}
    {$endif}
  {$endif}
{$endif}

{$ifdef DELPHI}
  {$H-}
  {$J+}

  {$Z1}

  {$undef FPCPROCVAR}
{$endif}


{ assume a processor with flags }
{$define cpuflags}

{$ifdef i386}
  {$ifdef delphi}
  {$define oldset}
  {$endif}
  {$define x86}
  {$define cpuextended}
{$else}
  {$define oldset}
{$endif i386}

{$ifdef x86_64}
  {$define x86}
  {$define cpu64bit}
  {$define cpuextended}
  {$define callparatemp}
{$endif x86_64}

{$ifdef alpha}
  {$define cpu64bit}
  {$undef cpuflags}
  {$define callparatemp}
{$endif alpha}

{$ifdef sparc}
  {$define usetempparaloc}
  { define callparatemp}
{$endif sparc}

{$ifdef powerpc}
  {$define callparatemp}
{$endif powerpc}

{$ifdef arm}
  {$define callparatemp}
{$endif arm}

{ FPU Emulator support }
{$ifdef m68k}
  {$define cpufpemu}
{$endif m68k}

{ Optimizer is x86 only }
{$ifndef i386}
  {$define noopt}
{$endif i386}

{
  $Log$
  Revision 1.22  2003-08-11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.21  2003/07/21 11:52:57  florian
    * very basic stuff for the arm

  Revision 1.20  2003/05/24 21:12:57  florian
    * if something doesn't work with callparatemp, the define callparatemp
      should be used because other processors with reigster calling conventions
      depend on this as well

  Revision 1.19  2003/05/09 17:47:02  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.18  2003/04/30 09:42:42  florian
    + first changes to make self a hidden parameter

  Revision 1.17  2003/04/24 22:29:57  florian
    * fixed a lot of PowerPC related stuff

  Revision 1.16  2003/04/23 13:47:41  peter
    * optimizer is x86

  Revision 1.15  2003/04/23 12:35:34  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.14  2002/12/06 16:56:57  peter
    * only compile cs_fp_emulation support when cpufpuemu is defined
    * define cpufpuemu for m68k only

  Revision 1.13  2002/10/16 19:01:43  peter
    + $IMPLICITEXCEPTIONS switch to turn on/off generation of the
      implicit exception frames for procedures with initialized variables
      and for constructors. The default is on for compatibility

  Revision 1.12  2002/10/05 12:43:24  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.11  2002/09/30 07:00:45  florian
    * fixes to common code to get the alpha compiler compiled applied

  Revision 1.10  2002/09/29 23:19:05  florian
    + added define 64bit

  Revision 1.9  2002/09/07 15:25:02  peter
    * old logs removed and tabs fixed

  Revision 1.8  2002/08/30 13:42:29  mazen
  - undefining win32 when SPARC defined removed, no more needed.


  Revision 1.6  2002/08/19 18:17:48  carl
    + optimize64_op_const_reg implemented (optimizes 64-bit constant opcodes)
    * more fixes to m68k for 64-bit operations

  Revision 1.5  2002/08/15 15:11:53  carl
    * oldset define is now correct for all cpu's except i386
    * correct compilation problems because of the above

  Revision 1.4  2002/07/23 12:34:29  daniel
  * Readded old set code. To use it define 'oldset'. Activated by default
    for ppc.

  Revision 1.3  2002/07/04 18:56:50  florian
    + log added
}
