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
  {$define USEEXCEPT}
{$endif}

{$ifdef i386}
  {$define x86}
{$else}
  {$define oldset}
{$endif i386}
{$ifdef x86_64}
  {$define x86}
{$endif x86_64}
{
  $Log$
  Revision 1.9  2002-09-07 15:25:02  peter
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
