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
{$endif i386}
{$ifdef x86_64}
  {$define x86}
{$endif x86_64}
{$ifdef ppc}
  {$define oldset}
{$endif}

{
  $Log$
  Revision 1.4  2002-07-23 12:34:29  daniel
  * Readded old set code. To use it define 'oldset'. Activated by default
    for ppc.

  Revision 1.3  2002/07/04 18:56:50  florian
    + log added

}
