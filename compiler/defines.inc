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

{
  $Log$
  Revision 1.8  2002-07-04 18:57:18  florian
    + log added

}
