{$ifdef FPC}
  {$ifdef DELPHI}
    {$mode delphi}
    {$asmmode intel}
  {$else}
    {$mode objfpc}
    {$H-}
    {$goto on}

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
