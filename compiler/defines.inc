{$ifdef FPC}
  {$mode objfpc}
  {$H-}
  {$goto on}

  { Packing of enumerated types to save space }
  {$ifdef FPC}
    {$ifdef PACKENUMFIXED}
      {$PACKENUM 1}
    {$endif}
  {$endif}

  {$define FPCPROCVAR}
  {$ifdef I386}
    {$define USEEXCEPT}
  {$endif}
{$endif}

{$ifdef DELPHI}
  {$H-}

  { Packing of enumerated types to save space }
  {$Z1}

  {$undef FPCPROCVAR}
  {$define USEEXCEPT}
{$endif}
