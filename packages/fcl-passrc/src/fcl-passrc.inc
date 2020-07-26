{$mode objfpc}{$H+}
{$inline on}

{$ifdef fpc}
  {$define UsePChar}
  {$define HasInt64}
{$endif}

{$IF FPC_FULLVERSION>30100}
  {$warn 6058 off} // cannot inline
{$ENDIF}

