{ **********************************************************************
  Define one of the following versions. Default 2.0 will be taken if none
  is defined  
  **********************************************************************}

{ $define apache1_3}
{ $define apache2_0}
{ $define apache2_2}

{ *********************************************************************
  No need to edit below this line.
  *********************************************************************}

{ *********************************************************************
  The mode must be objfpc on this unit because the unix code uses
  some extensions introduced on Free Pascal
  *********************************************************************}

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

{$IFNDEF FPC}
  {$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE WINDOWS}
{$ENDIF}

{ Set at least one define. Default to 2.0 for now}

{$ifndef apache1_3}
{$ifndef apache2_0}
{$ifndef apache2_2}

{ Default 2.0 }
{$define apache2_0}

{$endif}
{$endif}
{$endif}