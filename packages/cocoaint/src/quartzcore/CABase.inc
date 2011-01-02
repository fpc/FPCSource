{ Parsed from Quartzcore.framework CABase.h }
{ Version: 2.1.4 - Sun Jan 2 15:08:20 CET 2011 }


{$ifdef TYPES}
{$ifndef CABASE_PAS_T}
{$define CABASE_PAS_T}

{ Defines }
const
  CA_WARN_DEPRECATED = 1;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CABASE_PAS_R}
{$define CABASE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CABASE_PAS_F}
{$define CABASE_PAS_F}

{ Functions }
function CACurrentMediaTime: CFTimeInterval; cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CABASE_PAS_S}
{$define CABASE_PAS_S}

{$endif}
{$endif}
