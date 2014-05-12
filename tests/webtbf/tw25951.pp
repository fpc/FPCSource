{ %fail }
// Mainly used for compiler discovery and turning on global compiler settings.
// This simplifies vwrdefines.inc maintenance, since vwrdefines.inc is per application, and this one is global

// delphi 2009 related

{$if compilerversion>=20}  //D2009
   {$define Delphiunicode}
   {$define has_pointermath}
{$ifend}
{$if compilerversion>=21} // D2010
   {$define has_debugthread}
   {$define has_bigrtti}
{$ifend}
{$if compilerversion>=22} // DXE
   {$define has_keyboard}
{$ifend}
{$if compilerversion>=23} // DXE2..DXE4 (5?)
   {$define has_globalformatsettings}
   {$define has_extendedTrect} // ?
{$ifend}

 {$ifdef has_pointermath}
   {$pointermath on} // aka FPC emulation
 {$endif}

// most code nowadays probably won't compile under D2009. And even D2009 might be difficult
// due to increased generics usage in 20.
 {$ifndef ver150}{$define inlineon}{$endif} // if not D7, then can use inline. Nicer than version.
 {$ifndef Delphiunicode}
   {$define vstoldstring}
 {$else}
   {$define newvst}
   {$define newcomport}
{$endif}

begin
end.
