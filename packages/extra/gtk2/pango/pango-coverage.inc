// included by pango.pp
{$IFDEF read_interface_types}
   PPangoCoverageLevel = ^TPangoCoverageLevel;
   TPangoCoverageLevel = (
     PANGO_COVERAGE_NONE,
     PANGO_COVERAGE_FALLBACK,
     PANGO_COVERAGE_APPROXIMATE,
     PANGO_COVERAGE_EXACT
   );

   PPangoBlockInfo = ^TPangoBlockInfo;
   TPangoBlockInfo = record
       data: Pguchar;
       level: TPangoCoverageLevel;     // Used if data == NULL
     end;

   PPangoCoverage = ^TPangoCoverage;
   TPangoCoverage = record
       ref_count: guint;
       n_blocks: longint;
       data_size: longint;
       blocks: PPangoBlockInfo;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}
function pango_coverage_new:PPangoCoverage; cdecl; external pangolib;
function pango_coverage_ref(coverage:PPangoCoverage):PPangoCoverage; cdecl; external pangolib;
procedure pango_coverage_unref(coverage:PPangoCoverage); cdecl; external pangolib;
function pango_coverage_copy(coverage:PPangoCoverage):PPangoCoverage; cdecl; external pangolib;
function pango_coverage_get(coverage:PPangoCoverage; index:longint):TPangoCoverageLevel; cdecl; external pangolib;
procedure pango_coverage_set(coverage:PPangoCoverage; index:longint; level:TPangoCoverageLevel); cdecl; external pangolib;
procedure pango_coverage_max(coverage:PPangoCoverage; other:PPangoCoverage); cdecl; external pangolib;
procedure pango_coverage_to_bytes(coverage:PPangoCoverage; bytes:PPguchar; n_bytes:Plongint); cdecl; external pangolib;
function pango_coverage_from_bytes(bytes:Pguchar; n_bytes:longint):PPangoCoverage; cdecl; external pangolib;
{$endif}


