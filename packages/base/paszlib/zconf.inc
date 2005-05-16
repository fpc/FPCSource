{ -------------------------------------------------------------------- }

{$DEFINE MAX_MATCH_IS_258}

{ Compile with -DMAXSEG_64K if the alloc function cannot allocate more
  than 64k bytes at a time (needed on systems with 16-bit int). }

{- $DEFINE MAXSEG_64K}
{$IFNDEF WIN32}
  {$DEFINE UNALIGNED_OK}  { requires SizeOf(ush) = 2 ! }
{$ENDIF}

{$UNDEF DYNAMIC_CRC_TABLE}
{$UNDEF FASTEST}
{$define patch112}        { apply patch from the zlib home page }
{ -------------------------------------------------------------------- }
{$IFDEF FPC}
 {$DEFINE Use32}
 {$UNDEF DPMI}
 {$UNDEF MSDOS}
 {$UNDEF UNALIGNED_OK}  { requires SizeOf(ush) = 2 ! }
 {$UNDEF MAXSEG_64K}
{$ENDIF}

{
  $Log: zconf.inc,v $
  Revision 1.3  2005/02/14 17:13:19  peter
    * truncate log

}
