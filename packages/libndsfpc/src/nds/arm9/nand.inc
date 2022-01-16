{$ifdef NDS_INTERFACE}
function nand_ReadSectors(sector: sec_t; numSectors: sec_t; buffer: pointer): cbool; cdecl; external;
function nand_WriteSectors(sector: sec_t; numSectors: sec_t; buffer: pointer): cbool; cdecl; external;
function nand_GetSize(): ssize_t; cdecl; external;

{$endif NDS_INTERFACE}
