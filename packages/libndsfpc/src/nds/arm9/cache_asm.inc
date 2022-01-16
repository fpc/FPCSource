{$ifdef NDS_INTERFACE}
const
  PAGE_4K		= (%01011 shl 1);
  PAGE_8K		= (%01100 shl 1);
  PAGE_16K	= (%01101 shl 1);
  PAGE_32K	= (%01110 shl 1);
  PAGE_64K	= (%01111 shl 1);
  PAGE_128K	= (%10000 shl 1);
  PAGE_256K	= (%10001 shl 1);
  PAGE_512K	= (%10010 shl 1);
  PAGE_1M		= (%10011 shl 1);
  PAGE_2M		= (%10100 shl 1);
  PAGE_4M		= (%10101 shl 1);
  PAGE_8M		= (%10110 shl 1);
  PAGE_16M	= (%10111 shl 1);
  PAGE_32M	= (%11000 shl 1);
  PAGE_64M	= (%11001 shl 1);
  PAGE_128M	= (%11010 shl 1);
  PAGE_256M	= (%11011 shl 1);
  PAGE_512M	= (%11100 shl 1);
  PAGE_1G		= (%11101 shl 1);
  PAGE_2G		= (%11110 shl 1);
  PAGE_4G		= (%11111 shl 1);

  ITCM_LOAD	= (1 shl 19);
  ITCM_ENABLE	= (1 shl 18);
  DTCM_LOAD	= (1 shl 17);
  DTCM_ENABLE	= (1 shl 16);
  DISABLE_TBIT	= (1 shl 15);
  ROUND_ROBIN	= (1 shl 14);
  ALT_VECTORS	= (1 shl 13);
  ICACHE_ENABLE	= (1 shl 12);
  BIG_ENDIAN	= (1 shl 7);
  DCACHE_ENABLE	= (1 shl 2);
  PROTECT_ENABLE	= (1 shl 0);

{$endif NDS_INTERFACE}
