unit Exe;

  interface

    type
       exe = record
          eid : word;
          elast : word;
          epagsiz : word;
          erelcnt : word;
          ehdrsiz : word;
          eminfre : word;
          emaxfre : word;
          eiSS : word;
          eiSP : word;
          enegsum : word;
          eiIP : word;
          eiCS : word;
          ereloff : word;
          eovlnum : word;
          ever : word;
          dumy : word;
          ebb : word;
          dumy2 : array[0..6] of word;
       end;

    const
       EXEID = $5A4D;

{ Declarations specific for LX-type executables follow }
    const
       BITPERword = 16;
       BITPERBYTE = 8;
       OBJPAGELEN = 4096;
       E32MAGIC = $584C;
       E32RESBYTES1 = 0;
       E32RESBYTES2 = 0;
       E32RESBYTES3 = 24;
       E32LEBO = $00;
       E32BEBO = $01;
       E32LEWO = $00;
       E32BEWO = $01;
       E32LEVEL = 0;
       E32CPU286 = $001;
       E32CPU386 = $002;
       E32CPU486 = $003;

    type
       e32_exe = record
          e32_magic : array[0..1] of byte;
          e32_border : byte;
          e32_worder : byte;
          e32_level : longint;
          e32_cpu : word;
          e32_os : word;
          e32_ver : longint;
          e32_mflags : longint;
          e32_mpages : longint;
          e32_startobj : longint;
          e32_eip : longint;
          e32_stackobj : longint;
          e32_esp : longint;
          e32_pagesize : longint;
          e32_pageshift : longint;
          e32_fixupsize : longint;
          e32_fixupsum : longint;
          e32_ldrsize : longint;
          e32_ldrsum : longint;
          e32_objtab : longint;
          e32_objcnt : longint;
          e32_objmap : longint;
          e32_itermap : longint;
          e32_rsrctab : longint;
          e32_rsrccnt : longint;
          e32_restab : longint;
          e32_enttab : longint;
          e32_dirtab : longint;
          e32_dircnt : longint;
          e32_fpagetab : longint;
          e32_frectab : longint;
          e32_impmod : longint;
          e32_impmodcnt : longint;
          e32_impproc : longint;
          e32_pagesum : longint;
          e32_datapage : longint;
          e32_preload : longint;
          e32_nrestab : longint;
          e32_cbnrestab : longint;
          e32_nressum : longint;
          e32_autodata : longint;
          e32_debuginfo : longint;
          e32_debuglen : longint;
          e32_instpreload : longint;
          e32_instdemand : longint;
          e32_heapsize : longint;
          e32_res3 : array[0..E32RESBYTES3-1] of byte;
       end;

    const
       E32NOTP = $8000;
       E32NOLOAD = $2000;
       E32PMAPI = $0300;
       E32PMW = $0200;
       E32NOPMW = $0100;
       E32NOEXTFIX = $0020;
       E32NOINTFIX = $0010;
       E32LIBINIT = $0004;
       E32LIBTERM = $40000000;
       E32APPMASK = $0700;
       E32PROTDLL = $10000;
       E32DEVICE = $20000;
       E32MODEXE = $00000;
       E32MODDLL = $08000;
       E32MODPROTDLL = $18000;
       E32MODPDEV = $20000;
       E32MODVDEV = $28000;
       E32MODMASK = $38000;
       RINTSIZE16 = 8;
       RINTSIZE32 = 10;
       RORDSIZE = 8;
       RNAMSIZE16 = 8;
       RNAMSIZE32 = 10;
       RADDSIZE16 = 10;
       RADDSIZE32 = 12;
       NRSTYP = $0f;
       NRSBYT = $00;
       NRSSEG = $02;
       NRSPTR = $03;
       NRSOFF = $05;
       NRPTR48 = $06;
       NROFF32 = $07;
       NRSOFF32 = $08;
       NRSRCMASK = $0f;
       NRALIAS = $10;
       NRCHAIN = $20;
       NRRTYP = $03;
       NRRINT = $00;
       NRRORD = $01;
       NRRNAM = $02;
       NRADD = $04;
       NRRENT = $03;
       NR32BITOFF = $10;
       NR32BITADD = $20;
       NR16OBJMOD = $40;
       NR8BITORD = $80;
       PAGEPERDIR = 62;
       LG2DIR = 7;

    type
       OBJPAGEDIR = record
          next : longint;
          ht : array[0..PAGEPERDIR-1] of word;
       end;

       e32_obj = record
          o32_size : longint;
          o32_base : longint;
          o32_flags : longint;
          o32_pagemap : longint;
          o32_mapsize : longint;
          o32_reserved : longint;
       end;

    const
       OBJREAD = $0001;
       OBJWRITE = $0002;
       OBJRSRC = $0008;
       OBJINVALID = $0080;
       LNKNONPERM = $0600;
       OBJNONPERM = $0000;
       OBJPERM = $0100;
       OBJRESIDENT = $0200;
       OBJCONTIG = $0300;
       OBJDYNAMIC = $0400;
       OBJTYPEMASK = $0700;
       OBJALIAS16 = $1000;
       OBJBIGDEF = $2000;
       OBJIOPL = $8000;
       NSDISCARD = $0010;
       NSMOVE = NSDISCARD;
       NSSHARED = $0020;
       NSPRELOAD = $0040;
       NSEXRD = $0004;
       NSCONFORM = $4000;

    type
       o32_map = record
          o32_pagedataoffset : longint;
          o32_pagesize : word;
          o32_pageflags : word;
       end;

    const
       VALID = $0000;
       ITERDATA = $0001;
       INVALID = $0002;
       ZEROED = $0003;
       RANGE = $0004;
       ITERDATA2 = $0005;

    type
       rsrc32 = record
          _type : word;
          name : word;
          cb : longint;
          obj : word;
          offset : longint;
       end;

{$PACKRECORDS 1}

       LX_Iter = record
          LX_nIter : word;
          LX_nBytes : word;
          LX_Iterdata : byte;
       end;

       b32_bundle = record
          b32_cnt : byte;
          b32_type : byte;
          b32_obj : word;
       end;

{$PACKRECORDS NORMAL}
    const
       FIXENT16 = 3;
       FIXENT32 = 5;
       GATEENT16 = 5;
       FWDENT = 7;
       EMPTY = $00;
       ENTRY16 = $01;
       GATE16 = $02;
       ENTRY32 = $03;
       ENTRYFWD = $04;
       TYPEINFO = $80;
       E32EXPORT = $01;
       E32SHARED = $02;
       E32PARAMS = $f8;
       FWD_ORDINAL = $01;

  implementation

end.
