unit NEWEXE;

  interface

    const
       EMAGIC = $5A4D;
       ENEWEXE = 8*5;
       ENEWHDR = $003C;
       ERESWDS = $0010;
       ERES1WDS = $0004;
       ERES2WDS = $000A;
       ECP = $0004;
       ECBLP = $0002;
       EMINALLOC = $000A;

    type
       exe_hdr = record
          e_magic : word;
          e_cblp : word;
          e_cp : word;
          e_crlc : word;
          e_cparhdr : word;
          e_minalloc : word;
          e_maxalloc : word;
          e_ss : word;
          e_sp : word;
          e_csum : word;
          e_ip : word;
          e_cs : word;
          e_lfarlc : word;
          e_ovno : word;
          e_res : array[0..ERES1WDS-1] of word;
          e_oemid : word;
          e_oeminfo : word;
          e_res2 : array[0..ERES2WDS-1] of word;
          e_lfanew : longint;
       end;

    const
       NEMAGIC = $454E;
       NERESBYTES = 8;
       NECRC = 8;

    type
       new_exe = record
          ne_magic : word;
          ne_ver : byte;
          ne_rev : byte;
          ne_enttab : word;
          ne_cbenttab : word;
          ne_crc : longint;
          ne_flags : word;
          ne_autodata : word;
          ne_heap : word;
          ne_stack : word;
          ne_csip : longint;
          ne_sssp : longint;
          ne_cseg : word;
          ne_cmod : word;
          ne_cbnrestab : word;
          ne_segtab : word;
          ne_rsrctab : word;
          ne_restab : word;
          ne_modtab : word;
          ne_imptab : word;
          ne_nrestab : longint;
          ne_cmovent : word;
          ne_align : word;
          ne_cres : word;
          ne_exetyp : byte;
          ne_flagsothers : byte;
          ne_res : array[0..NERESBYTES-1] of char;
       end;

    const
       NE_UNKNOWN = $0;
       NE_OS2 = $1;
       NE_WINDOWS = $2;
       NE_DOS4 = $3;
       NE_DEV386 = $4;
       NENOTP = $8000;
       NEIERR = $2000;
       NEBOUND = $0800;
       NEAPPTYP = $0700;
       NENOTWINCOMPAT = $0100;
       NEWINCOMPAT = $0200;
       NEWINAPI = $0300;
       NEFLTP = $0080;
       NEI386 = $0040;
       NEI286 = $0020;
       NEI086 = $0010;
       NEPROT = $0008;
       NEPPLI = $0004;
       NEINST = $0002;
       NESOLO = $0001;
       NElongintNAMES = $01;
       NEWINISPROT = $02;
       NEWINGETPROPFON = $04;
       NEWLOAPPL = $80;

    type
       new_seg = record
          ns_sector : word;
          ns_cbseg : word;
          ns_flags : word;
          ns_minalloc : word;
       end;

    const
       NSTYPE = $0007;
       NSCODE = $0000;
       NSDATA = $0001;
       NSITER = $0008;
       NSMOVE = $0010;
       NSSHARED = $0020;
       NSPRELOAD = $0040;
       NSEXRD = $0080;
       NSRELOC = $0100;
       NSCONFORM = $0200;
       NSEXPDOWN = $0200;
       NSDPL = $0C00;
       SHIFTDPL = 10;
       NSDISCARD = $1000;
       NS32BIT = $2000;
       NSHUGE = $4000;
       NSGDT = $8000;
       NSPURE = NSSHARED;
       NSALIGN = 9;
       NSLOADED = $0004;

    type
       new_rlcinfo = record
          nr_nreloc : word;
       end;

{$PACKRECORDS NORMAL}
    const
       NRSTYP = $0f;
       NRSBYT = $00;
       NRSSEG = $02;
       NRSPTR = $03;
       NRSOFF = $05;
       NRPTR48 = $06;
       NROFF32 = $07;
       NRSOFF32 = $08;
       NRADD = $04;
       NRRTYP = $03;
       NRRINT = $00;
       NRRORD = $01;
       NRRNAM = $02;
       NRROSF = $03;

    type
       rsrc_string = record
          rs_len : char;
          rs_string : array[0..1-1] of char;
       end;

       rsrc_typeinfo = record
          rt_id : word;
          rt_nres : word;
          rt_proc : longint;
       end;

       rsrc_nameinfo = record
          rn_offset : word;
          rn_length : word;
          rn_flags : word;
          rn_id : word;
          rn_handle : word;
          rn_usage : word;
       end;

    const
       RSORDID = $8000;
       RNMOVE = $0010;
       RNPURE = $0020;
       RNPRELOAD = $0040;
       RNDISCARD = $F000;

    type
       new_rsrc = record
          rs_align : word;
          rs_typeinfo : rsrc_typeinfo;
       end;

  implementation

end.
