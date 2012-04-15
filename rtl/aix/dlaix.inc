

{ aix 5.3 doesn't have dladdr -> own implementation (from
  http://root.cern.ch/drupal/content/aix-and-dladdr }


  const
    L_GETINFO = 2;

  type
    pld_info = ^ld_info;
    ld_info = record
      ldinfo_next: cuint;
{$ifndef cpu64}
      ldinfo_flags: cuint;
{$endif}
      _file: record
        case byte of
          0: (_ldinfo_fd: cint);
          1: (_ldinfo_fp: pointer);
          2: (_core_offset: ptrint);
      end;
      ldinfo_textorg: pointer;
      ldinfo_textsize: ptrint;
      ldinfo_dataorg: pointer;
      ldinfo_datasize: ptrint;
      ldinfo_filename: pchar;
    end;

  function loadquery(__lflags: cint; __buffer: pointer; __length: cuint): longint; cdecl; varargs; external;


  function aix53_dladdr(Lib: pointer; info: Pdl_info): Longint; cdecl;
    var
      buf: array[0..4095] of byte;
      pbuf: pbyte;
      ldi: pld_info;
      text_begin, text_end: pointer;
    begin
      fillchar(info^,sizeof(info^),0);
      aix53_dladdr:=loadquery(L_GETINFO,@buf,sizeof(buf));
      if aix53_dladdr=-1 then
        begin
          aix53_dladdr:=0;
          exit;
        end;
      pbuf:=@buf[0];
      ldi:=pld_info(pbuf);
      // First is main(), skip.
      while ldi^.ldinfo_next<>0 do
        begin
          inc(pbuf,ldi^.ldinfo_next);
          ldi:=pld_info(pbuf);
          text_begin:=ldi^.ldinfo_textorg;
          if text_begin<Lib then
            begin
              text_end:=text_begin+ldi^.ldinfo_textsize;
              if text_end>Lib then
                begin
                  info^.dli_fname:=ldi^.ldinfo_filename;
                  info^.dli_fbase:=ldi^.ldinfo_textorg;
                  { no info about symbols -> leave nil/0 (valid for regular
                    dladdr call as well) }
                  aix53_dladdr:=1;
                  exit;
                end;
           end;
        end;
      aix53_dladdr:=0;
    end;


  type
    tdladdrfunc = function(lib: pointer; info: Pdl_info): longint; cdecl;

  function dladdr(Lib: pointer; info: Pdl_info): Longint; cdecl;
    const
      dladdrf: tdladdrfunc = nil;
    var
      libdl: pointer;
    begin
      { dladdr is only available on AIX 6.0 and later.

        AIX does not support undefined weak external symbols, so we cannot
        simply define dladdr as weakexternal and be done with it -> look up
        the address of dladdr using dlsym }
      if not assigned(dladdrf) then
        begin
          libdl:=dlopen('libdl.a',RTLD_LAZY);
          if assigned(libdl) then
            dladdrf:=tdladdrfunc(dlsym(libdl,'dladdr'));
          if not assigned(dladdrf) then
            dladdrf:=@aix53_dladdr;
          { can't be the last reference that causes it to be unloaded, since
            most functions from this unit come from it }
          if assigned(libdl) then
            dlclose(libdl);
        end;
      dladdr:=dladdrf(Lib,info);
    end;

