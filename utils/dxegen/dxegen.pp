{
   Copyright (C) 1995 Charles Sandmann (sandmann@clio.rice.edu)
   This software may be freely distributed with above copyright, no warranty.
   Based on code by DJ Delorie, it's really his, enhanced, bugs fixed.

   DXEGEN converts COFF object files to .DXE files that can be loaded and
   relocated runtime. See (1.0.6+) manual for more details.

   Pascal translation, improvements, enhancements
                   (C) 2001 by Marco van de Voort (Free Pascal member).

}

Uses strings,dxetype,coff,dos;

{$inline on}

Const
  DirSep  = System.DirectorySeparator;
  Tempname= 'dxe__tmp.o';

{ This next function is needed for cross-compiling when the machine
   isn't little-endian like the i386 }

Type csize_t = cardinal;

Procedure dosswap(vdata:pointer;Const pattern:String);

{ interpretive way of changing structures to bigendian.
 Pattern contains the structures. l (32-bit) and s (16-bit) will be swapped,
 a char between 1 and 9 skips that much bytes)

Excellent candidate to be converted to something generic that inlines and
isn't interpretive. Until generics, this is the only reusable way.

}

{$ifdef FPC_BIG_ENDIAN}
Var data : pbyte;
    c    : byte;
    i,j  : longint;
{$endif FPC_BIG_ENDIAN}

Begin

 {$ifdef FPC_BIG_ENDIAN}
  I := 1;
  j := length(pattern);
  data := pbyte(vdata);
  while I< = j Do
             Begin
               Case Pattern[i] Of
                 '1'..'9' : inc(data,ord(pattern[i])-ord('0'));
                 's'      :
                            Begin
                              c := data[1];
                              data[1] := data[0];
                              data[0] := c;
                              inc(data,2);
                            End;
                 'l'      :
                            Begin
                              c := data[3];
                              data[3] := data[0];
                              data[0] := c;
                              c := data[1];
                              data[1] := data[2];
                              data[2] := c;
                              inc(data,4);
                              // bswap (Data) ?
                            End
                            Else
                              inc(data);
               End;
               inc(i);
             End;
 {$endif FPC_BIG_ENDIAN}
End;

Var blaat : pointer;

Procedure exit_cleanup;

Var f: file;

Begin
  assign(f,tempname);
  {$I-}
  erase(f);
  {$I+}
  exitproc := blaat;
End;

Var
  Errors        : Longint;
  Bss_start     : Cardinal;  {type "unsigned" ?}
  fh            : FILHDR;
  Input_f,
  Output_F      : file;
  Sc            : SCNHDR;
  Data,
  Strngs        : PChar; {strings is not reserved, but not wise to use in C translations}
  Sym           : ^SYMENT;
  Relocs        : pRELOC;
  Strsz         : Longint;
  I             : csize_t;
  Dh            : dxe_header;
  Command,
  Param,
  Libdir        : ansistring;
  Name          : pchar;
  Tmp           : array[0..8] Of char;
  Written       : Word;

Function fixrelocs(i:longint): preloc; {$ifdef HASINLINE}inline;{$endif}

Begin
  fixrelocs := preloc(longint(relocs)+i*SIZEOF(reloc));
End;

Begin
  Errors   := 0;
  Bss_start := 0;

  If paramcount<3 Then
    Begin
      Writeln('Usage: dxegen output.dxe symbol input.o [input2.o ... -lgcc -lc]');
      Halt(1);
    End;
  Assign (input_f,paramstr(3));
  Filemode := 0;
  {$I-}
  Reset(input_f,1);
  {$I+}
  If IOResult<>0 Then
    Begin
      Writeln('File: ',ParamStr(3),' couldn''t be opened');
      halt(1);
    End;

  {Read the COFF .O fileheader}

  Blockread(input_f,fh,FILHSZ);

  dosswap(@fh,'sslllss');
  If (fh.f_nscns <>1) And (paramcount>3) Then
    Begin
      Close(input_f);
{$ifdef DXE_LD}
      command := DXE_LD;
{$else}
      command := 'ld';
{$endif}
      param := '-X -S -r -o '+tempname+' -L';
      libdir := getenv('DXE_LD_LIBRARY_PATH');
      If libdir<>'' Then
        param := param+libdir
      Else
        Begin
          libdir := getenv('DJDIR'); {FPCDIR ?}
          If libdir='' Then
            Begin
              Writeln('Error: neither DXE_LD_LIBRARY_PATH nor DJDIR are set in environment');
              Halt(1);
            End;
          param := param+libdir+dirsep+'lib';
        End;

      For i:= 3 To ParamCount Do
        param := param+' '+paramstr(i);

      param := param+' -T dxe.ld ';
      Writeln('Executing: "',Command,' ',param,'"');
      Exec(Command,Param);
      Errors := DosExitCode;
      If Errors<>0 Then
        Begin
          Writeln('Dos returned errorcode: ',Errors);
          Halt(Errors);
        End;

      Assign(input_f,tempname);
      FileMode := 0;
    {$I-}
      Reset(Input_f,1);
    {$I+}
      If IOresult<>0 Then
        Begin
          Close(input_f);
          Writeln('couldn''t open file: '+tempname);
          halt(1);
        End
      Else
        Begin
          blaat := exitproc;
          exitproc := @exit_cleanup;
        End;

      blockread(input_f,fh,FILHSZ);
      dosswap(@fh, 'sslllss');
      If (fh.f_nscns <>1) Then
        Begin
          Close(input_f);
          Writeln('Error: input file has more than one section; use -M for map');
          halt(1);
        End;
    End;

  seek(input_f,FilePos(Input_f)+fh.f_opthdr);
  BlockRead(input_f,sc,SCNHSZ);
  dosswap(@sc, '8llllllssl');
  dh.magic         := DXE_MAGIC;
  dh.symbol_offset := cardinal (-1);
  dh.element_size  := sc.s_size;
  dh.nrelocs       := sc.s_nreloc;
  Getmem(Data,sc.s_size);
  Seek(input_f,sc.s_scnptr);
  BlockRead(input_f,data^, sc.s_size);
  Getmem(sym,sizeof(SYMENT)*fh.f_nsyms);
  Seek(input_f,fh.f_symptr);
  Blockread(input_f,sym^,fh.f_nsyms*SYMESZ);
  Blockread(input_f,strsz,4);
  Dosswap(@strsz,'l');
  Getmem(Strngs,strsz);
  Blockread(input_f,strngs[4],strsz-4);

  plongint(strsz)[0] := 0;  // {?}
  I := 0;
  while I < fh.f_nsyms do
    Begin
      If (sym[i].e.e.e_zeroes<>0) Then
        Begin
          dosswap(@sym[i], '8lscc');
          move(sym[i].e.e_name,tmp,8);
          tmp[8] := #0;
          name := @tmp;
        End
      Else
        Begin
          dosswap(@sym[i], 'lllscc');
          name := strngs + sym[i].e.e.e_offset;
        End;

      If (sym[i].e_scnum = 0) Then
        Begin
          Writeln('Error: object contains unresolved external symbols (', name,')');
          inc(errors);
        End;
      If (strlcomp(name, argv[2], strlen(argv[2])) = 0) Then
        Begin
          If (dh.symbol_offset <> cardinal (-1)) Then
            Begin
              Writeln('Error: multiple symbols that start with ',paramstr(2),' (',name,
              ')!');
              Inc(errors);
            End;
          dh.symbol_offset := sym[i].e_value;
        End
      Else If (strcomp(name, '.bss') = 0) And  (bss_start=0) Then
             Begin
               bss_start := sym[i].e_value;
               Fillchar(data[bss_start], sc.s_size - bss_start,#0);
             End;
      Inc (I, Succ (sym[i].e_numaux));  (* Original C for loop iteration   *)
                                        (* plus increment for found value. *)
    End;

  If (dh.symbol_offset = cardinal (-1)) Then
    Begin
      Writeln('Error: symbol ',argv[2],' not found!');
      Inc(Errors);
    End;

  Getmem(Relocs,sizeof(RELOC)*sc.s_nreloc);
  seek(input_f, sc.s_relptr);
  Blockread (input_f,relocs^,sc.s_nreloc*RELSZ);;

  Close(input_f);
  If errors>0 Then
    Begin
      Writeln(' Errors: ',Errors);
      Halt(Errors);
    End;

  Assign(Output_F,argv[1]);
 {$I-}
  Rewrite(output_f,1);
 {$I+}
  If Ioresult<>0 Then
    Begin
      Writeln('can''t write file ',argv[1]);
      Halt(1);
    End;
  If sc.s_nreloc<>0 Then
    For I:=0 To sc.s_nreloc-1 Do
      Begin
        If (fixrelocs(i)^.r_type And 255)=$14 Then
          Dec(dh.nrelocs); { Don't do these, they are relative }
      End;
  Dosswap(@dh,'llll');
  Dosswap(@dh, 'llll');
  BlockWrite(output_f,dh,sizeof(dh));
  Blockwrite(output_f,data^,sc.s_size,I);
  If sc.s_nreloc<>0 Then
    For I:=0 To sc.s_nreloc-1 Do
      Begin
        If (fixrelocs(i)^.r_type And 255)<>$14 Then
          blockwrite(output_f,fixrelocs(i)^.r_vaddr , 4,written);
      End;
  Close(output_f);
End.
