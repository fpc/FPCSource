{------8<-------------Snip---------------8<------------Snip------------8<-------}
{$I-}
UNIT zipviewu;

(*/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\\/\/\/\/\/\/\/\*)
(* Unit : Zip View                    Date : March 23, 1994                  *)
(* By   : John Shipley                Ver  : 1.0                             *)
(*                                                                           *)
(* Credits : Steve Wierenga - ZIPV.PAS found in SWAG - Got me started on the *)
(*           zipviewu code since ZIPV.PAS was fairly easy to read unlike     *)
(*           some other code I had seen.                                     *)
(*                                                                           *)
(*           Tom Guinther - ZIPPER.PAS found in ZIPPER.ZIP (1989) available  *)
(*           on my BBS "The Brook Forest Inn 714-951-5282" This code helped  *)
(*           clarify many things. The zipper code is probably better than    *)
(*           this code and well documented.                                  *)
(*                                                                           *)
(*           PkWare's APPNOTE.TXT found in PKZ110.EXE                        *)
(*                                                                           *)
(* This unit is offered to the Public Domain so long as credit is given      *)
(* where credit is due. I accept NO liablity for what this code does to your *)
(* system or your friends or anyone elses. You have the code, so you can fix *)
(* it. If this code formats your hard drive and you loose your lifes work,   *)
(* then all I can say is "Why didn't you back it up?"                        *)
(*                                                                           *)
(* Purpose: To mimic "PKUNZIP -v <filename>" output. (v2.04g)                *)
(*          The code is pretty close to the purpose, but not perfect.        *)
(*                                                                           *)
(* Demo :                                                                    *)
(*                                                                           *)
(* PROGRAM zip_viewit;                                                       *)
(* USES DOS,CRT,zipviewu;                                                    *)
(* BEGIN                                                                     *)
(*   IF PARAMCOUNT<>0 THEN                                                   *)
(*     BEGIN                                                                 *)
(*       zipview(PARAMSTR(1));                                               *)
(*     END;                                                                  *)
(* END.                                                                      *)
(*/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\\/\/\/\/\/\/\/\*)

INTERFACE

USES DOS,CRT;

PROCEDURE zipview(zipfile: STRING);

IMPLEMENTATION

CONST hexdigit : ARRAY[0..15] OF CHAR = '0123456789abcdef';

FUNCTION hexbyte(b: byte): STRING;                        (* Byte to Hexbyte *)
  BEGIN
    hexbyte := hexdigit[b SHR 4]+hexdigit[b AND $f];
  END;

FUNCTION hexlong(l: LONGINT): STRING;                  (* Longint to Hexlong *)
  VAR n : ARRAY[1..4] OF BYTE ABSOLUTE l;
  BEGIN
    hexlong := hexbyte(n[4])+hexbyte(n[3])+hexbyte(n[2])+hexbyte(n[1]);
  END;

FUNCTION lenn(s: STRING): INTEGER;     (* Like LENGTH, but skips color codes *)
  VAR i,len : INTEGER;
  BEGIN
    len := LENGTH(s);
    i := 1;
    WHILE (i<=LENGTH(s)) DO
      BEGIN
        IF (s[i] IN [#3,'^']) THEN
          IF (i<LENGTH(s)) THEN
            BEGIN
              DEC(len,2);
              INC(i);
            END;
        INC(i);
      END;
    lenn := len;
  END;

FUNCTION mln(s: STRING; l: INTEGER): STRING;                 (* Left Justify *)
  BEGIN
    WHILE (lenn(s)<l) DO s := s+' ';
    IF (lenn(s)>l) THEN
      REPEAT
        s := COPY(s,1,LENGTH(s)-1)
      UNTIL (lenn(s)=l) OR (LENGTH(s)=0);
    mln := s;
  END;

FUNCTION mrn(s: STRING; l: INTEGER): STRING;                (* Right Justify *)
  BEGIN
    WHILE lenn(s)<l DO s := ' '+s;
    IF lenn(s)>l THEN s := COPY(s,1,l);
    mrn := s;
  END;

FUNCTION cstr(i: LONGINT): STRING;         (* convert integer type to string *)
  VAR c : STRING[16];
  BEGIN
    STR(i,c);
    cstr := c;
  END;

FUNCTION tch(s: STRING): STRING;                          (* Ensure 2 Digits *)
  BEGIN
    IF (LENGTH(s)>2) THEN s := COPY(s,LENGTH(s)-1,2)
    ELSE IF (LENGTH(s)=1) THEN s := '0'+s;
    tch := s;
  END;

FUNCTION b2attr(a,g: BYTE): STRING;                     (* Byte to Attribute *)
  VAR attr : STRING[5];
  BEGIN
    attr := '--w- ';
    IF (g AND 1)=1 THEN attr[5]:='*';                          (* Encrypted? *)
    IF (a AND 1)=1 THEN attr[3]:='r';                          (* Read Only? *)
    IF (a AND 2)=2 THEN attr[2]:='h';                             (* Hidden? *)
    IF (a AND 4)=4 THEN attr[1]:='s';                             (* System? *)
    IF (a AND 8)=8 THEN attr[4]:='?';                (* Unknown at this time *)
    b2attr := attr;
  END;

FUNCTION w2date(d: WORD): STRING;                            (* Word to Date *)
  VAR s : STRING;
  BEGIN
    s := tch(cstr((d SHR 5) AND 15 ))+'-'+                          (* Month *)
         tch(cstr((d      ) AND 31 ))+'-'+                            (* Day *)
         tch(cstr(((d SHR 9) AND 127)+80));                          (* Year *)
    w2date := s;
  END;

FUNCTION w2time(t: WORD): STRING;                            (* Word to Time *)
  VAR s : STRING;
  BEGIN
    s := tch(cstr((t SHR 11) AND 31))+':'+                           (* Hour *)
         tch(cstr((t SHR  5) AND 63));                             (* Minute *)
    w2time := s;
  END;

PROCEDURE zipview(zipfile: STRING);                     (* View the ZIP File *)
  CONST lsig = $04034B50;                                 (* Local Signature *)
        csig = $02014b50;                               (* Central Signature *)
  TYPE lheader = RECORD                                      (* Local Header *)
                   signature  : LONGINT;      (* local file header signature *)
                   version,                                (* version mad by *)
                   gpflag,                          (* general purpose flags *)
                   compress,                           (* compression method *)
                   time,date  : WORD;         (* last mod file time and date *)
                   crc32,                                          (* crc-32 *)
                   csize,                                 (* compressed size *)
                   usize      : LONGINT;                (* uncompressed size *)
                   fnamelen,                              (* filename length *)
                   extrafield : WORD;                  (* extra field length *)
                 END;
       cheader = RECORD                                    (* Central Header *)
                   signature  : LONGINT;    (* central file header signature *)
                   version    : WORD;                     (* version made by *)
                   vneeded    : WORD;           (* version needed to extract *)
                   gpflag     : ARRAY[1..2] OF BYTE;(* general purpose flags *)
                   compress   : WORD;                  (* compression method *)
                   time       : WORD;                  (* last mod file time *)
                   date       : WORD;                  (* last mod file date *)
                   crc32      : LONGINT;                           (* crc-32 *)
                   csize      : LONGINT;                  (* compressed size *)
                   usize      : LONGINT;                (* uncompressed size *)
                   fnamelen   : WORD;                     (* filename length *)
                   extrafield : WORD;                  (* extra field length *)
                   fcl        : WORD;                 (* file comment length *)
                   dns        : WORD;                   (* disk number start *)
                   ifa        : WORD;            (* internal file attributes *)
                   efa        : ARRAY[1..4] OF BYTE;   (* external file attr *)
                   roolh      : LONGINT;  (* relative offset of local header *)
                 END;

VAR z          : INTEGER;               (* Number of files processed counter *)
    totalu,                              (* Total bytes that were compressed *)
    totalc     : LONGINT;          (* result of total bytes being compressed *)
    hdr        : ^cheader;            (* temporary cental header file record *)
    f          : FILE;                                           (* file var *)
    s          : STRING;                          (* archive filename string *)
    percent    : BYTE;           (* Temporary var holding percent compressed *)
    numfiles   : WORD;                         (* Number of files in archive *)

CONST comptypes : ARRAY[0..8] OF STRING[7] =            (* Compression Types *)
                  ('Stored ',                              (* Not Compressed *)
                   'Shrunk ',                                      (* Shrunk *)
                   'Reduce1',                                   (* Reduced 1 *)
                   'Reduce2',                                   (* Reduced 2 *)
                   'Reduce3',                                   (* Reduced 3 *)
                   'Reduce4',                                   (* Reduced 4 *)
                   'Implode',                                    (* Imploded *)
                   'NotSure',                        (* Unknown at this time *)
                   'DeflatN');                                   (* Deflated *)

FUNCTION seekc(VAR f: FILE): BOOLEAN;
  VAR curpos  : LONGINT;                           (* current file position *)
      buf     : lheader;                   (* Temporary local header record *)
      ioerror : INTEGER;                       (* Temporary IOResult holder *)
      result  : WORD;                                   (* Blockread Result *)
  BEGIN
    seekc := FALSE;                                           (* init seekc *)
    curpos := 0;                              (* init current file position *)
    SEEK(f,0);                                        (* goto start of file *)
    BLOCKREAD(f,buf,SIZEOF(lheader),result);     (* Grab first local header *)
    ioerror := IORESULT;                                  (* Test for error *)
    WHILE (ioerror = 0) AND (buf.signature=lsig) DO (* Test if OK..continue *)
      BEGIN
        INC(numfiles);                         (* Increment number of files *)
        WITH buf DO                             (* Find end of local header *)
          curpos := FILEPOS(f)+fnamelen+extrafield+csize;
        SEEK(f,curpos);                         (* Goto end of local header *)
        BLOCKREAD(f,buf,SIZEOF(lheader),result);  (* Grab next local header *)
        ioerror := IORESULT;                              (* Test for error *)
      END;
      IF ioerror<>0 THEN EXIT;               (* If error then exit function *)
      IF (buf.signature=csig) THEN (* Did we find the first central header? *)
        BEGIN
          seekc := TRUE;                      (* Found first central header *)
          SEEK(f,curpos); (* Ensure we are at central headers file position *)
        END;
  END;

  VAR curpos : LONGINT;

  BEGIN
    numfiles := 0;      (* Counter of Number of Files to Determine When Done *)
    z        := 0;                   (* Counter of Number of Files Processed *)
    totalu   := 0;                      (* Total Bytes of Uncompressed Files *)
    totalc   := 0;                      (* Total Size after being Compressed *)
    NEW(hdr);        (* Dynamically Allocate Memory for a Temp Header Record *)
    ASSIGN(f,zipfile);                        (* Assign Filename to File Var *)
    {$I-}
    RESET(f,1);                                         (* Open Untyped File *)
    {$I+}
    IF IORESULT<>0 THEN                  (* If we get an error, exit program *)
      BEGIN
        WRITELN('Error - File not found.');
        HALT(253);
      END;
    IF NOT seekc(f) THEN (* Skip Local Headers and goto first Central Header *)
      BEGIN                       (* If we could not locate a Central Header *)
        CLOSE(f);                                      (* Close Untyped File *)
        WRITELN('Error - Corrupted or Not a ZIP File.');
        HALT(254);                                           (* Exit Program *)
      END;

    WRITELN(' Length  Method   Size  Ratio   Date    Time    CRC-32 '+
      ' Attr  Name');
    WRITELN(' ------  ------   ----- -----   ----    ----   --------'+
      ' ----  ----');
    REPEAT
      FILLCHAR(s,SIZEOF(s),#0);                         (* Clear Name String *)
      BLOCKREAD(f,hdr^,SIZEOF(cheader));                 (* Read File Header *)
      BLOCKREAD(f,MEM[SEG(s):OFS(s)+1],hdr^.fnamelen);  (* Read Archive Name *)
      s[0] := CHR(hdr^.fnamelen);                 (* Get Archive Name Length *)
      IF (hdr^.signature=csig) THEN                           (* Is a header *)
        BEGIN
          INC(z);                                  (* Increment File Counter *)
          WRITE(mrn(cstr(hdr^.usize),7));       (* Display Uncompressed Size *)
          WRITE(' '+mrn(comptypes[hdr^.compress],7));  (* Compression Method *)
          WRITE(mrn(cstr(hdr^.csize),8));         (* Display Compressed Size *)
          percent := ROUND(100.0-(hdr^.csize/hdr^.usize*100.0));
          WRITE(mrn(cstr(percent),4)+'% ');   (* Display Compression Percent *)
          WRITE(' '+w2date(hdr^.date)+' ');    (* Display Date Last Modified *)
          WRITE(' '+w2time(hdr^.time)+' ');    (* Display Time Last Modified *)
          WRITE(' '+hexlong(hdr^.crc32)+' ');       (* Display CRC-32 in Hex *)
          WRITE(b2attr(hdr^.efa[1],hdr^.gpflag[1]));   (* Display Attributes *)
          WRITELN(' '+mln(s,13));                (* Display Archive Filename *)
          INC(totalu,hdr^.usize);             (* Increment size uncompressed *)
          INC(totalc,hdr^.csize);               (* Increment size compressed *)
        END;
      SEEK(f,FILEPOS(f)+hdr^.extrafield+hdr^.fcl);
    UNTIL (hdr^.signature<>csig) OR EOF(f) OR (z=numfiles); (* No more Files *)
    WRITELN(' ------          ------  ---                                 '+
      ' -------');
    WRITE(mrn(cstr(totalu),7)+'         ');    (* Display Total Uncompressed *)
    WRITE(mrn(cstr(totalc),7)+' ');              (* Display Total Compressed *)
    WRITE((100-TotalC/TotalU*100):3:0,'%'+mrn(' ',34));   (* Display Percent *)
    WRITELN(mrn(cstr(z),7));                      (* Display Number of Files *)
    CLOSE(f);                                          (* Close Untyped File *)
    DISPOSE(hdr);                            (* Deallocate Header Var Memory *)
  END;

END.
