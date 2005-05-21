{
    Copyright (c) 1999-2000 by Peter Vreman (msg2inc) and
                          Marco van de Voort (data2inc)
    Placed under LGPL (See the file COPYING.FPC, included in this
    distribution, for details about the copyright)

    E-Mail Marco : Marcov@stack.nl
    Homepage Marco: www.stack.nl/~marcov/xtdlib.htm

    Data2Inc is a heavily modified version of msg2inc.pp which compiles the
     inputfile to include files containing array of char( or byte) typed
     constants.

     (e.g. CONST xxx : ARRAY[0..xxx] OF CHAR =( aa,bb,cc,dd,ee); ,
     or the same but ARRAY OF BYTE )

    Two types of input file are allowed:

    1 A special kind of textfile. Records start with '!'name and all following
       non empty and non comment (starting with '#',':' or '%') lines until
       the next line starting with '!' or EOF are the data. Data are either
       plain text (with \xxx ordinal constants) lines or a kinbd of
       Basic DATA command (these lines start with DATA).
       See demo.txt included with this package for a commented example.

    2  (special parameter -b)
       An arbitrary binary file can get converted to constants. In this mode
        only one constant per include file is possible.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program data2inc;
uses strings;

CONST
  version='1.00';

  maxbufsize = 1024*1024;  { 1 mb buffer }

type
  TOutputMode=(OutByte,OutChar,OutString);


{*****************************************************************************
            Simple service routines. These are copied from EPasStr.
*****************************************************************************}

TYPE CHARSET=SET OF CHAR;

FUNCTION NextCharPos(CONST S : String;C:CHAR;Count:LONGINT):LONGINT;

VAR I,J:LONGINT;

BEGIN
 I:=ORD(S[0]);
 IF I=0 THEN
  J:=0
 ELSE
  BEGIN
   J:=Count;
  IF J>I THEN
   BEGIN
    NextCharPos:=0;
    EXIT
   END;
  WHILE (S[J]<>C) AND (J<=I) DO INC(J);
   IF (J>I) THEN
    J:=0;
  END;
 NextCharPos:=J;
END;

FUNCTION NextCharPosSet(CONST S : String;CONST C:CHARSET;Count:LONGINT):LONGINT;

VAR I,J:LONGINT;

BEGIN
   I:=Length(S);
   IF I=0 THEN
    J:=0
   ELSE
    BEGIN
   J:=Count;
   IF J>I THEN
    BEGIN
     NextCharPosSet:=0;
     EXIT;
    END;
   WHILE (j<=i) AND (NOT (S[J] IN C)) DO INC(J);
   IF (J>I) THEN
    J:=0;                                        // NOT found.
   END;
 NextCharPosSet:=J;
END;


PROCEDURE RTrim(VAR P : String;Ch:Char);

VAR I,J : LONGINT;

BEGIN
 I:=ORD(P[0]);      { Keeping length in local data eases optimalisations}
 IF (I>0) THEN
  BEGIN
   J:=I;
   WHILE (P[J]=Ch) AND (J>0) DO DEC(J);
   IF J<>I THEN
    Delete(P,J+1,I-J+1);
   END;
END;

PROCEDURE UpperCase(VAR S : String);

VAR L,I : LONGINT;

BEGIN
 L:=Length(S);
 IF L>0 THEN
  FOR I:=1 TO L DO
   IF (S[I]>CHR(96)) AND (S[I]<CHR(123)) THEN
    S[I]:=CHR(ORD(S[I])-32);
END;

PROCEDURE LTrim(VAR P : String;Ch:Char);

VAR I,J : LONGINT;

BEGIN
 I:=ORD(P[0]);      { Keeping length in local data eases optimalisations}
 IF (I>0) THEN
  BEGIN
   J:=1;
   WHILE (P[J]=Ch) AND (J<=I) DO INC(J);
   IF J>1 THEN
    Delete(P,1,J-1);
   END;
END;


{*****************************************************************************
                              Parsing helpers
*****************************************************************************}

FUNCTION XlatString(Var S : String):BOOLEAN;
{replaces \xxx in string S with #x, and \\ with \ (escaped)
 which can reduce size of string.

Returns false when an error in the line exists}


Function GetNumber(Position:LONGINT):LONGINT;

VAR C,
    Value,
    I : LONGINT;

BEGIN
 I:=0; Value:=0;
 WHILE I<3 DO
  BEGIN
   C:=ORD(S[Position+I]);
   IF (C>47) AND (C<56) THEN
    C:=C-48
   ELSE
    BEGIN
     GetNumber:=-1;
     EXIT;
    END;
   IF I=0 THEN
    C:=C SHL 6;
   IF I=1 THEN
    C:=C SHL 3;
   Value:=Value + C;
   INC(I);
   END;
 GetNumber:=Value;
END;

VAR S2:String;
    A,B : LONGINT;
    Value : LONGINT;

BEGIN
 A:=1; B:=1;
 WHILE A<=Length(S) DO
  BEGIN
   IF S[A]='\' THEN
    IF S[A+1]='\' THEN
     BEGIN
      S2[B]:='\';
      INC (A,2); INC(B);
     END
    ELSE
     BEGIN
      Value:=GetNumber(A+1);
      IF Value=-1 THEN
       BEGIN
        XlatString:=FALSE;
        EXIT;
       END;
      S2[B]:=CHR(Value);
      INC(B); INC(A,4);
     END
   ELSE
    BEGIN
     S2[B]:=S[A];
     INC (A);
     INC (B);
    END;
  END;
 S2[0]:=CHR(B-1);
 S:=S2;
 XlatString:=TRUE;
END;

{Global equates}

VAR
  Inname,                     { Name of input file }
  OutName,                    { Name of output (.inc) file }
  BinConstName : string;      { (-b only) commandline name of constant }
  OutputMode   : TOutputMode; { Output mode (char,byte,string) }
  I_Binary     : BOOLEAN;     { TRUE is binary input, FALSE textual }
  MsgTxt       : pchar;       { Temporary storage of data }
  msgsize      : longint;     { Bytes used in MsgTxt }
  C            : CHAR;


{*****************************************************************************
                               WriteCharFile
*****************************************************************************}

{Dump the contents of MsgTxt (msgsize bytes) to file T (which has been opened),
using CONSTNAME as the name of the ARRAY OF CHAR constant}
procedure WriteCharFile(var t:text;constname:string);

  function createconst(b:byte):string;
  {decides whether to use the #xxx code or 'c' style for each char}
  begin
    if (b in [32..127]) and (b<>39) then
     createconst:=''''+chr(b)+''''
    else
     createconst:='#'+chr(b div 100+48)+chr((b mod 100) div 10+48)+chr(b mod 10+48)
  end;

var
  cidx,i  : longint;
  p       : PCHAR;
begin
  Writeln('Writing constant: ',constname,' to file '#39,outname,#39);
{Open textfile}
  write(t,'const ',constname,' : array[0..'); Writeln(t,msgsize-1,'] of char=(');
  p:=msgtxt;
  cidx:=0;
  for i:=0 to msgsize-1 do
   begin
     if cidx=15 then
      begin
        if cidx>0 then
         writeln(t,',')
        else
         writeln(t,'');
        write(t,'  ');
        cidx:=0;
      end
     else
       IF cidx>0 THEN
        write(t,',')
       ELSE
        Write(T,'  ');
     write(t,createconst(ord(p^)));
     inc(cidx);
     inc(p);
   end;
  writeln(t,');');
  Writeln(T);
end;


{*****************************************************************************
                               WriteByteFile
*****************************************************************************}

{Dump the contents of MsgTxt (msgsize bytes) to file T (which has been opened),
using CONSTNAME as the name of the ARRAY OF BYTE constant}
procedure WriteByteFile(var t:text;constname:string);

  function createconst(b:byte):string;
  {Translates byte B to a $xx hex constant}
  VAR l : Byte;
  begin
   createconst[1]:='$'; createconst[0]:=#3;
   l:=ORD(B SHR 4) +48;
   IF l>57 THEN
    l:=L+7;
   createconst[2]:=CHR(l);
   l:=ORD(B and 15) +48;
   IF l>57 THEN
    INC(L,7);
   createconst[3]:=CHR(l);
  end;

var
  cidx,i  : longint;
  p       : pchar;
begin
  Writeln('Writing constant: ',constname,' to file '#39,outname,#39);
{Open textfile}
  write(t,'const ',constname,' : array[0..'); Writeln(t,msgsize-1,'] of byte=(');
  p:=msgtxt;
  cidx:=0;
  for i:=0 to msgsize-1 do
   begin
     if cidx=15 then
      begin
        if cidx>0 then
         writeln(t,',')
        else
         writeln(t,'');
        write(t,'  ');
        cidx:=0;
      end
     else
       IF cidx>0 THEN
        write(t,',')
       ELSE
        Write(T,'  ');
     write(t,createconst(ord(p^)));
     inc(cidx);
     inc(p);
   end;
  writeln(t,');');
  Writeln(T);
end;


{*****************************************************************************
                               WriteStringFile
*****************************************************************************}

procedure WriteStringFile(var t:text;constname:string);
const
  maxslen=240; { to overcome aligning problems }

  function l0(l:longint):string;
  var
    s : string[16];
  begin
    str(l,s);
    while (length(s)<5) do
     s:='0'+s;
    l0:=s;
  end;

var
  slen,
  len,i  : longint;
  p      : pchar;
  start,
  quote  : boolean;
begin
  Writeln('Writing constant: ',constname,' to file '#39,outname,#39);
{Open textfile}
  writeln(t,'{$ifdef Delphi}');
  writeln(t,'const '+constname+' : array[0..',(msgsize-1) div maxslen,'] of string[',maxslen,']=(');
  writeln(t,'{$else Delphi}');
  writeln(t,'const '+constname+' : array[0..',(msgsize-1) div maxslen,',1..',maxslen,'] of char=(');
  write(t,'{$endif Delphi}');
{Parse buffer in msgbuf and create indexs}
  p:=msgtxt;
  slen:=0;
  len:=0;
  quote:=false;
  start:=true;
  for i:=1 to msgsize do
   begin
     if slen>=maxslen then
      begin
        if quote then
         begin
           write(t,'''');
           quote:=false;
         end;
        write(t,',');
        slen:=0;
        inc(len);
      end;
     if (len>70) or (start) then
      begin
        if quote then
         begin
           write(t,'''');
           quote:=false;
         end;
        if slen>0 then
          writeln(t,'+')
        else
          writeln(t);
        len:=0;
        start:=false;
      end;
     if (len=0) then
      write(t,'  ');
     if (ord(p^)>=32) and (p^<>#39) then
      begin
        if not quote then
         begin
           write(t,'''');
           quote:=true;
           inc(len);
         end;
        write(t,p^);
        inc(len);
      end
     else
      begin
        if quote then
         begin
           write(t,'''');
           inc(len);
           quote:=false;
         end;
        write(t,'#'+chr(ord(p^) div 100+48)+chr((ord(p^) mod 100) div 10+48)+chr(ord(p^) mod 10+48));
        inc(len,3);
      end;
     { start a new line when a #0 or #10 is found }
     if p^ in [#0,#10] then
      start:=true;
     inc(slen);
     inc(p);
   end;
  if quote then
   write(t,'''');
  writeln(t,'');
  writeln(t,');');
end;


{*****************************************************************************
                                   Parser
*****************************************************************************}

FUNCTION SpecialItem(S : String):LONGINT;
{ This procedure finds the next comma, (or the end of the string)
    but comma's within single or double quotes should be ignored.
    Single quotes within double quotes and vice versa are also ignored.}

VAR DataItem : LONGINT;

CONST xFcl : CHARSET = [',',#39,'"'];

BEGIN

    DataItem:=0;
    REPEAT
     DataItem:=NextCharPosSet(S,xFcl,DataItem+1);   {Find first " ' or ,}
      IF (DataItem<>0) AND ((S[DataItem]='"') OR (S[DataItem]=#39)) THEN { (double)Quote found?}
       DataItem:=NextCharPos(S,S[DataItem],DataItem+1);  { then find other one}
     UNTIL (DataItem=0) OR (S[DataItem]=',');
     IF DataItem=0 THEN     {Last data field of this line?}
      DataItem:=Length(S);
    SpecialItem:=DataItem;
END;


{ Handles reading and processing of a textual file}
procedure DoFile;
var
  Infile,
  Outfile : text;       {in and output textfiles}
  line, DataItem,       {line number, position in DATA line}
  I1,I2,                {4 temporary counters}
  I3,I4  : longint;
  s,S1    : string;     {S is string after reading, S1 is temporary string or
                          current DATA-item being processed }
  VarName : String;     { Variable name of constant to be written}

  PROCEDURE ParseError;
  {Extremely simple errorhandler}
  BEGIN
   Writeln('Error in line : ',Line, ' Somewhere near :',#39,S1,#39);
   Close(InfIle); Close(Outfile);
   HALT;
  END;

  PROCEDURE FixDec;
  { Reads decimal value starting at S1[1].
       Value in I3, number of digits found in I1}
       var I1,I2,i3 : longint;

  BEGIN
   I1:=1;
   WHILE ((S1[I1]>#47) AND (S1[I1]<#58)) AND (I1<=Length(S1)) DO
    INC(I1);
   DEC(I1);
   IF I1=0 THEN
    ParseError;
   I3:=0;
   FOR I2:=1 TO I1 DO
    I3:=(I3*10)+ ORD(S1[I2])-48;
  {Calc no of bytes(1,2 or 4) required from no of digits found}
   IF (I1<3) THEN
    I2:=1
   ELSE
    IF (I1=3) AND (I3<256) THEN
     I2:=1
    ELSE
     BEGIN
      IF I1<5 THEN
       I2:=2
       ELSE
        IF (I1=5) AND (i3<65536) THEN
         I2:=2
        ELSE
         I2:=4;
     END;
  END;

  PROCEDURE DoChar;
  { Reads a #xxx constant at S1[1], and puts it in msgtxt array.
      Deletes #xxx constant from S1}
  BEGIN
   Delete(S1,1,1);
   FixDec;
   msgtxt[Msgsize]:=CHR(I3);
   inc(msgsize);
   Delete(S1,1,I1);
  END;

  PROCEDURE DoQuote;
  { Reads a quoted text-string ('xxx' or "xxx"). Quotechar is in S1[1]
    (always ' or "), any char except the quotechar is allowed between two
    quotechars.
      Deletes quoted textstring incl quotes from S1}
  VAR
    C : Char;
  BEGIN
    C:=S1[1];
    Delete(S1,1,1);
    I1:=Pos(C,S1);                       {Find other quote}
    IF I1=0 THEN
     ParseError;                    {Quotes have to be matched}
    Dec(I1);
    IF I1<>0 THEN
     BEGIN
      Move(S1[1],Msgtxt[Msgsize],I1);
      INC(msgsize,I1);
     END;
    Delete(S1,1,I1+1);
    LTrim(S1,' ');
  END;

  PROCEDURE FixHex(base2:LONGINT);
  { Reads a base 2,8 or 16 constant from S1.
    Parameter = 2Log of base (1,3 or 4 corresponding to base 2,8 and 16)
    Constant is processed, the number of digits estimated (1,2 or 4 bytes) and
    the value is appended to msgtxt accordingly}
  BEGIN
    I3:=0;
    I2:=1;
    WHILE (S1[I2] IN ['0'..'9','A'..'F','a'..'f']) AND (I2<=Length(S1)) DO
     BEGIN
      IF (S1[I2]>#47) AND (S1[I2]<#58) THEN
       I3:=(I3 SHL base2)+ ORD(S1[I2])-48
      ELSE
       IF (S1[I2]>#64) AND (S1[I2]<#71) THEN
        I3:=(I3 SHL base2)+ ORD(S1[I2])-55
       ELSE
        IF (S1[I2]>#96) AND (S1[I2]<#103) THEN
         I3:=(I3 SHL base2)+ ORD(S1[I2])-87
       ELSE
        ParseError;
       INC(I2);
     END;
    DEC(I2);
    CASE Base2 OF
     4 :   BEGIN
           I4:=(I2 SHR 1);
           IF ODD(I2) THEN
            INC(I4);
           IF I4=3 THEN I4:=4
          END;
     3 :   I4:=(I2*3 DIV 8)+1;
     1 :   BEGIN
            IF I2<9 THEN
             I4:=1
            ELSE
             IF I2<17 THEN
              I4:=2
             ELSE
             I4:=4;
           END;
      ELSE
       BEGIN
        Writeln(' severe internal error ');
        ParseError;
       END; {else}
    END; {Case}
    move(I3,msgtxt[Msgsize],i4);
    inc(msgsize,i4);
  END;

  PROCEDURE DoTextual;
  { processes aggregates of textual data like 'xxx'+#39"2143124"+'1234'#123}

  BEGIN
   REPEAT
    CASE S1[1] OF
     '#' : DoChar;
     '"',#39 : DoQuote;           {Should I support octal codes here?}
    ELSE
     ParseError;
     END;
    LTrim(S1,' ');
    IF (S1[1]='+') THEN
     Delete(S1,1,1);
    LTrim(S1,' ');
   UNTIL Length(S1)=0;
  END;

  PROCEDURE FlushMsgTxt;            {Flush MsgTxt array}
  BEGIN
   IF msgsize>0 THEN          {In memory? Then flush}
    BEGIN
      case outputmode of
        OutByte :
          WriteByteFile(outfile,Varname);
        OutChar :
          WriteCharFile(outfile,varname);
        OutString :
          WriteStringFile(outfile,varname);
      end;
     msgsize:=0;
    END;
  END;

{Actual DoFile}
begin
  Getmem(msgtxt,maxbufsize);
  Writeln('processing file : ',inname);
{Read the message file}
  assign(infile,inname);
  {$I-}
   reset(infile);
  {$I+}
  if ioresult<>0 then
   begin
     WriteLn('file '+inname+' not found');
     exit;
   end;
{Create output file}
  assign (outfile,outname);
  rewrite(outfile);
  msgsize:=0;
  Line:=0;
  while not eof(infile) do
   begin
    readln(infile,s);      {Read a line}
    INC(Line);
    S1:=Copy(S,1,5);
    Uppercase(S1);
    IF S1='DATA ' THEN   {DATA keyword?}
     BEGIN
      Delete(S,1,5);
      REPEAT
       DataItem:=SpecialItem(S);  {Yes. Determine size of DATA field.}
       IF DataItem<>0 THEN
        BEGIN
         I1:=DataItem;
         IF DataItem=Length(S) THEN
          INC(i1);        {DataItem fix for last field}
         S1:=Copy(S,1,I1-1);    { copy field to S1}
         Delete(S,1,I1);        {Delete field from S}
         LTrim(S1,' ');
         RTrim(S1,' ');
         LTrim(S,' ');
         CASE S1[1] OF        {Select field type}
          #39,'"','#' : DoTextual; { handles textual aggregates
                                     e.g. #124"142"#123'sdgf''ads'}
          '$' : BEGIN         {Handle $xxxx hex codes}
                 Delete(S1,1,1);
                 RTrim(S1,' ');
                 IF Length(S1)>0 THEN
                  FixHex(4)
                 ELSE
                  ParseError;
                 END;
    '0'..'9'  : BEGIN    { handles 0x124,124124,124124H,234h,666o,353d,24b}
                 IF (Length(S1)>1) AND (S1[2]='x') THEN {C style 0xABCD hex}
                  BEGIN
                   Delete(S1,1,2);
                   FixHex(4);
                  END
                 ELSE {other types (HP notation suffix h,o,d and b (and upcase versions,
                                                       and no suffix) }
                  BEGIN
                   CASE S1[Length(S1)] OF
                    'H','h' : FixHex(4);      {Hex}
                    'o','O' : FixHex(3);      {octal}
                        'B','b' : BEGIN       {Binary}
                                   DEC(S1[0]); {avoid 'b' char being treated as
                                                 hex B }
                                   FixHex(1);
                                  END;
               '0'..'9','d','D' : BEGIN      {decimal versions}
                                   FixDec;   {Fixdec is safe for trailing chars}
                                   {I1 =no of digits, I3=value, I2= no bytes needed}
                                   move(I3,msgtxt[Msgsize],i2);
                                   inc(msgsize,i2)
                                  END
                             ELSE
                              ParseError; {otherwise wrong suffix}
                           END {Nested case}
                       END; { IF S1[2]='x'}
                      END; { '0'..'9'}
              '%'   : BEGIN          {%101010 binary constants}
                       Delete(S1,1,1);
                       FixHex(1);
                      END;
              '\'   : BEGIN          {\xxx octal constants}
                       Delete(S1,1,1);
                       FixHex(3);
                      END;
          END; {Case}
         END; {IF <>0}
        UNTIL {(DataItem:=Length(S)) OR} (DataItem=0); {parse until String is empty}
      END {S1='DATA'}
     ELSE
      BEGIN                          {Non DATA line}
       IF (Length(S)<>0) AND NOT (S[1] IN ['#',';','%']) THEN
        BEGIN
         C:=S[1];
         IF NOT XlatString(S) THEN  {Expand \xxx octal constants}
          BEGIN
           Writeln('Some error with a \xxx constant or a stale (unescaped) backslash');
           ParseError;
          END;
         IF C='!' THEN         { New variable}
          BEGIN
           FlushMsgTxt;
           I1:=1;
           OutputMode:=OutChar;
           IF S[2]='$' THEN      {Flag for ARRAY OF BYTE?}
            BEGIN
             INC(I1);
             OutputMode:=OutByte;
            END;
           Delete(S,1,I1);
           VarName:=S;
          END
         ELSE
          BEGIN {Normal line}
           i1:=Length(S);
           move(s[1],msgtxt[Msgsize],i1);
           inc(msgsize,i1);
          END;
      END;
    END;
   end;
  close(infile);
  FlushMsgTxt;                    {Flush variable if msgtxt is occupied}
  Close(Outfile);
end;


{*****************************************************************************
                                    Binary File
*****************************************************************************}

procedure DoBinary;
var
  Infile  : File;
  Outfile : text;
  i       : longint;
begin
  Writeln('processing file : ',inname);
{ Read the file }
  assign(infile,inname);
  {$I-}
   reset(infile,1);
  {$I+}
  if ioresult<>0 then
   begin
     WriteLn('file '+inname+' not found');
     exit;
   end;
{ First parse the file and count bytes needed }
  msgsize:=FileSize(InFile);
  Getmem(msgtxt,msgsize);
  BlockRead(InFile,msgTxt[0],msgsize,i);
  close(infile);
  IF I<>msgsize THEN
   BEGIN
     Writeln('Error while reading file',inName);
     HALT(1);
   END;
{ Output }
  assign (outfile,outname);
  rewrite(outfile);
  case outputmode of
    OutByte :
      WriteByteFile(outfile,BinconstName);
    OutChar :
      WriteCharFile(outfile,BinconstName);
    OutString :
      WriteStringFile(outfile,BinconstName);
  end;
  Close(Outfile);
end;


{*****************************************************************************
                                Main Program
*****************************************************************************}

procedure getpara;
var
  ch      : char;
  para    : string;
  files,i : word;

  procedure helpscreen;
  begin
    writeln('usage : data2inc [Options] <msgfile> [incfile] [constname]');
    Writeln('  The constname parameter is only valid in combination');
    writeln('  with -b, otherwise the constname must be specified in the inputfile');
    Writeln;
    writeln('<Options> can be :');
    writeln('              -B     File to read is binary.');
    writeln('              -A     array of byte output (default is array of char)');
    writeln('              -S     array of string output');
    writeln('              -V     Show version');
    writeln('        -? or -H     This HelpScreen');
    writeln;
    Writeln(' See data2inc.exm for a demonstration source');
    halt(1);
  end;


begin
  I_binary:=FALSE;
  OutputMode:=OutChar;
  FIles:=0;
  for i:=1to paramcount do
   begin
     para:=paramstr(i);
     if (para[1]='-') then
      begin
        ch:=upcase(para[2]);
        delete(para,1,2);
        case ch of
         'B' : I_Binary:=TRUE;
         'A' : OutputMode:=OutByte;
         'S' : OutputMode:=OutString;
         'V' : begin
                 Writeln('Data2Inc ',version,' (C) 1999 Peter Vreman and Marco van de Voort');
                 Writeln;
                 Halt;
               end;
     '?','H' : Helpscreen;

        end;
     end
    else
     begin
       inc(Files);
       if Files>3 then
        HelpScreen;
       case Files of
        1 : InName:=Para;
        2 : OutName:=Para;
        3 : BinConstName:=Para;
       end;
     end;
    END;
   if (FIles<3) AND I_Binary then
     HelpScreen;
   IF Files<2 THEN
    HelpScreen;
end;

begin
  GetPara;
  IF I_Binary THEN
   DoBinary
  ELSE
   DoFile;
end.
