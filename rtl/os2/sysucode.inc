{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Tomas Hajny,
    member of the Free Pascal development team.

    OS/2 UnicodeStrings support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

(* The implementation is based on native Unicode support available under
   OS/2 Warp 4 and above; if running under OS/2 Warp 3 and UCONV.DLL
   library is not available, this implementation will resort to dummy
   routines. This still allows providing 3rd party implementation based
   e.g. on the ICONV library as an external unit.
*)

const
  MaxSpecialCPTranslation = 2;
  MaxNonEqualCPMapping = 35;
  MaxCPMapping = 76;
  CpxAll = 0;
  CpxSpecial = 1;
  CpxMappingOnly = 2;
  Uls_Success = 0;
  Uls_Other = $20401;
  Uls_IllegalSequence = $20402;
  Uls_MaxFilesPerProc = $20403;
  Uls_MaxFiles = $20404;
  Uls_NoOp = $20405;
  Uls_TooManyKbd = $20406;
  Uls_KbdNotFound = $20407;
  Uls_BadHandle = $204008;
  Uls_NoDead = $20409;
  Uls_NoScan = $2040A;
  Uls_InvalidScan = $2040B;
  Uls_NotImplemented = $2040C;
  Uls_NoMemory = $2040D;
  Uls_Invalid = $2040E;
  Uls_BadObject = $2040F;
  Uls_NoToken = $20410;
  Uls_NoMatch = $20411;
  Uls_BufferFull = $20412;
  Uls_Range = $20413;
  Uls_Unsupported = $20414;
  Uls_BadAttr = $20415;
  Uls_Version = $20416;
  UConvName: array [0..5] of char = 'UCONV'#0;
  OrdUniCreateUconvObject = 1;
  OrdUniUconvToUcs = 2;
  OrdUniUconvFromUcs = 3;
  OrdUniFreeUconvObject = 4;
  OrdUniQueryUconvObject = 7;
  OrdUniSetUconvObject = 8;
  OrdUniQueryUconvCp = 9;
  OrdUniMapCpToUcsCp = 10;
  OrdUniStrFromUcs = 11;
  OrdUniStrToUcs = 12;
  Ord_UniMalloc = 13;
  Ord_UniFree = 14;
  LibUniName: array [0..6] of char = 'LIBUNI'#0;
  WNull: WideChar = #0;



type
(* CP_UTF16 should be in exceptions too, because OS/2 supports only UCS2 *)
(* rather than UTF-16 - ignored at least for now.                        *)
(*  ExceptionWinCodepages = (CP_UTF16BE, CP_UTF7, 12000 {UTF32}, 12001 {UTF32BE});
  SpecialWinCodepages = (CP_UTF8, CP_ASCII);*)
  TCpRec = record
   WinCP: TSystemCodepage;
   OS2CP: word;
   UConvObj: TUConvObject;
  end;
  TCpXList = array [1..MaxCPMapping] of TCpRec;
  TLocaleObject = pointer;
  TDummyUConvObject = record
   CP: cardinal;
   CPNameLen: byte;
   CPName: record end;
  end;
  PDummyUConvObject = ^TDummyUConvObject;

const
  DefCpRec: TCpRec = (WinCP: 0; OS2CP: 0; UConvObj: nil);
  IBMPrefix: packed array [1..4] of WideChar = 'IBM-';

threadvar
(* Temporary allocations may be performed in parallel in different threads *)
  TempCpRec: TCpRec;


function OS2GetStandardCodePage (const stdcp: TStandardCodePageEnum): TSystemCodePage;
var
  RC, C, RetSize: cardinal;
  NoUConvObject: TUConvObject;
begin
  RC := DosQueryCP (SizeOf (C), @C, RetSize);
  if (RC <> 0) and (RC <> 473) then
   begin
    OSErrorWatch (RC);
    C := 850;
   end;
  OS2GetStandardCodePage := OS2CpToRtlCp (C, cpxMappingOnly, NoUConvObject);
end;


function DummyUniCreateUConvObject (const CpName: PWideChar;
                               var UConv_Object: TUConvObject): longint; cdecl;
var
  P: pointer;
  PW, PCPN: PWideChar;
  S: string [20];
  C: cardinal;
  L: PtrInt;
  I: longint;
  A: array [0..7] of char;
  CPN2: UnicodeString;
  RC, RetSize: cardinal;
begin
  UConv_Object := nil;
  if (CpName = nil) or (CpName^ = #0) then
   begin
    RC := DosQueryCP (SizeOf (C), @C, RetSize);
    if (RC <> 0) and (RC <> 473) then
     begin
      C := 850;
      OSErrorWatch (RC);
     end;
    Str (C, CPN2); (* Str should hopefully not use this function recurrently *)
    L := Length (CPN2);
    Insert (IBMPrefix, CPN2, 1);
    PCPN := @CPN2 [1];
   end
  else
   begin
    PCPN := CpName;
    for I := 0 to 7 do
     if I mod 2 = 0 then
      A [I] := UpCase (PChar (@PCPN [0]) [I])
     else
      A [I] := PChar (@PCPN [0]) [I];
    if PQWord (@A)^ <> PQWord (@IBMPrefix)^ then
     begin
      DummyUniCreateUConvObject := Uls_Invalid;
      Exit;
     end;
    L := 0;
    PW := PCPN + 4;
    while ((PW + L)^ <> #0) and (L <= SizeOf (S)) do
     begin
      S [Succ (L)] := char (Ord ((PW + L)^));
      Inc (L);
     end;
    if L > SizeOf (S) then
     begin
      DummyUniCreateUConvObject := Uls_Other;
      Exit;
     end;
    SetLength (S, L);
    Val (S, C, I);
    if I <> 0 then
     begin
      DummyUniCreateUConvObject := Uls_Invalid;
      Exit;
     end;
   end;
  Inc (L);
  GetMem (P, SizeOf (TDummyUConvObject) + (L + 4) * 2);
  if P = nil then
   DummyUniCreateUConvObject := Uls_NoMemory
  else
   begin
    DummyUniCreateUConvObject := Uls_Success;
    PDummyUConvObject (P)^.CP := C;
    PDummyUConvObject (P)^.CpNameLen := Pred (L) + 4;
    Move (PCPN [0], PDummyUConvObject (P)^.CpName, (L + 4) * 2);
    UConv_Object := TUConvObject (P);
   end;
end;


function DummyUniFreeUConvObject (UConv_Object: TUConvObject): longint; cdecl;
begin
  if UConv_Object <> nil then
   FreeMem (UConv_Object, SizeOf (TDummyUConvObject) +
                       Succ (PDummyUConvObject (UConv_Object)^.CpNameLen) * 2);
  DummyUniFreeUConvObject := Uls_Success;
end;


function DummyUniMapCpToUcsCp (const Codepage: cardinal;
                   CodepageName: PWideChar; const N: cardinal): longint; cdecl;
var
  S: UnicodeString;
  RC, CP, RetSize: cardinal;
begin
  if Codepage = 0 then
   begin
    RC := DosQueryCP (SizeOf (CP), @CP, RetSize);
    if (RC <> 0) and (RC <> 473) then
     begin
      CP := 850;
      OSErrorWatch (RC);
     end;
    Str (CP, S);
   end
  else
   Str (Codepage, S);
  if (N <= Length (S) + 4) or (CodepageName = nil) then
   DummyUniMapCptoUcsCp := Uls_Invalid
  else
   begin
    Move (IBMPrefix, CodepageName^, SizeOf (IBMPrefix));
    Move (S [1], CodepageName [4], Length (S) * SizeOf (WideChar));
    CodepageName [Length (S) + 4] := #0;
    DummyUniMapCpToUcsCp := Uls_Success;
   end;
end;


function DummyUniUConvFromUcs (UConv_Object: TUConvObject;
       var UcsBuf: PWideChar; var UniCharsLeft: longint; var OutBuf: PChar;
         var OutBytesLeft: longint; var NonIdentical: longint): longint; cdecl;
var
  Dest, Dest2: RawByteString;
  NoUConvObj: TUConvObject;
  RtlCp: TSystemCodepage;
  UcsLen: PtrInt;
begin
  if UConv_Object = nil then
   RtlCp := OS2GetStandardCodePage (scpAnsi)
  else
   RtlCp := OS2CpToRtlCp (PDummyUConvObject (UConv_Object)^.CP, cpxMappingOnly,
                                                                   NoUConvObj);
  DefaultUnicode2AnsiMove (UcsBuf, Dest, RtlCp, UniCharsLeft);
  NonIdentical := 1; { Assume at least one substitution with dummy implementation }
  if Length (Dest) > OutBytesLeft then
   begin
    UcsLen := 1;
    repeat
     DefaultUnicode2AnsiMove (UcsBuf, Dest2, RtlCp, UcsLen);
     if Length (Dest2) <= OutBytesLeft then
      begin
       Dest := Dest2;
      end;
     Inc (UcsLen);
    until Length (Dest2) > OutBytesLeft;
    Dec (UcsLen);
    Inc (UcsBuf, UcsLen);
    Dec (UniCharsLeft, UcsLen);
    DummyUniUConvFromUcs := Uls_BufferFull;
   end
  else
   begin
    Inc (UcsBuf, UniCharsLeft);
    UniCharsLeft := 0;
    DummyUniUConvFromUcs := Uls_Success;
   end;
  Move (Dest [1], OutBuf^, Length (Dest));
  Inc (OutBuf, Length (Dest));
  Dec (OutBytesLeft, Length (Dest));
end;


function DummyUniUConvToUcs (UConv_Object: TUConvObject; var InBuf: PChar;
   var InBytesLeft: longint; var UcsBuf: PWideChar; var UniCharsLeft: longint;
                                    var NonIdentical: longint): longint; cdecl;
var
  Dest, Dest2: UnicodeString;
  NoUConvObj: TUConvObject;
  RtlCp: TSystemCodepage;
  SrcLen: PtrInt;
begin
  if UConv_Object = nil then
   RtlCp := OS2GetStandardCodePage (scpAnsi)
  else
   RtlCp := OS2CpToRtlCp (PDummyUConvObject (UConv_Object)^.CP, cpxMappingOnly,
                                                                   NoUConvObj);
  DefaultAnsi2UnicodeMove (InBuf, RtlCp, Dest, InBytesLeft);
  NonIdentical := 0; { Assume no need for substitutions in this direction }
  if Length (Dest) > UniCharsLeft then
   begin
    SrcLen := 1;
    repeat
     DefaultAnsi2UnicodeMove (InBuf, RtlCp, Dest2, SrcLen);
     if Length (Dest2) <= UniCharsLeft then
      begin
       Dest := Dest2;
      end;
     Inc (SrcLen);
    until Length (Dest2) > UniCharsLeft;
    Dec (SrcLen);
    Inc (InBuf, SrcLen);
    Dec (InBytesLeft, SrcLen);
    DummyUniUConvToUcs := Uls_BufferFull; { According to IBM documentation Uls_Invalid and not Uls_BufferFull as returned by UniUConvFromUcs?! }
   end
  else
   begin
    Inc (InBuf, InBytesLeft); { Shall it be increased in case of success too??? }
    InBytesLeft := 0;
    DummyUniUConvToUcs := Uls_Success;
   end;
  Move (Dest [1], UcsBuf^, Length (Dest) * 2);
  Inc (UcsBuf, Length (Dest)); { Shall it be increased in case of success too??? }
  Dec (UniCharsLeft, Length (Dest));
end;



const
  CpXList: TCpXList = (
   (WinCP: CP_UTF8; OS2CP: 1208; UConvObj: nil),
   (WinCP: CP_ASCII; OS2CP: 367; UConvObj: nil),
   (WinCP: 28597; OS2CP: 813; UConvObj: nil),
   (WinCP: 28591; OS2CP: 819; UConvObj: nil),
   (WinCP: 28592; OS2CP: 912; UConvObj: nil),
   (WinCP: 28593; OS2CP: 913; UConvObj: nil),
   (WinCP: 28594; OS2CP: 914; UConvObj: nil),
   (WinCP: 28595; OS2CP: 915; UConvObj: nil),
   (WinCP: 28598; OS2CP: 916; UConvObj: nil),
   (WinCP: 28599; OS2CP: 920; UConvObj: nil),
   (WinCP: 28603; OS2CP: 921; UConvObj: nil),
   (WinCP: 28605; OS2CP: 923; UConvObj: nil),
   (WinCP: 10000; OS2CP: 1275; UConvObj: nil),
   (WinCP: 10006; OS2CP: 1280; UConvObj: nil),
   (WinCP: 10081; OS2CP: 1281; UConvObj: nil),
   (WinCP: 10029; OS2CP: 1282; UConvObj: nil),
   (WinCP: 10007; OS2CP: 1283; UConvObj: nil),
   (WinCP: 20273; OS2CP: 273; UConvObj: nil),
   (WinCP: 20277; OS2CP: 277; UConvObj: nil),
   (WinCP: 20278; OS2CP: 278; UConvObj: nil),
   (WinCP: 20280; OS2CP: 280; UConvObj: nil),
   (WinCP: 20284; OS2CP: 284; UConvObj: nil),
   (WinCP: 20285; OS2CP: 285; UConvObj: nil),
   (WinCP: 20290; OS2CP: 290; UConvObj: nil),
   (WinCP: 20297; OS2CP: 297; UConvObj: nil),
   (WinCP: 20420; OS2CP: 420; UConvObj: nil),
   (WinCP: 20424; OS2CP: 424; UConvObj: nil),
   (WinCP: 20833; OS2CP: 833; UConvObj: nil),
   (WinCP: 20838; OS2CP: 838; UConvObj: nil),
   (WinCP: 20866; OS2CP: 878; UConvObj: nil),
   (WinCP: 737; OS2CP: 851; UConvObj: nil),
   (WinCP: 20924; OS2CP: 924; UConvObj: nil),
   (WinCP: 20932; OS2CP: 932; UConvObj: nil),
   (WinCP: 20936; OS2CP: 936; UConvObj: nil),
   (WinCP: 21025; OS2CP: 1025; UConvObj: nil),
   (WinCP: CP_UTF16; OS2CP: CP_UTF16; UConvObj: nil),
   (WinCP: 37; OS2CP: 37; UConvObj: nil),
   (WinCP: 437; OS2CP: 437; UConvObj: nil),
   (WinCP: 500; OS2CP: 500; UConvObj: nil),
   (WinCP: 850; OS2CP: 850; UConvObj: nil),
   (WinCP: 852; OS2CP: 852; UConvObj: nil),
   (WinCP: 855; OS2CP: 855; UConvObj: nil),
   (WinCP: 857; OS2CP: 857; UConvObj: nil),
   (WinCP: 860; OS2CP: 860; UConvObj: nil),
   (WinCP: 861; OS2CP: 861; UConvObj: nil),
   (WinCP: 862; OS2CP: 862; UConvObj: nil),
   (WinCP: 863; OS2CP: 863; UConvObj: nil),
   (WinCP: 864; OS2CP: 864; UConvObj: nil),
   (WinCP: 865; OS2CP: 865; UConvObj: nil),
   (WinCP: 866; OS2CP: 866; UConvObj: nil),
   (WinCP: 869; OS2CP: 869; UConvObj: nil),
   (WinCP: 870; OS2CP: 870; UConvObj: nil),
   (WinCP: 874; OS2CP: 874; UConvObj: nil),
   (WinCP: 875; OS2CP: 875; UConvObj: nil),
   (WinCP: 949; OS2CP: 949; UConvObj: nil),
   (WinCP: 950; OS2CP: 950; UConvObj: nil),
   (WinCP: 1026; OS2CP: 1026; UConvObj: nil),
   (WinCP: 1047; OS2CP: 1047; UConvObj: nil),
   (WinCP: 1140; OS2CP: 1140; UConvObj: nil),
   (WinCP: 1141; OS2CP: 1141; UConvObj: nil),
   (WinCP: 1142; OS2CP: 1142; UConvObj: nil),
   (WinCP: 1143; OS2CP: 1143; UConvObj: nil),
   (WinCP: 1144; OS2CP: 1144; UConvObj: nil),
   (WinCP: 1145; OS2CP: 1145; UConvObj: nil),
   (WinCP: 1146; OS2CP: 1146; UConvObj: nil),
   (WinCP: 1147; OS2CP: 1147; UConvObj: nil),
   (WinCP: 1148; OS2CP: 1148; UConvObj: nil),
   (WinCP: 1149; OS2CP: 1149; UConvObj: nil),
   (WinCP: 1250; OS2CP: 1250; UConvObj: nil),
   (WinCP: 1251; OS2CP: 1251; UConvObj: nil),
   (WinCP: 1252; OS2CP: 1252; UConvObj: nil),
   (WinCP: 1253; OS2CP: 1253; UConvObj: nil),
   (WinCP: 1254; OS2CP: 1254; UConvObj: nil),
   (WinCP: 1255; OS2CP: 1255; UConvObj: nil),
   (WinCP: 1256; OS2CP: 1256; UConvObj: nil),
   (WinCP: 1257; OS2CP: 1257; UConvObj: nil)
   );

(* Possibly add index tables for both directions and binary search??? *)


function UConvObjectForCP (CP: cardinal; var UConvObj: TUConvObject): longint;
var
  RC: longint;
  A: array [0..12] of WideChar;
begin
  UConvObj := nil;
  RC := Sys_UniMapCpToUcsCp (CP, @A, 12);
  if RC = 0 then
   RC := Sys_UniCreateUconvObject (@A, UConvObj);
{$WARNING: TODO: Deallocate some previously allocated UConvObj and try again if failed}
  UConvObjectForCP := RC;
  if RC <> 0 then
   OSErrorWatch (RC);
end;


function OS2CPtoRtlCP (CP: cardinal; ReqFlags: byte;
                                  var UConvObj: TUConvObject): TSystemCodepage;
var
  I, I2: cardinal;
  RCI: longint;
begin
  OS2CPtoRtlCP := TSystemCodePage (CP);
  UConvObj := nil;
  if not UniAPI then (* No UniAPI => no need for UConvObj *)
   ReqFlags := ReqFlags or CpxMappingOnly;
  if CP = DefCpRec.OS2CP then
   begin
    if RTLUsesWinCP then
     OS2CPtoRtlCP := DefCpRec.WinCP;
    if ReqFlags and CpxMappingOnly = 0 then
     UConvObj := DefCpRec.UConvObj;
   end
  else
   begin
    I := 1;
    if ReqFlags and CpxSpecial = CpxSpecial then
     I2 := 2
    else
     if ReqFlags and CpxMappingOnly = CpxMappingOnly then
      I2 := MaxNonEqualCPMapping
     else
      I2 := MaxCPMapping;
    while I <= I2 do
     begin
      if CP = CpXList [I].OS2CP then
       begin
        if RTLUsesWinCP then
         OS2CPtoRtlCP := CpXList [I].WinCP;
        if ReqFlags and CpxMappingOnly = 0 then
         begin
          if CpXList [I].UConvObj = nil then
           begin
            if UConvObjectForCP (CpXList [I].OS2CP, UConvObj) = Uls_Success
                                                                           then
             CpXList [I].UConvObj := UConvObj
            else
             UConvObj := nil;
           end
          else
           UConvObj := CpXList [I].UConvObj;
         end;
        Exit;
       end;
      Inc (I);
     end;
(* If codepage was not found in the translation table and UConvObj is
   requested, allocate one in the temporary record. *)
    if ReqFlags and CpxMappingOnly = 0 then
     begin
      if TempCpRec.OS2CP = CP then
       UConvObj := TempCpRec.UConvObj
      else
       begin
        if TempCpRec.UConvObj <> nil then
         begin
          RCI := Sys_UniFreeUConvObject (TempCpRec.UConvObj);
          if RCI <> 0 then
           OSErrorWatch (cardinal (RCI));
         end;
        if UConvObjectForCP (CP, UConvObj) = Uls_Success then
         begin
          TempCpRec.UConvObj := UConvObj;
          TempCpRec.OS2CP := CP;
         end
        else
         UConvObj := nil;
       end;
     end;
   end;
end;


function RtlCPtoOS2CP (RtlCP: TSystemCodepage; ReqFlags: byte;
                                         var UConvObj: TUConvObject): cardinal;
var
  I, I2: cardinal;
begin
  RtlCPtoOS2CP := RtlCP;
  UConvObj := nil;
  if not UniAPI then (* No UniAPI => no need for UConvObj *)
   ReqFlags := ReqFlags or CpxMappingOnly;
  if not (RTLUsesWinCP) then
   begin
    if ReqFlags and CpxMappingOnly = 0 then
     OS2CPtoRtlCP (cardinal (RtlCp), ReqFlags, UConvObj);
   end
  else if RtlCP = DefCpRec.WinCP then
   begin
    RtlCPtoOS2CP := DefCpRec.WinCP;
    if ReqFlags and CpxMappingOnly = 0 then
     UConvObj := DefCpRec.UConvObj;
   end
  else
   begin
    I := 1;
    if ReqFlags and CpxSpecial = CpxSpecial then
     I2 := 2
    else
     if ReqFlags and CpxMappingOnly = CpxMappingOnly then
      I2 := MaxNonEqualCPMapping
     else
      I2 := MaxCPMapping;
    while I <= I2 do
     begin
      if RtlCP = CpXList [I].WinCP then
       begin
        RtlCPtoOS2CP := CpXList [I].OS2CP;
        if ReqFlags and CpxMappingOnly = 0 then
         begin
           begin
            if UConvObjectForCP (CpXList [I].OS2CP, UConvObj) = Uls_Success then
             CpXList [I].UConvObj := UConvObj
            else
             UConvObj := nil;
           end
         end;
        Exit;
       end;
      Inc (I);
     end;
(*
Special processing for
 ExceptionWinCodepages = (CP_UTF16BE, CP_UTF7, 12000 {UTF32}, 12001 {UTF32BE})
might be added here...or not ;-)

       if (TempCpRec.OS2CP <> High (TempCpRec.OS2CP)) or
                                                (TempCpRec.WinCP <> RtlCp) then
        begin
         if TempCpRec.UConvObj <> nil then
          begin
           RCI := Sys_UniFreeUConvObject (TempCpRec.UConvObj);
           if RCI <> 0 then
            OSErrorWatch (cardinal (RCI));
          end;
         TempCpRec.OS2CP := High (TempCpRec.OS2CP);
         TempCpRec.WinCP := RtlCp;
        end;

  Map to CP_ASCII aka OS2CP=367 if RtlCP not recognized and UConvObject
  is requested???
*)

(* Signalize unrecognized (untranslatable) MS Windows codepage *)
    OSErrorWatch (Uls_Invalid);            
   end;
end;


function OS2CPtoRtlCP (CP: cardinal; ReqFlags: byte): TSystemCodepage;
var
  NoUConvObj: TUConvObject;
begin
  if RtlUsesWinCP then
   OS2CPtoRtlCP := OS2CPtoRtlCP (CP, ReqFlags or CpxMappingOnly, NoUConvObj)
  else
   OS2CPtoRtlCP := TSystemCodepage (CP);
end;


function RtlCPtoOS2CP (RtlCP: TSystemCodepage; ReqFlags: byte): cardinal;
var
  NoUConvObj: TUConvObject;
begin
  if RtlUsesWinCP then
   RtlCPtoOS2CP := RtlCPtoOS2CP (RtlCP, ReqFlags or CpxMappingOnly, NoUConvObj)
  else
   RtlCPtoOS2CP := RtlCP;
end;


procedure OS2Unicode2AnsiMove (Source: PUnicodeChar; var Dest: RawByteString;
                                            CP: TSystemCodePage; Len: SizeInt);
var
  RCI: longint;
  UConvObj: TUConvObject;
  OS2CP: cardinal;
  Src2: PUnicodeChar;
  Len2, LenOut, OutOffset, NonIdentical: longint;
  Dest2: PChar;
begin
  OS2CP := RtlCpToOS2CP (CP, CpxAll, UConvObj);
{  if UniAPI and (UConvObj = nil) then  - OS2Unicode2AnsiMove should be never called if not UniAPI }
  if UConvObj = nil then
   begin
{$WARNING Special cases like UTF-7 should be handled here, otherwise signalize error - how???}

    DefaultUnicode2AnsiMove (Source, Dest, CP, Len);
    Exit;
   end;
  LenOut := Succ (Len); (* Standard OS/2 CP is a SBCS *)
  SetLength (Dest, LenOut);
  SetCodePage (Dest, CP, false);
  Src2 := Source;
  Len2 := Len;
  Dest2 := PChar (Dest);
  RCI := Sys_UniUConvFromUcs (UConvObj, Src2, Len2, Dest2, LenOut,
                                                                 NonIdentical);
  repeat
   case RCI of
    Uls_Success:
     begin
      if LenOut > 0 then
       SetLength (Dest, Length (Dest) - LenOut);
      Break;
     end;
    Uls_IllegalSequence:
     begin
      OSErrorWatch (Uls_IllegalSequence);
      { skip and set to '?' }
      Inc (Src2);
      Dec (Len2);
      Dest2^ := '?';
      Inc (Dest2);
      Dec (LenOut);
     end;
    Uls_BufferFull:
     begin
      OutOffset := Dest2 - PChar (Dest);
(* Use Len2 or Len decreased by difference between Source and Src2? *)
(* Extend more this time - target is probably a DBCS or UTF-8 *)
      SetLength (Dest, Length (Dest) + Succ (Len2 * 2));
      { string could have been moved }
      Dest2 := PChar (Dest) + OutOffset;
      Inc (LenOut, Succ (Len2 * 2));
     end
    else
     begin
      SetLength (Dest, 0);
      OSErrorWatch (cardinal (RCI));
      { Break }
      RunError (231);
     end;
    end;
   RCI := Sys_UniUConvFromUcs (UConvObj, Src2, Len2, Dest2, LenOut,
                                                                 NonIdentical);
  until false;
end;

procedure OS2Ansi2UnicodeMove (Source: PChar; CP: TSystemCodePage;
                                        var Dest: UnicodeString; Len: SizeInt);
var
  RCI: longint;
  UConvObj: TUConvObject;
  OS2CP: cardinal;
  Src2: PChar;
  Len2, LenOut, OutOffset, NonIdentical: longint;
  Dest2: PWideChar;
begin
  OS2CP := RtlCpToOS2CP (CP, CpxAll, UConvObj);
{  if UniAPI and (UConvObj = nil) then  - OS2Unicode2AnsiMove should be never called if not UniAPI }
  if UConvObj = nil then
   begin
{$WARNING Special cases like UTF-7 should be handled here, otherwise signalize error - how???}

    DefaultAnsi2UnicodeMove (Source, CP, Dest, Len);
    Exit;
   end;

  LenOut := Succ (Len); (* Standard OS/2 CP is a SBCS *)
  SetLength (Dest, LenOut);
  Src2 := Source;
  Len2 := Len;
  Dest2 := PWideChar (Dest);

  RCI := Sys_UniUConvToUcs (UConvObj, Src2, Len2, Dest2, LenOut, NonIdentical);
  repeat
   case RCI of
    Uls_Success:
     begin
      if LenOut > 0 then
       SetLength (Dest, Length (Dest) - LenOut);
      Break;
     end;
    Uls_IllegalSequence:
     begin
      OSErrorWatch (Uls_IllegalSequence);
      { skip and set to '?' }
      Inc (Src2);
      Dec (Len2);
      Dest2^ := '?';
      Inc (Dest2);
      Dec (LenOut);
     end;
    Uls_BufferFull:
     begin
      OutOffset := Dest2 - PWideChar (Dest);
(* Use Len2 or Len decreased by difference between Source and Src2? *)
      SetLength (Dest, Length (Dest) + Succ (Len2));
      { string could have been moved }
      Dest2 := PWideChar (Dest) + OutOffset;
      Inc (LenOut, Succ (Len2));
     end
    else
     begin
      SetLength (Dest, 0);
      OSErrorWatch (cardinal (RCI));
      { Break }
      RunError (231);
     end;
    end;
   RCI := Sys_UniUConvToUcs (UConvObj, Src2, Len2, Dest2, LenOut,
                                                                 NonIdentical);
  until false;

{???
        PUnicodeRec(pointer(dest)-UnicodeFirstOff)^.CodePage:=CP_UTF16;
}
end;


function RtlChangeCP (CP: TSystemCodePage): longint;
var
  OS2CP, I: cardinal;
  NoUConvObj: TUConvObject;
  RCI: longint;
begin
  OS2CP := RtlCpToOS2Cp (CP, cpxMappingOnly, NoUConvObj);
  RtlChangeCP := longint (DosSetProcessCP (OS2CP));
  if RtlChangeCP <> 0 then
   OSErrorWatch (RtlChangeCP)
  else
   begin
    DefaultSystemCodePage := CP;
    DefaultRTLFileSystemCodePage := DefaultSystemCodePage;
    DefaultFileSystemCodePage := DefaultSystemCodePage;

    if OS2CP <> DefCpRec.OS2CP then
     begin
      if DefCpRec.UConvObj <> nil then
       begin
        RCI := Sys_UniFreeUConvObject (DefCpRec.UConvObj);
        if RCI <> 0 then
         OSErrorWatch (cardinal (RCI));
        DefCpRec.UConvObj := nil;
       end;
      DefCPRec.OS2CP := OS2CP;
      RCI := Sys_UniCreateUConvObject (@WNull, DefCpRec.UConvObj);
      if RCI <> 0 then
       OSErrorWatch (cardinal (RCI));
(* Find out WinCP _without_ considering RtlUsesWinCP *)
      I := 1;
      while (I <= MaxNonEqualCPMapping) and (CpXList [I].OS2CP <> OS2CP) do
       Inc (I);
      if CpXList [I].OS2CP = OS2CP then
       DefCpRec.WinCP := CpXList [I].WinCP
      else
       DefCpRec.WinCP := OS2CP;
     end;
   end;
end;

{
function Win32UnicodeUpper(const s : UnicodeString) : UnicodeString;
  begin
    result:=s;
    UniqueString(result);
    if length(result)>0 then
      CharUpperBuff(LPWSTR(result),length(result));
  end;


function Win32UnicodeLower(const s : UnicodeString) : UnicodeString;
  begin
    result:=s;
    UniqueString(result);
    if length(result)>0 then
      CharLowerBuff(LPWSTR(result),length(result));
  end;
}


(*
CWSTRING:

function LowerWideString(const s : WideString) : WideString;
  var
    i : SizeInt;
  begin
    SetLength(result,length(s));
    for i:=0 to length(s)-1 do
      pwidechar(result)[i]:=WideChar(towlower(wint_t(s[i+1])));
  end;


function UpperWideString(const s : WideString) : WideString;
  var
    i : SizeInt;
  begin
    SetLength(result,length(s));
    for i:=0 to length(s)-1 do
      pwidechar(result)[i]:=WideChar(towupper(wint_t(s[i+1])));
  end;


procedure EnsureAnsiLen(var S: AnsiString; const len: SizeInt); inline;
begin
  if (len>length(s)) then
    if (length(s) < 10*256) then
      setlength(s,length(s)+10)
    else
      setlength(s,length(s)+length(s) shr 8);
end;


procedure ConcatCharToAnsiStr(const c: char; var S: AnsiString; var index: SizeInt);
begin
  EnsureAnsiLen(s,index);
  pchar(@s[index])^:=c;
  inc(index);
end;


{ concatenates an utf-32 char to a widestring. S *must* be unique when entering. }
{$ifndef beos}
procedure ConcatUTF32ToAnsiStr(const nc: wint_t; var S: AnsiString; var index: SizeInt; var mbstate: mbstate_t);
{$else not beos}
procedure ConcatUTF32ToAnsiStr(const nc: wint_t; var S: AnsiString; var index: SizeInt);
{$endif beos}
var
  p     : pchar;
  mblen : size_t;
begin
  { we know that s is unique -> avoid uniquestring calls}
  p:=@s[index];
  if (nc<=127) then
    ConcatCharToAnsiStr(char(nc),s,index)
  else
    begin
      EnsureAnsiLen(s,index+MB_CUR_MAX);
{$ifndef beos}
      mblen:=wcrtomb(p,wchar_t(nc),@mbstate);
{$else not beos}
      mblen:=wctomb(p,wchar_t(nc));
{$endif not beos}
      if (mblen<>size_t(-1)) then
        inc(index,mblen)
      else
        begin
          { invalid wide char }
          p^:='?';
          inc(index);
        end;
    end;
end;


function LowerAnsiString(const s : AnsiString) : AnsiString;
  var
    i, slen,
    resindex : SizeInt;
    mblen    : size_t;
{$ifndef beos}
    ombstate,
    nmbstate : mbstate_t;
{$endif beos}
    wc       : wchar_t;
  begin
{$ifndef beos}
    fillchar(ombstate,sizeof(ombstate),0);
    fillchar(nmbstate,sizeof(nmbstate),0);
{$endif beos}
    slen:=length(s);
    SetLength(result,slen+10);
    i:=1;
    resindex:=1;
    while (i<=slen) do
      begin
        if (s[i]<=#127) then
          begin
            wc:=wchar_t(s[i]);
            mblen:= 1;
          end
        else
{$ifndef beos}
          mblen:=mbrtowc(@wc, pchar(@s[i]), slen-i+1, @ombstate);
{$else not beos}
          mblen:=mbtowc(@wc, pchar(@s[i]), slen-i+1);
{$endif not beos}
        case mblen of
          size_t(-2):
            begin
              { partial invalid character, copy literally }
              while (i<=slen) do
                begin
                  ConcatCharToAnsiStr(s[i],result,resindex);
                  inc(i);
                end;
            end;
          size_t(-1), 0:
            begin
              { invalid or null character }
              ConcatCharToAnsiStr(s[i],result,resindex);
              inc(i);
            end;
          else
            begin
              { a valid sequence }
              { even if mblen = 1, the lowercase version may have a }
              { different length                                     }
              { We can't do anything special if wchar_t is 16 bit... }
{$ifndef beos}
              ConcatUTF32ToAnsiStr(towlower(wint_t(wc)),result,resindex,nmbstate);
{$else not beos}
              ConcatUTF32ToAnsiStr(towlower(wint_t(wc)),result,resindex);
{$endif not beos}
              inc(i,mblen);
            end;
          end;
      end;
    SetLength(result,resindex-1);
  end;


function UpperAnsiString(const s : AnsiString) : AnsiString;
  var
    i, slen,
    resindex : SizeInt;
    mblen    : size_t;
{$ifndef beos}
    ombstate,
    nmbstate : mbstate_t;
{$endif beos}
    wc       : wchar_t;
  begin
{$ifndef beos}
    fillchar(ombstate,sizeof(ombstate),0);
    fillchar(nmbstate,sizeof(nmbstate),0);
{$endif beos}
    slen:=length(s);
    SetLength(result,slen+10);
    i:=1;
    resindex:=1;
    while (i<=slen) do
      begin
        if (s[i]<=#127) then
          begin
            wc:=wchar_t(s[i]);
            mblen:= 1;
          end
        else
{$ifndef beos}
          mblen:=mbrtowc(@wc, pchar(@s[i]), slen-i+1, @ombstate);
{$else not beos}
          mblen:=mbtowc(@wc, pchar(@s[i]), slen-i+1);
{$endif beos}
        case mblen of
          size_t(-2):
            begin
              { partial invalid character, copy literally }
              while (i<=slen) do
                begin
                  ConcatCharToAnsiStr(s[i],result,resindex);
                  inc(i);
                end;
            end;
          size_t(-1), 0:
            begin
              { invalid or null character }
              ConcatCharToAnsiStr(s[i],result,resindex);
              inc(i);
            end;
          else
            begin
              { a valid sequence }
              { even if mblen = 1, the uppercase version may have a }
              { different length                                     }
              { We can't do anything special if wchar_t is 16 bit... }
{$ifndef beos}
              ConcatUTF32ToAnsiStr(towupper(wint_t(wc)),result,resindex,nmbstate);
{$else not beos}
              ConcatUTF32ToAnsiStr(towupper(wint_t(wc)),result,resindex);
{$endif not beos}
              inc(i,mblen);
            end;
          end;
      end;
    SetLength(result,resindex-1);
  end;


function utf16toutf32(const S: WideString; const index: SizeInt; out len: longint): UCS4Char; external name 'FPC_UTF16TOUTF32';

function WideStringToUCS4StringNoNulls(const s : WideString) : UCS4String;
  var
    i, slen,
    destindex : SizeInt;
    len       : longint;
    uch       : UCS4Char;
  begin
    slen:=length(s);
    setlength(result,slen+1);
    i:=1;
    destindex:=0;
    while (i<=slen) do
      begin
        uch:=utf16toutf32(s,i,len);
        if (uch=UCS4Char(0)) then
          uch:=UCS4Char(32);
        result[destindex]:=uch;
        inc(destindex);
        inc(i,len);
      end;
    result[destindex]:=UCS4Char(0);
    { destindex <= slen }
    setlength(result,destindex+1);
  end;


function CompareWideString(const s1, s2 : WideString) : PtrInt;
  var
    hs1,hs2 : UCS4String;
  begin
    { wcscoll interprets null chars as end-of-string -> filter out }
    hs1:=WideStringToUCS4StringNoNulls(s1);
    hs2:=WideStringToUCS4StringNoNulls(s2);
    result:=wcscoll(pwchar_t(hs1),pwchar_t(hs2));
  end;


function CompareTextWideString(const s1, s2 : WideString): PtrInt;
  begin
    result:=CompareWideString(UpperWideString(s1),UpperWideString(s2));
  end;


function CharLengthPChar(const Str: PChar): PtrInt;
  var
    nextlen: ptrint;
    s: pchar;
{$ifndef beos}
    mbstate: mbstate_t;
{$endif not beos}
  begin
    result:=0;
    s:=str;
{$ifndef beos}
    fillchar(mbstate,sizeof(mbstate),0);
{$endif not beos}
    repeat
{$ifdef beos}
      nextlen:=ptrint(mblen(str,MB_CUR_MAX));
{$else beos}
      nextlen:=ptrint(mbrlen(str,MB_CUR_MAX,@mbstate));
{$endif beos}
      { skip invalid/incomplete sequences }
      if (nextlen<0) then
        nextlen:=1;
      inc(result,nextlen);
      inc(s,nextlen);
    until (nextlen=0);
  end;


function CodePointLength(const Str: PChar; maxlookahead: ptrint): PtrInt;
  var
    nextlen: ptrint;
{$ifndef beos}
    mbstate: mbstate_t;
{$endif not beos}
  begin
{$ifdef beos}
    result:=ptrint(mblen(str,maxlookahead));
{$else beos}
    fillchar(mbstate,sizeof(mbstate),0);
    result:=ptrint(mbrlen(str,maxlookahead,@mbstate));
    { mbrlen can also return -2 for "incomplete but potially valid character
      and data has been processed" }
    if result<0 then
      result:=-1;
{$endif beos}
  end;


function StrCompAnsiIntern(s1,s2 : PChar; len1, len2: PtrInt; canmodifys1, canmodifys2: boolean): PtrInt;
  var
    a,b: pchar;
    i: PtrInt;
  begin
    if not(canmodifys1) then
      getmem(a,len1+1)
    else
      a:=s1;
    for i:=0 to len1-1 do
      if s1[i]<>#0 then
        a[i]:=s1[i]
      else
        a[i]:=#32;
    a[len1]:=#0;

    if not(canmodifys2) then
      getmem(b,len2+1)
    else
      b:=s2;
    for i:=0 to len2-1 do
      if s2[i]<>#0 then
        b[i]:=s2[i]
      else
        b[i]:=#32;
    b[len2]:=#0;
    result:=strcoll(a,b);
    if not(canmodifys1) then
      freemem(a);
    if not(canmodifys2) then
      freemem(b);
  end;


function CompareStrAnsiString(const s1, s2: ansistring): PtrInt;
  begin
    result:=StrCompAnsiIntern(pchar(s1),pchar(s2),length(s1),length(s2),false,false);
  end;


function StrCompAnsi(s1,s2 : PChar): PtrInt;
  begin
    result:=strcoll(s1,s2);
  end;


function AnsiCompareText(const S1, S2: ansistring): PtrInt;
  var
    a, b: AnsiString;
  begin
    a:=UpperAnsistring(s1);
    b:=UpperAnsistring(s2);
    result:=StrCompAnsiIntern(pchar(a),pchar(b),length(a),length(b),true,true);
  end;


function AnsiStrIComp(S1, S2: PChar): PtrInt;
  begin
    result:=AnsiCompareText(ansistring(s1),ansistring(s2));
  end;


function AnsiStrLComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
  var
    a, b: pchar;
begin
  if (maxlen=0) then
    exit(0);
  if (s1[maxlen]<>#0) then
    begin
      getmem(a,maxlen+1);
      move(s1^,a^,maxlen);
      a[maxlen]:=#0;
    end
  else
    a:=s1;
  if (s2[maxlen]<>#0) then
    begin
      getmem(b,maxlen+1);
      move(s2^,b^,maxlen);
      b[maxlen]:=#0;
    end
  else
    b:=s2;
  result:=StrCompAnsiIntern(a,b,maxlen,maxlen,a<>s1,b<>s2);
  if (a<>s1) then
    freemem(a);
  if (b<>s2) then
    freemem(b);
end;


function AnsiStrLIComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
  var
    a, b: ansistring;
begin
  if (maxlen=0) then
    exit(0);
  setlength(a,maxlen);
  move(s1^,a[1],maxlen);
  setlength(b,maxlen);
  move(s2^,b[1],maxlen);
  result:=AnsiCompareText(a,b);
end;


procedure ansi2pchar(const s: ansistring; const orgp: pchar; out p: pchar);
var
  newlen: sizeint;
begin
  newlen:=length(s);
  if newlen>strlen(orgp) then
    fpc_rangeerror;
  p:=orgp;
  if (newlen>0) then
    move(s[1],p[0],newlen);
  p[newlen]:=#0;
end;


function AnsiStrLower(Str: PChar): PChar;
var
  temp: ansistring;
begin
  temp:=loweransistring(str);
  ansi2pchar(temp,str,result);
end;


function AnsiStrUpper(Str: PChar): PChar;
var
  temp: ansistring;
begin
  temp:=upperansistring(str);
  ansi2pchar(temp,str,result);
end;

{$ifdef FPC_HAS_CPSTRING}
{$i textrec.inc}
procedure SetStdIOCodePage(var T: Text); inline;
begin
  case TextRec(T).Mode of
    fmInput:TextRec(T).CodePage:=GetStandardCodePage(scpConsoleInput);
    fmOutput:TextRec(T).CodePage:=GetStandardCodePage(scpConsoleOutput);
  end;
end;

procedure SetStdIOCodePages; inline;
begin
  SetStdIOCodePage(Input);
  SetStdIOCodePage(Output);
  SetStdIOCodePage(ErrOutput);
  SetStdIOCodePage(StdOut);
  SetStdIOCodePage(StdErr);
end;
{$endif FPC_HAS_CPSTRING}
*)

procedure InitOS2WideStringManager; inline;
var
  RC: cardinal;
  ErrName: array [0..MaxPathLen] of char;
  P: pointer;
begin
  RC := DosLoadModule (@ErrName [0], SizeOf (ErrName), @UConvName [0],
                                                                  UConvHandle);
  if RC = 0 then
   begin
    RC := DosQueryProcAddr (UConvHandle, OrdUniCreateUConvObject, nil, P);
    if RC = 0 then
     begin
      Sys_UniCreateUConvObject := TUniCreateUConvObject (P);
      RC := DosQueryProcAddr (UConvHandle, OrdUniMapCpToUcsCp, nil, P);
      if RC = 0 then
       begin
        Sys_UniMapCpToUcsCp := TUniMapCpToUcsCp (P);
        RC := DosQueryProcAddr (UConvHandle, OrdUniFreeUConvObject, nil, P);
        if RC = 0 then
         begin
          Sys_UniFreeUConvObject := TUniFreeUConvObject (P);
          RC := DosQueryProcAddr (UConvHandle, OrdUniUConvFromUcs, nil, P);
          if RC = 0 then
           begin
            Sys_UniUConvFromUcs := TUniUConvFromUcs (P);
            RC := DosQueryProcAddr (UConvHandle, OrdUniUConvToUcs, nil, P);
            if RC = 0 then
             begin
              Sys_UniUConvToUcs := TUniUConvToUcs (P);

              UniAPI := true;
             end;
           end;
         end;
       end;
     end;
   end;
  if RC <> 0 then
   OSErrorWatch (RC);
  if not (UniAPI) then
   begin
    Sys_UniCreateUConvObject := @DummyUniCreateUConvObject;
    Sys_UniMapCpToUcsCp := @DummyUniMapCpToUcsCp;
    Sys_UniFreeUConvObject := @DummyUniFreeUConvObject;
    Sys_UniUConvFromUcs := @DummyUniUConvFromUcs;
    Sys_UniUConvToUcs := @DummyUniUConvToUcs;

   end;

    { Widestring }
  WideStringManager.Wide2AnsiMoveProc := @OS2Unicode2AnsiMove;
  WideStringManager.Ansi2WideMoveProc := @OS2Ansi2UnicodeMove;
{  WideStringManager.UpperWideStringProc := @OS2UnicodeUpper;
  WideStringManager.LowerWideStringProc := @OS2UnicodeLower;}
    { Unicode }
  WideStringManager.Unicode2AnsiMoveProc := @OS2Unicode2AnsiMove;
  WideStringManager.Ansi2UnicodeMoveProc := @OS2Ansi2UnicodeMove;
{  WideStringManager.UpperUnicodeStringProc := @OS2UnicodeUpper;
  WideStringManager.LowerUnicodeStringProc := @OS2UnicodeLower;}
    { Codepage }
  WideStringManager.GetStandardCodePageProc := @OS2GetStandardCodePage;
(*
      Wide2AnsiMoveProc:=@Wide2AnsiMove;
      Ansi2WideMoveProc:=@Ansi2WideMove;

      UpperWideStringProc:=@UpperWideString;
      LowerWideStringProc:=@LowerWideString;

      CompareWideStringProc:=@CompareWideString;
      CompareTextWideStringProc:=@CompareTextWideString;

      CharLengthPCharProc:=@CharLengthPChar;
      CodePointLengthProc:=@CodePointLength;

      UpperAnsiStringProc:=@UpperAnsiString;
      LowerAnsiStringProc:=@LowerAnsiString;
      CompareStrAnsiStringProc:=@CompareStrAnsiString;
      CompareTextAnsiStringProc:=@AnsiCompareText;
      StrCompAnsiStringProc:=@StrCompAnsi;
      StrICompAnsiStringProc:=@AnsiStrIComp;
      StrLCompAnsiStringProc:=@AnsiStrLComp;
      StrLICompAnsiStringProc:=@AnsiStrLIComp;
      StrLowerAnsiStringProc:=@AnsiStrLower;
      StrUpperAnsiStringProc:=@AnsiStrUpper;
      ThreadInitProc:=@InitThread;
      ThreadFiniProc:=@FiniThread;
      { Unicode }
      Unicode2AnsiMoveProc:=@Wide2AnsiMove;
      Ansi2UnicodeMoveProc:=@Ansi2WideMove;
      UpperUnicodeStringProc:=@UpperWideString;
      LowerUnicodeStringProc:=@LowerWideString;
      CompareUnicodeStringProc:=@CompareWideString;
      CompareTextUnicodeStringProc:=@CompareTextWideString;
*)
end;
