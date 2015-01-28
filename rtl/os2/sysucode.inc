{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014-2015 by Tomas Hajny and other members
    of the Free Pascal development team.

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
  Uls_API_Error_Base = $20400;
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
  OrdUniQueryXdigit = 1;
  OrdUniQuerySpace = 2;
  OrdUniQueryPrint = 3;
  OrdUniQueryGraph = 4;
  OrdUniQueryCntrl = 5;
  OrdUniQueryAlpha = 6;
  OrdUniFreeAttrObject = 7;
  OrdUniQueryCharAttr = 8;
  OrdUniQueryUpper = 9;
  OrdUniQueryPunct = 10;
  OrdUniQueryLower = 11;
  OrdUniQueryDigit = 12;
  OrdUniQueryBlank = 13;
  OrdUniQueryAlnum = 14;
  OrdUniScanForAttr = 15;
  OrdUniCreateAttrObject = 16;
  OrdUniCreateTransformObject = 17;
  OrdUniFreeTransformObject = 18;
  OrdUniQueryLocaleObject = 19;
  OrdUniCreateLocaleObject = 20;
  OrdUniFreeLocaleObject = 21;
  OrdUniFreeMem = 22;
  OrdUniFreeLocaleInfo = 28;
  OrdUniQueryLocaleInfo = 29;
  OrdUniQueryLocaleItem = 30;
  OrdUniStrcat = 31;
  OrdUniStrchr = 32;
  OrdUniStrcmp = 33;
  OrdUniStrcmpi = 34;
  OrdUniStrColl = 35;
  OrdUniStrcpy = 36;
  OrdUniStrcspn = 37;
  OrdUniStrfmon = 38;
  OrdUniStrftime = 39;
  OrdUniStrlen = 40;
  OrdUniStrncat = 41;
  OrdUniStrncmp = 42;
  OrdUniStrncmpi = 43;
  OrdUniStrncpy = 44;
  OrdUniStrpbrk = 45;
  OrdUniStrptime = 46;
  OrdUniStrrchr = 47;
  OrdUniStrspn = 48;
  OrdUniStrstr = 49;
  OrdUniStrtod = 50;
  OrdUniStrtol = 51;
  OrdUniStrtoul = 52;
  OrdUniStrxfrm = 53;
  OrdUniLocaleStrToToken = 54;
  OrdUniLocaleTokenToStr = 55;
  OrdUniTransformStr = 56;
  OrdUniTransLower = 57;
  OrdUniTransUpper = 58;
  OrdUniTolower = 59;
  OrdUniToupper = 60;
  OrdUniStrupr = 61;
  OrdUniStrlwr = 62;
  OrdUniStrtok = 63;
  OrdUniMapCtryToLocale = 67;
  OrdUniMakeKey = 70;
  OrdUniQueryChar = 71;
  OrdUniGetOverride = 72;
  OrdUniGetColval = 73;
  OrdUniQueryAttr = 74;
  OrdUniQueryStringType = 75;
  OrdUniQueryCharType = 76;
  OrdUniQueryNumericValue = 77;
  OrdUniQueryCharTypeTable = 78;
  OrdUniProcessUconv = 80;
  OrdLocale = 151;
  OrdUniMakeUserLocale = 152;
  OrdUniSetUserLocaleItem = 153;
  OrdUniDeleteUserLocale = 154;
  OrdUniCompleteUserLocale = 155;
  OrdUniQueryLocaleValue = 156;
  OrdUniQueryLocaleList = 157;
  OrdUniQueryLanguageName = 158;
  OrdUniQueryCountryName = 159;
  Uni_Token_Pointer = 1;
  Uni_MBS_String_Pointer = 2;
  Uni_UCS_String_Pointer = 3;
  Uni_System_Locales = 1;
  Uni_User_Locales = 2;
  WNull: WideChar = #0;
  WUniv: array [0..4] of WideChar = 'UNIV'#0;



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
  TDummyUConvObject = record
   CP: cardinal;
   CPNameLen: byte;
   CPName: record end;
  end;
  PDummyUConvObject = ^TDummyUConvObject;


var
  DBCSLeadRanges: array [0..11] of char;
  CollationSequence: array [char] of char;


const
  DefCpRec: TCpRec = (WinCP: 0; OS2CP: 0; UConvObj: nil);
  InInitDefaultCP: int64 = -1; (* Range is bigger than TThreadID to avoid conflict *)
  DefLocObj: TLocaleObject = nil;
  IBMPrefix: packed array [1..4] of WideChar = 'IBM-';
  CachedDefFSCodepage: TSystemCodepage = 0;
  EmptyCC: TCountryCode = (Country: 0; Codepage: 0); (* Empty = current *)
  (* 819 = IBM codepage number for ISO 8859-1 used in FPC default *)
  (* dummy translation between UnicodeString and AnsiString.      *)
  IsoCC: TCountryCode = (Country: 1; Codepage: 819); (* Empty = current *)
  (* The following two arrays are initialized on startup in case that *)
  (* Dummy* routines must be used. First for current codepage...      *)
  DBCSLeadRangesEnd: byte = 0;
  LowerChars: array [char] of char =
   (#0, #1, #2, #3, #4, #5, #6, #7, #8, #9, #10, #11, #12, #13, #14, #15, #16,
    #17, #18, #19, #20, #21, #22, #23, #24, #25, #26, #27, #28, #29, #30, #31,
    #32, #33, #34, #35, #36, #37, #38, #39, #40, #41, #42, #43, #44, #45, #46,
    #47, #48, #49, #50, #51, #52, #53, #54, #55, #56, #57, #58, #59, #60, #61,
    #62, #63, #64, #65, #66, #67, #68, #69, #70, #71, #72, #73, #74, #75, #76,
    #77, #78, #79, #80, #81, #82, #83, #84, #85, #86, #87, #88, #89, #90, #91,
    #92, #93, #94, #95, #96, #97, #98, #99, #100, #101, #102, #103, #104, #105,
    #106, #107, #108, #109, #110, #111, #112, #113, #114, #115, #116, #117,
    #118, #119, #120, #121, #122, #123, #124, #125, #126, #127, #128, #129,
    #130, #131, #132, #133, #134, #135, #136, #137, #138, #139, #140, #141,
    #142, #143, #144, #145, #146, #147, #148, #149, #150, #151, #152, #153,
    #154, #155, #156, #157, #158, #159, #160, #161, #162, #163, #164, #165,
    #166, #167, #168, #169, #170, #171, #172, #173, #174, #175, #176, #177,
    #178, #179, #180, #181, #182, #183, #184, #185, #186, #187, #188, #189,
    #190, #191, #192, #193, #194, #195, #196, #197, #198, #199, #200, #201,
    #202, #203, #204, #205, #206, #207, #208, #209, #210, #211, #212, #213,
    #214, #215, #216, #217, #218, #219, #220, #221, #222, #223, #224, #225,
    #226, #227, #228, #229, #230, #231, #232, #233, #234, #235, #236, #237,
    #238, #239, #240, #241, #242, #243, #244, #245, #246, #247, #248, #249,
    #250, #251, #252, #253, #254, #255);
  (* ...and now for ISO 8859-1 aka IBM codepage 819 *)
  LowerCharsISO88591: array [char] of char =
   (#0, #1, #2, #3, #4, #5, #6, #7, #8, #9, #10, #11, #12, #13, #14, #15, #16,
    #17, #18, #19, #20, #21, #22, #23, #24, #25, #26, #27, #28, #29, #30, #31,
    #32, #33, #34, #35, #36, #37, #38, #39, #40, #41, #42, #43, #44, #45, #46,
    #47, #48, #49, #50, #51, #52, #53, #54, #55, #56, #57, #58, #59, #60, #61,
    #62, #63, #64, #65, #66, #67, #68, #69, #70, #71, #72, #73, #74, #75, #76,
    #77, #78, #79, #80, #81, #82, #83, #84, #85, #86, #87, #88, #89, #90, #91,
    #92, #93, #94, #95, #96, #97, #98, #99, #100, #101, #102, #103, #104, #105,
    #106, #107, #108, #109, #110, #111, #112, #113, #114, #115, #116, #117,
    #118, #119, #120, #121, #122, #123, #124, #125, #126, #127, #128, #129,
    #130, #131, #132, #133, #134, #135, #136, #137, #138, #139, #140, #141,
    #142, #143, #144, #145, #146, #147, #148, #149, #150, #151, #152, #153,
    #154, #155, #156, #157, #158, #159, #160, #161, #162, #163, #164, #165,
    #166, #167, #168, #169, #170, #171, #172, #173, #174, #175, #176, #177,
    #178, #179, #180, #181, #182, #183, #184, #185, #186, #187, #188, #189,
    #190, #191, #192, #193, #194, #195, #196, #197, #198, #199, #200, #201,
    #202, #203, #204, #205, #206, #207, #208, #209, #210, #211, #212, #213,
    #214, #215, #216, #217, #218, #219, #220, #221, #222, #223, #224, #225,
    #226, #227, #228, #229, #230, #231, #232, #233, #234, #235, #236, #237,
    #238, #239, #240, #241, #242, #243, #244, #245, #246, #247, #248, #249,
    #250, #251, #252, #253, #254, #255);
  NoIso88591Support: boolean = false;


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
   end
  else
   if RetSize < SizeOf (C) then
    C := 850;
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


procedure InitDBCSLeadRanges;
var
  RC: cardinal;
begin
  RC := DosQueryDBCSEnv (SizeOf (DBCSLeadRanges), EmptyCC,
                                                          @DBCSLeadRanges [0]);
  DBCSLeadRangesEnd := 0;
  if RC <> 0 then
   while (DBCSLeadRangesEnd < SizeOf (DBCSLeadRanges)) and
                ((DBCSLeadRanges [DBCSLeadRangesEnd] <> #0) or
                          (DBCSLeadRanges [Succ (DBCSLeadRangesEnd)] <> #0)) do
    Inc (DBCSLeadRangesEnd, 2);
end;


procedure InitDummyAnsiSupport;
var
  C: char;
  AllChars: array [char] of char;
  RetSize: cardinal;
begin
  if DosQueryCollate (SizeOf (CollationSequence), EmptyCC, @CollationSequence,
                                                             RetSize) <> 0 then
   Move (LowerChars, CollationSequence, SizeOf (CollationSequence));
  Move (LowerChars, AllChars, SizeOf (AllChars));
  if DosMapCase (SizeOf (AllChars), IsoCC, @AllChars [#0]) <> 0 then
(* Codepage 819 may not be supported in all old OS/2 versions. *)
   begin
    Move (LowerCharsIso88591, AllChars, SizeOf (AllChars));
    DosMapCase (SizeOf (AllChars), EmptyCC, @AllChars [#0]);
    NoIso88591Support := true;
   end;
  for C := Low (char) to High (char) do
   if AllChars [C] <> C then
    LowerCharsIso88591 [AllChars [C]] := C;
  if NoIso88591Support then
   Move (LowerCharsIso88591, LowerChars, SizeOf (LowerChars))
  else
   begin
    Move (LowerChars, AllChars, SizeOf (AllChars));
    DosMapCase (SizeOf (AllChars), EmptyCC, @AllChars [#0]);
    for C := Low (char) to High (char) do
     if AllChars [C] <> C then
      LowerChars [AllChars [C]] := C;
   end;
  InitDBCSLeadRanges;
end;


procedure ReInitDummyAnsiSupport;
var
  C: char;
  AllChars: array [char] of char;
  RetSize: cardinal;
begin
  for C := Low (char) to High (char) do
   AllChars [C] := C;
  if DosQueryCollate (SizeOf (CollationSequence), EmptyCC, @CollationSequence,
                                                             RetSize) <> 0 then
   Move (AllChars, CollationSequence, SizeOf (CollationSequence));
  DosMapCase (SizeOf (AllChars), EmptyCC, @AllChars [#0]);
  for C := Low (char) to High (char) do
   if AllChars [C] <> C then
    LowerChars [AllChars [C]] := C;
  InitDBCSLeadRanges;
end;


function DummyUniToLower (UniCharIn: WideChar): WideChar; cdecl;
var
  C: char;
begin
  C := UniCharIn;
  DummyUniToLower := LowerCharsIso88591 [C];
end;


function DummyUniToUpper (UniCharIn: WideChar): WideChar; cdecl;
var
  C: char;
begin
  DummyUniToUpper := UniCharIn;
  C := UniCharIn;
  if NoIso88591Support then
   begin
    if DosMapCase (1, EmptyCC, @C) = 0 then
     DummyUniToUpper := C;
   end
  else
   if DosMapCase (1, IsoCC, @C) = 0 then
    DummyUniToUpper := C
end;


function DummyUniStrColl (Locale_Object: TLocaleObject;
                                  const UCS1, UCS2: PWideChar): longint; cdecl;
var
  S1, S2: ansistring;
begin
  S1 := UCS1;
  S2 := UCS2;
  if S1 = S2 then
   DummyUniStrColl := 0
  else if S1 < S2 then
   DummyUniStrColl := -1
  else
   DummyUniStrColl := 1;
end;


function DummyUniCreateLocaleObject (LocaleSpecType: longint;
  const LocaleSpec: pointer; var Locale_Object: TLocaleObject): longint; cdecl;
begin
  DummyUniCreateLocaleObject := ULS_Unsupported;
end;


function DummyUniFreeLocaleObject (Locale_Object: TLocaleObject): longint;
                                                                         cdecl;
begin
  DummyUniFreeLocaleObject := ULS_BadObject;
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

{
function GetRtlCpFromCpRec (const CpRec: TCpRec): TSystemCodepage; inline;
begin
  if RtlUsesWinCp then
   GetRtlCp := CpRec.WinCP
  else
   GetRtlCp := TSystemCodepage (CpRec.Os2Cp);
end;
}

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


procedure InitDefaultCP;
var
  OS2CP, I: cardinal;
  NoUConvObj: TUConvObject;
  RCI: longint;
  RC: cardinal;
  CPArr: TCPArray;
  ReturnedSize: cardinal;
begin
  if InInitDefaultCP <> -1 then
   begin
    repeat
     DosSleep (5);
    until InInitDefaultCP <> -1;
    Exit;
   end;
  InInitDefaultCP := ThreadID;
  if DefCpRec.UConvObj <> nil then
   begin
(*  Do not free the UConv object from DefCpRec, because it is also stored in
    the respective CPXList record! *)
{
    RCI := Sys_UniFreeUConvObject (DefCpRec.UConvObj);
    if RCI <> 0 then
     OSErrorWatch (cardinal (RCI));
}
    DefCpRec.UConvObj := nil;
   end;
  RC := DosQueryCP (SizeOf (CPArr), @CPArr, ReturnedSize);
  if (RC <> 0) and (RC <> 473) then
   begin
    OSErrorWatch (RC);
    CPArr [0] := 850;
   end
  else if (ReturnedSize < 4) then
   CPArr [0] := 850;
  DefaultFileSystemCodePage := OS2CPtoRtlCP (CPArr [0], cpxAll,
                                                            DefCpRec.UConvObj);
  CachedDefFSCodepage := DefaultFileSystemCodePage;
  DefCpRec.OS2CP := CPArr [0];
(* Find out WinCP _without_ considering RtlUsesWinCP *)
  I := 1;
  while (I <= MaxNonEqualCPMapping) and (CpXList [I].OS2CP <> OS2CP) do
   Inc (I);
  if CpXList [I].OS2CP = CPArr [0] then
   DefCpRec.WinCP := CpXList [I].WinCP
  else
   DefCpRec.WinCP := CPArr [0];

  if DefLocObj <> nil then
   begin
    RCI := Sys_UniFreeLocaleObject (DefLocObj);
    if RCI <> 0 then
     OSErrorWatch (cardinal (RCI));
    DefLocObj := nil;
   end;
  RCI := Sys_UniCreateLocaleObject (Uni_UCS_String_Pointer, @WNull, DefLocObj);
  if RCI <> 0 then
   begin
    OSErrorWatch (cardinal (RCI));
(* The locale dependent routines like comparison require a valid locale *)
(* setting, but the locale set using environment variable LANG is not *)
(* recognized by OS/2 -> we try the "Universal" locale as a fallback. *)
    RCI := Sys_UniCreateLocaleObject (Uni_UCS_String_Pointer, @WUniv [0],
                                                                    DefLocObj);
    if RCI <> 0 then
     begin
      OSErrorWatch (cardinal (RCI));
      DefLocObj := nil;
     end;
   end;
  if not (UniAPI) then
   ReInitDummyAnsiSupport;
  InInitDefaultCP := -1;
end;


function OS2CPtoRtlCP (CP: cardinal; ReqFlags: byte;
                                  var UConvObj: TUConvObject): TSystemCodepage;
var
  I, I2: cardinal;
  RCI: longint;

 function CheckDefaultOS2CP: boolean;
 begin
  if CP = DefCpRec.OS2CP then
   begin
    CheckDefaultOS2CP := true;
    if RTLUsesWinCP then
     OS2CPtoRtlCP := DefCpRec.WinCP;
    if ReqFlags and CpxMappingOnly = 0 then
     UConvObj := DefCpRec.UConvObj;
   end
  else
   CheckDefaultOS2CP := false;
 end;

begin
  OS2CPtoRtlCP := TSystemCodePage (CP);
  UConvObj := nil;
  if not UniAPI then (* No UniAPI => no need for UConvObj *)
   ReqFlags := ReqFlags or CpxMappingOnly;
  if CheckDefaultOS2CP then
   Exit;
  if (CachedDefFSCodepage <> DefaultFileSystemCodePage) and
                                             (InInitDefaultCP <> ThreadID) then
(* InInitDefaultCP = ThreadID -> this thread is already re-initializing the cached information *)
   begin
    if InInitDefaultCP <> -1 then
     repeat
      DosSleep (5) (* Let's wait until the other thread finishes re-initialization of the cache *)
     until InInitDefaultCP = -1
    else
     InitDefaultCP;
    if CheckDefaultOS2CP then
     Exit;
   end;
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
          if UConvObjectForCP (CpXList [I].OS2CP, UConvObj) = Uls_Success then
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


function RtlCPtoOS2CP (RtlCP: TSystemCodepage; ReqFlags: byte;
                                         var UConvObj: TUConvObject): cardinal;
var
  I, I2: cardinal;

 function CheckDefaultWinCP: boolean;
 begin
  if RtlCP = DefCpRec.WinCP then
   begin
    CheckDefaultWinCP := true;
    RtlCPtoOS2CP := DefCpRec.WinCP;
    if ReqFlags and CpxMappingOnly = 0 then
     UConvObj := DefCpRec.UConvObj;
   end
  else
  CheckDefaultWinCP := false;
 end;

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
  else if CheckDefaultWinCp then
   Exit
  else
   begin
    if (CachedDefFSCodepage <> DefaultFileSystemCodePage) and
                                             (InInitDefaultCP <> ThreadID) then
(* InInitDefaultCP = ThreadID -> this thread is already re-initializing the cached information *)
     begin
      if InInitDefaultCP <> -1 then
       repeat
(* Let's wait until the other thread finishes re-initialization of the cache *)
        DosSleep (5)
       until InInitDefaultCP = -1
      else
       InitDefaultCP;
      if CheckDefaultWinCP then
       Exit;
     end;
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
(*  Do not free the UConv object from DefCpRec, because it is also stored in
    the respective CpXList record! *)
{
        RCI := Sys_UniFreeUConvObject (DefCpRec.UConvObj);
        if RCI <> 0 then
         OSErrorWatch (cardinal (RCI));
}
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


function OS2UpperUnicodeString (const S: UnicodeString): UnicodeString;
var
  I: cardinal;
begin
  SetLength (Result, Length (S));
  for I := 0 to Pred (Length (S)) do
   PWideChar (Result) [I] := Sys_UniToUpper (S [Succ (I)]);
end;


function OS2LowerUnicodeString (const S: UnicodeString): UnicodeString;
var
  I: cardinal;
begin
  SetLength (Result, Length (S));
  for I := 0 to Pred (Length (S)) do
   PWideChar (Result) [I] := Sys_UniToLower (S [Succ (I)]);
end;


function NoNullsUnicodeString (const S: UnicodeString): UnicodeString;
var
  I: cardinal;
begin
  Result := S;
  UniqueString (Result);
  for I := 1 to Length (S) do
   if Result [I] = WNull then
    Result [I] := ' ';
end;


function OS2CompareUnicodeString (const S1, S2: UnicodeString): PtrInt;
var
  HS1, HS2: UnicodeString;
begin
  { UniStrColl interprets null chars as end-of-string -> filter out }
  HS1 := NoNullsUnicodeString (S1);
  HS2 := NoNullsUnicodeString (S2);
  Result := Sys_UniStrColl (DefLocObj, PWideChar (HS1), PWideChar (HS2));
  if Result < -1 then
   Result := -1
  else if Result > 1 then
   Result := 1;
end;


function OS2CompareTextUnicodeString (const S1, S2: UnicodeString): PtrInt;
begin
  Result := OS2CompareUnicodeString (OS2UpperUnicodeString (S1),
                                                   OS2UpperUnicodeString (S2));
{$WARNING Language independent uppercase routine may not be appropriate for language dependent case insensitive comparison!}
end;


function OS2UpperAnsiString (const S: AnsiString): AnsiString;
var
  RC: cardinal;
begin
  Result := S;
  UniqueString (Result);
  FillChar (EmptyCC, SizeOf (EmptyCC), 0);
  RC := DosMapCase (Length (Result), EmptyCC, PChar (Result));
{ What to do in case of a failure??? }
  if RC <> 0 then
   Result := UpCase (S); { Use a fallback? }
end;

      
function OS2LowerAnsiString (const S: AnsiString): AnsiString;
var
  I: PtrUInt;

  function IsDBCSLeadChar (C: char): boolean;
  var
    D: byte;
  begin
    IsDBCSLeadChar := false;
    D := 0;
    while D < DBCSLeadRangesEnd do
     begin
      if (C >= DBCSLeadRanges [D]) and (C <= DBCSLeadRanges [Succ (D)]) then
       begin
        IsDBCSLeadChar := true;
        Exit;
       end;
      Inc (D, 2);
     end;
  end;

begin
(*
  OS/2 provides no direct solution for lowercase conversion of MBCS strings.
  If Unicode support is available, using Unicode routines is the best solution.
  If not, we use a translation table built at startup by translating the full
  character set to uppercase and using that for creation of a lookup table
  (as already done in sysutils). However, we need to check for DBCS (MBCS)
  codepages and avoid translating the DBCS lead bytes and the following
  character.
*)
  if UniAPI then
   Result := OS2LowerUnicodeString (S)
{ Two implicit conversions... ;-) }
  else
   begin
    Result := S;
    if Length (Result) > 0 then
     begin
      UniqueString (Result);
      if DBCSLeadRangesEnd > 0 then
       begin
        I := 1;
        while I <= Length (Result) do
         begin
          if IsDBCSLeadChar (Result [I]) then
           Inc (I, 2)
          else
           begin
            Result [I] := LowerChars [Result [I]];
            Inc (I);
           end;
         end;
       end
      else
       for I := 1 to Length (Result) do
        Result [I] := LowerChars [Result [I]];
     end;
   end;
end;


function OS2CompareStrAnsiString (const S1, S2: AnsiString): PtrInt;
var
  I, MaxLen: PtrUInt;
begin
  if UniAPI then
   Result := OS2CompareUnicodeString (S1, S2) (* implicit conversions *)
  else
(* Older OS/2 versions without Unicode support do not provide direct means *)
(* for case sensitive and codepage and language-aware string comparison.   *)
(* We have to resort to manual comparison of the original strings together *)
(* with strings translated using the case insensitive collation sequence.  *)
   begin
    if Length (S1) = 0 then
     begin
      if Length (S2) = 0 then
       Result := 0
      else
       Result := -1;
      Exit;
     end
    else
     if Length (S2) = 0 then
      begin
       Result := 1;
       Exit;
      end;
    I := 1;
    MaxLen := Length (S1);
    if Length (S2) < MaxLen then
     MaxLen := Length (S2);
    repeat
     if CollationSequence [S1 [I]] = CollationSequence [S2 [I]] then
      begin
       if S1 [I] < S2 [I] then
        begin
         Result := -1;
         Exit;
        end
       else if S1 [I] > S2 [I] then
        begin
         Result := 1;
         Exit;
        end;
      end
     else
      begin
       if CollationSequence [S1 [I]] < CollationSequence [S2 [I]] then
        Result := -1
       else
        Result := 1;
       Exit;
      end;
     Inc (I);
    until (I > MaxLen);
    if Length (S2) > MaxLen then
     Result := -1
    else if Length (S1) > MaxLen then
     Result := 1
    else
     Result := 0;
   end;
end;


function OS2StrCompAnsiString (S1, S2: PChar): PtrInt;
var
  HSA1, HSA2: AnsiString;
  HSU1, HSU2: UnicodeString;
begin
(* Do not call OS2CompareUnicodeString to skip scanning for #0. *)
  HSA1 := AnsiString (S1);
  HSA2 := AnsiString (S2);
  if UniApi then
   begin
    HSU1 := HSA1; (* implicit conversion *)
    HSU2 := HSA2; (* implicit conversion *)
    Result := Sys_UniStrColl (DefLocObj, PWideChar (HSU1), PWideChar (HSU2));
    if Result < -1 then
     Result := -1
    else if Result > 1 then
     Result := 1;
   end
  else
   Result := OS2CompareStrAnsiString (HSA1, HSA2);
end;


function OS2CompareTextAnsiString (const S1, S2: AnsiString): PtrInt;
var
  HSA1, HSA2: AnsiString;
  I: PtrUInt;
begin
  if UniAPI then
   Result := OS2CompareTextUnicodeString (S1, S2) (* implicit conversions *)
  else
   begin
(* Let's use collation strings here as a fallback *)
    SetLength (HSA1, Length (S1));
    if Length (HSA1) > 0 then
(* Using assembler would be much faster, but never mind... *)
     for I := 1 to Length (HSA1) do
      HSA1 [I] := CollationSequence [S1 [I]];
{$WARNING Results of using collation sequence with DBCS not known/tested!}
    SetLength (HSA2, Length (S2));
    if Length (HSA2) > 0 then
     for I := 1 to Length (HSA2) do
      HSA2 [I] := CollationSequence [S2 [I]];
    if HSA1 = HSA2 then
     Result := 0
    else if HSA1 < HSA2 then
     Result := -1
    else
     Result := 1;
   end;
end;


function OS2StrICompAnsiString (S1, S2: PChar): PtrInt;
begin
  Result := OS2CompareTextAnsiString (AnsiString (S1), AnsiString (S2));
end;


function OS2StrLCompAnsiString (S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
var
  A, B: AnsiString;
begin
  if (MaxLen = 0) then
   Exit (0);
  SetLength (A, MaxLen);
  Move (S1^, A [1], MaxLen);
  SetLength (B, MaxLen);
  Move (S2^, B [1], MaxLen);
  Result := OS2CompareStrAnsiString (A, B);
end;


function OS2StrLICompAnsiString (S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
var
  A, B: AnsiString;
begin
  if (MaxLen = 0) then
   Exit (0);
  SetLength (A, MaxLen);
  Move (S1^, A [1], MaxLen);
  SetLength (B, MaxLen);
  Move (S2^, B [1], MaxLen);
  Result := OS2CompareTextAnsiString (A, B);
end;


procedure FPC_RangeError; [external name 'FPC_RANGEERROR'];


procedure Ansi2PChar (const S: AnsiString; const OrgP: PChar; out P: Pchar);
var
  NewLen: SizeUInt;
begin
  NewLen := Length (S);
  if NewLen > StrLen (OrgP) then
   FPC_RangeError;
  P := OrgP;
  if (NewLen > 0) then
   Move (S [1], P [0], NewLen);
  P [NewLen] := #0;
end;


function OS2StrUpperAnsiString (Str: PChar): PChar;
var
  Temp: AnsiString;
begin
  Temp := OS2UpperAnsiString (Str);
  Ansi2PChar (Temp, Str, Result);
end;


function OS2StrLowerAnsiString (Str: PChar): PChar;
var
  Temp: AnsiString;
begin
  Temp := OS2LowerAnsiString (Str);
  Ansi2PChar (Temp, Str, Result);
end;


(*
CWSTRING:
{ return value: number of code points in the string. Whenever an invalid
  code point is encountered, all characters part of this invalid code point
  are considered to form one "character" and the next character is
  considered to be the start of a new (possibly also invalid) code point }
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
      nextlen:=ptrint(mblen(s,MB_CUR_MAX));
{$else beos}
      nextlen:=ptrint(mbrlen(s,MB_CUR_MAX,@mbstate));
{$endif beos}
      { skip invalid/incomplete sequences }
      if (nextlen<0) then
        nextlen:=1;
      inc(result,1);
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

              RC := DosLoadModule (@ErrName [0], SizeOf (ErrName),
                                                @LibUniName [0], LibUniHandle);
              if RC = 0 then
               begin
                RC := DosQueryProcAddr (LibUniHandle, OrdUniToLower, nil, P);
                if RC = 0 then
                 begin
                  Sys_UniToLower := TUniToLower (P);
                  RC := DosQueryProcAddr (LibUniHandle, OrdUniToUpper, nil, P);
                  if RC = 0 then
                   begin
                    Sys_UniToUpper := TUniToUpper (P);
                    RC := DosQueryProcAddr (LibUniHandle, OrdUniStrColl, nil,
                                                                            P);
                    if RC = 0 then
                     begin
                      Sys_UniStrColl := TUniStrColl (P);
                      RC := DosQueryProcAddr (LibUniHandle,
                                             OrdUniCreateLocaleObject, nil, P);
                      if RC = 0 then
                       begin
                        Sys_UniCreateLocaleObject := TUniCreateLocaleObject
                                                                           (P);
                        RC := DosQueryProcAddr (LibUniHandle,
                                               OrdUniFreeLocaleObject, nil, P);
                        if RC = 0 then
                         begin
                          Sys_UniFreeLocaleObject := TUniFreeLocaleObject (P);

                          UniAPI := true;
                         end;
                       end;
                     end;
                   end;
                 end;
               end;
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
    Sys_UniToLower := @DummyUniToLower;
    Sys_UniToUpper := @DummyUniToUpper;
    Sys_UniStrColl := @DummyUniStrColl;
    Sys_UniCreateLocaleObject := @DummyUniCreateLocaleObject;
    Sys_UniFreeLocaleObject := @DummyUniFreeLocaleObject;
    InitDummyAnsiSupport;
   end;

    { Widestring }
  WideStringManager.Wide2AnsiMoveProc := @OS2Unicode2AnsiMove;
  WideStringManager.Ansi2WideMoveProc := @OS2Ansi2UnicodeMove;
  WideStringManager.UpperWideStringProc := @OS2UpperUnicodeString;
  WideStringManager.LowerWideStringProc := @OS2LowerUnicodeString;
  WideStringManager.CompareWideStringProc := @OS2CompareUnicodeString;
  WideStringManager.CompareTextWideStringProc := @OS2CompareTextUnicodeString;
    { Unicode }
  WideStringManager.Unicode2AnsiMoveProc := @OS2Unicode2AnsiMove;
  WideStringManager.Ansi2UnicodeMoveProc := @OS2Ansi2UnicodeMove;
  WideStringManager.UpperUnicodeStringProc := @OS2UpperUnicodeString;
  WideStringManager.LowerUnicodeStringProc := @OS2LowerUnicodeString;
  WideStringManager.CompareUnicodeStringProc := @OS2CompareUnicodeString;
  WideStringManager.CompareTextUnicodeStringProc :=
                                                  @OS2CompareTextUnicodeString;
    { Codepage }
  WideStringManager.GetStandardCodePageProc := @OS2GetStandardCodePage;
(*
      CharLengthPCharProc:=@CharLengthPChar;
      CodePointLengthProc:=@CodePointLength;
*)
  WideStringManager.UpperAnsiStringProc := @OS2UpperAnsiString;
  WideStringManager.LowerAnsiStringProc := @OS2LowerAnsiString;
  WideStringManager.CompareStrAnsiStringProc := @OS2CompareStrAnsiString;
  WideStringManager.CompareTextAnsiStringProc := @OS2CompareTextAnsiString;
  WideStringManager.StrCompAnsiStringProc := @OS2StrCompAnsiString;
  WideStringManager.StrICompAnsiStringProc := @OS2StrICompAnsiString;
  WideStringManager.StrLCompAnsiStringProc := @OS2StrLCompAnsiString;
  WideStringManager.StrLICompAnsiStringProc := @OS2StrLICompAnsiString;
  WideStringManager.StrLowerAnsiStringProc := @OS2StrLowerAnsiString;
  WideStringManager.StrUpperAnsiStringProc := @OS2StrUpperAnsiString;
end;
