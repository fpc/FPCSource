{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{**********[ SOURCE FILE OF FREE VISION ]***************}
{                                                       }
{   Parts Copyright (c) 1992,96 by Florian Klaempfl     }
{   fnklaemp@cip.ft.uni-erlangen.de                     }
{                                                       }
{   Parts Copyright (c) 1996 by Frank ZAGO              }
{   zago@ecoledoc.ipc.fr                                }
{                                                       }
{   Parts Copyright (c) 1995 by MH Spiegel              }
{                                                       }
{   Parts Copyright (c) 1996 by Leon de Boer            }
{   ldeboer@ibm.net                                     }
{                                                       }
{              THIS CODE IS FREEWARE                    }
{*******************************************************}

{***************[ SUPPORTED PLATFORMS ]*****************}
{  16 and 32 Bit compilers                              }
{     DOS      - Turbo Pascal 7.0 +      (16 Bit)       }
{              - FPK Pascal              (32 Bit)       }
{     DPMI     - Turbo Pascal 7.0 +      (16 Bit)       }
{     WINDOWS  - Turbo Pascal 7.0 +      (16 Bit)       }
{     OS2      - Virtual Pascal 0.3 +    (32 Bit)       }
{                SpeedPascal 1.5 G +     (32 Bit)       }
{                C'T patch to BP         (16 Bit)       }
{*******************************************************}

UNIT Objects;

{$I os.inc}

INTERFACE

   CONST
      Sw_MaxData = 128*1024*1024;                     { Maximum data size }

   TYPE
      Sw_Word    = LongInt;                           { Long integer now }
      Sw_Integer = LongInt;                           { Long integer now }

   TYPE
      FuncPtr = FUNCTION (Item: Pointer; _EBP: Sw_Word): Boolean;
      ProcPtr = PROCEDURE (Item: Pointer; _EBP: Sw_Word);


CONST
   stOk         =  0;                                 { No stream error }
   stError      = -1;                                 { Access error }
   stInitError  = -2;                                 { Initialize error }
   stReadError  = -3;                                 { Stream read error }
   stWriteError = -4;                                 { Stream write error }
   stGetError   = -5;                                 { Get object error }
   stPutError   = -6;                                 { Put object error }
   stSeekError  = -7;                                 { Seek error in stream }
   stOpenError  = -8;                                 { Error opening stream }

CONST
   stCreate    = $3C00;                               { Create new file }
   stOpenRead  = $3D00;                               { Read access only }
   stOpenWrite = $3D01;                               { Write access only }
   stOpen      = $3D02;                               { Read/write access }

CONST
   coIndexError = -1;                                 { Index out of range }
   coOverflow   = -2;                                 { Overflow }

CONST
   sa_XMSFirst   = $8000;                             { Use XMS memory 1st }
   sa_EMSFirst   = $4000;                             { Use EMS memory 1st }
   sa_RAMFirst   = $2000;                             { Use RAM memory 1st }
   sa_DISKFirst  = $1000;                             { Use DISK space 1st }
   sa_XMSSecond  = $0800;                             { Use XMS memory 2nd }
   sa_EMSSecond  = $0400;                             { Use EMS memory 2nd }
   sa_RAMSecond  = $0200;                             { Use RAM memory 2nd }
   sa_DISKSecond = $0100;                             { Use DISK space 2nd }
   sa_XMSThird   = $0080;                             { Use XMS memory 3rd }
   sa_EMSThird   = $0040;                             { Use EMS memory 3rd }
   sa_RAMThird   = $0020;                             { Use RAM memory 3rd }
   sa_DISKThird  = $0010;                             { Use DISK space 3rd }
   sa_XMSFourth  = $0008;                             { Use XMS memory 4th }
   sa_EMSFourth  = $0004;                             { Use EMS memory 4th }
   sa_RAMFourth  = $0002;                             { Use RAM memory 4th }
   sa_DISKFourth = $0001;                             { Use DISK space 4th }

CONST
   vmtHeaderSize = 8;                                 { VMT header size }

CONST
   MaxCollectionSize = Sw_MaxData DIV SizeOf(Pointer);{ Max collection size }

TYPE
   TCharSet = SET Of Char;                            { Character set }
   PCharSet = ^TCharSet;                              { Character set ptr }

TYPE
   TByteArray = ARRAY [0..Sw_MaxData-1] Of Byte;      { Byte array }
   PByteArray = ^TByteArray;                          { Byte array pointer }

   TWordArray = ARRAY [0..Sw_MaxData DIV 2-1] Of Word;{ Word array }
   PWordArray = ^TWordArray;                          { Word array pointer }

TYPE
   FNameStr = String;

TYPE
   AsciiZ = Array [0..255] Of Char;                   { Filename array }

TYPE
   PByte    = ^Byte;                                  { Byte pointer }
   PWord    = ^Word;                                  { Word pointer }
   PLongInt = ^LongInt;                               { LongInt pointer }
   PString  = ^String;                                { String pointer }

TYPE
   WordRec = RECORD
     Lo, Hi: Byte;                                    { Word to bytes }
   END;

   LongRec = RECORD
     Lo, Hi: Word;                                    { LongInt to words }
   END;

   PtrRec = RECORD
     Ofs, Seg: Word;                                  { Pointer to words }
   END;

TYPE
   PStreamRec = ^TStreamRec;                          { Stream record ptr }
   TStreamRec = RECORD
      ObjType: Sw_Word;                               { Object type id }
      VmtLink: Sw_Word;                               { VMT link }
      Load : Pointer;                                 { Object load code }
      Store: Pointer;                                 { Object store code }
      Next : Sw_Word;                                 { Bytes to next }
   END;


TYPE
   TPoint = OBJECT
      X, Y: Integer;                                  { Point co-ordinates }
   END;

   TRect = OBJECT
         A, B: TPoint;                                { Corner points }
      FUNCTION Empty: Boolean;
      FUNCTION Equals (R: TRect): Boolean;
      FUNCTION Contains (P: TPoint): Boolean;
      PROCEDURE Copy (R: TRect);
      PROCEDURE Union (R: TRect);
      PROCEDURE Intersect (R: TRect);
      PROCEDURE Move (ADX, ADY: Integer);
      PROCEDURE Grow (ADX, ADY: Integer);
      PROCEDURE Assign (XA, YA, XB, YB: Integer);
   END;
   PRect = ^TRect;

TYPE
   TObject = OBJECT
      CONSTRUCTOR Init;
      PROCEDURE Free;
      DESTRUCTOR Done;                                               Virtual;
   END;
   PObject = ^TObject;

TYPE
   TStream = OBJECT (TObject)
         Status   : Integer;                          { Stream status }
         ErrorInfo: Integer;                          { Stream error info }
      FUNCTION Get: PObject;
      FUNCTION StrRead: PChar;
      FUNCTION GetPos: LongInt;                                      Virtual;
      FUNCTION GetSize: LongInt;                                     Virtual;
      FUNCTION ReadStr: PString;
      PROCEDURE Close;                                               Virtual;
      PROCEDURE Reset;
      PROCEDURE Flush;                                               Virtual;
      PROCEDURE Truncate;                                            Virtual;
      PROCEDURE Put (P: PObject);
      PROCEDURE Seek (Pos: LongInt);                                 Virtual;
      PROCEDURE StrWrite (P: PChar);
      PROCEDURE WriteStr (P: PString);
      PROCEDURE Open (OpenMode: Word);                               Virtual;
      PROCEDURE Error (Code, Info: Integer);                         Virtual;
      PROCEDURE Read (Var Buf; Count: Sw_Word);                      Virtual;
      PROCEDURE Write (Var Buf; Count: Sw_Word);                     Virtual;
      PROCEDURE CopyFrom (Var S: TStream; Count: Longint);
   END;
   PStream = ^TStream;

TYPE
   TDosStream = OBJECT (TStream)
         Handle: Integer;                             { DOS file handle }
         FName : AsciiZ;                              { AsciiZ filename }
      CONSTRUCTOR Init (FileName: FNameStr; Mode: Word);
      DESTRUCTOR Done;                                               Virtual;
      FUNCTION GetPos: Longint;                                      Virtual;
      FUNCTION GetSize: Longint;                                     Virtual;
      PROCEDURE Close;                                               Virtual;
      PROCEDURE Seek (Pos: LongInt);                                 Virtual;
      PROCEDURE Open (OpenMode: Word);                               Virtual;
      PROCEDURE Read (Var Buf; Count: Sw_Word);                      Virtual;
      PROCEDURE Write (Var Buf; Count: Sw_Word);                     Virtual;
   END;
   PDosStream = ^TDosStream;

TYPE
   TBufStream = OBJECT (TDosStream)
   END;
   PBufStream = ^TBufStream;

TYPE
   TEmsStream = OBJECT (TStream)
   END;
   PEmsStream = ^TEmsStream;

TYPE
   TXmsStream = OBJECT (TStream)
   END;
   PXmsStream = ^TXmsStream;

TYPE
   TMemoryStream = OBJECT (TStream)
   END;
   PMemoryStream = ^TMemoryStream;

TYPE
  TItemList = Array [0..MaxCollectionSize - 1] Of Pointer;
  PItemList = ^TItemList;

   TCollection = OBJECT (TObject)
         Items: PItemList;                            { Item list pointer }
         Count: Sw_Integer;                           { Item count }
         Limit: Sw_Integer;                           { Item limit count }
         Delta: Sw_Integer;                           { Inc delta size }
      CONSTRUCTOR Init (ALimit, ADelta: Sw_Integer);
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done;                                               Virtual;
      FUNCTION At (Index: Sw_Integer): Pointer;
      FUNCTION IndexOf (Item: Pointer): Sw_Integer;                  Virtual;
      FUNCTION GetItem (Var S: TStream): Pointer;                    Virtual;
      FUNCTION LastThat (Test: Pointer): Pointer;
      FUNCTION FirstThat (Test: Pointer): Pointer;
      PROCEDURE Pack;
      PROCEDURE FreeAll;
      PROCEDURE DeleteAll;
      PROCEDURE Free (Item: Pointer);
      PROCEDURE Insert (Item: Pointer);                              Virtual;
      PROCEDURE Delete (Item: Pointer);
      PROCEDURE AtFree (Index: Sw_Integer);
      PROCEDURE FreeItem (Item: Pointer);                            Virtual;
      PROCEDURE AtDelete (Index: Sw_Integer);
      PROCEDURE ForEach (Action: Pointer);
      PROCEDURE SetLimit (ALimit: Sw_Integer);                       Virtual;
      PROCEDURE Error (Code, Info: Integer);                         Virtual;
      PROCEDURE AtPut (Index: Sw_Integer; Item: Pointer);
      PROCEDURE AtInsert (Index: Sw_Integer; Item: Pointer);
      PROCEDURE Store (Var S: TStream);
      PROCEDURE PutItem (Var S: TStream; Item: Pointer);             Virtual;
   END;
   PCollection = ^TCollection;

TYPE
   TSortedCollection = OBJECT (TCollection)
         Duplicates: Boolean;                         { Duplicates flag }
      CONSTRUCTOR Init (ALimit, ADelta: Sw_Integer);
      CONSTRUCTOR Load (Var S: TStream);
      FUNCTION KeyOf (Item: Pointer): Pointer;                       Virtual;
      FUNCTION IndexOf (Item: Pointer): Sw_Integer;                  Virtual;
      FUNCTION Compare (Key1, Key2: Pointer): Sw_Integer;            Virtual;
      FUNCTION Search (Key: Pointer; Var Index: Sw_Integer): Boolean;Virtual;
      PROCEDURE Insert (Item: Pointer);                              Virtual;
      PROCEDURE Store (Var S: TStream);
   END;
   PSortedCollection = ^TSortedCollection;

TYPE
   TStringCollection = OBJECT (TSortedCollection)
      FUNCTION GetItem (Var S: TStream): Pointer;                    Virtual;
      FUNCTION Compare (Key1, Key2: Pointer): Sw_Integer;            Virtual;
      PROCEDURE FreeItem (Item: Pointer);                            Virtual;
      PROCEDURE PutItem (Var S: TStream; Item: Pointer);             Virtual;
   END;
   PStringCollection = ^TStringCollection;

TYPE
   TUnSortedStrCollection = OBJECT (TStringCollection)
      PROCEDURE Insert (Item: Pointer);                              Virtual;
   END;
   PUnSortedStrCollection = ^TUnSortedStrCollection;

FUNCTION NewStr (Const S: String): PString;
PROCEDURE DisposeStr (P: PString);

PROCEDURE Abstract;
PROCEDURE RegisterError;

FUNCTION CreateStream (Strategy: Word; ReqSize: LongInt): PStream;

FUNCTION DosFileOpen (Var FileName: AsciiZ; Mode: Word): Word;
FUNCTION DosRead(Handle: Word; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;
FUNCTION DosWrite(Handle: Word; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;
FUNCTION DosSetFilePtr (Handle: Word; Pos: LongInt; MoveType: Word;
Var NewPos: LongInt): Word;
PROCEDURE DosClose (Handle: Word);

CONST
   StreamError: Pointer = Nil;                        { Stream error ptr }

VAR HoldEBP: Sw_Word; TransferHandle: Sw_Word;
    DosStreamError: Sw_Word ;                         { Dos stream error }


IMPLEMENTATION

CONST
   StreamTypes: Sw_Word = $0;                         { Stream types }


PROCEDURE CheckEmpty (Var Rect: TRect);
BEGIN
   If (Rect.A.X >= Rect.B.X) OR
   (Rect.A.Y >= Rect.B.Y) Then Begin                  { Zero of reversed }
     Rect.A.X := 0;                                   { Clear a.x }
     Rect.A.Y := 0;                                   { Clear a.y }
     Rect.B.X := 0;                                   { Clear b.x }
     Rect.B.Y := 0;                                   { Clear b.y }
   End;
END;

FUNCTION TRect.Empty: Boolean;
BEGIN
   Empty := (A.X >= B.X) OR (A.Y >= B.Y);             { Empty result }
END;

FUNCTION TRect.Equals (R: TRect): Boolean;
BEGIN
   Equals := (A.X = R.A.X) AND (A.Y = R.A.Y) AND
   (B.X = R.B.X) AND (B.Y = R.B.Y);                   { Equals result }
END;

FUNCTION TRect.Contains (P: TPoint): Boolean;
BEGIN
   Contains := (P.X >= A.X) AND (P.X < B.X) AND
     (P.Y >= A.Y) AND (P.Y < B.Y);                    { Contains result }
END;

PROCEDURE TRect.Copy (R: TRect);
BEGIN
   A := R.A;                                          { Copy point a }
   B := R.B;                                          { Copy point b }
END;

PROCEDURE TRect.Union (R: TRect);
BEGIN
   If (R.A.X < A.X) Then A.X := R.A.X;                { Take if smaller }
   If (R.A.Y < A.Y) Then A.Y := R.A.Y;                { Take if smaller }
   If (R.B.X > B.X) Then B.X := R.B.X;                { Take if larger }
   If (R.B.Y > B.Y) Then B.Y := R.B.Y;                { Take if larger }
END;

PROCEDURE TRect.Intersect (R: TRect);
BEGIN
   If (R.A.X > A.X) Then A.X := R.A.X;                { Take if larger }
   If (R.A.Y > A.Y) Then A.Y := R.A.Y;                { Take if larger }
   If (R.B.X < B.X) Then B.X := R.B.X;                { Take if smaller }
   If (R.B.Y < B.Y) Then B.Y := R.B.Y;                { Take if smaller }
   CheckEmpty(Self);                                  { Check if empty }
END;

PROCEDURE TRect.Move (ADX, ADY: Integer);
BEGIN
   Inc(A.X, ADX);                                     { Adjust A.X }
   Inc(A.Y, ADY);                                     { Adjust A.Y }
   Inc(B.X, ADX);                                     { Adjust B.X }
   Inc(B.Y, ADY);                                     { Adjust B.Y }
END;

PROCEDURE TRect.Grow (ADX, ADY: Integer);
BEGIN
   Dec(A.X, ADX);                                     { Adjust A.X }
   Dec(A.Y, ADY);                                     { Adjust A.Y }
   Inc(B.X, ADX);                                     { Adjust B.X }
   Inc(B.Y, ADY);                                     { Adjust B.Y }
   CheckEmpty(Self);                                  { Check if empty }
END;

PROCEDURE TRect.Assign (XA, YA, XB, YB: Integer);
BEGIN
   A.X := XA;                                         { Hold A.X value }
   A.Y := YA;                                         { Hold A.Y value }
   B.X := XB;                                         { Hold B.X value }
   B.Y := YB;                                         { Hold B.Y value }
END;


TYPE
   DummyObject = OBJECT (TObject)                     { Internal object }
     Data: RECORD END;                                { Helps size VMT link }
   END;

CONSTRUCTOR TObject.Init;
VAR LinkSize: LongInt; Dummy: DummyObject;
BEGIN
   LinkSize := LongInt(@Dummy.Data)-LongInt(@Dummy);  { Calc VMT link size }
   FillChar(Pointer(LongInt(@Self)+LinkSize)^,
     SizeOf(Self)-LinkSize, #0);                      { Clear data fields }
END;

PROCEDURE TObject.Free;
BEGIN
   Dispose(PObject(@Self), Done);                     { Dispose of self }
END;

DESTRUCTOR TObject.Done;
BEGIN                                                 { Abstract method }
END;

FUNCTION TStream.StrRead: PChar;
VAR L: Word; P: PChar;
BEGIN
   Read(L, SizeOf(L));                                { Read length }
   If (L=0) Then StrRead := Nil Else Begin            { Check for empty }
     GetMem(P, L + 1);                                { Allocate memory }
     If (P<>Nil) Then Begin                           { Check allocate okay }
       Read(P[0], L);                                 { Read the data }
       P[L] := #0;                                    { Terminate with #0 }
     End;
     StrRead := P;                                    { Return PChar }
   End;
END;

FUNCTION TStream.ReadStr: PString;
VAR L: Byte; P: PString;
BEGIN
   Read(L, 1);                                        { Read string length }
   If (L > 0) Then Begin
     GetMem(P, L + 1);                                { Allocate memory }
     If (P<>Nil) Then Begin                           { Check allocate okay }
       P^[0] := Char(L);                              { Hold length }
       Read(P^[1], L);                                { Read string data }
     End;
     ReadStr := P;                                    { Return string ptr }
   End Else ReadStr := Nil;
END;

FUNCTION TStream.GetPos: LongInt;
BEGIN                                                 { Abstract method }
   Abstract;                                          { Abstract error }
END;

FUNCTION TStream.GetSize: LongInt;
BEGIN                                                 { Abstract method }
   Abstract;                                          { Abstract error }
END;

PROCEDURE TStream.Close;
BEGIN                                                 { Abstract method }
END;

PROCEDURE TStream.Reset;
BEGIN
   Status := 0;                                       { Clear status }
   ErrorInfo := 0;                                    { Clear error info }
END;

PROCEDURE TStream.Flush;
BEGIN                                                 { Abstract method }
END;

PROCEDURE TStream.Truncate;
BEGIN
   Abstract;                                          { Abstract error }
END;

PROCEDURE TStream.Seek (Pos: LongInt);
BEGIN
   Abstract;                                          { Abstract error }
END;

PROCEDURE TStream.StrWrite (P: PChar);
VAR L: Word; Q: PByteArray;
BEGIN
   L := 0;                                            { Preset no size }
   Q := PByteArray(P);                                { Transfer type }
   If (Q<>Nil) Then While (Q^[L]<>0) Do Inc(L);       { Calc PChar length }
   Write(L, SizeOf(L));                               { Store PChar length }
   If (P<>Nil) Then Write(P[0], L);                   { Write data }
END;

PROCEDURE TStream.WriteStr (P: PString);
CONST Empty: String[1] = '';
BEGIN
   If (P<>Nil) Then Write(P^, Length(P^) + 1)         { Write string }
     Else Write(Empty, 1);                            { Write empty string }
END;

PROCEDURE TStream.Open (OpenMode: Word);
BEGIN                                                 { Abstract method }
END;

PROCEDURE TStream.Error (Code, Info: Integer);
TYPE TErrorProc = Procedure(Var S: TStream);
BEGIN
   Status := Code;                                    { Hold error code }
   ErrorInfo := Info;                                 { Hold error info }
   If (StreamError<>Nil) Then
     TErrorProc(StreamError)(Self);                   { Call error ptr }
END;

PROCEDURE TStream.Read (Var Buf; Count: Sw_Word);
BEGIN
   Abstract;                                          { Abstract error }
END;

PROCEDURE TStream.Write (Var Buf; Count: Sw_Word);
BEGIN
   Abstract;                                          { Abstract error }
END;

PROCEDURE TStream.CopyFrom (Var S: TStream; Count: Longint);
VAR W: Word; Buffer: Array[0..1023] of Byte;
BEGIN
   While (Count > 0) Do Begin
     If (Count > SizeOf(Buffer)) Then                 { To much data }
       W := SizeOf(Buffer) Else W := Count;           { Size to transfer }
     S.Read(Buffer, W);                               { Read from stream }
     Write(Buffer, W);                                { Write to stream }
     Dec(Count, W);                                   { Dec write count }
   End;
END;

CONSTRUCTOR TDosStream.Init (FileName: FNameStr; Mode: Word);
BEGIN
   Inherited Init;                                    { Call ancestor }
   FileName := FileName+#0;                           { Make asciiz }
   Move(FileName[1], FName, Length(FileName));        { Create asciiz name }
   Handle := DosFileOpen(FName, Mode);                { Open the file }
   If (Handle=0) Then Begin                           { Open failed }
     Error(stInitError, DosStreamError);              { Call error }
     Status := stInitError;                           { Set fail status }
     Handle := -1;                                    { Set invalid handle }
   End;
END;

DESTRUCTOR TDosStream.Done;
BEGIN
   If (Handle <> -1) Then DosClose(Handle);           { Close the file }
   Inherited Done;                                    { Call ancestor }
END;

FUNCTION TDosStream.GetPos: LongInt;
VAR NewPosition: LongInt;
BEGIN
   If (Status=stOk) Then Begin                        { Check status okay }
     If (Handle = -1) Then DosStreamError := 103      { File not open }
       Else DosStreamError := DosSetFilePtr(Handle,
        0, 1, NewPosition);                           { Get file position }
     If (DosStreamError<>0) Then Begin                { Check for error }
        Error(stError, DosStreamError);               { Identify error }
        NewPosition := -1;                            { Invalidate position }
     End;
     GetPos := NewPosition;                           { Return file position }
   End Else GetPos := -1;                             { Stream in error }
END;

FUNCTION TDosStream.GetSize: LongInt;
VAR CurrentPos, FileEndPos: LongInt;
BEGIN
   If (Status=stOk) Then Begin                        { Check status okay }
     If (Handle = -1) Then DosStreamError := 103      { File not open }
       Else DosStreamError := DosSetFilePtr(Handle,
        0, 1, CurrentPos);                            { Current position }
     If (DosStreamError=0) Then Begin                 { Check no errors }
        DosStreamError := DosSetFilePtr(Handle, 0, 2,
          FileEndPos);                                { Locate end of file }
        If (DosStreamError=0) Then
          DosSetFilePtr(Handle, 0, 1, CurrentPos);    { Reset position }
     End;
     If (DosStreamError<>0) Then Begin                { Check for error }
        Error(stError, DosStreamError);               { Identify error }
        FileEndPos := -1;                             { Invalidate size }
     End;
     GetSize := FileEndPos;                           { Return file size }
   End Else GetSize := -1;                            { Stream in error }
END;

PROCEDURE TDosStream.Close;
BEGIN
   If (Handle <> -1) Then DosClose(Handle);           { Close the file }
   Handle := -1;                                      { Handle now invalid }
END;

PROCEDURE TDosStream.Seek (Pos: LongInt);
VAR NewPosition: LongInt;
BEGIN
   If (Status=stOk) Then Begin                        { Check status okay }
     If (Pos < 0) Then Pos := 0;                      { Negatives removed }
     If (Handle = -1) Then DosStreamError := 103      { File not open }
       Else DosStreamError := DosSetFilePtr(Handle,
         Pos, 0, NewPosition);                        { Set file position }
     If ((DosStreamError<>0) OR (NewPosition<>Pos))   { We have an error }
     Then Begin
       If (DosStreamError<>0) Then                    { Error was detected }
         Error(stError, DosStreamError)               { Specific seek error }
         Else Error(stSeekError, 0);                  { General seek error }
     End;
   End;
END;

PROCEDURE TDosStream.Open (OpenMode: Word);
BEGIN
   If (Handle = -1) Then Begin                        { File not open }
     Handle := DosFileOpen(FName, OpenMode);          { Open the file }
     If (Handle=0) Then Begin                         { File open failed }
       Error(stOpenError, DosStreamError);            { Call error }
       Handle := -1;                                  { Set invalid handle }
     End;
   End;
END;

PROCEDURE TDosStream.Read (Var Buf; Count: Sw_Word);
VAR BytesMoved: Sw_Word;
BEGIN
   If (Status=stOk) Then Begin                        { Check status }
     If (Handle = -1) Then BytesMoved := 0 Else       { File not open }
       DosStreamError := DosRead(Handle, Buf, Count,
         BytesMoved);                                 { Read from file }
     If ((DosStreamError<>0) OR (BytesMoved<>Count))  { We have an error }
     Then Begin
       If (DosStreamError<>0) Then                    { Error was detected }
         Error(stError, DosStreamError)               { Specific read error }
         Else Error(stReadError, 0);                  { General read error }
     End;
   End Else FillChar(Buf, Count, #0);                 { Error clear buffer }
END;

PROCEDURE TDosStream.Write (Var Buf; Count: Sw_Word);
VAR BytesMoved: Sw_Word;
BEGIN
   If (Status=stOk) Then Begin
     If (Handle=-1) Then BytesMoved := 0 Else         { File not open }
       DosStreamError := DosWrite(Handle, Buf, Count,
         BytesMoved);                                 { Write to file }
     If ((DosStreamError<>0) OR (BytesMoved<>Count))  { We have an error }
     Then Begin
       If (DosStreamError<>0) Then                    { Error was detected }
         Error(stError, DosStreamError)               { Specific write error }
         Else Error(stWriteError, 0);                 { General write error }
     End;
   End;
END;

CONSTRUCTOR TCollection.Init (ALimit, ADelta: Sw_Integer);
BEGIN
   Inherited Init;                                    { Call ancestor }
   Delta := ADelta;                                   { Set increment }
   SetLimit(ALimit);                                  { Set limit }
END;

CONSTRUCTOR TCollection.Load (Var S: TStream);
VAR C, I: Sw_Integer;
BEGIN
   S.Read(Count, SizeOf(Count));                      { Read count }
   S.Read(Limit, SizeOf(Limit));                      { Read limit }
   S.Read(Delta, SizeOf(Delta));                      { Read delta }
   Items := Nil;                                      { Clear item pointer }
   C := Count;                                        { Hold count }
   I := Limit;                                        { Hold limit }
   Count := 0;                                        { Clear count }
   Limit := 0;                                        { Clear limit }
   SetLimit(I);                                       { Set requested limit }
   Count := C;                                        { Set count }
   For I := 0 To C-1 Do AtPut(I, GetItem(S));         { Get each item }
END;

DESTRUCTOR TCollection.Done;
BEGIN
   FreeAll;                                           { Free all items }
   SetLimit(0);                                       { Release all memory }
END;

FUNCTION TCollection.At (Index: Sw_Integer): Pointer;
BEGIN
   If (Index < 0) OR (Index >= Count) Then Begin      { Invalid index }
     Error(coIndexError, Index);                      { Call error }
     At := Nil;                                       { Return nil }
   End Else At := Items^[Index];                      { Return item }
END;

FUNCTION TCollection.IndexOf (Item: Pointer): Sw_Integer;
VAR I: Sw_Integer;
BEGIN
   If (Count>0) Then Begin                            { Count is positive }
     For I := 0 To Count-1 Do                         { For each item }
       If (Items^[I]=Item) Then Begin                 { Look for match }
         IndexOf := I;                                { Return index }
         Exit;                                        { Now exit }
       End;
   End;
   IndexOf := -1;                                     { Return index }
END;

FUNCTION TCollection.GetItem (Var S: TStream): Pointer;
BEGIN
   GetItem := S.Get;                                  { Item off stream }
END;

FUNCTION TCollection.LastThat (Test: Pointer): Pointer;
VAR I: LongInt; P: FuncPtr; 

BEGIN
   ASM
     MOVL (%EBP), %EAX;                               { Load EBP }
     MOVL %EAX, U_OBJECTS_HOLDEBP;                    { Store to global }
   END;
   P := FuncPtr(Test);                                { Set function ptr }
   For I := Count DownTo 1 Do 
     Begin                   { Down from last item }
       Begin          { Test each item }
       LastThat := Items^[I-1];                       { Return item }
       Exit;                                          { Now exit }
       End;
     End;
   LastThat := Nil;                                   { None passed test }
END;

FUNCTION TCollection.FirstThat (Test: Pointer): Pointer;
VAR I: LongInt; P: FuncPtr; {$IFDEF NotFPKPascal} Hold_EBP: Sw_Word; {$ENDIF}
BEGIN
   ASM
     MOVL (%EBP), %EAX;                               { Load EBP }
     MOVL %EAX, U_OBJECTS_HOLDEBP;                    { Store to global }
   END;
   P := FuncPtr(Test);                                { Set function ptr }
   For I := 1 To Count Do Begin                       { Up from first item }
       Begin          { Test each item }
       FirstThat := Items^[I-1];                      { Return item }
       Exit;                                          { Now exit }
     End;
   End;
   FirstThat := Nil;                                  { None passed test }
END;

PROCEDURE TCollection.Pack;
VAR I, J: Sw_Integer;
BEGIN
   If (Count>0) Then Begin                            { Count is positive }
     I := 0;                                          { Initialize dest }
     For J := 1 To Count Do Begin                     { For each item }
       If (Items^[J]<>Nil) Then Begin                 { Entry is non nil }
         Items^[I] := Items^[J];                      { Transfer item }
         Inc(I);                                      { Advance dest }
       End;
     End;
     Count := I;                                      { Adjust count }
   End;
END;

PROCEDURE TCollection.FreeAll;
VAR I: Sw_Integer;
BEGIN
   For I := 0 To Count-1 Do FreeItem(At(I));          { Release each item }
   Count := 0;                                        { Clear item count }
END;

PROCEDURE TCollection.DeleteAll;
BEGIN
   Count := 0;                                        { Clear item count }
END;

PROCEDURE TCollection.Free (Item: Pointer);
BEGIN
   Delete(Item);                                      { Delete from list }
   FreeItem(Item);                                    { Free the item }
END;

PROCEDURE TCollection.Insert (Item: Pointer);
BEGIN
   AtInsert(Count, Item);                             { Insert item }
END;

PROCEDURE TCollection.Delete (Item: Pointer);
BEGIN
   AtDelete(IndexOf(Item));                           { Delete from list }
END;

PROCEDURE TCollection.AtFree (Index: Sw_Integer);
VAR Item: Pointer;
BEGIN
   Item := At(Index);                                 { Retreive item ptr }
   AtDelete(Index);                                   { Delete item }
   FreeItem(Item);                                    { Free the item }
END;

PROCEDURE TCollection.FreeItem (Item: Pointer);
VAR P: PObject;
BEGIN
   P := PObject(Item);                                { Convert pointer }
   If (P<>Nil) Then Dispose(P, Done);                 { Dispose of object }
END;

PROCEDURE TCollection.AtDelete (Index: Sw_Integer);
BEGIN
   If (Index >= 0) AND (Index < Count) Then Begin     { Valid index }
     Dec(Count);                                      { One less item }
     If (Count>Index) Then Move(Items^[Index+1],
      Items^[Index], (Count-Index)*Sizeof(Pointer));  { Shuffle items down }
   End Else Error(coIndexError, Index);               { Index error }
END;

PROCEDURE TCollection.ForEach (Action: Pointer);
VAR I: LongInt; P: ProcPtr; 

BEGIN
   ASM
     MOVL (%EBP), %EAX;                               { Load EBP }
     MOVL %EAX, U_OBJECTS_HOLDEBP;                    { Store to global }
   END;
   P := ProcPtr(Action);                              { Set procedure ptr }
   For I := 1 To Count Do                             { Up from first item }
       P(Items^[I-1], HoldEBP);                       { Call with each item }
END;

PROCEDURE TCollection.SetLimit (ALimit: Sw_Integer);
VAR AItems: PItemList;
BEGIN
   If (ALimit < Count) Then ALimit := Count;          { Stop underflow }
   If (ALimit > MaxCollectionSize) Then
     ALimit := MaxCollectionSize;                     { Stop overflow }
   If (ALimit <> Limit) Then Begin                    { Limits differ }
     If (ALimit = 0) Then AItems := Nil Else          { Alimit=0 nil entry }
       GetMem(AItems, ALimit * SizeOf(Pointer));      { Allocate memory }
     If (AItems<>Nil) OR (ALimit=0) Then Begin        { Check success }
       If (AItems <>Nil) AND (Items <> Nil) Then      { Check both valid }
         Move(Items^, AItems^, Count*SizeOf(Pointer));{ Move existing items }
       If (Limit <> 0) AND (Items <> Nil) Then        { Check old allocation }
         FreeMem(Items, Limit * SizeOf(Pointer));     { Release memory }
       Items := AItems;                               { Update items }
       Limit := ALimit;                               { Set limits }
     End;
   End;
END;

PROCEDURE TCollection.Error (Code, Info: Integer);
BEGIN
   RunError(212 - Code);                              { Run error }
END;

PROCEDURE TCollection.AtPut (Index: Sw_Integer; Item: Pointer);
BEGIN
   If (Index >= 0) AND (Index < Count) Then           { Index valid }
     Items^[Index] := Item                            { Put item in index }
     Else Error(coIndexError, Index);                 { Index error }
END;

PROCEDURE TCollection.AtInsert (Index: Sw_Integer; Item: Pointer);
VAR I: Sw_Integer;
BEGIN
   If (Index >= 0) AND (Index <= Count) Then Begin    { Valid index }
     If (Count=Limit) Then  SetLimit(Limit+Delta);    { Expand size if able }
     If (Limit>Count) Then Begin
       If (Index < Count) Then Begin                  { Not last item }
         For I := Count DownTo Index Do               { Start from back }
           Items^[I] := Items^[I-1];                  { Move each item }
       End;
       Items^[Index] := Item;                         { Put item in list }
       Inc(Count);                                    { Inc count }
     End Else Error(coOverflow, Index);               { Expand failed }
   End Else Error(coIndexError, Index);               { Index error }
END;

PROCEDURE TCollection.Store (Var S: TStream);

   PROCEDURE DoPutItem (P: Pointer); FAR;
   BEGIN
     PutItem(S, P);                                   { Put item on stream }
   END;

BEGIN
   S.Write(Count, SizeOf(Count));                     { Write count }
   S.Write(Limit, SizeOf(Limit));                     { Write limit }
   S.Write(Delta, SizeOf(Delta));                     { Write delta }
   ForEach(@DoPutItem);                               { Each item to stream }
END;

PROCEDURE TCollection.PutItem (Var S: TStream; Item: Pointer);
BEGIN
   S.Put(Item);                                       { Put item on stream }
END;

CONSTRUCTOR TSortedCollection.Init (ALimit, ADelta: Sw_Integer);
BEGIN
   Inherited Init(ALimit, ADelta);                    { Call ancestor }
   Duplicates := False;                               { Clear flag }
END;

CONSTRUCTOR TSortedCollection.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   S.Read(Duplicates, SizeOf(Duplicates));            { Read duplicate flag }
END;

FUNCTION TSortedCollection.KeyOf (Item: Pointer): Pointer;
BEGIN
   KeyOf := Item;                                     { Return item }
END;

FUNCTION TSortedCollection.IndexOf (Item: Pointer): Sw_Integer;
VAR I: Sw_Integer;
BEGIN
   IndexOf := -1;                                     { Preset result }
   If Search(KeyOf(Item), I) Then Begin               { Search for item }
     If Duplicates Then                               { Duplicates allowed }
       While (I < Count) AND (Item <> Items^[I]) Do
         Inc(I);                                      { Count duplicates }
     If (I < Count) Then IndexOf := I;                { Return result }
   End;
END;

FUNCTION TSortedCollection.Compare (Key1, Key2: Pointer): Sw_Integer;
BEGIN
   Abstract;                                          { Abstract method }
END;

FUNCTION TSortedCollection.Search (Key: Pointer; Var Index: Sw_Integer): Boolean;
VAR L, H, I, C: Sw_Integer;
BEGIN
   Search := False;                                   { Preset failure }
   L := 0;                                            { Start count }
   H := Count - 1;                                    { End count }
   While (L <= H) Do Begin
     I := (L + H) SHR 1;                              { Mid point }
     C := Compare(KeyOf(Items^[I]), Key);             { Compare with key }
     If (C < 0) Then L := I + 1 Else Begin            { Item to left }
       H := I - 1;                                    { Item to right }
       If C = 0 Then Begin                            { Item match found }
         Search := True;                              { Result true }
         If NOT Duplicates Then L := I;               { Force kick out }
       End;
     End;
   End;
   Index := L;                                        { Return result }
END;

PROCEDURE TSortedCollection.Insert (Item: Pointer);
VAR I: Sw_Integer;
BEGIN
   If NOT Search(KeyOf(Item), I) OR Duplicates Then   { Item valid }
     AtInsert(I, Item);                               { Insert the item }
END;

PROCEDURE TSortedCollection.Store (Var S: TStream);
BEGIN
   TCollection.Store(S);                              { Call ancestor }
   S.Write(Duplicates, SizeOf(Duplicates));           { Write duplicate flag }
END;

FUNCTION TStringCollection.GetItem (Var S: TStream): Pointer;
BEGIN
   GetItem := S.ReadStr;                              { Get new item }
END;

FUNCTION TStringCollection.Compare (Key1, Key2: Pointer): Sw_Integer;
VAR I, J: Integer; P1, P2: PString;
BEGIN
   P1 := PString(Key1);                               { String 1 pointer }
   P2 := PString(Key2);                               { String 2 pointer }
   If (Length(P1^)<Length(P2^)) Then J := Length(P1^)
     Else J := Length(P2^);                           { Shortest length }
   I := 1;                                            { First character }
   While (I<J) AND (P1^[I]=P2^[I]) Do Inc(I);         { Scan till fail }
   If (P1^[I]=P2^[I]) Then Compare := 0 Else          { Strings matched }
     If (P1^[I]<P2^[I]) Then Compare := -1 Else       { String1 < String2 }
        Compare := 1;                                 { String1 > String2 }
END;

PROCEDURE TStringCollection.FreeItem (Item: Pointer);
BEGIN
   DisposeStr(Item);                                  { Dispose item }
END;

PROCEDURE TStringCollection.PutItem (Var S: TStream; Item: Pointer);
BEGIN
   S.WriteStr(Item);                                  { Write string }
END;

PROCEDURE TUnSortedStrCollection.Insert (Item: Pointer);
BEGIN
   AtInsert(Count, Item);                             { NO sorting insert }
END;

FUNCTION TStream.Get: PObject;
BEGIN
END;

PROCEDURE TStream.Put (P: PObject);
BEGIN
END;

FUNCTION NewStr (Const S: String): PString;
VAR P: PString;
BEGIN
   If (S = '') Then P := Nil Else Begin               { Return nil }
     GetMem(P, Length(S) + 1);                        { Allocate memory }
     If (P<>Nil) Then P^ := S;                        { Hold string }
   End;
   NewStr := P;                                       { Return result }
END;

PROCEDURE DisposeStr (P: PString);
BEGIN
   If (P <> Nil) Then FreeMem(P, Length(P^) + 1);     { Release memory }
END;

PROCEDURE Abstract;
BEGIN
   RunError(211);                                     { Abstract error }
END;

PROCEDURE RegisterError;
BEGIN
   RunError(212);                                     { Register error }
END;

FUNCTION CreateStream (Strategy: Word; ReqSize: LongInt): PStream;
VAR Stream: PStream;
BEGIN
   Stream := Nil;                                     { Preset failure }
   While (Strategy <> 0) AND (Stream = Nil) Do Begin
     If (Strategy AND sa_XMSFirst <> 0) Then Begin    { ** XMS STREAM ** }
     End Else
     If (Strategy AND sa_EMSFirst <> 0) Then Begin    { ** EMS STREAM ** }
     End Else
     If (Strategy AND sa_RamFirst <> 0) Then Begin    { ** RAM STREAM ** }
     End Else
     If (Strategy AND sa_DiskFirst <> 0) Then Begin   { ** DISK STREAM ** }
     End;
     If (Stream<>Nil) AND (Stream^.Status <> stOk)    { Stream in error }
     Then Begin
       Dispose(Stream, Done);                         { Dispose stream }
       Stream := Nil;                                 { Clear pointer }
     End;
     Strategy := Strategy SHL 4;                      { Next strategy mask }
   End;
   CreateStream := Stream;                            { Return stream result }
END;

{ For linux we 'steal' the following from system unit, this way
  we don't need to change the system unit interface. }

Var errno : Longint;

{$i sysnr.inc}
{$i errno.inc}
{$i sysconst.inc}
{$i systypes.inc}
{$i syscalls.inc}

FUNCTION DosFileOpen (Var FileName: AsciiZ; Mode: Word): Word;

Var LinuxMode : Word;

BEGIN
  LinuxMode:=0;
  if (Mode and stCreate)=stCreate then LinuxMode:=Open_Creat;
  if (Mode and stOpenRead)=stOpenRead then LinuxMode:=LinuxMode or Open_RdOnly;
  If (Mode and stOpenWrite)=stOpenWrite then LinuxMode:=LinuxMode or Open_WrOnly;
  if (Mode and stOpen)=stOpen then LinuxMode:=LinuxMode or Open_RdWr;
  DosFileOpen:=SYS_Open (pchar(@FileName[0]),438 {666 octal},LinuxMode);
  DosStreamError:=Errno;
  DosFileOpen:=Errno;
END;

FUNCTION DosRead (Handle: Word; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;
BEGIN
  BytesMoved:=Sys_read (Handle,Pchar(@BufferArea),BufferLength);
  DosStreamError:=Errno;
END;

FUNCTION DosWrite (Handle: Word; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;
BEGIN
  BytesMoved:=Sys_Write (Handle,Pchar(@BufferArea),BufferLength);
  DosWrite:=Errno;
  DosStreamError:=Errno;
END;

FUNCTION DosSetFilePtr (Handle: Word; Pos: LongInt; MoveType: Word;
VAR NewPos: LongInt): Word;

BEGIN
  NewPos:=Sys_LSeek (Handle,Pos,MoveType);
  DosSetFilePtr:=Errno;
END;

PROCEDURE DosClose (Handle: Word);
BEGIN
  Sys_Close (Handle);
  DosStreamError:=Errno;
END;

END.

{
  $Log$
  Revision 1.2  1998-05-06 12:35:26  michael
  + Removed log from before restored version.

  Revision 1.1.1.1  1998/03/25 11:18:43  root
  * Restored version
}
