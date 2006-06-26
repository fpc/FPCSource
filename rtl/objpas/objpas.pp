{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This unit makes Free Pascal as much as possible Delphi compatible

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$Mode ObjFpc}
{$I-}
{$ifndef Unix}
  {$S-}
{$endif}
unit objpas;

  interface

    { first, in object pascal, the integer type must be redefined }
    const
       MaxInt  = MaxLongint;
    type
       Integer  = longint;
       PInteger = ^Integer;

       { Ansistring are the default }
       PString = PAnsiString;

       { array types }
       IntegerArray  = array[0..$effffff] of Integer;
       TIntegerArray = IntegerArray;
       PIntegerArray = ^IntegerArray;
       PointerArray  = array [0..512*1024*1024-2] of Pointer;
       TPointerArray = PointerArray;
       PPointerArray = ^PointerArray;
       TBoundArray = array of integer;

{****************************************************************************
                             Compatibility routines.
****************************************************************************}

    { Untyped file support }

     Procedure AssignFile(Var f:File;const Name:string);
     Procedure AssignFile(Var f:File;p:pchar);
     Procedure AssignFile(Var f:File;c:char);
     Procedure CloseFile(Var f:File);

     { Text file support }
     Procedure AssignFile(Var t:Text;const s:string);
     Procedure AssignFile(Var t:Text;p:pchar);
     Procedure AssignFile(Var t:Text;c:char);
     Procedure CloseFile(Var t:Text);

     { Typed file supoort }

     Procedure AssignFile(Var f:TypedFile;const Name:string);
     Procedure AssignFile(Var f:TypedFile;p:pchar);
     Procedure AssignFile(Var f:TypedFile;c:char);

     { ParamStr should return also an ansistring }
     Function ParamStr(Param : Integer) : Ansistring;

{****************************************************************************
                             Resource strings.
****************************************************************************}

   type
     TResourceIterator = Function (Name,Value : AnsiString; Hash : Longint;arg:pointer) : AnsiString;

   Function Hash(S : AnsiString) : longint;
   Procedure ResetResourceTables;
   Procedure SetResourceStrings (SetFunction :  TResourceIterator;arg:pointer);
   Function ResourceStringTableCount : Longint;
   Function ResourceStringCount(TableIndex : longint) : longint;
   Function GetResourceStringName(TableIndex,StringIndex : Longint) : Ansistring;
   Function GetResourceStringHash(TableIndex,StringIndex : Longint) : Longint;
   Function GetResourceStringDefaultValue(TableIndex,StringIndex : Longint) : AnsiString;
   Function GetResourceStringCurrentValue(TableIndex,StringIndex : Longint) : AnsiString;
   Function SetResourceStringValue(TableIndex,StringIndex : longint; Value : Ansistring) : Boolean;

   { Delphi compatibility }
   type
     PResStringRec=^AnsiString;
     TResStringRec=AnsiString;
   Function LoadResString(p:PResStringRec):AnsiString;


  implementation

{****************************************************************************
                             Compatibility routines.
****************************************************************************}

{ Untyped file support }

Procedure AssignFile(Var f:File;const Name:string);

begin
  System.Assign (F,Name);
end;

Procedure AssignFile(Var f:File;p:pchar);

begin
  System.Assign (F,P);
end;

Procedure AssignFile(Var f:File;c:char);

begin
  System.Assign (F,C);
end;

Procedure CloseFile(Var f:File); [IOCheck];

begin
  { Catch Runtime error/Exception }
  System.Close(f);
end;

{ Text file support }

Procedure AssignFile(Var t:Text;const s:string);

begin
  System.Assign (T,S);
end;

Procedure AssignFile(Var t:Text;p:pchar);

begin
  System.Assign (T,P);
end;

Procedure AssignFile(Var t:Text;c:char);

begin
  System.Assign (T,C);
end;

Procedure CloseFile(Var t:Text); [IOCheck];

begin
  { Catch Runtime error/Exception }
  System.Close(T);
end;

{ Typed file support }

Procedure AssignFile(Var f:TypedFile;const Name:string);

begin
  system.Assign(F,Name);
end;

Procedure AssignFile(Var f:TypedFile;p:pchar);

begin
  system.Assign (F,p);
end;

Procedure AssignFile(Var f:TypedFile;c:char);

begin
  system.Assign (F,C);
end;

Function ParamStr(Param : Integer) : Ansistring;

Var Len : longint;

begin
{
  Paramstr(0) should return the name of the binary.
  Since this functionality is included in the system unit,
  we fetch it from there.
  Normally, pathnames are less than 255 chars anyway,
  so this will work correct in 99% of all cases.
  In time, the system unit should get a GetExeName call.
}
  if (Param=0) then
    Result:=System.Paramstr(0)
  else if (Param>0) and (Param<argc) then
    begin
    Len:=0;
    While Argv[Param][Len]<>#0 do
      Inc(len);
    SetLength(Result,Len);
    If Len>0 then
      Move(Argv[Param][0],Result[1],Len);
    end
  else
    paramstr:='';
end;



{ ---------------------------------------------------------------------
    ResourceString support
  ---------------------------------------------------------------------}
Type

  PResourceStringRecord = ^TResourceStringRecord;
  TResourceStringRecord = Packed Record
     DefaultValue,
     CurrentValue : AnsiString;
     HashValue : longint;
     Name : AnsiString;
   end;

   TResourceStringTable = Packed Record
     Count : longint;
     Resrec : Array[Word] of TResourceStringRecord;
   end;
   PResourceStringTable = ^TResourceStringTable;

   TResourceTableList = Packed Record
     Count : longint;
     Tables : Array[Word] of PResourceStringTable;
     end;



Var
  ResourceStringTable : TResourceTablelist; External Name 'FPC_RESOURCESTRINGTABLES';

Function Hash(S : AnsiString) : longint;

Var thehash,g,I : longint;

begin
   thehash:=0;
   For I:=1 to Length(S) do { 0 terminated }
     begin
     thehash:=thehash shl 4;
     inc(theHash,Ord(S[i]));
     g:=thehash and longint($f shl 28);
     if g<>0 then
       begin
       thehash:=thehash xor (g shr 24);
       thehash:=thehash xor g;
       end;
     end;
   If theHash=0 then
     Hash:=Not(0)
   else
     Hash:=TheHash;
end;

Function GetResourceString(Const TheTable: TResourceStringTable;Index : longint) : AnsiString;[Public,Alias : 'FPC_GETRESOURCESTRING'];
begin
  If (Index>=0) and (Index<TheTAble.Count) then
     Result:=TheTable.ResRec[Index].CurrentValue
  else
     Result:='';
end;

(*
Function SetResourceString(Hash : Longint;Const Name : ShortString; Const Value : AnsiString) : Boolean;

begin
  Hash:=FindIndex(Hash,Name);
  Result:=Hash<>-1;
  If Result then
    ResourceStringTable.ResRec[Hash].CurrentValue:=Value;
end;
*)

Procedure SetResourceStrings (SetFunction :  TResourceIterator;arg:pointer);

Var I,J : longint;

begin
  With ResourceStringTable do
    For I:=0 to Count-1 do
      With Tables[I]^ do
         For J:=0 to Count-1 do
           With ResRec[J] do
             CurrentValue:=SetFunction(Name,DefaultValue,HashValue,arg);
end;


Procedure ResetResourceTables;

Var I,J : longint;

begin
  With ResourceStringTable do
  For I:=0 to Count-1 do
    With Tables[I]^ do
        For J:=0 to Count-1 do
          With ResRec[J] do
            CurrentValue:=DefaultValue;
end;

Procedure FinalizeResourceTables;

Var I,J : longint;

begin
  With ResourceStringTable do
  For I:=0 to Count-1 do
    With Tables[I]^ do
        For J:=0 to Count-1 do
          With ResRec[J] do
            CurrentValue:='';
end;
Function ResourceStringTableCount : Longint;

begin
  Result:=ResourceStringTable.Count;
end;

Function CheckTableIndex (Index: longint) : Boolean;
begin
  Result:=(Index<ResourceStringTable.Count) and (Index>=0)
end;

Function CheckStringIndex (TableIndex,Index: longint) : Boolean;
begin
  Result:=(TableIndex<ResourceStringTable.Count) and (TableIndex>=0) and
          (Index<ResourceStringTable.Tables[TableIndex]^.Count) and (Index>=0)
end;

Function ResourceStringCount(TableIndex : longint) : longint;

begin
  If not CheckTableIndex(TableIndex) then
     Result:=-1
  else
    Result:=ResourceStringTable.Tables[TableIndex]^.Count;
end;

Function GetResourceStringName(TableIndex,StringIndex : Longint) : Ansistring;

begin
  If not CheckStringIndex(Tableindex,StringIndex) then
    Result:=''
  else
    result:=ResourceStringTable.Tables[TableIndex]^.ResRec[StringIndex].Name;
end;

Function GetResourceStringHash(TableIndex,StringIndex : Longint) : Longint;

begin
  If not CheckStringIndex(Tableindex,StringIndex) then
    Result:=0
  else
    result:=ResourceStringTable.Tables[TableIndex]^.ResRec[StringIndex].HashValue;
end;

Function GetResourceStringDefaultValue(TableIndex,StringIndex : Longint) : AnsiString;

begin
  If not CheckStringIndex(Tableindex,StringIndex) then
    Result:=''
  else
    result:=ResourceStringTable.Tables[TableIndex]^.ResRec[StringIndex].DefaultValue;
end;

Function GetResourceStringCurrentValue(TableIndex,StringIndex : Longint) : AnsiString;

begin
  If not CheckStringIndex(Tableindex,StringIndex) then
    Result:=''
  else
    result:=ResourceStringTable.Tables[TableIndex]^.ResRec[StringIndex].CurrentValue;
end;

Function SetResourceStringValue(TableIndex,StringIndex : longint; Value : Ansistring) : Boolean;

begin
  Result:=CheckStringIndex(Tableindex,StringIndex);
  If Result then
   ResourceStringTable.Tables[TableIndex]^.ResRec[StringIndex].CurrentValue:=Value;
end;

Function LoadResString(p:PResStringRec):AnsiString;

begin
  Result:=p^;
end;


Initialization
  ResetResourceTables;
finalization
  FinalizeResourceTables;
end.
