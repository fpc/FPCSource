{
    $Id$
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
{$ifndef linux}
  {$S-}
{$endif}
unit objpas;

  interface

    { first, in object pascal, the integer type must be redefined }
    const
       MaxInt  = MaxLongint;
    type
       integer = longint;

{ Old compilers search for these variables in objpas unit }
{$ifndef SYSTEMTVARREC}
       pvarrec = system.pvarrec;
       tvarrec = system.tvarrec;
{$endif}

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


{$ifdef HasResourceStrings}
Type
   TResourceIterator = Function (Name,Value : AnsiString; Hash : Longint) : AnsiString;

   Function Hash(S : AnsiString) : longint;
   Procedure ResetResourceTables;
   Procedure SetResourceStrings (SetFunction :  TResourceIterator);
   Function ResourceStringTableCount : Longint;
   Function ResourceStringCount(TableIndex : longint) : longint;
   Function GetResourceStringName(TableIndex,StringIndex : Longint) : Ansistring;
   Function GetResourceStringHash(TableIndex,StringIndex : Longint) : Longint;
   Function GetResourceStringDefaultValue(TableIndex,StringIndex : Longint) : AnsiString;
   Function GetResourceStringCurrentValue(TableIndex,StringIndex : Longint) : AnsiString;
   Function SetResourceStringValue(TableIndex,StringIndex : longint; Value : Ansistring) : Boolean;

{$endif}


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

Procedure CloseFile(Var f:File);

begin
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

Procedure CloseFile(Var t:Text);

begin
  Close(T);
end;

{ Typed file supoort }

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
    if (Param>=0) and (Param<argc) then
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


{$IFDEF HasResourceStrings}

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
     g:=thehash and ($f shl 28);
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

Procedure SetResourceStrings (SetFunction :  TResourceIterator);

Var I,J : longint;

begin
  With ResourceStringTable do
    For I:=0 to Count-1 do
      With Tables[I]^ do
         For J:=0 to Count-1 do
           With ResRec[J] do
             CurrentValue:=SetFunction(Name,DefaultValue,HashValue);
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

{$endif}


Initialization
{$IFDEF HasResourceStrings}
  ResetResourceTables;
{$endif}

finalization

end.

{
  $Log$
  Revision 1.48  2000-02-09 16:59:32  peter
    * truncated log

  Revision 1.47  2000/01/07 16:41:44  daniel
    * copyright 2000

  Revision 1.46  2000/01/07 16:32:29  daniel
    * copyright 2000 added

  Revision 1.45  1999/12/20 11:20:35  peter
    * integer is defined as longint, removed smallint which is now in system

  Revision 1.44  1999/11/06 14:41:30  peter
    * truncated log

  Revision 1.43  1999/10/30 17:39:05  peter
    * memorymanager expanded with allocmem/reallocmem

  Revision 1.42  1999/10/03 19:41:30  peter
    * moved tvarrec to systemunit

  Revision 1.41  1999/09/28 21:13:33  florian
    * fixed bug 626, objpas must redefine maxint!

  Revision 1.40  1999/09/17 17:14:12  peter
    + new heap manager supporting delphi freemem(pointer)

  Revision 1.39  1999/08/28 13:03:23  michael
  + Added Hash function to interface

  Revision 1.38  1999/08/27 15:54:15  michael
  + Added many resourcestring methods

  Revision 1.37  1999/08/25 16:41:08  peter
    * resources are working again

  Revision 1.36  1999/08/24 22:42:56  michael
  * changed resourcestrings to new mechanism

  Revision 1.35  1999/08/24 12:02:29  michael
  + Changed external var for resourcestrings

  Revision 1.34  1999/08/20 10:50:55  michael
  + Fixed memory leak

  Revision 1.33  1999/08/19 19:52:26  michael
  * Fixed freemem bug; reported by Sebastian Guenther

  Revision 1.32  1999/08/15 21:28:57  michael
  + Pass hash also for speed reasons.

  Revision 1.31  1999/08/15 21:02:56  michael
  + Changed resource string mechanism to use names.

  Revision 1.30  1999/08/15 18:56:13  michael
  + Delphi-style getmem and freemem

  Revision 1.29  1999/07/23 23:13:54  peter
    * array[cardinal] is buggy, use array[word]
    * small fix in getresourcestring

  Revision 1.28  1999/07/23 22:51:11  michael
  * Added HasResourceStrings check

}
