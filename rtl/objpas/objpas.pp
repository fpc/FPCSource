{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998,99 by the Free Pascal development team

    This unit makes Free Pascal as much as possible Delphi compatible

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$Mode ObjFpc}
{$I-,S-}

unit objpas;

  interface

    type
       { first, in object pascal, the types must be redefined }
       smallint = system.integer;
       integer  = system.longint;

       { the compiler searches in the objpas unit for the tvarrec symbol }
       TVarRec = System.TVarRec;
       PVarRec = ^TVarRec;
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


    Procedure Getmem(Var p:pointer;Size:Longint);
    Procedure Freemem(Var p:pointer;Size:Longint);
    Procedure Freemem(Var p:pointer);


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

{ ---------------------------------------------------------------------
    Delphi-Style memory management
  ---------------------------------------------------------------------}

  Type PLongint = ^Longint;


    Procedure Getmem(Var p:pointer;Size:Longint);

    begin
      Inc(Size,SizeOf(Longint));
      SysGetmem(P,Size);
      PLongint(P)^:=Size;
      Inc(P,SizeOf(Longint));
    end;

    Procedure DummyFreemem(Var p:pointer;Size:Longint);
    begin
      FreeMem(P);
    end;

    Procedure Freemem(Var p:pointer;Size:Longint);

    begin
      Freemem(P);
    end;

    Procedure Freemem(Var p:pointer);

    begin
      If P<>Nil then
        begin
        Dec(P,SizeOf(Longint));
        SysFreemem(P,Plongint(P)^);
        end;
    end;


Var OldMM,NEWMM : TmemoryManager;

    Procedure InitMemoryManager;

    begin
      GetMemoryManager(OldMM);
      NewMM.FreeMem:=@DummyFreeMem;
      NewMM.GetMem:=@GetMem;
      SetMemoryManager(NewMM);
    end;

    Procedure ResetMemoryManager;
    begin
      SetMemoryManager(OldMM);
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
  InitMemoryManager;
finalization
  ResetMemoryManager;
end.

{
  $Log$
  Revision 1.39  1999-08-28 13:03:23  michael
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

  Revision 1.27  1999/07/22 20:30:13  michael
  + Implemented resource stuff

  Revision 1.26  1999/07/07 10:04:04  michael
  + Paramstr now returns cmdline args >255 chars in ansistring objpas.pp

  Revision 1.25  1999/07/06 22:44:22  florian
    + implemented a paramstr function which returns an ansistring, nevertheless
      it is limited to 255 chars because it maps to the system.paramstr, maybe
      we should use cmdline instead

  Revision 1.24  1999/05/17 21:52:43  florian
    * most of the Object Pascal stuff moved to the system unit

  Revision 1.23  1999/05/13 21:54:28  peter
    * objpas fixes

  Revision 1.22  1999/04/16 20:47:20  florian
    + tobject.messagestringtable function for Megido/GTK support
      added

  Revision 1.21  1999/02/23 14:04:36  pierre
   * call %edi => call *%edi

  Revision 1.20  1999/02/22 23:30:54  florian
    + TObject.Dispatch and TObject.DispatchStr added, working

  Revision 1.19  1998/12/24 10:12:03  michael
  Implemented AssignFile and CloseFile compatibility

  Revision 1.18  1998/10/12 12:42:58  florian
    * as operator runtime error can be now caught by an errorproc

  Revision 1.17  1998/10/05 12:32:53  peter
    + assert() support

  Revision 1.16  1998/10/03 15:07:16  florian
    + TObject.AfterConstruction and TObject.BeforeDestruction of Delphi 4

  Revision 1.15  1998/09/24 16:13:48  michael
  Changes in exception and open array handling

  Revision 1.14  1998/09/23 12:40:43  michael
  Fixed TVarRec again. Should be OK now

  Revision 1.13  1998/09/23 12:18:32  michael
  + added VType in TVArRec

  Revision 1.12  1998/09/23 10:00:47  peter
    * tvarrec should be 8 bytes

  Revision 1.11  1998/09/22 15:30:07  peter
    * array of const update

  Revision 1.9  1998/09/16 13:08:19  michael
  Added AbstractErrorHandler

  Revision 1.8  1998/09/06 21:27:31  florian
    + method tobject.classinfo added

  Revision 1.7  1998/09/04 08:49:06  peter
    * 0.99.5 doesn't compile a whole objpas anymore to overcome crashes

  Revision 1.6  1998/08/23 20:58:52  florian
    + rtti for objects and classes
    + TObject.GetClassName implemented

  Revision 1.5  1998/07/30 16:10:11  michael
  + Added support for ExceptProc+

  Revision 1.4  1998/07/29 15:44:33  michael
   included sysutils and math.pp as target. They compile now.

  Revision 1.3  1998/07/29 10:09:28  michael
  + put in exception support

  Revision 1.2  1998/03/25 23:40:24  florian
    + stuff from old objpash.inc and objpas.inc merged in

}
