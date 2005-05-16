{ Source provided for Free Pascal Bug Report 3222 }
{ Submitted by "Christian Iversen" on  2004-07-28 }
{ e-mail: chrivers@iversen-net.dk }
(******************************************************************************
 *
 *  (C)Copyright 1999-2003 Technetium Group.  All Rights Reserved.
 *
 *  File:          LibXThread.pas
 *  Content:
 *
 *  Compatiblity:  32-Bit, Delphi, FPC approved
 *
 *  Programmer:    Ivo Steinmann
 *  E-Mail:        isteinmann@bluewin.ch
 *  Homepage:      http://www.technetium.dk
 *
 *  $Date: 2004/08/27 21:05:10 $
 *  $Rev: 1651 $
 *
 *****************************************************************************)


Unit tw3222;

{$mode delphi}

Interface

Type
  XThreadMethod = Function: LongInt Of Object;

  PThreadSyncRec = ^XThreadSyncRec;
  XThreadSyncRec = Record
    Method : XThreadMethod;
  End;

  PSyncRecArray = ^XSyncRecArray;
  XSyncRecArray = Array[Byte] Of PThreadSyncRec;

Implementation

Function TXThreadSynchronizerProcess: LongInt;
Var
  Local: PSyncRecArray;
Begin
  With Local[0]^ Do
  Begin
    Result := Method; // Doesn't work
    Result := Method(); // Works
  End;
End;

End.
