{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
    History:

    Added DoMethodA, DoSuperMethodA, CoerceMethodA and SetSuperAttrsA.

    I've translated those from amigae. I'm not sure that they are
    correct but it's a start. Now you can try to make some tests
    with mui.
    30 Jul 2000.

    Added stuff for commodities.
    FreeIEvents
    CxCustom
    CxDebug
    CxFilter
    CxSender
    CxSignal
    CxTranslate
    19 Aug 2000.

    Rewrote Createport and DeletePort.
    06 Sep 2000.

    Added two printf, one with pchar and one with string.
    They use array of const so this unit compiles with
    mode objfpc.
    05 Nov 2002.

    Added the define use_amiga_smartlink
    13 Jan 2003.

    nils.sjoholm@mailbox.swipnet.se
}
{$INLINE ON}
{$mode objfpc}
unit amigalib
  deprecated 'Unit will be removed. Functions are moved to exec, intuition, utility and commodities unit.';


INTERFACE

uses exec,intuition,utility,commodities,inputevent,amigados;

// moved to exec, use them from there
{*  Exec support functions from amiga.lib  *}

procedure BeginIO (ioRequest: pIORequest); inline;
function CreateExtIO (port: pMsgPort; size: Longint): pIORequest; inline;
procedure DeleteExtIO (ioReq: pIORequest); inline;
function CreateStdIO (port: pMsgPort): pIOStdReq; inline;
procedure DeleteStdIO (ioReq: pIOStdReq); inline;
function CreatePort (name: PChar; pri: longint): pMsgPort; inline;
procedure DeletePort (port: pMsgPort); inline;
function CreateTask (name: STRPTR; pri: longint;
                     initPC : Pointer;
             stackSize : ULONG): pTask; inline;
procedure DeleteTask (task: pTask); inline;
procedure NewList (list: pList); inline;

// moved to commodities, use them from there
{* Commodities support functions from amiga.lib *}
procedure FreeIEvents (events: pInputEvent); inline;
function CxCustom
                (action: pointer;
                id: longint): pCxObj; inline;

function CxDebug (id: long): pCxObj; inline;
function CxFilter (d: STRPTR): pCxObj; inline;
function CxSender
                (port: pMsgPort;
                id: longint): pCxObj; inline;

function CxSignal
                (task: pTask;
                sig: byte): pCxObj; inline;

function CxTranslate (ie: pInputEvent): pCxObj; inline;

// moved to intuition, use them from there
function DoMethodA(obj : pObject_; msg : APTR): ulong; inline;
function DoSuperMethodA(cl : pIClass; obj : pObject_; msg : APTR): ulong; inline;
function CoerceMethodA(cl : pIClass; obj : pObject_; msg : APTR): ulong; inline;
function SetSuperAttrsA(cl : pIClass; obj: pObject_; msg : APTR): ulong; inline;

function DoMethod(obj: PObject_; Params: array of DWord): LongWord; inline;

// moved to utility, use them from there
procedure HookEntry;
procedure HookEntryPas;

{

   NAME
        printf - print a formatted output line to the standard output.

   SYNOPSIS
        printf(formatstring [,value [,values] ] );

   FUNCTION
        Format the output in accordance with specifications in the format
        string.

   INPUTS
        formatString - a C-language-like NULL-terminated format string,
                       with the following supported % options:

          %[flags][width][.limit][length]type

            $     - must follow the arg_pos value, if specified
          flags   - only one allowed. '-' specifies left justification.
          width   - field width. If the first character is a '0', the
                    field is padded with leading 0s.
            .     - must precede the field width value, if specified
          limit   - maximum number of characters to output from a string.
                    (only valid for %s or %b).
          length  - size of input data defaults to word (16-bit) for types c,
                    d, u and x, 'l' changes this to long (32-bit).
          type    - supported types are:
                          b - BSTR, data is 32-bit BPTR to byte count followed
                              by a byte string. A NULL BPTR is treated as an
                              empty string. (V36)
                          d - signed decimal
                          u - unsigned decimal
                          x - hexadecimal with hex digits in uppercase
                          X - hexadecimal with hex digits in lowercase
                          s - string, a 32-bit pointer to a NULL-terminated
                              byte string. A NULL pointer is treated
                              as an empty string.
                          c - character

        value(s) - numeric variables or addresses of null-terminated strings
                   to be added to the format information.

   NOTE
        The global "_stdout" must be defined, and contain a pointer to
        a legal AmigaDOS file handle. Using the standard Amiga startup
        module sets this up. In other cases you will need to define
        stdout, and assign it to some reasonable value (like what the
        dos.library/Output() call returns). This code would set it up:

                ULONG stdout;
                stdout=Output();

   BUGS
        This function will crash if the resulting stream after
        parameter substitution is longer than 140 bytes.

}

procedure printf(Fmtstr : pchar; const Args : array of const);
procedure printf(Fmtstr : string; const Args : array of const);

IMPLEMENTATION

{*  Exec support functions from amiga.lib  *}

procedure BeginIO (ioRequest: pIORequest); inline;
begin
  Exec.BeginIO(ioRequest);
end;

function CreateExtIO (port: pMsgPort; size: Longint): pIORequest; inline;
begin
  CreateExtIO := Exec.CreateExtIO(port, size);
end;

procedure DeleteExtIO (ioReq: pIORequest); inline;
begin
  Exec.DeleteExtIO(ioReq);
end;

function CreateStdIO (port: pMsgPort): pIOStdReq; inline;
begin
    CreateStdIO := Exec.CreateStdIO(port)
end;

procedure DeleteStdIO (ioReq: pIOStdReq); inline;
begin
    Exec.DeleteStdIO(ioReq)
end;

function Createport(name : PChar; pri : longint): pMsgPort; inline;
begin
  Createport := Exec.Createport(name, pri);
end;

procedure DeletePort (port: pMsgPort); inline;
begin
  Exec.DeletePort(port);
end;

function CreateTask (name: STRPTR; pri: longint; initPC: pointer; stackSize: ULONG): pTask; inline;
begin
  CreateTask := Exec.CreateTask(name, pri, initPC, stacksize);
end;

procedure DeleteTask (task: pTask); inline;
begin
  Exec.DeleteTask(task)
end;

procedure NewList (list: pList); inline;
begin
  Exec.NewList(list);
end;


procedure FreeIEvents (events: pInputEvent); inline;
begin
  Commodities.FreeIEvents(events);
end;

function CxCustom(action: pointer; id: longint): pCxObj; inline;
begin
  CxCustom := Commodities.CxCustom(action, id)
end;

function CxDebug(id: long): pCxObj; inline;
begin
  CxDebug := Commodities.CxDebug(id)
end;

function CxFilter(d: STRPTR): pCxObj; inline;
begin
  CxFilter := Commodities.CxFilter(d);
end;

function CxSender(port: pMsgPort; id: longint): pCxObj; inline;
begin
  CxSender := Commodities.CxSender(port, id)
end;

function CxSignal(task: pTask; sig: byte): pCxObj; inline;
begin
  CxSignal:= Commodities.CxSignal(task, sig)
end;

function CxTranslate (ie: pInputEvent): pCxObj;
begin
  CxTranslate := Commodities.CxTranslate(ie)
end;

function DoMethodA(obj : pObject_; msg : APTR): ulong; inline;
begin
  DoMethodA := Intuition.DoMethodA(obj, msg);
end;

function DoMethod(obj: PObject_; Params: array of DWord): LongWord; inline;
begin
  DoMethod := Intuition.DoMethodA(obj, @Params);
end;

function DoSuperMethodA(cl : pIClass; obj : pObject_; msg : APTR): ulong; inline;
begin
  DoSuperMethodA := Intuition.DoSuperMethodA(cl, obj, msg);
end;

function CoerceMethodA(cl : pIClass; obj : pObject_; msg : APTR): ulong; inline;
begin
  CoerceMethodA := Intuition.CoerceMethodA(cl, obj, msg);
end;

function SetSuperAttrsA(cl : pIClass; obj: pObject_; msg : APTR): ulong; inline;
begin
  SetSuperAttrsA := Intuition.SetSuperAttrsA(cl, obj, msg);
end;

{ Do *NOT* change this to nostackframe! }
{ The compiler will build a stackframe with link/unlk. So that will actually correct
  the stackpointer for both Pascal/StdCall and Cdecl functions, so the stackpointer
  will be correct on exit. It also needs no manual RTS. The argument push order is
  also correct for both. (KB) }
procedure HookEntry; assembler;
asm
  move.l a1,-(a7)    // Msg
  move.l a2,-(a7)    // Obj
  move.l a0,-(a7)    // PHook
  move.l 12(a0),a0   // h_SubEntry = Offset 12
  jsr (a0)           // Call the SubEntry
end;

{ This is to be used with when the subentry function uses FPC's register calling
  convention, also see the comments above HookEntry. It is advised to actually
  declare Hook functions with cdecl instead of using this function, especially
  when writing code which is platform independent. (KB) }
procedure HookEntryPas; assembler;
asm
  move.l a2,-(a7)
  move.l a1,-(a7)    // Msg
  move.l a2,a1       // Obj
                     // PHook is in a0 already
  move.l 12(a0),a2   // h_SubEntry = Offset 12
  jsr (a2)           // Call the SubEntry
  move.l (a7)+,a2
end;

procedure printf(Fmtstr : pchar; const Args : array of const);
var
  i,j : longint;
  argarray : array of longint;
  strarray : array of RawByteString;
begin
  SetLength(argarray, length(args));
  SetLength(strarray, length(args));
  j:=0;
  for i := low(args) to High(args) do
    begin
      case args[i].vtype of
        vtinteger : argarray[i] := longint(args[i].vinteger);
        vtpchar   : argarray[i] := longint(args[i].vpchar);
        vtchar    : argarray[i] := longint(args[i].vchar);
        vtpointer : argarray[i] := longint(args[i].vpointer);
        vtstring  : begin
            strarray[j]:=RawByteString(args[i].vstring^);
            argarray[i]:=longint(PChar(strarray[j]));
            inc(j);
          end;
      end;
    end;
  VPrintf(Fmtstr,@argarray[0]);
end;

procedure printf(Fmtstr : string; const Args : array of const);
begin
  printf(PChar(RawByteString(Fmtstr)), Args);
end;


end.
