Unit BSD;
{
   This file is part of the Free Pascal run time library.
   (c) 2005 by Marco van de Voort member of the 
   Free Pascal development team.
   Original kqueue implementation converted by Ales Katona 30.01.2006

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This unit is meant to contain all BSD specific routines. Not all
   routines might be implemented on all BSD platforms. 
 
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$IFDEF FPC}
  {$PACKRECORDS C}
  {$Inline On}
  {$Macro On}
  {$ifdef FPC_USE_LIBC}
  {$define extdecl:=cdecl; external 'c'}
  {$else}
  {$define extdecl:=inline}
  {$endif}
{$ENDIF}

interface

uses
  {$IFDEF FPC_USE_LIBC} initc, {$endif}
  Unix;

const
  EVFILT_READ     = -1;
  EVFILT_WRITE    = -2;
  EVFILT_AIO      = -3;  { attached to aio requests }
  EVFILT_VNODE    = -4;  { attached to vnodes }
  EVFILT_PROC     = -5;  { attached to struct proc }
  EVFILT_SIGNAL   = -6;  { attached to struct proc }
  EVFILT_TIMER    = -7;  { timers }
  EVFILT_NETDEV   = -8;  { network devices }
  EVFILT_FS       = -9;  { filesystem events }

  EVFILT_SYSCOUNT = 9;

  EV_ADD          = $0001;  { add event to kq }
  EV_DELETE       = $0002;  { delete event from kq  }
  EV_ENABLE       = $0004;  { enable event  }
  EV_DISABLE      = $0008;  { disable event (not reported)  }
  
{ flags  }
  EV_ONESHOT      = $0010;  { only report one occurrence  }
  EV_CLEAR        = $0020;  { clear event state after reporting  }
  EV_SYSFLAGS     = $F000;  { reserved by system  }
  EV_FLAG1        = $2000;  { filter-specific flag  }
  
{ returned values  }
  EV_EOF          = $8000;  { EOF detected  }
  EV_ERROR        = $4000;  { error, data contains errno  }
  
{ data/hint flags for EVFILT_READ|WRITE, shared with userspace   }
  NOTE_LOWAT      = $0001;  { low water mark  }
  
{ data/hint flags for EVFILT_VNODE, shared with userspace  }
  NOTE_DELETE     = $0001;  { vnode was removed  }
  NOTE_WRITE      = $0002;  { data contents changed  }
  NOTE_EXTEND     = $0004;  { size increased  }
  NOTE_ATTRIB     = $0008;  { attributes changed  }
  NOTE_LINK       = $0010;  { link count changed  }
  NOTE_RENAME     = $0020;  { vnode was renamed  }
  NOTE_REVOKE     = $0040;  { vnode access was revoked  }
  
{ data/hint flags for EVFILT_PROC, shared with userspace   }
  NOTE_EXIT       = $80000000;  { process exited  }
  NOTE_FORK       = $40000000;  { process forked  }
  NOTE_EXEC       = $20000000;  { process exec'd  }
  NOTE_PCTRLMASK  = $f0000000;  { mask for hint bits  }
  NOTE_PDATAMASK  = $000fffff;  { mask for pid  }
  
{ additional flags for EVFILT_PROC  }
  NOTE_TRACK      = $00000001;  { follow across forks  }
  NOTE_TRACKERR   = $00000002;  { could not track child  }
  NOTE_CHILD      = $00000004;  { am a child process  }
  
{ data/hint flags for EVFILT_NETDEV, shared with userspace  }
  NOTE_LINKUP     = $0001;  { link is up  }
  NOTE_LINKDOWN   = $0002;  { link is down  }
  NOTE_LINKINV    = $0004;  { link state is invalid  }

type
  PKEvent = ^TKEvent;
  TKEvent = record
    Ident  : PtrUInt;      { identifier for this event }
    Filter : cshort;     { filter for event }
    Flags  : cushort;
    FFlags : cuint;
    Data   : PtrInt;
    uData  : Pointer;    { opaque user data identifier }
  end;

function kqueue: cint; extdecl;
  
function kevent(kq: cint; ChangeList: PKEvent; nChanged: cint;
                  EventList: PKevent; nEvents: cint; Timeout: PTimeSpec): cint; extdecl;

procedure EV_SET(kevp: PKEvent; const aIdent: PtrUInt; const aFilter: cshort;
                   const aFlags: cushort; const aFFlags: cuint;
                   const aData: PtrInt; const auData: Pointer);  inline;

implementation

{$IFNDEF FPC_USE_LIBC}
uses
  SysCall;
{$endif}

procedure EV_SET(kevp: PKEvent; const aIdent: PtrUInt; const aFilter: cshort; const aFlags: cushort;
                                const aFFlags: cuint; const aData: PtrInt; const auData: Pointer); inline;
begin
  kevp^.Ident  := aIdent;
  kevp^.Filter := aFilter;
  kevp^.Flags  := aFlags;
  kevp^.FFlags := aFFlags;
  kevp^.Data   := aData;
  kevp^.uData  := auData;
end;

{$ifndef FPC_USE_LIBC}
function KQueue: cint; inline;
begin
  KQueue:=Do_SysCall(syscall_nr_kqueue);
end;

function KEvent(kq: cint; ChangeList: PKEvent; nChanged: cint;
                EventList: PKevent; nEvents: cint; Timeout: PTimeSpec): cint; inline;
begin
  KEvent:=Do_SysCall(syscall_nr_kevent, kq, TSysParam(ChangeList), nChanged,
                     TSysParam(EventList), nEvents, TSysParam(Timeout));
end;
{$ENDIF}

end.
