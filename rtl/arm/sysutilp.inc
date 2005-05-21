{
    This file is part of the Free Pascal run time library.

    Copyright (c) 2001 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ ---------------------------------------------------------------------
  This include contains cpu-specific routines
  ---------------------------------------------------------------------}


{ the ARM doesn't know multiprocessor system which would require locking }


function InterLockedDecrement (var Target: longint) : longint;
  begin
    dec(Target);
    result:=target;
  end;


function InterLockedIncrement (var Target: longint) : longint;
  begin
    inc(Target);
    result:=target;
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint;
  begin
    Result:=Target;
    Target:=Source;
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  begin
    Result:=Target;
    inc(Target,Source);
  end;


{
  $Log: sysutilp.inc,v $
  Revision 1.3  2005/02/14 17:13:21  peter
    * truncate log

}
