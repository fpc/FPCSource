{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by the Free Pascal development team

    System Entry point for MorphOS, Pascal only programs

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit si_prc;

interface

implementation

const
  abox_signature: dword = 1; public name '__abox__';

var
  ExecBase: Pointer; public name '_ExecBase';
  realExecBase: Pointer absolute $4;

function PascalSysInit: longint; external name 'PASCALSYSINIT';

function _FPC_proc_start: longint; public name '_start';
begin
  ExecBase:=realExecBase;
  _FPC_proc_start:=PascalSysInit;
end;

end.
