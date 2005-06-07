{
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   implementation for FreeBSD/i386 specific functions

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$packrecords C}

TYPE uint=CARDINAL;

CONST
        I386_GET_LDT    =0;
        I386_SET_LDT    =1;
                                { I386_IOPL }
        I386_GET_IOPERM =3;
        I386_SET_IOPERM =4;
                                { xxxxx }
        I386_VM86       =6;

{
type i386_ldt_args = record
        int     start : longint;
        union   descriptor *descs;
        int     num;
        end;
}
type
 i386_ioperm_args = record
        start    : uint;
        length   : uint;
        enable   : longint;
        end;


    i386_vm86_args = record
        sub_op   : longint;             { sub-operation to perform }
        sub_args : pchar;               { args }
        end;

   sysarch_args     = record
                        op    : longint;
                        parms : pchar;
                       end;

{
int i386_get_ldt __P((int, union descriptor *, int));
int i386_set_ldt __P((int, union descriptor *, int));
int i386_get_ioperm __P((unsigned int, unsigned int *, int *));
int i386_set_ioperm __P((unsigned int, unsigned int, int));
int i386_vm86 __P((int, void *));
int i386_set_watch __P((int watchnum, unsigned int watchaddr, int size,
                        int access, struct dbreg * d));
int i386_clr_watch __P((int watchnum, struct dbreg * d));
}

Function IOPerm(From,Num:CARDINAL;Value:Longint):cint;

var sg : i386_ioperm_args;
    sa : sysarch_args;

begin
  sg.start:=From;
  sg.length:=Num;
  sg.enable:=value;
  sa.op:=i386_SET_IOPERM;
  sa.parms:=@sg;
  IOPerm:=do_syscall(syscall_nr_sysarch,longint(@sa));
end;

