{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1997-98 by Pierre Muller

    Code to generate execution of all c functions
    with constructors attributes

    Based on .ctor and .dtor sections of DJGPP gcc compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit InitC;

interface

implementation

  type
     simple_proc = procedure;
  var
     first_ctor : simple_proc;external name 'djgpp_first_ctor';
     last_ctor  : simple_proc;external name 'djgpp_last_ctor';
     first_dtor : simple_proc;external name 'djgpp_first_dtor';
     last_dtor  : simple_proc;external name 'djgpp_last_dtor';
     bss_count : longint;external name '___bss_count';
  const
     save_exit : pointer = nil;

procedure run_c_constructors;

  const
     already_done : longint = -1;
  var
     f : simple_proc;
     i : longint;
  begin
     if already_done=bss_count then
       exit;
     already_done:=bss_count;
     f:=first_ctor;
     for i:=1 to ((longint(last_ctor)-longint(first_ctor)) div sizeof(pointer)) do
       begin
          f();
          inc(longint(f),sizeof(pointer));
       end;
  end;
  
procedure run_c_destructors;
  const
     already_done : longint = -1;
  var
     f : simple_proc;
     i : longint;
  begin
     exitproc:=save_exit;
     if already_done=bss_count then
       exit;
     already_done:=bss_count;
     f:=first_dtor;
     for i:=1 to ((longint(last_dtor)-longint(first_dtor)) div sizeof(pointer)) do
       begin
          f();
          inc(longint(f),sizeof(pointer));
       end;
  end;
  
begin
   run_c_constructors;
   If first_dtor<>last_dtor then
     begin
        { can exitproc be allready non nil here ?
          you have to make really weird things to achieve
          that be lets suppose it is possible !! (PM) }
        save_exit:=exitproc;
        exitproc:=@run_c_destructors;
     end;
end.

{
  $Log$
  Revision 1.1  1998-12-21 13:14:30  peter
    * moved

  Revision 1.1  1998/12/21 11:56:26  pierre
   First implementation of intc unit

}
