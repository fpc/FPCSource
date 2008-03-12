{
    Copyright (c) 2008 by Giulio Bernardi

    Common resource target infos

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

unit rescmn;

{$i fpcdefs.inc}

interface

uses
  Systems;

    const
       res_elf_info : tresinfo =
          (
             id     : res_elf;
             resbin : 'fpcres';
             rescmd : '-o $OBJ -a $ARCH -of elf $DBG';
             { cross compiled windres can be used to compile .rc files on linux }
             rcbin  : 'windres';
             rccmd  : '--include $INC -O res -o $RES $RC';
             resourcefileclass : nil;
             resflags : [];
          );

       res_ext_info : tresinfo =
          (
             id     : res_ext;
             resbin : 'fpcres';
             rescmd : '-o $OBJ -a $ENDIAN -of external $DBG';
             rcbin  : 'windres';
             rccmd  : '--include $INC -O res -o $RES $RC';
             resourcefileclass : nil;
             resflags : [res_external_file];
          );


implementation

end.
