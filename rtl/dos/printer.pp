{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  History:
  10.4.1994: Version 1.0
             Unit is completely implemented
}
 
unit printer;

  interface
  
    var
       lst : text;
       
  implementation
  
    var
       old_exit : pointer;
  
    procedure printer_exit;
  
      begin
         close(lst);
         exitproc:=old_exit;
      end;
    
begin
   assign(lst,'PRN');
   rewrite(lst);
   old_exit:=exitproc;
   exitproc:=@printer_exit;
end.

{
  $Log$
  Revision 1.1  1998-03-25 11:18:41  root
  Initial revision

  Revision 1.3  1998/01/26 11:56:59  michael
  + Added log at the end


  
  Working file: rtl/dos/printer.pp
  description:
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:15:48;  author: michael;  state: Exp;  lines: +13 -6
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:50;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:50;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
