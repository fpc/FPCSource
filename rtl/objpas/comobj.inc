{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Florian Klaempfl
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_COMOBJ_HAS_CREATE_CLASS_ID}
{ any ideas for a generic implementation? }
function CreateClassID : ansistring;
  begin
     result:='';
  end;
{$endif FPC_COMOBJ_HAS_CREATE_CLASS_ID}

{
  $Log$
  Revision 1.1  2002-10-10 16:09:25  florian
    + initial revision
}
