{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2002 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit contnrs;

  interface

    uses
       Classes;

    type
       TObjectList = class(TList)
       private
          ffreeobjects : boolean;
       public
          constructor create(freeobjects : boolean = true);
       end;

       TOrderedList = class
       end;

       TQueue = class(TOrderedList)
       end;

  implementation

    constructor tobjectlist.create(freeobjects : boolean);

      begin
         inherited create;
         ffreeobjects:=freeobjects;
      end;

end.
{
  $Log$
  Revision 1.1  2002-07-16 13:34:39  florian
    + skeleton for contnr.pp added

}
