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
          constructor create;
          constructor create(freeobjects : boolean);
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
      
    constructor tobjectlist.create;

      begin
         inherited create;
         ffreeobjects:=True;
      end;

end.
{
  $Log$
  Revision 1.2  2002-07-21 12:04:49  michael
  + No optional parameters in 1.0.6

  Revision 1.1  2002/07/16 13:34:39  florian
    + skeleton for contnr.pp added

}
