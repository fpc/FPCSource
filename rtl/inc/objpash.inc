{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1997,98 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{***********************************************************************
  This file contains the base declarations of Object Pascal
  as declared by Delphi 3.0

  This file is intended to be used with Free Pascal and should be
  included in the system unit.
 ***********************************************************************}

type
  tobject = class;   
  tclass = class of tobject;

  tobject = class
     { please don't change the order of virtual methods, because      }
     { their vmt offsets are used by some assembler code which uses   }
     { hard coded addresses      (FK)                                 }
     constructor create;
     destructor destroy;virtual;
     class function newinstance : tobject;virtual;
     procedure freeinstance;virtual;
     procedure free;
     class function initinstance(instance : pointer) : tobject;
     procedure cleanupinstance;
     function classtype : tclass;
     class function classinfo : pointer;
     class function classname : shortstring;
     class function classnameis(const name : string) : boolean;
     class function classparent : tclass;
     class function instancesize : longint;                                          
     class function inheritsfrom(aclass : tclass) : boolean;

     { message handling routines }
     procedure dispatch(var message);
     procedure defaulthandler(var message);virtual;

     class function methodaddress(const name : shortstring) : pointer;
     class function methodname(address : pointer) : shortstring;
     function fieldaddress(const name : shortstring) : pointer;

     { interface functions, I don't know if we need this }
{
     function getinterface(const iid : tguid;out obj) : boolean;
     class function getinterfaceentry(const iid : tguid) : pinterfaceentry;
     class function getinterfacetable : pinterfacetable;
}
     function safecallexception(exceptobject : tobject;
       exceptaddr : pointer) : integer;virtual;
  end;

  var
     abstracterrorproc : pointer;

{
  $Log$
  Revision 1.1  1998-03-25 11:18:43  root
  Initial revision

  Revision 1.5  1998/01/27 22:05:10  florian
    * again small fixes to DOM (Delphi Object Model)

  Revision 1.4  1998/01/26 12:00:25  michael
  + Added log at the end


  
  Working file: rtl/inc/objpash.inc
  description:
  ----------------------------
  revision 1.3
  date: 1998/01/23 15:54:49;  author: florian;  state: Exp;  lines: +5 -2
    + small extensions to FCL object model
  ----------------------------
  revision 1.2
  date: 1998/01/23 10:48:32;  author: florian;  state: Exp;  lines: +5 -1
    * syntax errors fixed
    + implementation of all methods added, at least with empty body
  ----------------------------
  revision 1.1
  date: 1998/01/09 16:05:43;  author: florian;  state: Exp;
    + ojbpash.inc and objpas.inc
    * $E- from objects.pp removed to avoid a warning
  =============================================================================
}
