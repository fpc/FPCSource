{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE OBJFPC}

{ determine the type of the resource/form file }
{$define Win16Res}

unit Classes;

interface

uses
  strings,
  sysutils;

{$i classesh.inc}

implementation

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

end.
{
  $Log$
  Revision 1.9  1999-02-02 21:32:02  michael
  - removed osfile.inc. All in sysutils now

  Revision 1.8  1998/11/04 10:46:43  peter
    * exceptions work

  Revision 1.7  1998/11/04 10:15:13  peter
    * fixes to compile

  Revision 1.6  1998/10/02 09:17:57  michael
  Removed objpas from uses clause

  Revision 1.5  1998/09/23 07:46:57  michael
  * patches by TSE

  Revision 1.4  1998/06/10 21:53:09  michael
  + Implemented Handle/FileStreams

  Revision 1.3  1998/05/06 13:00:25  michael
  + Added strings to uses clause, for TStrings class.

  Revision 1.2  1998/05/04 14:31:51  michael
  + Split classes file.

  Revision 1.1  1998/05/04 12:16:01  florian
    + Initial revisions after making a new directory structure

}
