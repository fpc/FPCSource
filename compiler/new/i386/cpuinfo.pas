{
    $Id$
    This file is part of the Free Pascal compiler
    Copyright (c) 1998-2000 by Florian Klaempfl

    Basic Processor information for i386

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit CPUInfo;

Interface

Type
   { Architecture word - Native unsigned type }
{$ifdef FPC}
   AWord = DWord;
{$else FPC}
   AWord = Longint;
{$endif FPC}

Const
   { Size of native extended type }
   extended_size = 10;
   
Implementation

end.