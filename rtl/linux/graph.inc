{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Graph include file for linux.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ decide what to load }
{$ifdef USEGGI}
{ use GGI libs }
{$i ggigraph.inc}
{$else}
{ use plain old vgalib }
{$i vgagraph.inc}
{$endif}
