{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1999 by Berczi Gabor

    Conditional defines logic for the IDE

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ --- Exclude debugger support --- }
{.$DEFINE NODEBUG}

{ --- Text lines end with LF only --- }
{$ifndef FPC}
  {$define TPUNIXLF}
{$endif}

{ --- Include VESA support --- }
{$ifndef LINUX}
  {$ifndef FV20}
    {$define VESA}
  {$endif}
{$endif}

