{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Internal resource support
    !!!NEVER USE THIS UNIT DIRECTLY!!!

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpintres;

interface

implementation

{$ifdef FPC_HAS_RESOURCES}
  {$ifdef WINDOWS}
    {$include winres.inc}
  {$else}
    {$include intres.inc}
  {$endif}

  initialization
    SetResourceManager(InternalResourceManager);
{$endif}

end.
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Internal resource support
    !!!NEVER USE THIS UNIT DIRECTLY!!!

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpintres;

interface

implementation

{$ifdef FPC_HAS_RESOURCES}
  {$ifdef WINDOWS}
    {$include winres.inc}
  {$else}
    {$include intres.inc}
  {$endif}

  initialization
    SetResourceManager(InternalResourceManager);
{$endif}

end.
