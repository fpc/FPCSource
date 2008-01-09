{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    External resource support
    !!!NEVER USE THIS UNIT DIRECTLY!!!

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpextres;

{$inline on}

interface

implementation

{$ifdef UNIX}
uses
  baseunix;
{$endif}

{$ifdef DARWIN}
  {$include extres_multiarch.inc}
{$else}
  {$ifdef UNIX}
    {$define EXTRES_MMAP}
  {$else}
    {$define EXTRES_GENERIC}
  {$endif}
  {$include extres.inc}
{$endif}

initialization
  InitResources;
  SetResourceManager(ExternalResourceManager);

finalization
  FinalizeResources;

end.
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    External resource support
    !!!NEVER USE THIS UNIT DIRECTLY!!!

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpextres;

{$inline on}

interface

implementation

{$ifdef UNIX}
uses
  baseunix;
{$endif}

{$ifdef DARWIN}
  {$include extres_multiarch.inc}
{$else}
  {$ifdef UNIX}
    {$define EXTRES_MMAP}
  {$else}
    {$define EXTRES_GENERIC}
  {$endif}
  {$include extres.inc}
{$endif}

initialization
  InitResources;
  SetResourceManager(ExternalResourceManager);

finalization
  FinalizeResources;

end.
