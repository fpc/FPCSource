{ lNet FastCGI Spawner

  CopyRight (C) 2006-2008 Ales Katona

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit lSpawnFCGI;

{$mode objfpc}{$H+}

interface

uses
  Sockets, lNet, lCommon;

  function SpawnFCGIProcess(App, Enviro: string; const aPort: Word): Integer;

implementation

{$ifdef UNIX}
  {$i lspawnfcgiunix.inc}
{$else}
  {$i lspawnfcgiwin.inc}
{$endif}

end.

