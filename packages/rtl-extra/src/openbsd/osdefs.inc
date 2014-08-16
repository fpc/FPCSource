{
    Copyright (c) 2000-2002 by Marco van de Voort

    Target dependent defines used when compiling the baseunix unit
    Copied over to opensd directory because of
    missing SigTimedWait syscall

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
    MA 02110-1301, USA.

 ****************************************************************************}

{$define usedomain}             // Allow uname with "domain" entry.
                                // (which is a GNU extension)
{$define hassysctl}             // Use sysctl unit

{ SigTimedWait syscal does not seem to exist for OpenBSD OS }
{$define FPC_SYS_SIGTIMEDWAIT_UNAVAILABLE}
