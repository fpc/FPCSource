{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit syncobjs;

  interface

    uses
       sysutils;

    {$syncobjsh.inc}

  implementation

     uses
        windows;

    {$syncobjs.inc}

    procedure TCriticalSection.Acquire;

      begin
         EnterCriticalSection(CriticalSection);
      end;

    procedure TCriticalSection.Release;

      begin
         LeaveCriticalSection(CriticalSection);
      end;

    constructor TCriticalSection.Create;

      begin
         inherited Create;
         InitializeCriticalSection(CriticalSection);
      end;

    destructor TCriticalSection.Destroy;

      begin
         DeleteCriticalSection(CriticalSection);
         inherited Destroy;
      end;

end.

{
  $Log$
  Revision 1.1  1998-09-29 11:15:24  florian
    + initial revision

}
