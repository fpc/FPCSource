{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 by the Free Pascal development team

    This unit provides an interface to the Objective-C 1.0
    run time as defined by Apple

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit objc1;

interface

{$ifdef darwin}
const
  libname = 'objc';
  {$linkframework Cocoa}
  {$define targetok}
{$endif}

{$ifndef targetok}
  {$error Add support for the current target to the objc1 unit }
{$endif}

implementation

end.
