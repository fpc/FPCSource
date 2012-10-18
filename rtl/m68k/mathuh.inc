{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by Sven Barth
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$warning TODO}

type
  TFPUException = (exInvalidOp, exDenormalized, exZeroDivide,
                   exOverflow, exUnderflow, exPrecision);
  TFPUExceptionMask = set of TFPUException;

function GetExceptionMask: TFPUExceptionMask;
function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;

