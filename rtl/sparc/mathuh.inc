{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
 type
  TFPURoundingMode = (rmNearest, rmDown, rmUp, rmTruncate);
  TFPUPrecisionMode = (pmSingle, pmReserved, pmDouble, pmExtended);
  TFPUException = (exInvalidOp, exDenormalized, exZeroDivide,
                   exOverflow, exUnderflow, exPrecision);
  TFPUExceptionMask = set of TFPUException;

function GetRoundMode: TFPURoundingMode;
function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
function GetPrecisionMode: TFPUPrecisionMode;
function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
function GetExceptionMask: TFPUExceptionMask;
function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
procedure ClearExceptions(RaisePending: Boolean {$ifndef VER1_0}=true{$endif});

{
  $Log$
  Revision 1.2  2005-02-13 18:58:27  florian
    + FPU controll routines in math unit

  Revision 1.1  2003/09/01 20:46:32  peter
    * new dummies

  Revision 1.1  2003/04/24 09:14:22  florian
    * initial implementation
}