{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe
    member of the Free Pascal development team.

    This file implements the helper routines for dyn. Arrays in FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

type
  TJByteArray = array of jbyte;
  TJShortArray = array of jshort;
  TJIntArray = array of jint;
  TJLongArray = array of jlong;
  TJCharArray = array of jchar;
  TJFloatArray = array of jfloat;
  TJDoubleArray = array of jdouble;
  TJObjectArray = array of JLObject;
  TJRecordArray = array of FpcBaseRecordType;
  TJStringArray = array of unicodestring;

const
  FPCJDynArrTypeJByte   = 'B';
  FPCJDynArrTypeJShort  = 'S';
  FPCJDynArrTypeJInt    = 'I';
  FPCJDynArrTypeJLong   = 'J';
  FPCJDynArrTypeJChar   = 'C';
  FPCJDynArrTypeJFloat  = 'F';
  FPCJDynArrTypeJDouble = 'D';
  FPCJDynArrTypeJObject = 'A';
  FPCJDynArrTypeRecord  = 'R';

{ 1-dimensional setlength routines

  Convention: aorg, is the current array, anew: is a newly allocated array of the
    size specified to setlength. The function either returns org if it had the
    right size already, or copies (part of) org in new and returns new.
}
function fpc_setlength_dynarr_generic(aorg, anew: JLObject; deepcopy: boolean; docopy: boolean = true): JLObject;
function fpc_setlength_dynarr_jrecord(aorg, anew: TJRecordArray; deepcopy: boolean): TJRecordArray;

{ array copying helpers }

procedure fpc_copy_shallow_array(src, dst: JLObject; srcstart: jint = -1; srccopylen: jint = -1);
procedure fpc_copy_jrecord_array(src, dst: TJRecordArray; srcstart: jint = -1; srccopylen: jint = -1);

{ multi-dimendional setlength routine: all intermediate dimensions are arrays
  of arrays, so that's the same for all array kinds. Only the type of the final
  dimension matters.

  org is the current array, new is a newly allocated array of the
  (multi-demensional) size specified to setlength.

  This routine uses the intermediate levels from the old array if possible so
  that an unchanged array remains in the same place.

  Warning: ndim must be >= 2 when this routine is called.
}
function fpc_setlength_dynarr_multidim(aorg, anew: TJObjectArray; deepcopy: boolean; ndim: longint; eletype: jchar): TJObjectArray;


function fpc_dynarray_copy(src: JLObject; start, len: longint; ndim: longint; eletype: jchar): JLObject;

