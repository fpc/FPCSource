{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Common types used by various readers and writers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpcrestypes;

{$MODE OBJFPC}

interface

type
  TResHdr32 = packed record
    rootptr     : longword;     //pointer to root node
    count       : longword;     //number of resources in the file
    usedhandles : longword;     //set at runtime
    handles     : longword;     //pointer to handles
  end;

  TResHdr64 = packed record
    rootptr     : qword;        //pointer to root node
    count       : longword;     //number of resources in the file
    usedhandles : longword;     //set at runtime
    handles     : qword;        //pointer to handles
  end;

  TResInfoNode32 = packed record
    nameid : longword;          //name offset / integer ID / languageID
    ncount : longword;          //named sub-entries count
    idcountsize : longword;     //id sub-entries count / resource size
    subptr : longword;          //first sub-entry offset
  end;

  TResInfoNode64 = packed record
    nameid : qword;             //name offset / integer ID / languageID
    ncount : longword;          //named sub-entries count
    idcountsize : longword;     //id sub-entries count / resource size
    subptr : qword;             //first sub-entry offset
  end;

implementation

end.
