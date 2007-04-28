/*
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by contributors of the Free Pascal Compiler

    pbeexe.cpp

    This file is part of the Pascal interface for the c++ API on Symbian OS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************/
#include <e32std.h>
#include <e32base.h>

extern "C"
{

/* Pascal Entry point declaration */
long int Pascal_E32Main();

/*******************************************************************
*  File e32std.h
*******************************************************************/

/*******************************************************************
*  Class User
*******************************************************************/

/*******************************************************************
*  User::InfoPrint
*******************************************************************/
TInt User_InfoPrint(const char* aString)
{
    TPtrC8 pStr(reinterpret_cast<const TUint8*>(aString));
    HBufC* buf = HBufC::New(pStr.Length());
    if (buf == NULL)
   	{
    	return KErrNoMemory;
    }
    buf->Des().Copy(pStr);
    User::InfoPrint(*buf);
    return KErrNone;
}


} /* extern "C" */

/*******************************************************************
*  Symbian OS Entry Point
*******************************************************************/
TInt E32Main()
{
     return Pascal_E32Main();
}
