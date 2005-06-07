{
Copyright © 1995 - 1998, 3Com Corporation or its subsidiaries ("3Com").
All rights reserved.

This software may be copied and used solely for developing products for
the Palm Computing platform and for archival and backup purposes.  Except
for the foregoing, no part of this software may be reproduced or transmitted
in any form or by any means or used to make any derivative work (such as
translation, transformation or adaptation) without express written consent
from 3Com.

3Com reserves the right to revise this software and to make changes in content
from time to time without obligation on the part of 3Com to provide notification
of such revision or changes.
3COM MAKES NO REPRESENTATIONS OR WARRANTIES THAT THE SOFTWARE IS FREE OF ERRORS
OR THAT THE SOFTWARE IS SUITABLE FOR YOUR USE.  THE SOFTWARE IS PROVIDED ON AN
"AS IS" BASIS.  3COM MAKES NO WARRANTIES, TERMS OR CONDITIONS, EXPRESS OR IMPLIED,
EITHER IN FACT OR BY OPERATION OF LAW, STATUTORY OR OTHERWISE, INCLUDING WARRANTIES,
TERMS, OR CONDITIONS OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, AND
SATISFACTORY QUALITY.

TO THE FULL EXTENT ALLOWED BY LAW, 3COM ALSO EXCLUDES FOR ITSELF AND ITS SUPPLIERS
ANY LIABILITY, WHETHER BASED IN CONTRACT OR TORT (INCLUDING NEGLIGENCE), FOR
DIRECT, INCIDENTAL, CONSEQUENTIAL, INDIRECT, SPECIAL, OR PUNITIVE DAMAGES OF
ANY KIND, OR FOR LOSS OF REVENUE OR PROFITS, LOSS OF BUSINESS, LOSS OF INFORMATION
OR DATA, OR OTHER FINANCIAL LOSS ARISING OUT OF OR IN CONNECTION WITH THIS SOFTWARE,
EVEN IF 3COM HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

3Com, HotSync, Palm Computing, and Graffiti are registered trademarks, and
Palm III and Palm OS are trademarks of 3Com Corporation or its subsidiaries.

IF THIS SOFTWARE IS PROVIDED ON A COMPACT DISK, THE OTHER SOFTWARE AND
DOCUMENTATION ON THE COMPACT DISK ARE SUBJECT TO THE LICENSE AGREEMENT
ACCOMPANYING THE COMPACT DISK.

 -------------------------------------------------------------------
  FileName:
             Common.inc

  Description:
             Common include file for all Pilot routines.
             Contains elementary data types

  History:
     10/19/94  RM - Created by Ron Marianetti
      4/24/97  SL - Changes for PalmOS 2.0 SDK
      8/05/98  Converted to pascal by Florian Klaempfl

}
    { Elementary data types }

    { Fixed size data types }
    type
       SByte = char;      // I'am not sure about that (FK)
       UInt16 = word;
       UInt32 = cardinal;
       SWord = integer;
       Int16 = integer;
       SDWord = longint;
       Int32 = longint;
       DWord = cardinal;
       UChar = char;      // I'am not sure about that (FK)
       Short = integer;
       UShort = word;
       Int = integer;
       UInt = word;
       Long = longint;
       ULong = cardinal;
       Err = integer;
       LocalID = DWord;

       { Pointer Types }
       VoidPtr = pointer;
       VoidHand = ^VoidPtr;
       SBytePtr = ^SByte;
       BytePtr = ^Byte;
       SWordPtr = ^SWord;
       WordPtr = ^Word;
       UInt16Ptr = ^word;
       SDWordPtr = ^SDWord;
       DWordPtr = ^DWord;

       { Logical data types }
       BooleanPtr = ^Boolean;

       CharPtr = ^Char;
       UCharPtr = ^UChar;
       ShortPtr = ^Short;
       UShortPtr = ^UShort;
       IntPtr = ^Int;
       UIntPtr = ^UInt;
       LongPtr = ^Long;
       ULongPtr = ^ULong;

       { Generic Pointer types used by Memory Manager }
       { We have to define Ptr as char* because that's what the Mac includes do. }
       { global pointer }
       Ptr = ^char;

       { global handle }
       Handle = ^Ptr;

       { Function types }
       ProcPtr = function : Long;cdecl;
