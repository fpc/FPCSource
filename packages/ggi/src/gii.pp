{
   Free Pascal conversion (c) 1999 Sebastian Guenther

   LibGII API header file

   Copyright (C) 1998 Andreas Beck      [becka@ggi-project.org]
   Copyright (C) 1999 Marcus Sundberg   [marcus@ggi-project.org]

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
   THE AUTHOR(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

{$MODE objfpc}
{$PACKRECORDS C}
{$LINKLIB c}

unit GII;

interface

const

  libgii = 'gii';

type

  TGIIEventMask = LongWord;

  TGIIEventType = (
    evNothing := 0,                     // event is not valid. (must be zero)

    evCommand,                          // report command/do action
    evInformation,                      // notification of new information

    evExpose,                           // exposure event
    // empty slot

    evKeyPress := 5,                    // key has been pressed
    evKeyRelease,                       // key has been released
    evKeyRepeat,                        // automatically repeated keypress

    evPtrRelative,                      // pointer movements reported relative
    evPtrAbsolute,                      // pointer movements reported absolute
    evPtrButtonPress,                   // pointer button pressed
    evPtrButtonRelease,                 // pointer button released

    evValRelative,                      // valuator change (reported relative)
    evValAbsolute,                      // valuator change (reported absolute)

    evLast                              // must be less than 33
  );

const
  emNothing             = 1 shl Ord(evNothing);
  emCommand             = 1 shl Ord(evCommand);
  emInformation         = 1 shl Ord(evInformation);
  emExpose              = 1 shl Ord(evExpose);
  emKeyPress            = 1 shl Ord(evKeyPress);
  emKeyRelease          = 1 shl Ord(evKeyRelease);
  emKeyRepeat           = 1 shl Ord(evKeyRepeat);
  emKey                 = emKeyPress or emKeyRelease or emKeyRepeat;
  emKeyboard            = emKey;
  emPtrRelative         = 1 shl Ord(evPtrRelative);
  emPtrAbsolute         = 1 shl Ord(evPtrAbsolute);
  emPtrButtonPress      = 1 shl Ord(evPtrButtonPress);
  emPtrButtonRelease    = 1 shl Ord(evPtrButtonRelease);
  emPtrMove             = emPtrRelative or emPtrAbsolute;
  emPtrButton           = emPtrButtonPress or emPtrButtonRelease;
  emPointer             = emPtrMove or emPtrButton;
  emValRelative         = 1 shl Ord(evValRelative);
  emValAbsolute         = 1 shl Ord(evValAbsolute);
  emValuator            = emValRelative or emValAbsolute;
  emZero                = 0;
  emAll                 = ((1 shl Ord(evLast)) - 1) and not emNothing;


{******************************************************************************
 Command/Information events
 ******************************************************************************}

  GII_CMDFLAG_NODATA    = 1 shl 31;     // Event has no data
  GII_CMDFLAG_PRIVATE   = 1 shl 30;     // The code is specific to a certain inputlib
  GII_CMDFLAG_EXTERNAL  = 1 shl 29;     // Event is sent to/from an external system (like LibGGI)



type

  TGIIEvent = record
    Size: Byte;
    {###}
  end;




type

  TGIIInput = Pointer;
  TGIIFilter = Pointer;




implementation

end.
