{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Michael Van Canneyt (michael@freepascal.org) member of the Free Pascal development team

    compatibility types

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}



Type
  Tguint16 = guint16;
  TGType = GType;
  TGUINT = guint;
  TGpointer = gpointer;
  Tgchar = gchar;
  Tgdouble = gdouble;
  Tgssize = gssize;
//  TGError = GError;
  Tgint = gint;
  Tguint8 = guint8;
  Tgint64 = gint64;
//  TGObject = GObject;
//  TGBytes = GBytes;

  TGboolean = gboolean;
  Tguint64 = guint64;
  Tgulong = gulong;
  Tgconstpointer = gconstpointer;
  TGHashTable = record end;
  Tva_list = pointer;
  PPGDate = ^PGDate;
  PPGValue = PGValue;
  Tgfloat = gfloat;
  Tgsize = gsize;
  Tguint32 = guint32;
  Tgint32 = gint32;
  T_GBytes = record end;
  TGBytes = T_GBytes;
  PPGObject = ^PGObject;

  _GMutex = record
    case integer of
      0 : ( p : gpointer );
      1 : ( i : array [0..1] of guint);
  end;
  TGMutex = _gmutex;

  T_GRWLock = Record
    p : gpointer ;
    i : array[0..1] of guint;
  end;
  TGRWLock = T_GRWLock;
  GRWLock = T_GRWLock;

T_GCond = record
  p : gpointer ;
  i : array[0..1] of guint;
end;
GCond = T_GCond;
TGCond = GCond;

  T_GRecMutex = record
   p: gpointer ;
   i : Array [0..1] of guint;
  end;
  TGRecMutex = T_GRecMutex;


  PPGList = ^PGList;

  // GDateTime is opaque
  TGdateTime = record

  end;
