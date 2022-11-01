{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Michael Van Canneyt (michael@freepascal.org) member of the Free Pascal development team

    Record types needed in several include files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type
  PGstMiniObject = ^TGstMiniObject;

  TGstMiniObjectCopyFunction = function (obj:PGstMiniObject):PGstMiniObject;cdecl;
  TGstMiniObjectDisposeFunction = function (obj:PGstMiniObject):Tgboolean;cdecl;
  TGstMiniObjectFreeFunction = procedure (obj:PGstMiniObject);cdecl;
  TGstMiniObjectNotify = procedure (user_data:Tgpointer; obj:PGstMiniObject);cdecl;

  T_GstMiniObject = record
      _type : TGType;
      refcount : Tgint;
      lockstate : Tgint;
      flags : Tguint;
      copy : TGstMiniObjectCopyFunction;
      _dispose : TGstMiniObjectDisposeFunction;
      free : TGstMiniObjectFreeFunction;
      priv_uint : Tguint;
      priv_pointer : Tgpointer;
    end;
  TGstMiniObject = T_GstMiniObject ;
  PPGstMiniObject = ^PGstMiniObject;
  TGstClockTime =  guint64;
  TGstClockTimeDiff = gint64;

  P_GstBufferPoolAcquireParams = ^T_GstBufferPoolAcquireParams;
  T_GstBufferPoolAcquireParams = record
      format : TGstFormat;
      start : Tgint64;
      stop : Tgint64;
      flags : TGstBufferPoolAcquireFlags;
      _gst_reserved : array[0..3] of Tgpointer;
    end;
  TGstBufferPoolAcquireParams = T_GstBufferPoolAcquireParams;

  PGstBufferPoolPrivate  = ^TGstBufferPoolPrivate;
  TGstBufferPoolPrivate = record end;
  T_GstBufferPoolPrivate = TGstBufferPoolPrivate;

  GInitiallyUnowned = TGObject;
  TGInitiallyUnowned = GInitiallyUnowned;
  TGInitiallyUnownedClass = TGObjectClass;

  P_GstObject = ^T_GstObject;
  PGstObject = P_GstObject;

  P_GstObjectClass = ^T_GstObjectClass;
  T_GstObjectClass = record
      parent_class : TGInitiallyUnownedClass;
      path_string_separator : Pgchar;
      deep_notify : procedure (object_:PGstObject; orig:PGstObject; pspec:PGParamSpec);cdecl;
      _gst_reserved : array[0..3] of Tgpointer;
    end;
  TGstObjectClass = T_GstObjectClass;

  T_GstObject = record
      object_ : TGInitiallyUnowned;
      lock : TGMutex;
      name : Pgchar;
      parent : PGstObject;
      flags : Tguint32;
      control_bindings : PGList;
      control_rate : Tguint64;
      last_sync : Tguint64;
      _gst_reserved : Tgpointer;
    end;
  TGstObject = T_GstObject;

  PPGstObject = ^PGstObject;

  P_GstBufferPool = ^T_GstBufferPool;
  T_GstBufferPool = record
      object_ : TGstObject;
      flushing : Tgint;
      priv : PGstBufferPoolPrivate;
      _gst_reserved : array[0..3] of Tgpointer;
    end;
  TGstBufferPool = T_GstBufferPool;
  PPGstBufferPool = ^P_GstBufferPool;

  PGstBufferPool = P_GstBufferPool;
  T_GstBuffer = record
      mini_object : TGstMiniObject;
      pool : PGstBufferPool;
      pts : TGstClockTime;
      dts : TGstClockTime;
      duration : TGstClockTime;
      offset : Tguint64;
      offset_end : Tguint64;
    end;
  TGstBuffer = T_GstBuffer;
  PGstBuffer = ^TGstBuffer;
  PPGstBuffer = ^PGstBuffer;

  T_GstMessage = record
      mini_object : TGstMiniObject;
      _type : TGstMessageType;
      timestamp : Tguint64;
      src : PGstObject;
      seqnum : Tguint32;
      lock : TGMutex;
      cond : GCond;
    end;
  TGstMessage = T_GstMessage;
  P_GstMessage  = ^T_GstMessage;
  PGstMessage  = ^TGstMessage;
  PPGstMessage = ^PGstMessage;


  TGStBusPrivate = record end;
  PGstBusPrivate  = ^TGstBusPrivate;
  T_GstBus = record
      object_ : TGstObject;
      priv : PGstBusPrivate;
     _gst_reserved : array[0..3] of Tgpointer;
  end;
  TGstBus = T_GstBus;
  PGstBus = ^TGstBus;

  TGstClockPrivate = record
  end;
  PGstClockPrivate  = ^TGstClockPrivate;

  T_GstClock = record
      object_ : TGstObject;
      priv : PGstClockPrivate;
      _gst_reserved : array[0..3] of Tgpointer;
    end;
  TGstClock = T_GstClock;
  GstClock = T_GstClock;
  PGstClock  = ^GstClock;


  T_GstElement = record
      object_ : TGstObject;
      state_lock : TGRecMutex;
      state_cond : TGCond;
      state_cookie : Tguint32;
      target_state : TGstState;
      current_state : TGstState;
      next_state : TGstState;
      pending_state : TGstState;
      last_return : TGstStateChangeReturn;
      bus : PGstBus;
      clock : PGstClock;
      base_time : TGstClockTimeDiff;
      start_time : TGstClockTime;
      numpads : Tguint16;
      pads : PGList;
      numsrcpads : Tguint16;
      srcpads : PGList;
      numsinkpads : Tguint16;
      sinkpads : PGList;
      pads_cookie : Tguint32;
      contexts : PGList;
      _gst_reserved : array[0..(4-1)-1] of Tgpointer;
    end;
  TGstElement = T_GstElement;
  PGstElement = ^TGstElement;


