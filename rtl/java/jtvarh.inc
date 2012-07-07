{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe,
    member of the Free Pascal development team.

    This file implements support routines for threadvarq with FPC/JVM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{ In Java, threadvars are represented by descendnts of java.lang.ThreadLocal.
  This class has three important methods: set, get and initialValue.

  If you call the "get" method of a JLThreadLocal instance in a thread for the
  first time before calling "set", it will call the initialValue method and
  return its result. After that, it will return whatever initialValue returned,
  or the previous value instated by "set". A JLThreadLocal always keeps track of
  a JLObject.

  We don't want to translate accesses to threadvars into calls to get/set, since
  that would mean that we would
    a) have to generate different code in the compiler for read and write
       accesses
    b) no be able to pass threadvars to var-parameters etc

  Instead, we add a method called getReadWriteReference to all of our
  descendants classes that returns a pointer to the actual value of the
  threadvar for this thread. This results in several cases:
    a) For primitive types, we store an array of one element of that ordinal
       type.

       Their initialValue is simply an array of one element (automatically
       initialized to 0).

       The pointer returned is the "address" of element 0 (pointers to primitive
       types are internally arrays pointing to element 0).

    b) For non-dynamic arrays, we store that array itself (all arrays are
       internally Java arrays, which descend from JLObject).

       When initializing the threadvar on startup, we pass an empty copy of such
       an array to the constructor and store it. Their initialValue is a deep
       copy of this array, created by fpc_dynarray_copy (it accepts a number of
       dimensions, because it also has to work for making a copy of dynamic
       arrays whose elements are regular arrays).

       The pointer returned is simply the address of the array.

    c) For implicit pointer types other than regular arrays, we also store the
       implicit pointer itself and keep an initialized empty instance around
       that is passed to the constructor.

       Their initialValue is a clone of this empty instance (can't use this for
       arrays, since it would make a shallow copy of the array). Because of the
       way the JLCloneable interface works, we have to call the clone method via
       reflection.

       The pointer returned is again simply the implicit pointer itself.

    d) For all other types, we store an array of JLObject of one element,
       similar as with primitive types.

       Their initialValue is either nil, or optionally a value passed to the
       constructor when creating the JLThreadLocal instance (e.g. an empty
       string for unicodestring/ansistring, or the enum instance whose ordinal
       value is 0)

       The pointer returned is the address of element 0 of the array.
}

type
  FpcImplicitPtrThreadVar = class(JLThreadLocal)
    protected
     { all implicit pointer types are clonable }
     fInstanceToClone: JLObject;
     { don't look up the clone method every time }
     fCloneMethod: JLRMethod;
     function initialValue: JLObject; override;
    public
     constructor create(initInstanceToClone: JLObject);
     function getReadWriteReference: Pointer;
  end;


  FpcNormalArrayThreadVar = class sealed (FpcImplicitPtrThreadVar)
    protected
     fArrDim: longint;
     fArrTyp: widechar;
     function initialValue: JLObject; override;
    public
     constructor create(initInstanceToClone: JLObject; arrdim: longint; arrtyp: widechar);
  end;


  FpcBooleanThreadVar = class sealed (JLThreadLocal)
   protected
    function initialValue: JLObject; override;
   public
    function getReadWriteReference: PBoolean;
  end;

  FpcByteThreadVar = class sealed (JLThreadLocal)
   protected
    function initialValue: JLObject; override;
   public
    function getReadWriteReference: PShortint;
  end;

  FpcShortThreadVar = class sealed (JLThreadLocal)
   protected
    function initialValue: JLObject; override;
   public
    function getReadWriteReference: PSmallint;
  end;

  FpcIntThreadVar = class sealed (JLThreadLocal)
   protected
    function initialValue: JLObject; override;
   public
    function getReadWriteReference: PLongint;
  end;

  FpcLongThreadVar = class sealed (JLThreadLocal)
   protected
    function initialValue: JLObject; override;
   public
    function getReadWriteReference: PInt64;
  end;

  FpcCharThreadVar = class sealed (JLThreadLocal)
   protected
    function initialValue: JLObject; override;
   public
    function getReadWriteReference: PWideChar;
  end;

  FpcFloatThreadVar = class sealed (JLThreadLocal)
   protected
    function initialValue: JLObject; override;
   public
    function getReadWriteReference: PSingle;
  end;

  FpcDoubleThreadVar = class sealed (JLThreadLocal)
   protected
    function initialValue: JLObject; override;
   public
    function getReadWriteReference: PDouble;
  end;

  FpcPointerThreadVar = class sealed (JLThreadLocal)
   protected
    fInitVal: JLObject;
    function initialValue: JLObject; override;
   public
    function getReadWriteReference: PPointer;
    constructor create(initVal: JLObject);overload;
  end;

