{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe,
    members of the Free Pascal development team.

    This file declares support infrastructure for records under the JVM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  { the JLCloneable interface does not declare any methods, but JLObject.clone()
    throws an exception if you try to clone a class that does not implement this
    interface }
  FpcBaseRecordType = class abstract (JLObject, JLCloneable)
    constructor create;
    { create a deep copy, overridden by actual record types }
    function clone: JLObject;override;
    { create an empty instance of the current type }
    function newEmpty: FpcBaseRecordType;
    procedure fpcDeepCopy(result: FpcBaseRecordType);virtual;abstract;
  end;

