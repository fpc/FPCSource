{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe,
    members of the Free Pascal development team.

    This file declares support infrastructure for sets under the JVM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  { Adds support for a "base" value that is used as lower bound for the set's
    contents }

  FpcBitSet = class sealed (JUBitSet,JLCloneable)
    { so we can look it up in this class rather than only in the parent }
    function clone: JLObject;override;
    function add(elem: jint): FpcBitSet;
    function addAll(s: FpcBitSet): FpcBitSet;
    function remove(elem: jint): FpcBitSet;
    function removeAll(s: FpcBitSet): FpcBitSet;
    function retainAll(s: FpcBitSet): FpcBitSet;
    function contains(elem: jint): boolean;
    function containsAll(s: FpcBitSet): boolean;
    function symdif(s: FpcBitSet): FpcBitSet;
    class function range(start, stop: jint): FpcBitSet; static;
    class function &of(elem: jint): FpcBitSet; static;
  end;

