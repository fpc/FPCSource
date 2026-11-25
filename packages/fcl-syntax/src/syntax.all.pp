{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Registers the syntax highlighters for all languages.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE objfpc}
{$H+}

unit Syntax.All;

interface

uses
  syntax.ada, syntax.assembly, syntax.barye, syntax.basic, syntax.bash, syntax.batch,
  syntax.c, syntax.cpp, syntax.csharp, syntax.css, syntax.d, syntax.diff, syntax.dockerfile,
  syntax.fortran, syntax.go, syntax.html, syntax.ini, syntax.java, syntax.javascript,
  syntax.json, syntax.kotlin, syntax.lfm, syntax.lua, syntax.makefile, syntax.markdown,
  syntax.modula2, syntax.oberon, syntax.pascal, syntax.perl, syntax.php, syntax.powershell,
  syntax.python, syntax.rst, syntax.ruby, syntax.rust, syntax.sql, syntax.swift, syntax.toml,
  syntax.typescript, syntax.xml, syntax.yaml, syntax.zig;

implementation

end.
