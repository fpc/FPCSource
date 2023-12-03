# Dotted units tools

# Intro
This directory contains tools to convert the sources of units or programs so
their uses clause contains dotted names.

It can also generate a dotted version of a unit in one of several ways.

The prefixer unit and the namespacetool unit contain the functionality
to change uses clauses in unit files.

# Config files for the tools

The tools expect one or two files:

## File transformation rules

A file with rules to apply to units.

Each line is constructed as follows:
  
```
FileName=Rule;Compile Options 
```

Here

*  FileName is the undotted unit name to convert a dotted unit.

*  Rule is the rule to construct the dotted unit. See below for the rule.

*  Compile options are compile options as understood by fcl-pascal needed to  correctly parse the unit source

This file is used to construct dotted unit files from undotted files.

The file must be contain all files in a give directory grouped:

If a line contains no rule or compile options, the last used rule/compile options for a file in the same directory is reused.

```
src/sysutils.pp=System;-S2
src/classes.pp
src/math/math.pp
```

will result in classes being parsed with -S2 and the resulting name will be System.classes
the name of the math unit will not be changed.


## Known units rules

A file with OldName=Rule pairs to apply to unit names in a uses clause. a Rule may never specify a path
or an extension. The same extension as the original is used. 

This file is used by the prefixunits tool.

# Conversion Rules

A rule can take the following forms:
  
"*DottedUnitName" : Use the dotted name as typed.
   
Example:

```
sysutils=*System.SysUtils
```

The resulting file is called System.SysUtils

"Prefix" : Prepend the non-dotted name with the given prefix, separated by a dot.

Example:

```
sysutils=system
```

will result in a file system.sysutils
 
"Prefix,*UnitSuffix" : This is equivalent to *Prefix.UnitSuffix

Example:

```
sysutils=System,*SysUtils 
```
  
will result in System.SysUtils


"Prefix,-TextToDelete" strips the indicated part from the start of the original filename and prepends the result with Prefix. 

Example:

```
fpreportdata=FpReport,-fpreport 
```

Will result in FpReport.Data

"Prefix,TextToDelete-" strips the indicated part from the end of the original filename and prepends the result with Prefix. 

Example:

```
elfresource=System.Resources,resource-
```


Will result in System.Resources.elf
  

# Available tools

## addnamespacetofpmake.pp

Utility to add a statement to add a namespace to a fpmake program file for FPC packages.
This can be used to let fpmake dynamically change non-dotted names to dotted
names in its targets and dependencies.

## conditionalprefix.pp

Takes a unit name from standard input, pretty print it (first letter
uppercased) and add a conditional define for a namespace.

```
echo "sysutils" | conditionalprefix System will result in 
```

```
{$IFDEF FPC_DOTTEDUNITS}System{$ENDIF}.Sysutils
```

To be used in shell scripts or to be invoked from an editor 
that allows you to filter a selection through a command

## dond.pp

Tool to lowercase values in a Name=Value list

(use to construct lowercase rules)

## fixuses.pp

Read a complete uses clause from standard input, and replace it with a conditional uses clause that 
allows dotted and non-dotted units. The command needs a file with unit transform rules to apply
to the units in the uses clause.

To be used in shell scripts or to be invoked from an editor 
that allows you to filter a selection through a command.

Example:

```
echo "uses sysutils, classes" | fixuses known.txt 
```

results in

```
{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes;
{$ELSE}
uses sysutils, classes
{$ENDIF}
```


## genunitnames.pp

Read a list of unit file transformations and generate a list of unit names for use in a Makefile.
The output consists of a XYZUNIT variable for each unit in the file list, twice: once dotted, once not dotted.
The variables can be used in target definitions and dependency lists.

Example

```
genunitnames list.txt
```

with list.txt containing

```
sysutils=*System.SysUtils
classes=*System.Classes
```

results in 

```
ifdef FPC_DOTTEDUNITS
SYSUTILSUNIT=System.SysUtils
CLASSESUNIT=System.Classes
else
SYSUTILSUNIT=sysutils
CLASSESUNIT=classes
endif
```

## makedottedfiles.pp

Application to Prefix units in uses clause of a list of programs, and
generate a dotted version of the unit. Optionally adapts an fpmake file.

This tool accepts a lot of options, run with -h to get an explanation of
what it does.

It needs 2 files to be specified using the command-line options: 
a rule file of units to treat and the 'known mappings' rule file.    

## prefixunits.pp

Prefix units in a uses clause of a single program or unit.
This is the main tool: it allows you to change the uses clause of your
program or unit so it uses dotted names, based on a list of known aliases.



## proxyunit.pp

Generate a skeleton unit with namespaced name which defines FPC_DOTTEDUNITS and 
includes the original non-dotted unit. The full path to the skeleton unit
must be given, the original non-dotted unit must be given only as a name.
The extension is optional, when not specified, .pp is assumed.

Example:

```
proxyunit namespaced/System.SysUtils.pp sysutils.pp
```

results in

```
unit System.SysUtils;
{$DEFINE FPC_DOTTEDUNITS}
{$i sysutils.pp}
```
## replaceunitnames.pp

Replace hardcoded unit names xyz in a Makefile rule by a variable XYZUNIT.
(see genunitnames for how to create the variables). Needs a rule file with
names of units that may be replaced.

Example:

replaceunitnames list.txt Makefile.fpc

with list.txt
```
sysutils=*System.SysUtils
classes=*System.Classes
```
and the Makefile:

```
sysutils($PPUEXT): sysutils.pp
	$(COMPILER) sysutils.pp

classes($PPUEXT): classes.pp sysutils.pp 
	$(COMPILER) classes.pp
```

is transformed to
sysutils($PPUEXT): $(SYSUTILSUNIT).pp
        $(COMPILER) $(SYSUTILSUNIT).pp

classes($PPUEXT): $(CLASSESUNIT).pp $(SYSUTILSUNIT).pp 
        $(COMPILER) $(CLASSESUNIT).pp
	

## reworkmakefile.pp  

Duplicate all rules in a Makefile.fpc [Rules] section according to the rules specified
in the aliases file. Skip rules that are in the skip file. Every rule is
duplicated so 2 targets are defined: a dotted target and a non-dotted
target. More than one file can be specified.

# Known aliases

The known.txt file contains the aliases list for the FPC rtl and packages.
This can be used to convert a project to use dotted names.