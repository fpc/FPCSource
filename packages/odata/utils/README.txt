
This program converts a ODATA service description to a pascal unit.
This is either CSDL (OData V2) or EDMX (OData V4) document.

It needs the WST (Web Services Toolkit) to compile, you will need to
download this separately from the Lazarus-CCR:
https://svn.code.sf.net/p/lazarus-ccr/svn/wst/trunk

------------------
About the sources:
------------------
The sources use a small trick to treat v4 and v2 OData formats: EDMX and CSDL. 

While describing virtually the same thing, the underlying EDMX and CSDL 
are nonetheless quite different at times. 

To cater for this, a single unit is made (edmx2pas) which uses a define USECSDL 
to differentiate between V2 (USECSDL defined) and V4 (USECSDL not defined). 
The csdl2pas unit just sets the define and includes edmx2pas.

This trick may confuse your IDE (Lazarus in my case).

-----
Usage
-----

Usage is explained by running the program

a --aliases=aliases       Schema aliases as comma-separated name=value pairs.
                          The form @aliases reads from file "aliases", one alias per line.

an CSDL or EDMX data description uses a schema namespace. 
This means a set of dotted names, used in all identifiers. 
For example, the Microsoft Graph api uses microsoft.graph. 
By default all dots are replaced by underscores, and the type or identifier name is appended to it.
You can instead specify microsoft.graph=X in which case the namespace is
reduced to X. This results in more readable identifiers (Note: X can be empty)

-b --basename=classname    Name of class to use as base class.

By default, all classes descend from TODataObject. This can be changed.


-d --odata=version         OData version to use: v2 or v4.

This determines what document is being converted. Note that not yet all
possibilities of V2 are supported.

-e --extraunits=extraunits Comma-separated list of unit names to add.

List of unit names to add to the uses clause of the generated unit.

-h --help                  This message.

Display help.

-i --input=filename        Name of the file to use as input. Mandatory

The name of an CSDL or EDMX XML document.

-o --output=filename       Name of the file to use as output.
                           (default: input file with extension changed to .pas)

This is the output filename name. By default the input filename will be
used, with the extension changed to .pas

-p --prefix=fieldprefix    Text to use as field prefix (default: F)

All field names for properties start with F, followed by the property name. 
You can change this here.

-u --enumerations=mode     How to treat enumerations. Possible values: scoped, prefixtypename, plain

How to generate code for enumerations. 

-x --servicesuffix=string  When constructing type names, add this to schema name. Default is _

By default a type name is constructed by apending the schema name with _ and
then the type name. You can use this option to change the _ character.

-v --verbose               Output some diagnostic messages


Enjoy,

Michael.