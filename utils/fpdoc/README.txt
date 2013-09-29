For more informations, see readme.html in the 'doc' subdirectory. There you
can also find the default CSS for the HTML output.


This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


(the file COPYING contains the whole GNU General Public License)


Source file overview
--------------------

dglobals.pp
  * Global declarations
  * Catalogue management (internal link and description lookup tables)

dwriter.pp
  * Basic writer (output generator) class

dw_html.pp
  * HTML/XHTML output generator

dwlinear.pp
  * Abstract linear documentation generator.

dw_latex.pp
  * LaTeX output generator, based on linear documentation generator.

dw_txt.pp
  * Plain text output generator, based on linear documentation generator.

dw_xml.pp
  * 'XML struct' output generator


dw_man.pp
  * 'Unix man page' output generator

fpdoc.pp
  * Main program

fpdocstripper.lpr
fpdocstripper.lpi
  * Utility program that strips fpdoc xml files of all elements 
    that have no documentation in them. Useful before submitting
	a documentation patch as it keeps file sizes down and makes
	it clearer what exactly is documented.
  
makeskel.pp
  * Skeleton XML description file generator

dw_template.pp
  * template for implementing a new writer back-end.

dw_lintmpl.pp
  * template for implementing a new linear writer back-end.


Contributors
------------
Initial French output strings by Pierre Muller
Initial Dutch output strings by Marco van de Voort
fpdocstripper by Reinier OliSlagers
