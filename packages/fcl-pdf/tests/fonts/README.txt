These sets of unit tests requires 5 font files of specific versions
each. Here is what the tests were designed against.

 Font File                  |  Size (bytes)   |  Version
----------------------------+-----------------+-----------------
DejaVuSans.ttf              |    622,280      |    2.30
FreeSans.ttf                |  1,563,256      |  412.2268
LiberationSans-Regular.ttf  |    350,200      |    2.00.1
LiberationSans-Italic.ttf   |    355,608      |    2.00.1
Ubuntu-R.ttf                |    353,824      |    0.80


Details of the above font files and download locations are as follows.


DejaVu Sans
===========
Official website:
    http://dejavu-fonts.org/wiki/Main_Page

Download URL:
    http://sourceforge.net/projects/dejavu/files/dejavu/2.30/dejavu-fonts-ttf-2.30.tar.bz2

Description:
    The DejaVu fonts are a font family based on the Vera Fonts. Its purpose is
    to provide a wider range of characters while maintaining the original look
    and feel through the process of collaborative development (see authors),
    under a Free license.


FreeSans
========
Official website:
    http://savannah.gnu.org/projects/freefont/

Download URL:
    http://ftp.gnu.org/gnu/freefont/freefont-ttf-20120503.zip

Description:
    We aim to provide a useful set of free outline (i.e. OpenType) fonts
    covering as much as possible of the Unicode character set. The set consists
    of three typefaces: one monospaced and two proportional (one with uniform
    and one with modulated stroke).

License:
    GNU General Public License v3 or later


Liberation
==========
Official website: 
    https://fedorahosted.org/liberation-fonts/

Download URL:
    https://fedorahosted.org/releases/l/i/liberation-fonts/liberation-fonts-ttf-2.00.1.tar.gz

Description:
    The Liberation(tm) Fonts is a font family which aims at metric compatibility
    with Arial, Times New Roman, and Courier New. It is sponsored by Red Hat.

License:
    * The Liberation(tm) version 2.00.0 onward are Licensed under the SIL Open 
      Font License, Version 1.1.
    * Older versions of the Liberation(tm) Fonts is released as open source under
      the GNU General Public License version 2 with exceptions. ​
      https://fedoraproject.org/wiki/Licensing/LiberationFontLicense 


Ubuntu
======
Official website:
    http://font.ubuntu.com/

Download URL:
    http://font.ubuntu.com/download/ubuntu-font-family-0.80.zip

Description:
    The Ubuntu typeface has been specially created to complement the Ubuntu
    tone of voice. It has a contemporary style and contains characteristics
    unique to the Ubuntu brand that convey a precise, reliable and free
    attitude.

License:
    Ubuntu Font Licence. This licence allows the licensed fonts to be used,
    studied, modified and redistributed freely.


TTF Dump output
===============
I used the Microsoft "ttfdump.exe" tool to generate the
file dump output for the Liberation Sans Regular font. I then used that to verify
the results of the TTF unit tests.

  http://www.microsoft.com/typography/tools/tools.aspx


