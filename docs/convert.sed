s/</\&lt;/g
s/>/\&gt;/g
s/\\_/_/g
s/\\#/#/g
s/^%\(.*\)$/<!-- \1 -->/
s/\([^\\]\)%\(.*\)$/<!-- \2 -->/
/\\Declaration/,/\\Description/c\
<short></short>\
<descr>
s%\\subsection{\([^{]*\)}%<topic><short>\1</short>%g
s%\\section{\([^{]*\)}%<topic><short>\1</short>%g
s%\\chapter{\([^{]*\)}%<topic><short>\1</short>%g
s%\\begin{verbatim}%</p>\n<code>%g
s%\\end{verbatim}%</code>\n<p>%g
s%\\var{\([^{]*\)}%<var>\1</var>%g
s%\\em{\([^{]*\)}%<em>\1</em>%g
s%{\\em\([^{]*\)}%<em>\1</em>%g
s%\\file{\([^{]*\)}%<file>\1</file>%g
s%\\menu{\([^{]*\)}%<var>\1</var>%g
s%\\key{\([^{]*\)}%<var>\1</var>%g
s%\\see[pf]l{\([^{]*\)}{\([^{]*\)}%<link id="\2">\1</link>%g
s%\\seeconstl{\([^{]*\)}{\([^{]*\)}%<link id="\2">\1</link>%g
s%\\seetypel{\([^{]*\)}{\([^{]*\)}%<link id="\2">\1</link>%g
s%\\seevarl{\([^{]*\)}{\([^{]*\)}%<link id="\2">\1</link>%g
s%\\see[pf]{\([^{]*\)}%<link id="\1"/>%g
s%\\seevar{\([^{]*\)}%<link id="\1"/>%g
s%\\seetype{\([^{]*\)}%<link id="\1"/>%g
s%\\seeconst{\([^{]*\)}%<link id="\1"/>%g
s%\\seec{\([^{]*\)}%<link id="\1"/>%g
s%\\seet{\([^{]*\)}%<link id="\1"/>%g
s%\\sees{\([^{]*\)}%<link id="\1"/>%g
s%\\seeo{\([^{]*\)}%<link id="\1"/>%g
s%\\seefig{\([^{]*\)}%<link id="\1"/>%g
s%\\Errors%</descr>\n<errors>%g
s%\\SeeAlso%</errors>\n<seealso>%
s%\\end{functionl*}%</seealso>\n</element>%g
s%\\end{procedurel*}%</seealso>\n</element>%g
s%\\end{propertyl*}%</seealso>\n</element>%g
s%\\end{methodl*}%</seealso>\n</element>%g
s%\\end{typel*}%</seealso>\n</element>%g
s%\\end{constantl*}%</seealso>\n</element>%g
s%\\begin{functionl}{\([^{]*\)}{\([^{]*\)}%<element name="\1">%g
s%\\begin{function}{\([^{]*\)}%<element name="\1">%g
s%\\begin{typel}{\([^{]*\)}{\([^{]*\)}%<element name="\1">%g
s%\\begin{type}{\([^{]*\)}%<element name="\1">%g
s%\\begin{varl}{\([^{]*\)}{\([^{]*\)}%<element name="\1">%g
s%\\begin{var}{\([^{]*\)}%<element name="\1">%g
s%\\begin{constl}{\([^{]*\)}{\([^{]*\)}%<element name="\1">%g
s%\\begin{const}{\([^{]*\)}%<element name="\1">%g
s%\\begin{procedure}{\([^{]*\)}%<element name="\1">%g
s%\\begin{procedurel}{\([^{]*\)}{\([^{]*\)}%<element name="\1">%g
s%\\FPCexample{\([^{]*\)}%<example file="\1"/>%
s%\\begin{remark}%</p>\n<remark>%g
s%\\end{remark}%</remark>\n<p>%g
s%\\\\%<br/>%g
s%\\begin{funclist}%<table>\n<th><td>Name</td><td>Description</td></th>%g
s%\\end{funclist}%</table>%g
s%\\funcref{\([^{]*\)}{\([^{]*\)}%<tr><td><link id="\1"/></td><td>\2</td></tr>%g
s%\\funcrefl{\([^{]*\)}{\([^{]*\)}{\([^{]*\)}%<tr><td><link id="\2">\1</link></td><td>\3</td></tr>%g
s%\\procref{\([^{]*\)}{\([^{]*\)}%<tr><td><link id="\1"/></td><td>\2</td></tr>%g
s%\\procrefl{\([^{]*\)}{\([^{]*\)}{\([^{]*\)}%<tr><td><link id="\2">\1</link></td><td>\3</td></tr>%g
s%>,[[:space:]]*<link%>\n<link%g
/\\begin{description}/,/\\end{description}/{
s%\\begin{description}%<dl>%g
s%\\end{description}%</dd>\n</dl>\n<p>%g
s%\\item\[\(.*\)]%</dd>\n<dt>\1</dt>\n<dd>%g
}
/\\begin{itemize}/,/\\end{itemize}/{
s%\\begin{itemize}%</p>\n<ul>%g
s%\\end{itemize}%</li>\n</ul>\n<p>%g
s%\\item%</li>\n<li>%g
}
/\\begin{enumerate}/,/\\end{enumerate}/{
s%\\begin{enumerate}%</p><ol>%g
s%\\end{enumerate}%</li>\n</ol>\n<p>%g
s%\\item%</li>\n<li>%g
}
s%\\linux%linux%g
s%\\unix%unix%g
s%\\dos%dos%
s%\\msdos%ms-dos%g
s%\\ostwo%os/2%g
s%\\windows%Windows%g
s%\\windowsnt%Windows NT%g
s%\\macos%Mac OS%g
s%\\fpc%Free Pascal%g
s%\\gnu%gnu%g
s%\\atari%Atari%g
s%\\amiga%Amiga%g
s%\\solaris%Solaris%g
s%\\qnx%QNX Realtime platform%g
s%\\beos%BeOS%g
s%\\palmos%PalmOS%g
s%\\netbsd%NetBSD%g
s%\\openbsd%OpenBSD%g
s%\\win%Win32%g
s%\\freebsd%FreeBSD%g
s%\\tp%Turbo Pascal%g
s%\\delphi%Delphi%g
