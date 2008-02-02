#
proc foo {str1 str2 str3} {
    writeln $str1
    writeln $str2
    writeln $str3
    writeln [max $str1 $str3]
    return -1
}
