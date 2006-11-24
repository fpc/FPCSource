<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" encoding="UTF-8"/>

<xsl:template match="/">
  <html>
  <head>
    <title>fpcUnit Results</title>
  	  <style type="text/css" title="fpcUnit" media="screen">
		    @import "fpcunit.css";
	    </style>
    <script>
window.onload = function () {
	var x = document.getElementsByTagName('div');
	for (var i=0;i&lt;x.length;i++)
	{
		if (x[i].className == 'testsuitelabel')
			x[i].onclick = clickSuite;
		if (x[i].className == 'testsuiteshowall')
			x[i].onclick = openSuites;
		if (x[i].className == 'testsuitehideall')
			x[i].onclick = closeSuites;
	}

	closeSuites();
}

function closeSuites()
{
	var x = document.getElementsByTagName('div');
	for (var i=0;i&lt;x.length;i++)
	{
		if (x[i].className == 'testcontent')
			x[i].style.display = 'none';
	}
}

function openSuites()
{
	var x = document.getElementsByTagName('div');
	for (var i=0;i&lt;x.length;i++)
	{
		if (x[i].className == 'testcontent')
			x[i].style.display = 'block';
	}
}

function clickSuite(e)
{
	if (!e) var e = window.event;
	if (e.target) var tg = e.target;
	else if (e.srcElement) var tg = e.srcElement;
	while (tg.nodeName != 'DIV') // Safari GRRRRRRRRRR
		tg = tg.parentNode;
	var nextSib = tg.nextSibling;
	while (nextSib.nodeType != 1)
		nextSib = nextSib.nextSibling;
	var nextSibStatus = (nextSib.style.display == 'none') ? 'block' : 'none';
	nextSib.style.display = nextSibStatus;
}
    </script>
  </head>
  <body>

  <a name="Summary"/>
  <h2>fpcUnit Results</h2>
  <xsl:apply-templates/>

	<address>
		<a href="http://opensoft.homeip.net">fpcUnit Report</a> 0.3.1 © 2006 by 
		<a href="mailto:graemeg@gmail.com?subject=Comments about fpcUnit Report">Graeme Geldenhuys</a>.<br/>
		Licensed under the <a href="http://www.gnu.org/copyleft/gpl.html">GNU General Public License</a>.<br/>
	</address>
</body>
</html>
</xsl:template>


<xsl:template match="TestResults">
  <xsl:variable name="runCount" select="NumberOfRunTests"/>
  <xsl:variable name="failureCount" select="NumberOfFailures"/>
  <xsl:variable name="errorCount" select="NumberOfErrors"/>
  <xsl:variable name="elapsedTime" select="TotalElapsedTime"/>
  <xsl:variable name="dateRan" select="DateTimeRan"/>

  <h3>Summary</h3>
  <!--  Summary Table -->
	<table border="0" rules="none" width="100%">
		<tr align="left" class="title">
			<th width="45%" align="left">Name</th>
			<th width="7%" align="left">Tests</th>
			<th width="8%" align="left">Failures</th>
			<th width="8%" align="left">Errors</th>
			<th width="11%" align="left">Elapsed Time</th>
			<th width="14%" align="left">Run Date</th>
		</tr>
  <xsl:choose>
   <xsl:when test="$errorCount &gt; 0">
    <tr class="error">
      <td>Summary</td>
      <td><xsl:value-of select="$runCount"/></td>
      <td><xsl:value-of select="$failureCount"/></td>
      <td><xsl:value-of select="$errorCount"/></td>
      <td><xsl:value-of select="$elapsedTime"/></td>
      <td><xsl:value-of select="$dateRan"/></td>
    </tr>
   </xsl:when>
   <xsl:when test="$failureCount &gt; 0">
    <tr class="failure">
      <td>Summary</td>
      <td><xsl:value-of select="$runCount"/></td>
      <td><xsl:value-of select="$failureCount"/></td>
      <td><xsl:value-of select="$errorCount"/></td>
      <td><xsl:value-of select="$elapsedTime"/></td>
      <td><xsl:value-of select="$dateRan"/></td>
    </tr>
   </xsl:when>
   <xsl:otherwise>
    <tr class="success">
      <td>Summary</td>
      <td><xsl:value-of select="$runCount"/></td>
      <td><xsl:value-of select="$failureCount"/></td>
      <td><xsl:value-of select="$errorCount"/></td>
      <td><xsl:value-of select="$elapsedTime"/></td>
      <td><xsl:value-of select="$dateRan"/></td>
    </tr>
   </xsl:otherwise>
  </xsl:choose>
	</table>

  <p>Note: <i>Failures</i> are anticipated and checked for with assertions. <i>Errors</i> are 
unexpected results.</p>
	<hr/>

  <xsl:call-template name="test_listing"/>
  <xsl:call-template name="test_failures"/>
  <xsl:call-template name="test_errors"/>
</xsl:template>


<xsl:template name="test_listing">
  <div id="testlisting">
  <a name="Test_Listing"/>
	<h3>Test Listing</h3>
	<p>
		[<a href="#Summary">Summary</a>]
		[<a href="#Test_Listing">Test Listing</a>]
		[<a href="#Failures">Failures</a>]
		[<a href="#Errors">Errors</a>]
	</p>
  <!--  Test Listing Table -->
	<table border="0" rules="none" width="100%">
		<tr align="left" class="title">
			<td align="left">Name<br/><div class="testsuiteshowall">[show all]</div><div class="testsuitehideall">[hide all]</div></td>
			<td width="150" align="right">Elapsed Time<br/>(hh:mm:ss.zzz)</td>
		</tr>
  </table>
	<xsl:for-each select="TestListing/TestSuite">
    <div class="testsuitelabel"><xsl:value-of select="@Name"/></div>
    <div class="testcontent">
      <table border="0" cellspacing="1" width="100%">
        <xsl:for-each select="./Test">
        <tr class="success">
          <td><xsl:value-of select="@Name"/></td>
          <td width="150" align="right"><xsl:value-of select="ElapsedTime"/></td>
        </tr>
    	  </xsl:for-each>  <!-- Test -->
      </table>
    </div>
  </xsl:for-each> <!-- TestSuite -->

</div>  <!-- testlisting -->
</xsl:template>



<xsl:template name="test_failures">
  <div id="failures">
  <a name="Failures"/>
  <h3>Failures:</h3>
	<p>
		[<a href="#Summary">Summary</a>]
		[<a href="#Test_Listing">Test Listing</a>]
		[<a href="#Failures">Failures</a>]
		[<a href="#Errors">Errors</a>]
	</p>
<xsl:for-each select="ListOfFailures/Failure">
	<p class="backToTop">
		[<a href="#Failures">Back to top</a>]
	</p>
  <table>
    <!--  Error Table Body  -->
  <TR>
     <TD valign="top" class="title" width="300">Message:</TD>
     <TD valign="top" class="resultmessage"><xsl:value-of select="Message"/></TD>
  </TR>
  <TR>
    <TD valign="top" class="title">Exception Class:</TD>
    <TD valign="top" class="resultmessage"><xsl:value-of select="ExceptionClass"/></TD>  
  </TR>
  <TR>
     <TD valign="top" class="title">Exception Message:</TD>
     <TD valign="top" class="resultmessage"><xsl:value-of select="ExceptionMessage"/></TD>
  </TR>
  </table>
</xsl:for-each>
</div>  <!-- failures -->
</xsl:template>



<xsl:template name="test_errors">
<div id="errors">
  <a name="Errors"/>
  <h3>Errors</h3>
	<p>
		[<a href="#Summary">Summary</a>]
		[<a href="#Test_Listing">Test Listing</a>]
		[<a href="#Failures">Failures</a>]
		[<a href="#Errors">Errors</a>]
	</p>
<xsl:for-each select="ListOfErrors/Error">
	<p class="backToTop">
		[<a href="#Errors">Back to top</a>]
	</p>
<table>
  <!--  Error Table Body  -->
<TR>
   <TD valign="top" class="title" width="300">Message:</TD>
   <TD valign="top" class="resultmessage"><xsl:value-of select="Message"/></TD>
</TR>
<TR>
   <TD valign="top" class="title">Exception Class:</TD>
   <TD valign="top" class="resultmessage"><xsl:value-of select="ExceptionClass"/></TD>
</TR>
<TR>
   <TD valign="top" class="title">Exception Message:</TD>
   <TD valign="top" class="resultmessage"><xsl:value-of select="ExceptionMessage"/></TD>
</TR>
<TR>
   <TD valign="top" class="title">UnitName:</TD>
   <TD valign="top" class="resultmessage"><xsl:value-of select="SourceUnitName"/></TD>
</TR>
<TR>
   <TD valign="top" class="title">LineNumber:</TD>
   <TD valign="top" class="resultmessage"><xsl:value-of select="LineNumber"/></TD>
</TR>
<TR>
   <TD valign="top" class="title">Method Name:</TD>
   <TD valign="top" class="resultmessage"><xsl:value-of select="FailedMethodName"/></TD>
</TR>
</table>
</xsl:for-each>
</div>  <!-- errors -->
</xsl:template>



</xsl:stylesheet>
