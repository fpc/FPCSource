<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- Change the encoding here if you need it, i.e. UTF-8 -->
  <xsl:output method="html" encoding="UTF-8" indent="yes"
    doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN"
  />

  <!-- ************************************ Parameters ************************************ -->
  <!-- expand-treeview, boolean - true if you want to expand the tree-view at the first print -->
  <xsl:param name="param-expand-treeview" select="'false'"/>
  <!-- preferred browser Netscape/Mozilla or Internet Explorer. Thanks to Bill, 90% of the sheeps use IE, but I don't so NS is the default here -->
  <xsl:param name="param-is-netscape" select="'true'"/>
  <!-- horizontal distance in pixels between a folder and its leaves -->
  <xsl:param name="param-shift-width" select="15"/>
  <!-- image source directory-->
  <xsl:param name="param-img-directory" select="'images/'"/>
  <!-- scripts and stylesheet source directory-->
  <xsl:param name="param-scripts-directory" select="'scripts/'"/>
  <!-- expand-errors-and-failures, boolean - true if you want to expand the testcase nodes (errors & failures) at the first print -->
  <xsl:param name="param-expand-errors-and-failures" select="'false'"/>
  <!-- if expand-treeview is false we can auto expand first few nodes. if 0 then expand until error or failure testcases. -->
  <xsl:param name="param-expand-depth" select="3"/>

  <!-- ************************************ Variables ************************************ -->
  <xsl:variable name="var-simple-quote">'</xsl:variable>
  <xsl:variable name="var-slash-quote">\'</xsl:variable>


<xsl:template match="/">
  <html>
  <head>
    <title>FPCUnit Results</title>
      <link href="{$param-scripts-directory}fpcunit.css" rel="stylesheet" type="text/css"/>
      <script src="{$param-scripts-directory}treeview.js" language="javascript" type="text/javascript"/>
  </head>
  <body>

  <a name="Summary"/>
  <h2>FPCUnit Results</h2>
  <xsl:apply-templates/>

  
  <address>
    <a href="http://opensoft.homeip.net">FPCUnit Report</a> 0.4.0 [beta3] &#169; 2006-2008 by 
    <a href="mailto:graemeg@gmail.com?subject=Comments about FPCUnit Report">Graeme Geldenhuys</a>.<br/>
    Licensed under the <a href="http://www.gnu.org/copyleft/gpl.html">GNU General Public License</a>.<br/>
  </address>

  <p align="right">
    <a href="http://validator.w3.org/check?uri=referer"><img
        src="http://www.w3.org/Icons/valid-html40" border="0"
        alt="Valid HTML 4.0 Transitional" height="31" width="88"/></a>
  </p>
</body>
</html>
</xsl:template>


<xsl:template name="summary" match="TestResults">
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
    <tr class="success">
      <!-- Set class attribute based on test results -->
      <xsl:if test="$failureCount &gt; 0">
        <xsl:attribute name="class">failure</xsl:attribute>
      </xsl:if>
      <xsl:if test="$errorCount &gt; 0">
        <xsl:attribute name="class">error</xsl:attribute>
      </xsl:if>

      <td>Summary</td>
      <td><xsl:value-of select="$runCount"/></td>
      <td><xsl:value-of select="$failureCount"/></td>
      <td><xsl:value-of select="$errorCount"/></td>
      <td><xsl:value-of select="$elapsedTime"/></td>
      <td><xsl:value-of select="$dateRan"/></td>
    </tr>
  </table>

  <p>Note: <i>Failures</i> are anticipated and checked for with assertions. <i>Errors</i> are 
unexpected results.</p>
  <hr/>

  <xsl:call-template name="test_listing"></xsl:call-template>

</xsl:template>


<!--
**********************************************************************
  Represents the test suites as a treeview
**********************************************************************  
-->
  <xsl:template name="test_listing">
    <div id="testlisting">
      <a name="Test_Listing"/>
      <h3>Test Listing</h3>
      <!-- Treeview Start -->
      <table border="0" cellspacing="0" cellpadding="0">
        <tr>
          <td>
            <!-- Apply the template TestSuite starting with a depth in the tree of 1-->
            <xsl:call-template name="test_suite">
              <xsl:with-param name="depth" select="1"/>
            </xsl:call-template>
          </td>
        </tr>
      </table>
      <!-- Treeview End -->
    </div>  <!-- testlisting -->
  </xsl:template>


<!--
**********************************************************************
  Creates a image and text representing a test suite.
**********************************************************************  
-->
  <xsl:template name="test_suite" match="TestSuite">
    <xsl:param name="depth"/>
      <table border="0" cellspacing="0" cellpadding="0">
        <tr>
          <!-- If first level of depth, do not shift of $param-shift-width-->
          <xsl:if test="$depth>1">
            <!-- highlight the test results -->
            <xsl:choose>
              <xsl:when test="@NumberOfErrors &gt; 0">
                <td width="{$param-shift-width}" class="error_">&#160;</td>
              </xsl:when>
              <xsl:when test="@NumberOfFailures &gt; 0">
                <td width="{$param-shift-width}" class="failure_">&#160;</td>
              </xsl:when>
              <xsl:otherwise>
                <td width="{$param-shift-width}">&#160;</td>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:if>
          <td>
            <a class="folder">
              <xsl:attribute name="onclick">toggle(this)</xsl:attribute>
              <!-- If the treeview is unfold, the image minus (-) is displayed-->
              <xsl:if test="@expanded">
                <xsl:if test="@expanded='true'">
                  <img src="{$param-img-directory}minus.gif" alt=""/>
                </xsl:if>
                <xsl:if test="@expanded='false'">
                  <img src="{$param-img-directory}plus.gif" alt=""/>
                </xsl:if>
              </xsl:if>

              <xsl:if test="not(@expanded)">
                <xsl:if test="$param-expand-treeview = 'true'">
                  <img src="{$param-img-directory}minus.gif" alt=""/>
                </xsl:if>
                <xsl:if test="$param-expand-treeview = 'false' or not(@expanded)">
                  <img src="{$param-img-directory}plus.gif" alt=""/>
                </xsl:if>
              </xsl:if>
              <img src="{$param-img-directory}testsuite.gif" alt="">
                <!-- if the attribut Name is present-->
                <xsl:if test="@Name">
                  <!-- if Netscape / Mozilla -->
                  <xsl:if test="$param-is-netscape='true'">
                    <xsl:attribute name="title"><xsl:value-of select="@Name"/></xsl:attribute>
                  </xsl:if>
                  <!-- if Internet Explorer -->
                  <xsl:if test="$param-is-netscape='false'">
                    <xsl:attribute name="alt"><xsl:value-of select="@Name"/></xsl:attribute>
                  </xsl:if>
                </xsl:if>
              </img>
              <!-- Tree node text can be decorated based on the test results -->
              <xsl:choose>
                <xsl:when test="@NumberOfErrors &gt; 0">
                  <span class="node_error"><xsl:value-of select="@Name"/></span>
                </xsl:when>
                <xsl:when test="@NumberOfFailures &gt; 0">
                  <span class="node_failure"><xsl:value-of select="@Name"/></span>
                </xsl:when>
                <xsl:otherwise>
                  <span class="node_success"><xsl:value-of select="@Name"/></span>
                </xsl:otherwise>
              </xsl:choose>
            </a>

            <!-- Shall we expand all the leaves of the treeview? No by default -->
            <div>
              <xsl:if test="@expanded">
                <xsl:if test="@expanded='true'">
                  <xsl:attribute name="style">display:block;</xsl:attribute>
                </xsl:if>
                <!-- plus (+) otherwise-->
                <xsl:if test="@expanded='false'">
                  <xsl:attribute name="style">display:none;</xsl:attribute>
                </xsl:if>
              </xsl:if>

              <xsl:if test="not(@expanded)">
                <xsl:if test="$param-expand-treeview = 'true'">
                  <xsl:attribute name="style">display:block;</xsl:attribute>
                </xsl:if>
                <xsl:if test="$param-expand-treeview = 'false'">
                  <xsl:attribute name="style">display:none;</xsl:attribute>
                </xsl:if>
                <!-- Auto expand any nodes containing failures or errors -->  
                <xsl:if test="($param-expand-treeview = 'false') and ($param-expand-depth = 0)">
                  <xsl:if test="@NumberOfErrors &gt; 0">
                    <xsl:attribute name="style">display:block;</xsl:attribute>
                  </xsl:if>
                  <xsl:if test="@NumberOfFailures &gt; 0">
                    <xsl:attribute name="style">display:block;</xsl:attribute>
                  </xsl:if>
                </xsl:if>
                <!-- Auto expand the first few nodes as defined by param-expand-depth -->  
                <xsl:if test="($param-expand-treeview = 'false') and ($param-expand-depth &gt; 0)">
                  <xsl:if test="($depth &lt; $param-expand-depth)">
                    <xsl:attribute name="style">display:block;</xsl:attribute>
                  </xsl:if>
                </xsl:if>
              </xsl:if>
              <!-- Thanks to the magic of recursive calls, all the descendants of 
                   the current folder are going to be built -->
              <xsl:apply-templates name="test_suite">
                <xsl:with-param name="depth" select="$depth+1"/>
              </xsl:apply-templates>
              <!-- print all the leaves of this folder-->
              <xsl:apply-templates select="/Test"></xsl:apply-templates>
            </div>
          </td>
        </tr>
      </table>
  </xsl:template>


<!--
**********************************************************************
  Represents the actual test.
**********************************************************************  
-->
  <xsl:template match="Test">
    <table border="0" cellspacing="1" cellpadding="0">
      <tr>
        <!-- highlight the test result -->
        <xsl:choose>
          <xsl:when test="@Result = 'Error'">
            <td width="{$param-shift-width}" class="error_">&#160;</td>
          </xsl:when>
          <xsl:when test="@Result = 'Failed'">
            <td width="{$param-shift-width}" class="failure_">&#160;</td>
          </xsl:when>
          <xsl:otherwise>
            <td width="{$param-shift-width}">&#160;</td>
          </xsl:otherwise>
        </xsl:choose>
        <td class="success">
          <!-- Set class attribute based on test result -->
          <xsl:if test="@Result = 'Error'">
            <xsl:attribute name="class">error</xsl:attribute>
          </xsl:if>
          <xsl:if test="@Result = 'Failed'">
            <xsl:attribute name="class">failure</xsl:attribute>
          </xsl:if>
          <a class="leaf">
            <xsl:attribute name="onclick">toggle(this)</xsl:attribute>
            <!-- if it is the last leaf, print a different image for the link to the folder-->
            <xsl:choose>
              <xsl:when test="($param-expand-errors-and-failures='false') and ((@Result = 'Error') or (@Result = 'Failed'))">
                <img src="{$param-img-directory}plus.gif" alt=""/>
              </xsl:when>
              <xsl:when test="($param-expand-errors-and-failures='true') and ((@Result = 'Error') or (@Result = 'Failed'))">
                <img src="{$param-img-directory}minus.gif" alt=""/>
              </xsl:when>
              <xsl:when test="position()=last()">
                <img src="{$param-img-directory}lastlink.gif" alt=""/>
              </xsl:when>
              <xsl:otherwise>
                <img src="{$param-img-directory}link.gif" alt=""/>
              </xsl:otherwise>
            </xsl:choose>

            <img src="{$param-img-directory}testcase.gif" alt="">
              <!-- if the attribut alt is present-->
              <xsl:if test="@ElapsedTime">
                <!-- if Netscape / Mozilla -->
                <xsl:if test="$param-is-netscape='true'">
                  <xsl:attribute name="title"><xsl:value-of select="@ElapsedTime"/></xsl:attribute>
                </xsl:if>
                <!-- if Internet Explorer -->
                <xsl:if test="$param-is-netscape='false'">
                  <xsl:attribute name="alt"><xsl:value-of select="@ElapsedTime"/></xsl:attribute>
                </xsl:if>
              </xsl:if>
            </img>

            <!-- Text background can be highlighted based on the test results -->
<!--            <xsl:choose>
              <xsl:when test="@Result = 'Error'">
                <span class="node_error"><xsl:value-of select="@Name"/></span>
              </xsl:when>
              <xsl:when test="@Result = 'Failed'">
                <span class="node_failure"><xsl:value-of select="@Name"/></span>
              </xsl:when>
              <xsl:otherwise>
                <span class="node_success"><xsl:value-of select="@Name"/></span>
              </xsl:otherwise>
            </xsl:choose>
-->
            <span class="node_success"><xsl:value-of select="@Name"/></span>
          </a>
          <!-- Show test result if they exist -->
          <xsl:choose>
            <xsl:when test="@Result = 'Error'">
              <xsl:call-template name="error_results"></xsl:call-template>
            </xsl:when>
            <xsl:when test="@Result = 'Failed'">
              <xsl:call-template name="failed_results"></xsl:call-template>
            </xsl:when>
          </xsl:choose>

        </td>
        <td width="150" align="right" class="success">
          <!-- Set class attribute based on test result -->
          <xsl:if test="@Result = 'Error'">
            <xsl:attribute name="class">error</xsl:attribute>
          </xsl:if>
          <xsl:if test="@Result = 'Failed'">
            <xsl:attribute name="class">failure</xsl:attribute>
          </xsl:if>

          <xsl:value-of select="@ElapsedTime"/>
        </td>
      </tr>
    </table>
  </xsl:template>


  <xsl:template name="error_results">
    <div style="display:none">
      <xsl:if test="$param-expand-errors-and-failures='true'">
        <xsl:attribute name="style">display:block;</xsl:attribute>
      </xsl:if>

      <table border="0">
        <!--  Error Table Body  -->
        <TR>
          <td width="{$param-shift-width}">&#160;</td>
          <TD width="200" valign="top" class="title">Message:</TD>
          <TD valign="top" class="resultmessage"><xsl:value-of select="Message"/></TD>
        </TR>
        <TR>
          <td width="{$param-shift-width}">&#160;</td>
          <TD valign="top" class="title">Exception Class:</TD>
          <TD valign="top" class="resultmessage"><xsl:value-of select="ExceptionClass"/></TD>
        </TR>
        <TR>
          <td width="{$param-shift-width}">&#160;</td>
          <TD valign="top" class="title">Exception Message:</TD>
          <TD valign="top" class="resultmessage"><xsl:value-of select="ExceptionMessage"/></TD>
        </TR>
        <TR>
          <td width="{$param-shift-width}">&#160;</td>
          <TD valign="top" class="title">UnitName:</TD>
          <TD valign="top" class="resultmessage"><xsl:value-of select="SourceUnitName"/></TD>
        </TR>
        <TR>
          <td width="{$param-shift-width}">&#160;</td>
          <TD valign="top" class="title">LineNumber:</TD>
          <TD valign="top" class="resultmessage"><xsl:value-of select="LineNumber"/></TD>
        </TR>
        <TR>
          <td width="{$param-shift-width}">&#160;</td>
          <TD valign="top" class="title">Method Name:</TD>
          <TD valign="top" class="resultmessage"><xsl:value-of select="FailedMethodName"/></TD>
        </TR>
      </table>
    </div>
  </xsl:template>


  <xsl:template name="failed_results">
    <div style="display:none">
      <xsl:if test="$param-expand-errors-and-failures='true'">
        <xsl:attribute name="style">display:block;</xsl:attribute>
      </xsl:if>

      <table border="0">
        <!--  Error Table Body  -->
        <tr>
          <td width="{$param-shift-width}">&#160;</td>
          <td width="200" valign="top" class="title">Message:</td>
          <td valign="top" class="resultmessage"><xsl:value-of select="Message"/></td>
        </tr>
        <tr>
          <td width="{$param-shift-width}">&#160;</td>
          <td valign="top" class="title">Exception Class:</td>
          <td valign="top" class="resultmessage"><xsl:value-of select="ExceptionClass"/></td>
        </tr>
        <tr>
          <td width="{$param-shift-width}">&#160;</td>
          <td valign="top" class="title">Exception Message:</td>
          <td valign="top" class="resultmessage"><xsl:value-of select="ExceptionMessage"/></td>
        </tr>
      </table>
    </div>
  </xsl:template>


  <xsl:template name="replace-string">
    <xsl:param name="text"/>
    <xsl:param name="from"/>
    <xsl:param name="to"/>
    <xsl:choose>
      <xsl:when test="contains($text, $from)">
        <xsl:variable name="before" select="substring-before($text, $from)"/>
        <xsl:variable name="after" select="substring-after($text, $from)"/>
        <xsl:variable name="prefix" select="concat($before, $to)"/>
        <xsl:value-of select="$before"/>
        <xsl:value-of select="$to"/>
        <xsl:call-template name="replace-string">
          <xsl:with-param name="text" select="$after"/>
          <xsl:with-param name="from" select="$from"/>
          <xsl:with-param name="to" select="$to"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
