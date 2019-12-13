<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="1.0" xmlns:dyn="http://exslt.org/dynamic">
  <xsl:output method="text"/>
  <xsl:param name="xpath"/>
  <xsl:param name="information" select='""'/>
  <xsl:template match="/">
    <xsl:if test="not(dyn:evaluate($xpath)='true')">
      <xsl:message terminate="yes">xpath expression failed.
<xsl:value-of select="$information" />
expression: <xsl:value-of select="$xpath" />
      </xsl:message>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
