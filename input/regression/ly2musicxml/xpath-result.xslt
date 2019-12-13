<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="1.0" xmlns:dyn="http://exslt.org/dynamic">
  <xsl:output method="text"/>
  <xsl:param name="xpath"/>
  <xsl:param name="information" select='""'/>
  <xsl:template match="/">
    <xsl:value-of select="(dyn:evaluate($xpath))" />
  </xsl:template>
</xsl:stylesheet>
