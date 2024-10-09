<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="3.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:output method="xml"
              indent="yes"/>

  <xsl:accumulator name="totalWidth" as="xs:double" initial-value="0">
    <xsl:accumulator-rule match="key" 
                          select="$value + (if (@width) then xs:double(@width) else 1)" />
    <xsl:accumulator-rule match="space" 
                          select="$value + xs:double(@width)" />
    <xsl:accumulator-rule match="row" select="0"/>
  </xsl:accumulator>

  <xsl:mode use-accumulators="totalWidth"
            on-no-match="shallow-skip"/>

  <xsl:template match="/">
    <result>
      <xsl:apply-templates/>
    </result>
  </xsl:template>

  <xsl:template match="row">
    <row>
      <xsl:apply-templates select="key" />
      <xsl:value-of select="accumulator-after('totalWidth')"/>
    </row>
  </xsl:template>

</xsl:stylesheet>
