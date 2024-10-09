<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="3.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:output method="xml"
              indent="yes"/>

  <xsl:accumulator name="x-position" as="xs:double" initial-value="0">
    <xsl:accumulator-rule match="key" 
                          select="$value + (if (@width) then xs:double(@width) else 1) * 100" />
    <xsl:accumulator-rule match="space" 
                          select="$value + xs:double(@width) * 100" />
    <xsl:accumulator-rule match="row" select="-100"/>
  </xsl:accumulator>

  <xsl:accumulator name="row-number" as="xs:double" initial-value="-100">
    <xsl:accumulator-rule match="row" select="$value + 100"/>
  </xsl:accumulator>

  <xsl:mode use-accumulators="x-position row-number"
            on-no-match="shallow-skip"/>

  <xsl:template match="/">
    <svg width="2400" height="700" xmlns="http://www.w3.org/2000/svg">
      <defs>
        <style type="text/css">
          /* Default styles for all elements */
          rect, path, circle, line {
            stroke: black;
          }
          text {
            fill: black;
            text-anchor: middle;
            font-family: D-DIN;
          }
          .thick-line {
            stroke-width: 2;
          }
          .standard-key text {
            fill: #242326;
          }
          .standard-key * {
            fill: #c7c1c9;
          }
          .medium-key text {
            fill: #e4e3e6;
          }
          .medium-key * {
            fill: #a09a9a;
          }
          .black text {
            fill: white;
          }
          .black circle, .black path, .black rect {
            fill: black;
            stroke: white;
          }
        </style>
      </defs>
      <xsl:apply-templates/>
    </svg>
  </xsl:template>

  <xsl:template match="key[@type='page-number']" priority="10">
    <g class="standard-key {@color}"
       transform="translate({accumulator-before('x-position')},{accumulator-before('row-number')})">
      <rect x="5" y="5" width="90" height="90" rx="10" ry="10"/>
      <circle cx="50" cy="50" r="35"/>
      <line x1="25" y1="25" x2="75" y2="25"/>
      <path d="M35,45 L50,30 70,50 50,75 35,60" class="thick-line"/>
      <text x="50" y="62" font-size="28"><xsl:value-of select="text()"/></text>
    </g>
  </xsl:template>

  <xsl:template match="key[@type='led']" priority="10">
    <g class="standard-key {@color}"
       transform="translate({accumulator-before('x-position')},{accumulator-before('row-number')})">
      <rect x="5" y="5" width="90" height="90" rx="10" ry="10"/>
      <circle cx="50" cy="50" r="35"/>
      <line x1="25" y1="25" x2="75" y2="25"/>
      <xsl:choose>
        <xsl:when test="label">
          <text x="50" y="55" font-size="18"><xsl:value-of select="label[1]"/></text>
          <text x="50" y="70" font-size="18"><xsl:value-of select="label[2]"/></text>
        </xsl:when>
        <xsl:otherwise>
          <text x="50" y="62" font-size="18"><xsl:value-of select="text()"/></text>
        </xsl:otherwise>
      </xsl:choose>
    </g>
  </xsl:template>

  <xsl:template match="key[@width]" priority="10">
    <xsl:variable name="stretch" select="(@width - 1) * 100"/>
    <g class="standard-key {@color}"
       transform="translate({accumulator-before('x-position')},{accumulator-before('row-number')})">
      <rect x="5" y="5" width="{90 + $stretch}" height="90" rx="10" ry="10"/>
      <path d="M25,25 A35,35 0 0,0 50,80 L{50 + $stretch},80 A35,35 0 0,0 {75 + $stretch},25 Z"/>
      <xsl:choose>
        <xsl:when test="label">
          <xsl:variable name="label-center-offset" select="$stretch div 2"/>
          <text x="{40 + $label-center-offset}" y="75" font-size="28"><xsl:value-of select="label[1]"/></text>
          <text x="{40 + $label-center-offset}" y="50" font-size="28"><xsl:value-of select="label[2]"/></text>
          <text x="{60 + $label-center-offset}" y="45" font-size="22"><xsl:value-of select="label[3]"/></text>
          <text x="{60 + $label-center-offset}" y="70" font-size="22"><xsl:value-of select="label[4]"/></text>
        </xsl:when>
        <xsl:otherwise>
          <text x="{@width * 50}" y="62" font-size="18"><xsl:value-of select="text()"/></text>
        </xsl:otherwise>
      </xsl:choose>
    </g>
  </xsl:template>

  <xsl:template match="key[not(@width) and not(@type)]" priority="1">
    <g class="standard-key {@color}"
       transform="translate({accumulator-before('x-position')},{accumulator-before('row-number')})">
      <rect x="5" y="5" width="90" height="90" rx="10" ry="10"/>
      <path d="M25,25 A35,35 0 1,0 75,25 Z"/>

      <xsl:choose>
        <xsl:when test="count(label) = 4">
          <text x="40" y="75" font-size="28"><xsl:value-of select="label[1]"/></text>
          <text x="40" y="50" font-size="28"><xsl:value-of select="label[2]"/></text>
          <text x="60" y="45" font-size="22"><xsl:value-of select="label[3]"/></text>
          <text x="60" y="70" font-size="22"><xsl:value-of select="label[4]"/></text>
        </xsl:when>
        <xsl:when test="count(label) = 3 and string-length(label[1]) = 1">
          <text x="40" y="65" font-size="38"><xsl:value-of select="label[1]"/></text>
          <text x="60" y="45" font-size="22"><xsl:value-of select="label[2]"/></text>
          <text x="60" y="70" font-size="22"><xsl:value-of select="label[3]"/></text>
        </xsl:when>
        <xsl:when test="count(label) = 3">
          <text x="50" y="45" font-size="18"><xsl:value-of select="label[1]"/></text>
          <text x="50" y="60" font-size="18"><xsl:value-of select="label[2]"/></text>
          <text x="50" y="75" font-size="18"><xsl:value-of select="label[3]"/></text>
        </xsl:when>
        <xsl:when test="count(label) = 2">
          <text x="50" y="55" font-size="18"><xsl:value-of select="label[1]"/></text>
          <text x="50" y="70" font-size="18"><xsl:value-of select="label[2]"/></text>
        </xsl:when>
        <xsl:when test="count(label) = 1">
          <text x="50" y="65" font-size="40"><xsl:value-of select="label"/></text>
        </xsl:when>
        <xsl:when test="count(label) = 0">
          <text x="50" y="62" font-size="18"><xsl:value-of select="text()"/></text>
        </xsl:when>
      </xsl:choose>
    </g>
  </xsl:template>

</xsl:stylesheet>
