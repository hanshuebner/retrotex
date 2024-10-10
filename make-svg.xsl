<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="3.0"
                xmlns="http://www.w3.org/2000/svg"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:local="local:"
                exclude-result-prefixes="xs xsl local">

  <xsl:output method="xml"
              indent="yes"/>

  <xsl:accumulator name="x-position" as="xs:double" initial-value="0">
    <xsl:accumulator-rule match="key"
                          phase="end"
                          select="$value + (if (@width) then xs:double(@width) else 1) * 100" />
    <xsl:accumulator-rule match="space" 
                          phase="end"
                          select="$value + xs:double(@width) * 100" />
    <xsl:accumulator-rule match="row" select="0"/>
  </xsl:accumulator>

  <xsl:accumulator name="y-position" as="xs:double" initial-value="0">
    <xsl:accumulator-rule match="row"
                          phase="end"
                          select="$value + 100"/>
  </xsl:accumulator>

  <xsl:accumulator name="key-number" as="xs:integer" initial-value="0">
    <xsl:accumulator-rule match="key"
                          phase="end"
                          select="$value + 1"/>
  </xsl:accumulator>

  <xsl:mode use-accumulators="x-position y-position key-number"
            on-no-match="shallow-skip"/>

  <xsl:function name="local:logo">
    <xsl:param name="name"/>
    <xsl:if test="starts-with($name, 'tv')">
      <path class="logo" stroke-width="2" d="M28,40 A60,60 0 0,1 72,40 A70,70 0 0,1 72,67 A60,60 0 0,1 28,67 A70,70 0 0,1 28,40"/>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="$name = 'tv-star'">
        <text x="50" y="92" font-size="70">*</text>
      </xsl:when>
      <xsl:when test="$name = 'tv-hash'">
        <text x="50" y="63" font-size="30">#</text>
      </xsl:when>
      <xsl:when test="$name = 'tv-show-attributes'">
        <path class="logo-black" d="M28,67 A70,70 0 0,1 28,40 A60,60 0 0,1 72,40 Z"/>
      </xsl:when>
      <xsl:when test="$name = 'tv-go-online'">
        <path class="logo-black-lines" d="M40,44 L70,44 M40,50 L60,50 M40,56 L50,56 M40,62 L70,62"/>
        <path class="logo-black-phone" transform="scale(0.70) translate(-130,-43)" d="m 173.9052,104.20538 -1.78908,6.85456 -0.43268,7.61816 0.78687,8.14524 2.26274,7.33476 3.78108,2.41991 4.22141,0.30306 -0.82784,-10.6196 h -3.86327 l -1.71931,-7.47705 0.61551,-7.49931 3.86327,-1.08919 2.27203,-7.42933 -4.20366,-2.10108 z"/>
      </xsl:when>
      <xsl:when test="$name = 'tv-go-offline'">
        <path class="logo-black-lines" d="M40,44 L70,44 M40,50 L60,50 M40,56 L50,56 M40,62 L70,62"/>
        <path class="logo-black-phone" transform="scale(0.70) translate(-130,-43)" d="m 173.9052,104.20538 -1.78908,6.85456 -0.43268,7.61816 0.78687,8.14524 2.26274,7.33476 3.78108,2.41991 4.22141,0.30306 -0.82784,-10.6196 h -3.86327 l -1.71931,-7.47705 0.61551,-7.49931 3.86327,-1.08919 2.27203,-7.42933 -4.20366,-2.10108 z"/>
        <path class="logo-black-lines" d="M35,75 L65,30"/>
      </xsl:when>
      <xsl:when test="$name = 'tv-sys-cursor'">
        <path class="logo-sys-cursor-lines" d="M30,44 L60,44 M30,50 L50,50 M30,56 L40,56 M30,62 L60,62 M63,60 A1,1 0 1,0 70,60 A1,1 0 1,0 63,60"/>
      </xsl:when>
    </xsl:choose>
  </xsl:function>

  <xsl:template match="/">
    <svg width="2400" height="700">
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
          .standard-key > * {
            fill: #c7c1c9;
          }
          .medium-key text {
            fill: #e4e3e6;
          }
          .medium-key * {
            fill: #a09a9a;
          }

          .black text, .red text, .green text, .yellow text, .blue text, .magenta text, .cyan text  {
            fill: white;
          }
          .black * {
            fill: black;
            stroke: white;
          }
          .red * { fill: #e00000; }
          .green * { fill: #00e000; }
          .yellow * { fill: #e0e000; }
          .blue * { fill: #0000e0; }
          .magenta * { fill: #e000e0; }
          .cyan * { fill: #00e0e0; }
          .white * { fill: white; }

          .mid-grey text, .red-key text, .f-key text, .sys-key text {
            fill: #e4e3e6;
            stroke: #e4e3e6;
          }
          .mid-grey * {
            fill: #a09a9a;
          }
          .f-key * {
            fill: #785f5f;
          }
          .sys-key * {
            fill: #473b43;
          }
          .sys-key .logo {
            stroke: #e4e3e6;
          }
          path.logo-sys-cursor-lines {
            stroke: #e4e3e6;
            fill: #e4e3e6;
            stroke-width: 3;
          }
          .standard-key path.logo-black {
            fill: black;
          }
          .standard-key path.logo-black-lines {
            fill: black;
            stroke-width: 3;
          }
          .standard-key path.logo-black-phone {
            fill: black;
            stroke-width: 0;
          }
          .red-key * {
            fill: #c32920;
          }
        </style>
      </defs>
      <xsl:apply-templates/>
    </svg>
  </xsl:template>

  <xsl:template match="key[@type='page-number']" priority="10">
    <g class="standard-key {@color}"
       id="key-{accumulator-before('key-number')}"
       transform="translate({accumulator-before('x-position')},{accumulator-before('y-position')})">
      <rect x="5" y="5" width="90" height="90" rx="10" ry="10"/>
      <circle cx="50" cy="50" r="35"/>
      <line x1="25" y1="25" x2="75" y2="25"/>
      <path d="M35,45 L50,30 70,50 50,75 35,60" class="thick-line"/>
      <text x="50" y="62" font-size="28"><xsl:value-of select="text()"/></text>
    </g>
  </xsl:template>

  <xsl:template match="key[@type='led']" priority="10">
    <g class="standard-key {@color}"
       id="key-{accumulator-before('key-number')}"
       transform="translate({accumulator-before('x-position')},{accumulator-before('y-position')})">
      <rect x="5" y="5" width="90" height="90" rx="10" ry="10"/>
      <circle cx="50" cy="50" r="35"/>
      <line x1="25" y1="25" x2="75" y2="25"/>
      <xsl:copy-of select="local:logo(@logo)"/>
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
       id="key-{accumulator-before('key-number')}"
       transform="translate({accumulator-before('x-position')},{accumulator-before('y-position')})">
      <rect x="5" y="5" width="{90 + $stretch}" height="90" rx="10" ry="10"/>
      <path d="M25,25 A35,35 0 0,0 50,80 L{50 + $stretch},80 A35,35 0 0,0 {75 + $stretch},25 Z"/>
      <xsl:variable name="label-center-offset" select="$stretch div 2"/>
      <xsl:choose>
        <xsl:when test="count(label) = 1">
          <text x="{50 + $label-center-offset}" y="65" font-size="40"><xsl:value-of select="label"/></text>
        </xsl:when>
        <xsl:when test="label">
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
       id="key-{accumulator-before('key-number')}"
       transform="translate({accumulator-before('x-position')},{accumulator-before('y-position')})">
      <rect x="5" y="5" width="90" height="90" rx="10" ry="10"/>
      <path d="M25,25 A35,35 0 1,0 75,25 Z"/>

      <xsl:copy-of select="local:logo(@logo)"/>
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
