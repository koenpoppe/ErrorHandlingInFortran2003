<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:exsl="http://exslt.org/common" xmlns:str="http://exslt.org/strings">
<xsl:output omit-xml-declaration="yes"  method="text"/>

	<!-- Just some nodes to use as counter -->
	<xsl:variable name="counter">
		<i>1</i>
		<i>2</i>
		<i>3</i>
		<i>4</i>
		<i>5</i>
		<i>6</i>
		<i>7</i>
		<i>8</i>
		<i>9</i>
		<i>10</i>
		<i>11</i>
		<i>12</i>
		<i>13</i>
		<i>14</i>
		<i>15</i>
		<i>16</i>
		<i>17</i>
		<i>18</i>
		<i>19</i>
		<i>20</i>
	</xsl:variable>

	<!-- The types that are known -->
	<xsl:variable name="types">
		<type>logical</type>
		<type>integer</type>
		<type>real</type>
		<type>double precision</type>
		<type>complex</type>
		<type>complex(kind=kind(1.0d0))</type>
	</xsl:variable>

	<!-- The ranks that must be considered (0=scalar,1=array,2=matrix,...) -->
	<xsl:variable name="ranks">
		<rank>0</rank>
		<rank>1</rank>
		<rank>2</rank>
		<rank>3</rank>
	</xsl:variable>

	<!-- Main: the version -->
	<xsl:template match="version">
		<!-- <xsl:param name="callback"/>
		<xsl:choose>
			<xsl:when test="string-length($callback) &gt; 0"> -->
				<xsl:call-template name="module-procedure"/>
				<!-- callback for <xsl:value-of select="$callback"/>
				<xsl:call-template name="{$callback}"/> -->
			<!-- </xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates select="../../allocate">
					<xsl:with-param name="version" select="."/>
				</xsl:apply-templates>
			</xsl:otherwise>
		</xsl:choose> -->
	</xsl:template>

	<xsl:template name="structure">
		<xsl:param name="indent">-</xsl:param><xsl:text>
</xsl:text><xsl:value-of select="$indent"/> <xsl:value-of select="name(.)"/>
		<xsl:for-each select="*">
			<xsl:call-template name="structure">
				<xsl:with-param name="indent">-<xsl:value-of select="$indent"/></xsl:with-param>
			</xsl:call-template>
		</xsl:for-each>
	</xsl:template>
	
	<!-- Placeholders for all types and all ranks -->
	<xsl:template match="alltypes_allranks">
		<xsl:param name="callback"/>
		<xsl:variable name="composed">
			<composed>
				<xsl:element name="{name(..)}">
					<xsl:for-each select="exsl:node-set($types)/type">
						<xsl:variable name="type" select="."/>
						<xsl:for-each select="exsl:node-set($ranks)/rank">
							<version>
								<xsl:attribute name="type"><xsl:value-of select="$type"/></xsl:attribute>
								<xsl:attribute name="rank"><xsl:value-of select="."/></xsl:attribute>
							</version>
						</xsl:for-each>
					</xsl:for-each>
				</xsl:element>
			</composed>
		</xsl:variable>
		<xsl:apply-templates select="exsl:node-set($composed)/composed/*/version">
			<xsl:with-param name="callback"><xsl:value-of select="$callback"/></xsl:with-param>
		</xsl:apply-templates>
	</xsl:template>
	<xsl:template match="alltypes_allhigherranks">
		<xsl:param name="callback"/>
		<xsl:variable name="composed">
			<composed>
				<xsl:element name="{name(..)}">
					<xsl:for-each select="exsl:node-set($types)/type">
						<xsl:variable name="type" select="."/>
						<xsl:for-each select="exsl:node-set($ranks)/rank[. &gt;= 1 ]">
							<version>
								<xsl:attribute name="type"><xsl:value-of select="$type"/></xsl:attribute>
								<xsl:attribute name="rank"><xsl:value-of select="."/></xsl:attribute>
							</version>
						</xsl:for-each>
					</xsl:for-each>
				</xsl:element>
			</composed>
		</xsl:variable>
		<xsl:apply-templates select="exsl:node-set($composed)/composed/*/version">
			<xsl:with-param name="callback"><xsl:value-of select="$callback"/></xsl:with-param>
		</xsl:apply-templates>
	</xsl:template>

	<!-- TEMPLATES -->
	
	<xsl:template name="rank-specification">
		<xsl:choose>
			<xsl:when test="@rank = 0">
				<!-- NOP -->
			</xsl:when>
			<xsl:otherwise>
				<xsl:variable name="rank" select="@rank"/>
				<xsl:text>, dimension(</xsl:text><xsl:for-each select="exsl:node-set($ranks)/rank[. &lt;= $rank]"><xsl:if test=". &gt; 1 ">,</xsl:if>:</xsl:for-each><xsl:text>)</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<xsl:template name="rank-dimension">
		<xsl:choose>
			<xsl:when test="@rank = 1">
				<!-- NOP -->
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>, dimension(</xsl:text><xsl:value-of select="@rank"/><xsl:text>)</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template name="module-procedure"><xsl:text>
        module procedure </xsl:text>	<xsl:call-template name="name-mangler"/>
	</xsl:template>

	<xsl:template name="name-mangler">
<xsl:value-of select="name(..)"/>_rank<xsl:value-of select="@rank"/><xsl:for-each select="str:tokenize(str:replace(@type,'complex(kind=kind(1.0d0))','double_complex'),' =().')">_<xsl:value-of select="."/></xsl:for-each>
	</xsl:template>

</xsl:stylesheet>