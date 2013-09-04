<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
	xmlns:lex="http://www.lexml.gov.br/1.0"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xhtml="http://www.w3.org/1999/xhtml"
	xmlns:p="http://www.lexml.gov.br/parser-ws"
	xmlns:tns="http://www.lexml.gov.br/schema/remissoes">

	<xsl:output method="xml" indent="yes" encoding="UTF-8"/>  
	
	<xsl:template match="/">
		
		<span class="tituloResultado">Resultado do processamento da conversão</span>
		
		<xsl:apply-templates />
		
	</xsl:template>

	<!-- Falhas -->
	<xsl:template match="p:ParserResultado/p:resultado/p:falhas">
		<xsl:if test="count(./p:falha) &gt; 0">	
		<xsl:element name="div">
			<xsl:attribute name="id">grupoFalhas</xsl:attribute>
			<xsl:attribute name="class">grupo</xsl:attribute>
			
			<xsl:element name="span"><xsl:attribute name="class">grupoTitulo</xsl:attribute>Falhas</xsl:element>
			
			<table>
			<thead>
				<tr>
					<th>Código</th>
					<th>Tipo da falha</th>
					<th>Descrição</th>
					<th>Posição</th>
				</tr>
			</thead>
			<tbody>
		
			<xsl:for-each select="p:falha">
				<tr>
					<td><xsl:value-of select="p:codigoTipoFalha" /></td>
					<td><xsl:value-of select="p:nomeTipoFalha" /></td>
					<xsl:choose>
						<xsl:when test="count(./p:mensagemUsuario) = 0">
							<td><xsl:value-of select="p:descricao" /></td>
							<td><xsl:value-of select="p:posicao" /></td>
						</xsl:when>
						<xsl:otherwise>
							<td colspan="2" class="substituteText">
								<xsl:value-of select="p:mensagemUsuario" disable-output-escaping="yes"/>
							</td>
						</xsl:otherwise>
					</xsl:choose>
				</tr>
			</xsl:for-each>
			
			</tbody>
			</table>
		</xsl:element>
		</xsl:if>
	</xsl:template>

	
	<!-- Características -->
	<xsl:template match="p:ParserResultado/p:resultado/p:caracteristicas">
	
		<xsl:element name="div">
			<xsl:attribute name="id">grupoCaracteristicas</xsl:attribute>
			<xsl:attribute name="class">grupo</xsl:attribute>
	
			<xsl:element name="span"><xsl:attribute name="class">grupoTitulo</xsl:attribute>Características</xsl:element>
		
			<table>
			<thead>
				<tr>
					<th>Descrição</th>
					<th>Presente</th>
					<th>Suportado pelo LexEdit</th>
				</tr>
			</thead>
			<tbody>
		
			<xsl:for-each select="p:caracteristica">
				<tr>
				
					<td><xsl:value-of select="p:descricao" /></td>
					<td>
						<xsl:choose>
							<xsl:when test="p:presente='true'">Sim</xsl:when>
							<xsl:otherwise>Não</xsl:otherwise>
						</xsl:choose>
					</td>
					<td>
						<xsl:choose>
							<xsl:when test="p:suportadoLexEdit='true'">Sim</xsl:when>
							<xsl:otherwise>Não</xsl:otherwise>
						</xsl:choose>
					</td>
				</tr>
			</xsl:for-each>
			
			</tbody>
			</table>
			
		</xsl:element>	
				
	</xsl:template>
	
	<!-- Saídas -->
	<xsl:template match="p:ParserResultado/p:resultado/p:saidas">
	
		<xsl:element name="div">
			<xsl:attribute name="id">grupoSaidas</xsl:attribute>
			<xsl:attribute name="class">grupo</xsl:attribute>
	
			<xsl:element name="span"><xsl:attribute name="class">grupoTitulo</xsl:attribute>Saídas</xsl:element>
		
			<xsl:for-each select="p:elementoSaida">
				<xsl:if test="@href">			
					<xsl:element name="a">
						<xsl:attribute name="href"><xsl:value-of select="@href" /></xsl:attribute>
						<xsl:attribute name="class">saidaLink</xsl:attribute>
						
						<xsl:element name="span">
							<xsl:attribute name="class">saidaImagem_<xsl:value-of select="substring(@tipoSaida, 1, 3)"></xsl:value-of></xsl:attribute>
						</xsl:element>
						
						<xsl:choose>
							<xsl:when test="@tipoSaida='DOCUMENTO_ORIGINAL'"><xsl:element name="span"><xsl:attribute name="class">saidaTextoLink</xsl:attribute>Documento original</xsl:element></xsl:when>
							<xsl:when test="@tipoSaida='RTF_DERIVADO'"><xsl:element name="span"><xsl:attribute name="class">saidaTextoLink</xsl:attribute>RTF derivado</xsl:element></xsl:when>
							<xsl:when test="@tipoSaida='PDF_ORIGINAL'"><xsl:element name="span"><xsl:attribute name="class">saidaTextoLink</xsl:attribute>PDF original</xsl:element></xsl:when>
							<xsl:when test="@tipoSaida='PDF_DERIVADO'"><xsl:element name="span"><xsl:attribute name="class">saidaTextoLink</xsl:attribute>PDF derivado</xsl:element></xsl:when>
							<xsl:when test="@tipoSaida='ZIP_DERIVADO'"><xsl:element name="span"><xsl:attribute name="class">saidaTextoLink</xsl:attribute>ZIP derivado</xsl:element></xsl:when>
							<xsl:when test="@tipoSaida='XML_DERIVADO'"><xsl:element name="span"><xsl:attribute name="class">saidaTextoLink</xsl:attribute>XML derivado</xsl:element></xsl:when>
							<xsl:when test="@tipoSaida='XML_REMISSOES'"><xsl:element name="span"><xsl:attribute name="class">saidaTextoLink</xsl:attribute>XML de remissões</xsl:element></xsl:when>
							<xsl:when test="@tipoSaida='EPUB_DERIVADO'"><xsl:element name="span"><xsl:attribute name="class">saidaTextoLink</xsl:attribute>EPUB derivado</xsl:element></xsl:when>
							<xsl:when test="@tipoSaida='PDF_DIFF'"><xsl:element name="span"><xsl:attribute name="class">saidaTextoLink</xsl:attribute>PDF diferenças</xsl:element></xsl:when>
						</xsl:choose>
					</xsl:element>
					<xsl:element name="br" />
				</xsl:if>	
			</xsl:for-each>
			
		</xsl:element>
		
		<xsl:call-template name="template-remissoes" />
				
	</xsl:template>	
	
	<xsl:template name="template-remissoes">

		<xsl:element name="div">
			<xsl:attribute name="id">grupoFalhas</xsl:attribute>
			<xsl:attribute name="class">grupo</xsl:attribute>
			<xsl:element name="span">
				<xsl:attribute name="class">grupoTitulo</xsl:attribute>
				Remissões
			</xsl:element>

			<xsl:for-each select="/p:ParserResultado/p:resultado/p:saidas/p:elementoSaida[@tipoSaida='XML_REMISSOES']/p:Remissoes/tns:documento">
				<xsl:element name="div">
					<xsl:attribute name="class">remissoesDocumento</xsl:attribute>
					
					<xsl:element name="a">
						<xsl:attribute name="href"><xsl:value-of select="@xlink:href" /></xsl:attribute>
						<xsl:value-of select="@tns:display" />
					</xsl:element>
					
					<xsl:for-each select="tns:fragmento">
					
						<xsl:element name="div">
							<xsl:attribute name="class">remissoesFragmento</xsl:attribute>
							
							<xsl:value-of select="@tns:display" /> (<xsl:value-of select="@tns:urn"></xsl:value-of>)
						</xsl:element>
					
					</xsl:for-each>
					
				</xsl:element>
			</xsl:for-each>
			
			
		</xsl:element>
	</xsl:template>
	
	
	<!-- Data/hora do processamento -->
	<xsl:template match="p:ParserResultado/p:resultado/p:dataHoraProcessamento">
	
		<!-- grupo remissões 
		<xsl:element name="div">
			<xsl:attribute name="id">divResultadoRemissoes</xsl:attribute>
			<xsl:attribute name="style">display: none;</xsl:attribute>
		</xsl:element>
		-->
		
		<!-- grupo data/hora do processamento -->
		<xsl:element name="div">
			<xsl:attribute name="id">grupoEncerramento</xsl:attribute>
			<xsl:attribute name="class">grupo</xsl:attribute>
			
			<xsl:element name="span">
				<xsl:attribute name="class">labelNome</xsl:attribute>
				Data/hora do processamento: 
			</xsl:element>
			<xsl:element name="span">
				<xsl:attribute name="class">labelValor</xsl:attribute>
				<xsl:value-of select="." />
			</xsl:element>
		</xsl:element>
			
	</xsl:template>
	
	
	<xsl:template match="node() | @*">
		<xsl:apply-templates />
	</xsl:template>

</xsl:stylesheet>
