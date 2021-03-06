function format2(valor){
	valor= ""+valor;
	while (valor.length< 2){
		valor= '0'+valor;
	}
	return valor;
}

function getXMLRequisicao(){
	
	//verificando campo localidade
	var localidade = $("#comboLocalidade").val();
	
	//verificando campo autoridade
	var autoridade = $("#comboAutoridade").val();
	
	//verificando campo tipoNorma
	var tipoNorma = $("#comboTipoNorma").val();
	
	//verificando data em comboEvento
	//monta o campo de data e hora no formato 2011-11-24t11.00
	var dh;
	if( $("#editEventoData").is(":visible") ) {
		dh = $("#editEventoData").datepicker("getDate");
	} else {
		dh = new Date();
	}
	var dataHora = dh.getFullYear()+"-"+
	   format2((dh.getMonth()+1))+"-"+
	   format2(dh.getDate())+"t"+
	   format2(dh.getHours())+"."+
	   format2(dh.getMinutes());

	//verificando campo numero
	var numero = 999;
	if( $("#editNumero").is(":visible") ) {
		numero = $("#editNumero").val();
	}

	//verificando campo ano
	var ano = dh.getFullYear();
	if( $("#editAno").is(":visible") ) {
		ano = $("#editAno").val();
	}
	
	//verificando comboEvento
	var evento = "leitura";
	if( $("#comboEvento").is(":visible") ) {
		evento = $("#comboEvento").val();
	}
	
	//monta o campo descritorEvento
	var descritorEvento = ano+";"+numero+"@data.evento;"+evento+";"+dataHora;
	
	var valor=
		'<Q1:ParserRequisicao xmlns:Q1="http://www.lexml.gov.br/parser-ws" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.lexml.gov.br/parser-ws ../../../../../lexml-parser-projeto-lei-ws-api/src/main/xsd/parser-ws.xsd ">'
		+'<Q1:metadado>'
			+'<Q1:localidade>'+localidade+'</Q1:localidade>'
			+'<Q1:autoridade>'+autoridade+'</Q1:autoridade>'
			+'<Q1:tipoNorma>'+tipoNorma+'</Q1:tipoNorma>'
			+'<Q1:descritorEvento>'+descritorEvento+'</Q1:descritorEvento>'
		+'</Q1:metadado>'
		+'<Q1:texto>'
			+'<Q1:tipoMime>text/rtf</Q1:tipoMime>'
			+'<Q1:TextoAnexo nomeCampo="fonte" />'
		+'</Q1:texto>'
		+'<Q1:saidas>'
		    + ($("#CHK_DOCUMENTO_ORIGINAL")[0].checked ?  '<Q1:tipoSaida tipo="DOCUMENTO_ORIGINAL" formato="EXTERNO"/>' : '')
            + ($("#CHK_PDF_DERIVADO")[0].checked ?  '<Q1:tipoSaida tipo="PDF_DERIVADO" formato="EXTERNO"/>' : '')
            + ($("#CHK_PDF_ORIGINAL")[0].checked ?  '<Q1:tipoSaida tipo="PDF_ORIGINAL" formato="EXTERNO"/>' : '')
            + ($("#CHK_XML_DERIVADO")[0].checked ?  '<Q1:tipoSaida tipo="XML_DERIVADO" formato="EXTERNO"/>' : '')
            + ($("#CHK_ZIP_DERIVADO")[0].checked ?  '<Q1:tipoSaida tipo="ZIP_DERIVADO" formato="EXTERNO"/>' : '')
            + ($("#CHK_DOCX_DERIVADO")[0].checked ?  '<Q1:tipoSaida tipo="DOCX_DERIVADO" formato="EXTERNO"/>' : '')
            + ($("#CHK_XML_REMISSOES")[0].checked ?  '<Q1:tipoSaida tipo="XML_REMISSOES" formato="EMBUTIDO"/>' : '')
            + ($("#CHK_EPUB_DERIVADO")[0].checked ?  '<Q1:tipoSaida tipo="EPUB_DERIVADO" formato="EXTERNO"/>' : '')
            + ($("#CHK_PDF_DIFF")[0].checked ?  '<Q1:tipoSaida tipo="PDF_DIFF" formato="EXTERNO"/>' : '')
            + ($("#CHK_XHTML_INTERMEDIARIO")[0].checked ?  '<Q1:tipoSaida tipo="XHTML_INTERMEDIARIO" formato="EXTERNO"/>' : '')
		+'</Q1:saidas>'
		+'</Q1:ParserRequisicao>';
	return valor;
}	

function msgErro(mensagem, objectFocus){
	$("#dialogErroMensagem").text(mensagem);
	$("#dialogErro").dialog("open");
	
	$(objectFocus).focus();
}

function atualizarProgressBar(posicao){
	$("#progressbar").progressbar({
		value: posicao
	});
}

var processamentoCancelado = false;

//fechando a janela aguarde
function cancelarProcessamento(){
	$("#dialogMessageAguardando").dialog("close");
	processamentoCancelado = true;
}

var tempoEntreTentativas = 1000;

function numMaxTentativas() {
  var tempoTotal = $("#INP_TIMEOUT")[0].value;
  return (tempoTotal * 1000 / tempoEntreTentativas);
}

function buscarResultado(caminhoResultado, tentativa){
	
	$("#labelTentativa").text("Esta operação ainda poderá levar "+((numMaxTentativas()-tentativa)*tempoEntreTentativas)/1000+" segundos.");
	atualizarProgressBar(tentativa*100 / numMaxTentativas());
	
	//cancelado
	if (processamentoCancelado){
		processamentoCancelado = false;
		
		msgErro("Conversão do documento cancelada pelo usuário.");
		
		return;
	}
	
	//timeout
	if (tentativa>= numMaxTentativas()){
	    $("#dialogMessageAguardando").dialog("close");

		msgErro("Desculpe, não foi possível converter o documento ou obter retorno do processador de matérias.");
		
		return;
	}
	
	//tenta obter o resultado
	$.ajax({
		  url: caminhoResultado+"/resultado.xml",
	
		  success: function(data, textStatus, jqXHR){
		    if(jqXHR.status == 200) {
			  //finaliza o processamento
          	  $("#dialogMessageAguardando").dialog("close");

			  //processa o resultado
			  $("#divResultado").transform({
				  	xml: caminhoResultado+"/resultado.xml", 
				  	xsl: "../resultado2xhtml.xsl",
				  	error: function(){
				  		msgErro("Desculpe, houve um erro ao exibir o resultado do processador de matérias.");
				  	},
				  	success: function(){
				  		 $("#divResultado").fadeIn("slow");
						  function fixHttps(txt) {
						    if (document.location.protocol == "https:") {
      							return txt.replace(new RegExp('http://','g'),'https://')
  					  	    } else {
    							return txt
  						    }
						  }
						  var els = $(".substituteText");						  
						  els.each(function (idx,el) {
						     var fixedContent = fixHttps(el.textContent);
			                             el.innerHTML = fixedContent;
			                          });						  
				  	},
				  	});
            } else {
			  setTimeout(function() { buscarResultado(caminhoResultado, tentativa+1) }, tempoEntreTentativas);
            }
		  },
		  
		  error: function(){
			  setTimeout(function() { buscarResultado(caminhoResultado, tentativa+1) }, tempoEntreTentativas);
		  }
	});		
}

function iniciarObterResultadoParser(caminhoResultado){
	
	if (!caminhoResultado.length){
		msgErro("Desculpe, não foi possível obter retorno do processador de matérias.")
		return false;
	}
	
	//abrindo a janela aguarde
	$("#dialogMessageAguardando").dialog("open");
	
	buscarResultado(caminhoResultado, 1);

	return true;
}

$(document).ready(function() {
	
	//diálogo de aguardar
	$("#dialogMessageAguardando").dialog({
		modal: true,
		autoOpen: false,
		width: 400,
		buttons: {
			Cancelar: function(){
				cancelarProcessamento();
			}
		}
	});
	
	//diálogo erro
	$("#dialogErro").dialog({
		modal: true,
		autoOpen: false,
		buttons: {
			OK: function(){
				$(this).dialog("close");
			}
		}
	});
		
	//formulário de submissão
    $('#formSubmeter').ajaxForm({
    		dataType: 'xml',
    		iframe: true,
    
    		beforeSubmit: function(arr, $form, options) {
    			
    			//obtendo o XML com a requisição
    			xmlRequisicao= getXMLRequisicao();
    			
    			$("#inputXMLRequisicao").val(xmlRequisicao);
    		},
    		
    		success: function(responseXML, statusText) { 
    			 // 'responseXML' is the XML document returned by the server; we use 
    		    // jQuery to extract the content of the message node from the XML doc
    			
    			// alert(xmlToString(responseXML));

    			var message = $(responseXML).find('Location').text();
                if (document.location.protocol === "https:") {
                    message = message.replace(new RegExp('http://','g'),'https://')
                }

    		    iniciarObterResultadoParser(message);
    		}
    }); 
}); 

// TESTE
function xmlToString(xmlData) { 

    var xmlString;
    //IE
    if (window.ActiveXObject){
        xmlString = xmlData.xml;
    }
    // code for Mozilla, Firefox, Opera, etc.
    else{
        xmlString = (new XMLSerializer()).serializeToString(xmlData);
    }
    return xmlString;
}   
