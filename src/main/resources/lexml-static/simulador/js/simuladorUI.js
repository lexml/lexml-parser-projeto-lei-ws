/* Dados válidos para cada combo selecionado */

var autoridadeByLocalidade = {
		"br": 
			[
				{"value":"senado.federal", "display":"Senado Federal"},
				{"value":"camara.deputados", "display":"Câmara dos Deputados"},
				{"value":"casa.legislativa", "display":"Casa Legislativa"},
				{"value":"congresso.nacional", "display":"Congresso Nacional"},
				{"value":"federal", "display":"Presidência da República"}
			]
	};

var tipoNormaByAutoridade = {
		"senado.federal": 
			[
				{"value":"projeto.lei;pls", "display":"PLS - Projeto de Lei"},
				{"value":"projeto.lei.complementar;pls", "display":"PLS - Projeto de Lei Complementar"},
				{"value":"projeto.lei;plc", "display":"PLC - Projeto de Lei"},
				{"value":"projeto.lei.complementar;plc", "display":"PLC - Projeto de Lei Complementar"},
                {"value":"projeto.lei;pl", "display": "PL - Projeto de Lei"},
                {"value":"projeto.lei.complementar;plp", "display":"PLP - Projeto de Lei Complementar"},
				{"value":"projeto.resolucao;prs", "display":"PRS - Projeto de Resolução"},
				{"value":"proposta.emenda.constitucional;pec", "display":"PEC - Proposta de Emenda Constitucional"},
				{"value":"projeto.decreto.legislativo;pds", "display":"PDS - Projeto de Decreto Legislativo"},
                {"value":"projeto.decreto.legislativo;pdl", "display":"PDL - Projeto de Decreto Legislativo"},
                {"value":"resolucao", "display":"Resolução"}
			],
		"camara.deputados" :
			[
				{"value":"projeto.lei;pl", "display": "PL - Projeto de Lei"},
                {"value":"projeto.lei.complementar;plp", "display":"PLP - Projeto de Lei Complementar"},
                {"value":"projeto.decreto.legislativo;pdl", "display":"PDL - Projeto de Decreto Legislativo"},
				{"value":"proposta.emenda.constitucional;pec", "display":"PEC - Proposta de Emenda Constitucional"},
                {"value":"resolucao", "display":"Resolução"}
			],
		"casa.legislativa" :
			[
				{"value":"norma", "display":"Norma"},
			],
		"congresso.nacional": 
			[
                {"value" : "decreto.legislativo", "display":"Decreto Legislativo"},
			    {"value" : "medida.provisoria;mpv", "display": "MPV - Medida Provisória (Projeto)"},
                {"value" : "projeto.lei;pln", "display": "MPV - Medida Provisória (Projeto)"},
                {"value" : "resolucao", "display":"Resolução"}
			],
		"federal": 
			[
                {"value" : "decreto", "display": "Decreto"},
                {"value" : "decreto.legislativo", "display":"Decreto Legislativo"},
                {"value" : "decreto.lei", "display": "Decreto-Lei"},
                {"value" : "emenda.constitucional", "display": "Emenda Constitucional"},
                {"value" : "lei", "display": "Lei"},
                {"value" : "lei.complementar", "display": "Lei Complementar"},
                {"value" : "lei.delegada", "display": "Lei Delegada"},
			 	{"value" : "medida.provisoria", "display": "Medida Provisória (Norma)"}
			]
	};

function changeCombo(idFonte, idDestino, data){
	var combo = $(idDestino).empty();
	var valueSelected = data[$(idFonte).val()];
	
	jQuery.each(valueSelected, function(){
		combo.append("<option value=\""+this.value+"\">"+this.display+"</option>");
	});

}

function abrirFormGrupoEffect(obj){
	$(obj).toggle("blind", "slow");
}


/**
 * Trata parâmetros vindos da URL
 */
function tratarParametros(){
	var paramLocalidade = $.getQueryString('comboLocalidade');
	var paramAutoridade = $.getQueryString('comboAutoridade');
	var paramTipoNorma = $.getQueryString('comboTipoNorma');
	
	if (paramLocalidade!= null){
		$("#comboLocalidade").val(paramLocalidade);
	}
	
	if (paramAutoridade!= null){
		$("#comboAutoridade").val(paramAutoridade);
	}
	
	if (paramTipoNorma!= null){
		$("#comboTipoNorma").val(paramTipoNorma);
	}
}

//configurações de UI
$(document).ready(function() {
	
	//botões
	$("input:submit, input:button, input:file, button").button();
	
	//formatação dos botões para os outros controles
	$("input:text, select").addClass("ui-state-default");
	
	//campos de formato numérico
	$('.inputNumber').forceNumericOnly();

	//campos de formato data
	$('.inputDate').mask("99/99/9999");
	$('.inputDate').datepicker({
		altFormat: 'dd/mm/yy',
		dateFormat: 'dd/mm/yy',
		buttonText: 'Escolher',
		prevText: 'Anterior',
		nextText: 'Próximo',
		constrainInput: true,
		monthNames: ['Janeiro','Fevereiro','Março','Abril','Maio','Junho','Julho','Agosto','Setembro','Outubro','Novembro','Dezembero'],
		dayNamesMin: ['Dom', 'Seg', 'Ter', 'Qua', 'Qui', 'Sex', 'Sáb'],
		showButtonPanel: true,
		closeText: 'X',
		currentText: 'Hoje',
		defaultDate: null,
	}).datepicker("setDate", "+0");

	//campo ano
	$("#editAno").mask("9999");
	$("#editAno").val((new Date()).getFullYear());
	
	//oculta os grupos de formulário	
	$(".formGrupo").hide();	
	
	//anexão efeito de exibir
	$("#buttonFormGrupoInformatIdent").click(function (){
		abrirFormGrupoEffect("#formGrupoIdentificacao");
		return false;
	});
	
	$("#buttonFormGrupoVersao").click(function (){
		abrirFormGrupoEffect("#formGrupoVersao");
		return false;
	});
	

	$("#comboLocalidade").change(function(){
		changeCombo("#comboLocalidade", "#comboAutoridade", autoridadeByLocalidade);
		$("#comboTipoNorma").empty();
		$("#comboAutoridade").change();
	});
	
	$("#comboAutoridade").change(function(){
		changeCombo("#comboAutoridade", "#comboTipoNorma", tipoNormaByAutoridade)
	});
	
	//abrir parâmetros GET nativos
	tratarParametros();
	
});
