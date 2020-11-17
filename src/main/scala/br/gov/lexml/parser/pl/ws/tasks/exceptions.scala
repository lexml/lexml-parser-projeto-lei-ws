package br.gov.lexml.parser.pl.ws.tasks

import br.gov.lexml.parser.pl.errors._

case object PC_ServicoWeb extends ProblemCategory(20,"Erro do serviço Web")

case object TAutoridadeInvalida extends ProblemType(100,"Autoridade inválida", PC_ServicoWeb)

case object TTipoNormaInvalido extends ProblemType(101,"Tipo de norma inválido", PC_ServicoWeb)

case object TDescritorEventoInvalido extends ProblemType(102,"DecritorEvento inválido", PC_ServicoWeb)

case object TTipoTextoNaoSuportado extends ProblemType(103, "Tipo de texto não suportado", PC_ServicoWeb)

case object TFalhaConversaoXHTML extends ProblemType(104, "Falha na conversão para XHTML", PC_ServicoWeb)

case object TTextoAnexoOmitido extends ProblemType(105, "Texto Anexo omitido", PC_ServicoWeb)

case object TFalhaGeracaoPDF extends ProblemType(106, "Falha na geração de PDF", PC_ServicoWeb)

final case class AutoridadeInvalida(autoridade : String) extends ParseProblem(TAutoridadeInvalida,
    Some("Autoridade inválida: " + autoridade))

final case class TipoNormaInvalido(tipoNorma : String) extends ParseProblem(TTipoNormaInvalido, Some("Tipo de norma inválido: " + tipoNorma))

final case class DescritorEventoInvalido(descritorEvento : String) extends ParseProblem(TDescritorEventoInvalido, Some("DescritorEvento inválido: " + descritorEvento))
  
//final case class TipoTextoNaoSuportado(tipoTexto : TipoTextoOption) extends ParseProblem(TTipoTextoNaoSuportado,Some("TipoTexto não suportado ainda: "  + tipoTexto))

final case class FalhaConversaoXHTML(mime : String) extends ParseProblem(TFalhaConversaoXHTML,Some("Falha na conversão para XHTML. MIME = " + mime))

case object TextoAnexoOmitido extends ParseProblem(TTextoAnexoOmitido,None)

final case class FalhaGeracaoPDF(ex : Option[Exception] = None) extends ParseProblem(TFalhaGeracaoPDF,ex.map(e => TFalhaGeracaoPDF.description + ": " + e.getMessage))