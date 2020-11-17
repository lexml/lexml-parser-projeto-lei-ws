package br.gov.lexml.parser.pl.ws.resources.proc

import br.gov.lexml.parser.pl.errors._
import br.gov.lexml.parser.pl.ws.data.TextoEntrada

case object TTipoTextoNaoSuportado extends ProblemType(1000,"TipoTexto não suportado", PC_ErroGeralParser)

final case class TipoTextoNaoSuportado[T <: TextoEntrada](c : Class[T])
	extends ParseProblem(TTipoTextoNaoSuportado,Some("TipoTexto não suportado: " + c.getName))

case object TTextoAnexoOmitido extends ProblemType(1001,"Texto anexo deveria estar presente mas foi omitido",PC_ErroGeralParser)

case object TextoAnexoOmitido extends ParseProblem(TTextoAnexoOmitido,None)