package br.gov.lexml.parser.pl.ws.resources.proc

import br.gov.lexml.parser.pl.Caracteristicas

object CaracteristicasImpeditivas  {
	
    import Caracteristicas._
  
    val caracteristicaImpeditiva: Set[String] = Set[String](
        POSSUI_TABELA_ARTICULACAO,        
        POSSUI_TITULO,
        POSSUI_PENA,
        POSSUI_IMAGEM
        )
  
	
}