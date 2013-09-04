package br.gov.lexml.parser.pl.ws.tasks
import br.gov.lexml.parser.pl.output.LexmlRenderer



object FragmentFormatter {
  
    abstract sealed class Numero {
      val n : Int
    }
    final case object Unico extends Numero {
      override val n = 1
    }
    final case class Algum(n : Int) extends Numero
  
    val compRe = "^([a-z]+)((?:1u|[0-9-])*)$".r
    type Comp = (String,List[Numero])
    
    def readInt : String => Numero = {
      case "1u" => Unico
      case x => Algum(x.toInt)
    }
    
	def format(urnFrag : String) = {
	  println("FragmentFormatter: urnFrag = '" + urnFrag + "'")
	  val comps = urnFrag
	  		.split("_").toList
	  		.flatMap(compRe.findFirstMatchIn(_))
	  		.map(m => (m.group(1),m.group(2).split("-").toList.filter(!_.isEmpty).map(readInt(_))))
	  		.flatMap(formatComp(_))
	  		.reverse
	  comps match {
	    case ((_,t) :: r) => t + r.map({ case (g,txt) => "d" + g + " " + txt }).mkString(" "," ","")
	    case _ => ""
	  } 	  
	}
    
    type FormattedComp = (String,String)
    
    val agregadores : Map[String,(String,String)] = Map(        
    	"prt" -> ("a","parte"),    	
    	"liv" -> ("o", "livro"),
    	"cap" -> ("o", "capítulo"),
    	"sec" -> ("a", "seção"),
    	"sub" -> ("a", "subseção")
        )
    
    def formatComp : Comp => Option[FormattedComp] = {
      case ("art",Unico :: _) => 
        Some(("o","artigo único"))
      case ("art",Algum(n) :: cs) => 
        Some(("o","artigo " + formatOrdinal(n) + formatComplementos(cs)))
      case ("cpt",_) => None
      case ("par",Unico :: _) => Some(("o","parágrafo único"))
      case ("par",Algum(n) :: cs) => 
        Some(("o","parágrafo " + formatOrdinal(n) + formatComplementos(cs)))
      case ("inc",n :: cs) =>
        Some(("o","inciso " + formatRomano(n.n).toUpperCase + formatComplementos(cs)))
      case ("ali",n :: cs) => 
        Some(("a","alínea " + formatAlfa(n.n).toLowerCase + formatComplementos(cs)))
      case ("ite",n :: cs) =>
        Some(("o","item " + n.n.toString + formatComplementos(cs)))      
      case (tip,n :: cs) if agregadores contains tip => {
        val (g,t) = agregadores(tip)
        val ntxt = n match {
          case Unico => "único"
          case Algum(n) => formatRomano(n).toUpperCase
        }
        Some((g,t + " " + ntxt))
      }
      case _ => None
    }
    
    def formatOrdinal(num : Int) : String =  LexmlRenderer.renderOrdinal(num)        
    
    def formatRomano(n : Int) : String = LexmlRenderer.renderRomano(n)
    
    def formatAlfa(n : Int) : String = LexmlRenderer.renderAlphaSeq(n - 1)
            
    def formatComplementos(cs : List[Numero]) : String = cs.map(c => formatComplemento(c.n)).map("-" + _).mkString("") 
    def formatComplemento(n : Int) : String = formatAlfa(n-1).toUpperCase
}