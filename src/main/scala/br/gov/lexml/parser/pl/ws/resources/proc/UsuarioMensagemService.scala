package br.gov.lexml.parser.pl.ws.resources.proc

import java.io.File
import scala.xml.NodeSeq
import br.gov.lexml.parser.pl.errors.ParseProblem
import br.gov.lexml.parser.pl.ws.ServiceParams
import br.gov.lexml.parser.pl.errors.NiveisDiferentes
import br.gov.lexml.parser.pl.errors.OrdemInvertida
import br.gov.lexml.parser.pl.errors.EmentaAusente
import br.gov.lexml.parser.pl.errors.DispositivosDescontinuos
import scala.xml.Elem
import scala.xml.Text

object UsuarioMensagem {
  
  def mensagemUsuario(p: ParseProblem) : Option[NodeSeq] = mensagemUsuarioP.lift(p)
  
  
   type MsgFunc = PartialFunction[ParseProblem,Elem]
    /**
     * Renderiza o id em HTML
     */
    def rid(id : String) : NodeSeq = {
      Text(id)
    }
    /**
     * Função parcial que retorna mensagem que deve ser mostrada ao usuário
     * em função do problema relatado pelo parser.
     * A mensagem deve está encapsulado em um elemento XML raiz que será 
     * ignorado (apenas o seu conteúdo será aproveitado)
     */
   val mensagemUsuarioP : MsgFunc = {
     case NiveisDiferentes(id1, id2) => (
       <div>
         <p>Dispositivos incompatíveis agrupados no mesmo nível:</p>
    	 <ul><li>{rid(id1)}</li><li>{rid(id2)}</li></ul>
    	 <p>Causa provável: erro no uso de aspas em uma alteração.</p>
       </div>)
     case OrdemInvertida(id1, id2) => (
       <div>
         <p>Dispositivos com ordem invertida:</p>
    	 <ul><li> {rid(id1)}</li><li>{rid(id2)}.</li></ul>
         <p>Causa provável: erro no uso de aspas em uma alteração.</p> 
       </div>
     )
     case DispositivosDescontinuos(id1, id2) => (
       <div>
         <p>Foi detectada uma discontinuidade na sequência de dispositivos:</p>
         <ul><li> {rid(id1)}</li><li>{rid(id2)}.</li></ul>
       </div>
     )      
     case EmentaAusente => (
       <div>
         <p>O Parser não conseguiu identificar a ementa do texto.
    		Verifique se o preâmbulo do texto segue as normas de redação.
         </p>
       </div>
     )     
   }
}