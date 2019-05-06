package br.gov.lexml.parser.pl.ws.resources.proc

import br.gov.lexml.parser.pl.errors._

import scala.xml.{Elem, NodeSeq, Text}

object UsuarioMensagem {
  
  def mensagemUsuario(p: ParseProblem) : Option[NodeSeq] = mensagemUsuarioP.lift(p)
  
  
   type MsgFunc = PartialFunction[ParseProblem,Elem]
    /**
     * Renderiza o id em HTML
     */
    def rid(id : String) : NodeSeq = {
      Text(id)
    }
    def contexto(p : ParseProblem) : NodeSeq  =
      if (p.context.isEmpty) { NodeSeq.Empty } else {
        NodeSeq.fromSeq(<p>Contexto: </p> +: p.context.map(x => <p>{x}</p>))
      }

    /**
     * Função parcial que retorna mensagem que deve ser mostrada ao usuário
     * em função do problema relatado pelo parser.
     * A mensagem deve está encapsulado em um elemento XML raiz que será 
     * ignorado (apenas o seu conteúdo será aproveitado)
     */
   val mensagemUsuarioP : MsgFunc = {
     case p@NiveisDiferentes(id1, id2) =>
       <div>
         <p>Dispositivos incompatíveis agrupados no mesmo nível:</p>
    	 <ul><li>{rid(id1)}</li><li>{rid(id2)}</li></ul>
    	 <p>Causa provável: erro no uso de aspas em uma alteração.</p>
         {contexto(p)}
       </div>
     case p@OrdemInvertida(id1, id2) =>
       <div>
         <p>Dispositivos com ordem invertida:</p>
    	 <ul><li> {rid(id1)}</li><li>{rid(id2)}.</li></ul>
         <p>Causa provável: erro no uso de aspas em uma alteração.</p>
         {contexto(p)}
       </div>

     case p@DispositivosDescontinuos(id1, id2) =>
       <div>
         <p>Foi detectada uma discontinuidade na sequência de dispositivos:</p>
         <ul><li> {rid(id1)}</li><li>{rid(id2)}.</li></ul>
         {contexto(p)}
       </div>

     case p@EmentaAusente =>
       <div>
         <p>O Parser não conseguiu identificar a ementa do texto.
    		Verifique se o preâmbulo do texto segue as normas de redação.
         </p>
         {contexto(p)}
       </div>        
   }
}