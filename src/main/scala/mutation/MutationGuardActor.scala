package mutation

import akka.actor.{Actor, ActorLogging}
import samples.Sample.Population

class MutationGuardActor[T](subpopulation: Population[T]) extends Actor with ActorLogging {



}

object MutationGuardActor {
  def props
}
