package iliad
package gesture

import iliad.algebra._

import cats._
import cats.data._

case class Recogniser[E, C, O](recognize: (E, C) => Status[O]) extends AnyVal {

  def contramapConfig[CC](f: CC => C): Recogniser[E, CC, O] = new Recogniser[E, CC, O]({ (e, cc) =>
    recognize(e, f(cc))
  })

  def contramapEvent[EE](f: EE => E): Recogniser[EE, C, O] = new Recogniser[EE, C, O]({ case (ee, c) =>
    recognize(f(ee), c)
  })

  def filterEvent[EE](f: EE => Option[E]): Recogniser[EE, C, O] = new Recogniser[EE, C, O]({ case (ee, c) =>
    f(ee) match {
      case None => Status.Failed
      case Some(e) => recognize(e, c)
    }
  })

  def filterConfig[CC](f: CC => Option[C]): Recogniser[E, CC, O] = new Recogniser[E, CC, O]({ case (e, cc) =>
    f(cc) match {
      case None => Status.Failed
      case Some(c) => recognize(e, c)
    }
  })

  def mapOC[OO](f: (C, O) => OO): Recogniser[E, C, OO] = new Recogniser[E, C, OO]({ (e, c) =>
    recognize(e, c) match {
      case Status.Succeeded(o) => Status.Succeeded(f(c, o))
      case Status.Began(o) => Status.Began(f(c, o))
      case Status.Failed => Status.Failed
      case Status.Possible => Status.Possible
    }
  })
}

sealed trait Status[+O]
sealed trait DiscreteStatus[O] extends Status[O]
sealed trait ContinuousStatus[O] extends Status[O]
object Status {
  case object Possible extends DiscreteStatus[Nothing] with ContinuousStatus[Nothing]
  case object Failed extends DiscreteStatus[Nothing] with ContinuousStatus[Nothing]
  case class Succeeded[O](output: O) extends DiscreteStatus[O]

  case class Began[O](output: O) extends ContinuousStatus[O]
  case class Changed[O](output: O) extends ContinuousStatus[O]
  case class Ended[O](output: O) extends ContinuousStatus[O]
  case object Cancelled extends ContinuousStatus[Nothing]
}

/*
 class ContinuousRecogniser[E, C, O](recognize: (E, C) => ContinuousStatus[O]) extends AnyVal {
}

sealed trait ContinuousStatus[+O]

object ContinuousState {
  case object Possible extends ContinuousStatus[Nothing]
  case object Failed extends ContinuousStatus[Nothing]
  case class Began[O](output: O) extends ContinuousStatus[O]
  case class Changed[O](output: O) extends ContinuousStatus[O] 
  case class Ended[O](output: O) extends ContinuousStatus[O]
  case object Cancelled extends ContinuousStatus[Nothing]
}*/



/*
//Test it out
case class SingleTap(position: Vec2d)

object TestSmth{

  val tap = Recogniser[TouchList, Unit, SingleTap]({ (e, _) =>
    e.toList.map(_.action) match {
      case Action.Up :: Action.Down :: Nil => Status.Succeeded(SingleTap(e.head.position))
      case Action.Move :: Action.Down :: Nil => Status.Failed
      case Action.Down :: Nil => Status.Possible
      case _ => Status.Failed
    }
  })

  case class ButtonColour(colour: Vec3d)
  case class ButtonTap(colour: Vec3d)
  
  val colourTapped: Recogniser[MultiTouchList, ButtonColour, ButtonTap] = 
    tap.contramapConfig[ButtonColour](bc => ()).mapOC { (bc, _) =>
      ButtonTap(bc.colour)
    }.filterEvent[MultiTouchList] { mtl =>
      if(mtl.toList.size == 1) Some(mtl.head)
      else None
    }
}
 */
//NOTES:
/**
We shouldn't duplicate the touch group state with the handler state.
Problems:
 - How do we determine if a touch group has been handled? Or do we just propagate the current touch all the time?

 - we should take in a touch list and not need an internal state

*/
