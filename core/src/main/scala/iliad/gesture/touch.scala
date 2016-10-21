package iliad
package gesture

import iliad.algebra._

case class Touch(position: Vec2d, action: Action)

sealed trait Action
object Action {
  case object Down extends Action
  case object Up extends Action
  case object Move extends Action
}
