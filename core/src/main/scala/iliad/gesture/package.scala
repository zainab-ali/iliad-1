package iliad

import cats.data._

package object gesture {

  type MultiTouchList = NonEmptyList[TouchList]
  type TouchList = NonEmptyList[Touch]
}
