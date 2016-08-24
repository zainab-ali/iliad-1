package iliad
package algebra

import org.scalacheck._

import shapeless._
import shapeless.ops.nat._


trait ArbitraryInstances {
  implicit def matrixArbitrary[N <: Nat, M <: Nat, A](implicit arbA: Arbitrary[A], toIntN: ToInt[N], toIntM: ToInt[M]): Arbitrary[Matrix[N, M, A]] = Arbitrary {
    MatrixGen.gen(arbA.arbitrary)
  }
  implicit def orthoMatrixArbitrary[N <: Nat, M <: Nat, A](implicit matrixArb: Arbitrary[Matrix[N, M, A]]): Arbitrary[OrthoMatrix[N, M, A]] = Arbitrary {
    matrixArb.arbitrary.map(OrthoMatrix(_))
  }
}

object arbitrary extends ArbitraryInstances
