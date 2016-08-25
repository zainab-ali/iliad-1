package iliad

import cats._
import cats.data._
import cats.implicits._

import spire.{algebra => spa, math => spm}
import spire.implicits._

import shapeless._

import iliad.implicits._

/** Cartesian rectangle */
case class Rect[A](x0y0: Vec2[A], dimensions: Vec2[A]) {

  def width: A = dimensions.x
  def height: A = dimensions.y

  def bottomLeft: Vec2[A] = x0y0
  def bottomRight(implicit F: spa.AdditiveMonoid[A]): Vec2[A] =
    bottomLeft + v"$width ${F.zero}"
  def topRight(implicit F: spa.AdditiveMonoid[A]): Vec2[A] =
    bottomLeft + v"$width $height"
  def topLeft(implicit F: spa.AdditiveMonoid[A]): Vec2[A] =
    bottomLeft + v"${F.zero} $height"

  def midpoint(implicit F: spm.Fractional[A]): Vec2[A] =
    (topRight + bottomLeft) :/ F.fromInt(2)

  def contains(xy: Vec2[A])(implicit F: spa.AdditiveMonoid[A],
                            G: spa.PartialOrder[A]): Boolean = {
    val x0y0 = bottomLeft
    val x1y1 = topRight
    xy.x > x0y0.x && xy.x < x1y1.x && xy.y > x0y0.y && xy.y < x1y1.y
  }
  def combine(that: Rect[A])(implicit F: spa.AdditiveGroup[A],
                             G: spa.Order[A]): Rect[A] = {
    val cx0y0 = (this.bottomLeft map2 that.bottomLeft)(_ min _)
    val cx1y1 = (this.topRight map2 that.topRight)(_ max _)
    val dxdy = cx1y1 - cx0y0
    Rect(cx0y0, dxdy)
  }
  def ===(that: Rect[A])(implicit EQ: Eq[A]): Boolean =
    (x0y0 === that.x0y0) && (width === that.width) && (height === that.height)

  def map[B](f: A => B): Rect[B] = Rect[B](x0y0.map(f), dimensions.map(f))
  def as[B: spm.ConvertableTo](implicit F: spm.ConvertableFrom[A]): Rect[B] =
    map(F.toType[B])
}

object Rect extends RectInstances {
  def square[A](x0y0: Vec2[A], dw: A): Rect[A] = Rect(x0y0, v"$dw $dw")
}

private[iliad] abstract class RectInstances {
  implicit def rectSemigroup[A](implicit f: spa.AdditiveGroup[A],
                                g: spa.Order[A]): Semigroup[Rect[A]] =
    new RectSemigroup[A] {
      val F = f
      val G = g
    }
  implicit def rectEq[A](implicit ea: Eq[A]): Eq[Rect[A]] = new RectEq[A] {
    val EA = ea
  }
}

private[iliad] sealed trait RectSemigroup[A] extends Semigroup[Rect[A]] {
  implicit val F: spa.AdditiveGroup[A]
  implicit val G: spa.Order[A]
  def combine(x: Rect[A], y: Rect[A]): Rect[A] = x combine y
}

private[iliad] sealed trait RectEq[A] extends Eq[Rect[A]] {
  implicit val EA: Eq[A]
  def eqv(x: Rect[A], y: Rect[A]): Boolean = x === y
}

case class Line[A](p0: Vec3[A], direction: Vec3[A]) {

  def contains(p: Vec3[A])(implicit F: spm.Fractional[A]): Boolean = {
    p == p0 || direction ⋅ (p - p0).normalize == F.one
  }

  def ===[AA <: A](that: Line[AA])(implicit ea: Eq[A]): Boolean =
    p0 === that.p0 && direction === that.direction
}

object Line {
  implicit def lineEq[A](implicit ea: Eq[A]): Eq[Line[A]] = new LineEq[A] {
    val EA = ea
  }
}

private[iliad] sealed trait LineEq[A] extends Eq[Line[A]] {
  implicit val EA: Eq[A]
  def eqv(x: Line[A], y: Line[A]): Boolean = x === y
}

case class BoundedLine[A](start: Vec3[A], end: Vec3[A]) {
  def line(implicit F: spm.Fractional[A]): Line[A] =
    Line(start, (end - start).normalize)
  def length(implicit F: spm.Fractional[A]): A = (end - start).norm
  def contains(p: Vec3[A])(implicit F: spm.Fractional[A]): Boolean =
    line.contains(p) && withinBounds(p)

  def withinBounds(p: Vec3[A])(implicit F: spm.Fractional[A]): Boolean = {
    val l = (p - start) ⋅ direction
    l > F.zero && l < length
  }

  def direction(implicit F: spm.Fractional[A]): Vec3[A] = line.direction
  def distance(implicit F: spm.Fractional[A]): A = (end - start).norm
  def midpoint(implicit F: spm.Fractional[A]): Vec3[A] =
    (end + start) :/ F.fromInt(2)

  def ===[AA <: A](that: BoundedLine[AA])(implicit ea: Eq[A]): Boolean =
    start === that.start && end === that.end
}

object BoundedLine {
  implicit def boundedLineEq[A](implicit ea: Eq[A]): Eq[BoundedLine[A]] =
    new BoundedLineEq[A] {
      val EA = ea
    }
}

private[iliad] sealed trait BoundedLineEq[A] extends Eq[BoundedLine[A]] {
  implicit val EA: Eq[A]
  def eqv(x: BoundedLine[A], y: BoundedLine[A]): Boolean = x === y
}

case class Plane[A: spm.Fractional: spa.Trig](p0: Vec3[A], normal: Vec3[A]) {
  def contains(p: Vec3[A]): Boolean =
    (p - p0) ⋅ normal == spa.Field[A].zero

  def intersection(l: Line[A], θ: A): Option[Vec3[A]] = {
    val ln = l.direction ⋅ normal
    if (ln.abs < spa.Trig[A].cos(θ)) None
    else {
      val d = (p0 - l.p0) ⋅ normal / ln
      val p = d *: l.direction + l.p0
      Some(p)
    }
  }
  def intersection(l: BoundedLine[A], θ: A): Option[Vec3[A]] =
    intersection(l.line, θ).filter(l.withinBounds)

  def ===[AA <: A](that: Plane[AA])(implicit ea: Eq[A]): Boolean =
    p0 === that.p0 && normal === that.normal
}

object Plane {
  implicit def planeEq[A](implicit ea: Eq[A]): Eq[Plane[A]] = new PlaneEq[A] {
    val EA = ea
  }
}

private[iliad] sealed trait PlaneEq[A] extends Eq[Plane[A]] {
  implicit val EA: Eq[A]
  def eqv(x: Plane[A], y: Plane[A]): Boolean = x === y
}

case class BoundedPlane[A: spm.Fractional: spa.Trig](x0y0: Vec3[A],
                                                     x0y1: Vec3[A],
                                                     x1y0: Vec3[A]) {
  lazy val xAxis: BoundedLine[A] = BoundedLine(x0y0, x1y0)
  lazy val yAxis: BoundedLine[A] = BoundedLine(x0y0, x0y1)
  lazy val normal: Vec3[A] = xAxis.direction cross yAxis.direction
  lazy val plane: Plane[A] = Plane(x0y0, normal)

  def withinBounds(p: Vec3[A]): Boolean =
    xAxis.withinBounds(p) && yAxis.withinBounds(p)

  def contains(p: Vec3[A]): Boolean =
    plane.contains(p) && withinBounds(p)

  def intersection(l: Line[A], θ: A): Option[Vec3[A]] =
    plane.intersection(l, θ).filter(withinBounds)

  def intersection(l: BoundedLine[A], θ: A): Option[Vec3[A]] =
    plane.intersection(l, θ).filter(withinBounds)

  def ===[AA <: A](that: BoundedPlane[AA])(implicit ea: Eq[A]): Boolean =
    x0y0 === that.x0y0 && x1y0 === that.x1y0 && x0y1 === that.x0y1
}

object BoundedPlane {

  def fromRect[A: spm.Fractional : spa.Trig](rect: Rect[A], z: A) = 
    BoundedPlane(rect.bottomLeft.pad(3, z), 
      rect.topLeft.pad(3, z), 
      rect.bottomRight.pad(3, z))

  implicit def boundedPlaneEq[A](implicit ea: Eq[A]): Eq[BoundedPlane[A]] =
    new BoundedPlaneEq[A] {
      val EA = ea
    }
}

private[iliad] sealed trait BoundedPlaneEq[A] extends Eq[BoundedPlane[A]] {
  implicit val EA: Eq[A]
  def eqv(x: BoundedPlane[A], y: BoundedPlane[A]): Boolean = x === y
}

case class Line2[A: spm.Fractional](p0: Vec2[A], direction: Vec2[A]) {

  def ===[AA <: A](that: Line2[AA])(implicit ea: Eq[A]): Boolean =
    p0 === that.p0 && direction === that.direction

  /** Finds the point of intersection
    * @param θ the minumum angle between intersecting lines
    *          θ = 0 gives the most precise intersecion, but falls prey to zero errors
    * @param α the maximum angle between the line and an axis for the line to be 
    *          considered parallel to the x or y axis
    *          α = 0 gives the most precise description, but falls prey to zero errors
    */
  def intersection(o: Line2[A], θ: A, α: A)(
      implicit T: spa.Trig[A]): Option[Vec2[A]] = {
    if ((direction ⋅ o.direction).abs >= spa.Trig[A].cos(θ)) None
    else {
      val β =
        if (o.direction.x.abs <= spa.Trig[A].sin(α))
          (o.p0.x - p0.x) / direction.x
        else if (o.direction.y.abs <= spa.Trig[A].sin(α))
          (o.p0.y - p0.y) / direction.y
        else {
          val dp = p0 - o.p0
          (dp.y * o.direction.x - dp.x * o.direction.y) /
          (direction.x * o.direction.y - direction.y * o.direction.x)
        }
      Some(p0 + (direction :* β))
    }
  }

  def intersects(o: Line2[A], θ: A, α: A)(implicit T: spa.Trig[A]): Boolean =
    intersection(o, θ, α).nonEmpty

  def normal: Vec2[A] = v"${-direction.y} ${direction.x}"

  def distance(p: Vec2[A]): A = ((p - p0) ⋅ normal).abs

  def contains(p: Vec2[A], ds: A): Boolean = distance(p) < ds

  def parallel(d: Vec2[A], α: A)(implicit T: spa.Trig[A]): Boolean =
    (direction ⋅ d).abs >= T.cos(α)

  def parallel(that: Line2[A], α: A)(implicit T: spa.Trig[A]): Boolean =
    parallel(that.direction, α)

  def equivalent(that: Line2[A], ds: A, α: A)(
      implicit T: spa.Trig[A]): Boolean =
    parallel(that, α) && contains(that.p0, ds)

  def equation: String = {
    if (direction.x === spa.Field[A].zero) s"[x = ${p0.x}]"
    else {
      val m = direction.y / direction.x
      val c = p0.y - p0.x * m
      s"[y = $m x + $c]"
    }
  }

  override def toString: String =
    s"Line2(p0 = $p0, direction = $direction, equation = $equation"
}

object Line2 {

  def line[A: spm.Fractional](p0: Vec2[A], direction: Vec2[A]): Line2[A] =
    Line2(p0, direction.normalize)

  implicit def line2Eq[A](implicit ea: Eq[A]): Eq[Line2[A]] = new Line2Eq[A] {
    val EA = ea
  }
}

private[iliad] sealed trait Line2Eq[A] extends Eq[Line2[A]] {
  implicit val EA: Eq[A]
  def eqv(x: Line2[A], y: Line2[A]): Boolean = x === y
}

case class BoundedLine2[A](start: Vec2[A], end: Vec2[A]) {
  def line(implicit F: spm.Fractional[A]): Line2[A] =
    Line2(start, (end - start).normalize)

  def mapPoints[B](f: Vec2[A] => Vec2[B]): BoundedLine2[B] =
    BoundedLine2(f(start), f(end))

  def map[B](f: A => B): BoundedLine2[B] =
    BoundedLine2(start.map(f), end.map(f))

  def as[B: spm.ConvertableTo](
      implicit F: spm.ConvertableFrom[A]): BoundedLine2[B] =
    map(F.toType[B])

  def length(implicit F: spm.Fractional[A]): A = (end - start).norm

  /**
    * Returns true if a point on the infinite line is within the finite bounds
    *  @param β the minimum distance from the end points that a point must lie
    *  @param p a point on the infinite line
    */
  def withinBounds(ds: A)(p: Vec2[A])(implicit F: spm.Fractional[A]): Boolean = {
    val l = (p - start) ⋅ direction
    l >= ds && l <= (length - ds)
  }

  def parallel(d: Vec2[A], α: A)(implicit T: spa.Trig[A],
                                 F: spm.Fractional[A]): Boolean =
    line.parallel(d, α)

  def intersection(o: BoundedLine2[A], θ: A, α: A, ds: A)(
      implicit F: spm.Fractional[A],
      T: spa.Trig[A]): Option[Vec2[A]] =
    line
      .intersection(o.line, θ, α)
      .filter(withinBounds(ds))
      .filter(o.withinBounds(ds))

  def intersects(o: BoundedLine2[A], θ: A, α: A, ds: A)(
      implicit F: spm.Fractional[A],
      T: spa.Trig[A]): Boolean =
    intersection(o, θ, α, ds).nonEmpty

  def direction(implicit F: spm.Fractional[A]): Vec2[A] = line.direction
  def distance(implicit F: spm.Fractional[A]): A = (end - start).norm
  def midpoint(implicit F: spm.Fractional[A]): Vec2[A] =
    (end + start) :/ F.fromInt(2)

  def overlays(o: BoundedLine2[A], ds: A, α: A)(implicit F: spm.Fractional[A],
                                                T: spa.Trig[A],
                                                EA: cats.Eq[A]): Boolean = {
    if (line.equivalent(o.line, ds, α)) {
      val l =
        if (o.direction ⋅ direction > F.zero) o
        else BoundedLine2(o.end, o.start)
      if ((l.start - start).norm < ds || (l.end - end).norm < ds) {
        println(s"points are within ds ${l}")
        true
      } else {
        val signs = Set(
            ((o.start - end) ⋅ direction).sign,
            ((o.end - end) ⋅ direction).sign,
            ((o.start - start) ⋅ direction).sign,
            ((o.end - start) ⋅ direction).sign
        ).filter(_ != spa.Sign.Zero)
        signs.size != 1
      }
    } else false
  }

  /** Axis angle rotation which rotates v to this direction */
  def rotate(v: Vec2[A])(implicit F: spm.Fractional[A],
                         T: spa.Trig[A]): Vec4[A] = {
    val v3 = v.padZero(3)
    val d3 = direction.padZero(3)
    val θ = T.acos(v3 ⋅ d3).abs * (v3 cross d3).z.sign
    v"${F.zero} ${F.zero} ${F.one} $θ"
  }

  def ===[AA <: A](that: BoundedLine2[AA])(implicit ea: Eq[A]): Boolean =
    start === that.start && end === that.end

  def equivalent[AA <: A](that: BoundedLine2[AA])(
      implicit ea: Eq[A]): Boolean =
    ===(that) || (start === that.end && end === that.start)
}

object BoundedLine2 {
  implicit def boundedLine2Eq[A](implicit ea: Eq[A]): Eq[BoundedLine2[A]] =
    new BoundedLine2Eq[A] {
      val EA = ea
    }
}

private[iliad] sealed trait BoundedLine2Eq[A] extends Eq[BoundedLine2[A]] {
  implicit val EA: Eq[A]
  def eqv(x: BoundedLine2[A], y: BoundedLine2[A]): Boolean = x === y
}
