package iliad
package gfx

import iliad.{gl => GL}
import iliad.algebra.{Vector => _, _}

import cats._
import cats.data._
import cats.implicits._

import monocle.macros._

import shapeless._

object Graphics {

  case class Config(pageSize: Int,
                    graph: Graph.Constructed,
                    graphTraversal: GraphTraversal,
                    dimensions: Vec2i)

  case class State(uniformCache: UniformCache.State, graph: Graph.Instance)

  type PRG = ReaderT[StateT[Xor[NonEmptyList[GraphicsError], ?], State, ?],
                     Config,
                     XorT[GL.GL.DSL, GL.GLError, Unit]]

  private val _graph: monocle.Lens[State, Graph.Instance] =
    GenLens[State](_.graph)
  private val _uniformCache: monocle.Lens[State, UniformCache.State] =
    GenLens[State](_.uniformCache)

  def empty(gc: Graph.Constructed): State =
    State(Map.empty, gc.instance)

  private def liftAction(fa: Action.Effect)
    : ReaderT[StateT[Xor[NonEmptyList[GraphicsError], ?], State, ?],
              Config,
              XorT[GL.GL.DSL, GL.GLError, Unit]] =
    Kleisli.lift(fa.applyLens(_graph).map(_ => XorT.pure(())))

  private def liftLoad(fa: Load.Effect)
    : ReaderT[StateT[Xor[NonEmptyList[GraphicsError], ?], State, ?],
              Config,
              XorT[GL.GL.DSL, GL.GLError, Unit]] =
    ReaderT(cfg => StateT.pure(fa.run(cfg)))

  private def liftUniformCache(fa: UniformCache.Effect)
    : ReaderT[StateT[Xor[NonEmptyList[GraphicsError], ?], State, ?],
              Config,
              XorT[GL.GL.DSL, GL.GLError, Unit]] =
    Kleisli.lift(
        fa.applyLens(_uniformCache)
          .transformF(a => a.leftMap(e => NonEmptyList(e, Nil)))
          .map(_ => XorT.pure(())))

  private def transform(
      g: GFX): ReaderT[StateT[Xor[NonEmptyList[GraphicsError], ?], State, ?],
                       Config,
                       XorT[GL.GL.DSL, GL.GLError, Unit]] = g match {
    case Inl(a) => liftUniformCache(UniformCache(a))
    case Inr(Inl(l)) => liftLoad(Load(l))
    case Inr(Inr(Inl(a))) => liftAction(Action(a))
    case Inr(Inr(Inr(_))) => sys.error("Impossible case!")
  }

  def apply(gs: List[GFX])
    : ReaderT[StateT[Xor[NonEmptyList[GraphicsError], ?], State, ?],
              Config,
              XorT[GL.GL.DSL, GL.GLError, Unit]] = {
    val start =
      ReaderT.pure[StateT[Xor[NonEmptyList[GraphicsError], ?], State, ?],
                   Config,
                   XorT[GL.GL.DSL, GL.GLError, Unit]](XorT.pure(()))
    gs.foldLeft(start) { (b, g) =>
      for {
        x <- b
        xx <- transform(g)
      } yield x >> xx
    }
  }

  def draws(s: State, us: UniformCache.Values)
    : ReaderT[XorT[GL.GL.DSL, IliadError, ?], Config, Unit] =
    for {
      ns <- s.graph
             .nodes(us)
             .local[Config](_.graphTraversal)
             .mapF(xor =>
                   XorT
                     .fromXor[GL.GL.DSL]
                     .apply[IliadError, Vector[Node.Drawable]](xor))
      gls <- ReaderT { (cfg: Config) =>
              val gls: List[XorT[GL.GL.DSL, GL.GLError, Unit]] =
                ToGL.run(ToGL(ns.toList)).run(cfg)
              gls.sequenceUnit.leftWiden[IliadError]
            }
    } yield gls

  def draws(s: State, us: UniformCache.Values, c: Config, glRunner: GL.GLRunner, gls: GL.GL.State): GL.GL.State = {
    val start = System.currentTimeMillis
    val nodes: List[Node.Drawable] = s.graph.nodes(us).run(c.graphTraversal).map(_.toList) match {
      case Xor.Right(n) => n
      case _ => sys.error("Unable to get nodes")
    }
    val afterNodes = System.currentTimeMillis

    val x: List[GL.GL.DSL[IliadError Xor Unit]] = 
      nodes.map(n => ToGL.run(ToGL(n)).run(c).leftWiden[IliadError].value)
    val afterGfx = System.currentTimeMillis
    val ss = x.foldLeft(gls) { (state, dsl) =>
      val xor = glRunner.run(dsl, state)
      xor.flatMap { case (ns, x) => x.map(_ => ns) } match {
        case Xor.Right(s) => s
        case _ => sys.error("Unable to execute gl")
      }
    }
    val end = System.currentTimeMillis
    println(s"total: ${end - start}, nodes: ${afterNodes - start}, gfx: ${afterGfx - afterNodes}, runner: ${end - afterGfx}")
    ss

  }
}
