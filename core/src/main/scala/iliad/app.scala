package iliad

import iliad.gfx._
import iliad.algebra.syntax.vector._

import scala.reflect._
import iliad.gl._

import cats._
import cats.~>
import cats.implicits._
import cats.data._

import fs2._
import fs2.util._
import fs2.async.mutable._

import freek._

import com.typesafe.scalalogging._

case class Config(
  graph: State[Graph.Constructor, Unit],
  graphTraversal: GraphTraversal
)


trait GLBootstrap extends LazyLogging {

  def config: Config

  def glRunner: GLRunner

  def width: Int
  def height: Int

  private def graphicsConfig: Task[Graphics.Config] =
    Construct
      .validate(config.graph)
      .map(Graphics.Config(Session.pageSize, _, config.graphTraversal, 
        v"$width $height"))
      .leftMap(_.toList.mkString("\n"))
      .bimap(s => Task.fail(new Error(s)), Task.now).merge[Task[Graphics.Config]]

  private val LogEGLInterpreter: EGL ~> ReaderT[Xor[EGLError, ?], EGL14.type, ?] = 
    EGL.logInterpreter

  private def EGLTask(window: EGL14.EGLNativeWindowType, display: EGL14.EGLNativeDisplayType)(
      cattrs: Attributes[ConfigAttrib, ConfigAttribValue],
      wattrs: Attributes[WindowAttrib, WindowAttribValue],
      cxattrs: Attributes[ContextAttrib, ContextAttribValue])(
      implicit S: Strategy): Task[(EGL14.EGLDisplay, EGL14.EGLSurface, EGL14.EGLContext)] = {
    val prg = (for {
      dpy <- XorT(EGL.initialise(display))
      _ <- XorT.right(EGL.properties(dpy))
      cfg <- XorT(
                EGL.config(dpy, cattrs)
                  .map(_.toRightXor(EGLConfigError(cattrs))))
              .leftWiden[EGLError]
      _ <- XorT.right(EGL.configAttribs(dpy, cfg))
      sfc <- XorT(EGL.windowSurface(dpy, cfg, window, wattrs))
              .leftWiden[EGLError]
      ctx <- XorT(EGL.context(dpy, cfg, cxattrs)).leftWiden[EGLError]
      _ <- XorT(EGL.makeCurrent(dpy, sfc, sfc, ctx)).leftWiden[EGLError]
    } yield (dpy, sfc, ctx)).value
    eglExecute(display, prg)
  }

  private def eglExecute[A](d: EGL14.EGLNativeDisplayType,
                            dsl: EGL.DSL[EGLError Xor A]): Task[A] = {
#+x11
    Session.lockDisplay(d)
#-x11
    val t = dsl.foldMap(LogEGLInterpreter).run(EGL14).flatMap(identity)
      .bimap(Task.fail, Task.now).merge[Task[A]]
#+x11
    Session.unlockDisplay(d)
#-x11
    t
  }

  val EGLStrategy = Strategy.fromFixedDaemonPool(1, "egl-thread")

  private def egl(
      w: EGL14.EGLNativeWindowType,
      d: EGL14.EGLNativeDisplayType): Task[(EGL14.EGLDisplay, EGL14.EGLSurface, EGL14.EGLContext)] =
    EGLTask(w, d)(
        Attributes(
            ConfigAttrib(EGL_LEVEL, 0),
            ConfigAttrib(EGL_SURFACE_TYPE, EGL_WINDOW_BIT),
            ConfigAttrib(EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT.value),
            ConfigAttrib(EGL_CONFORMANT, EGL_OPENGL_ES3_BIT),
            ConfigAttrib(EGL_BLUE_SIZE, 8),
            ConfigAttrib(EGL_GREEN_SIZE, 8),
            ConfigAttrib(EGL_RED_SIZE, 8),
            ConfigAttrib(EGL_ALPHA_SIZE, 8),
            ConfigAttrib(EGL_BUFFER_SIZE, 32),
            ConfigAttrib(EGL_DEPTH_SIZE, 16)
        ),
        Attributes.empty,
        Attributes(
            ContextAttrib(EGL_CONTEXT_CLIENT_VERSION, 3)
        ))(EGLStrategy)

  private def swapBuffers(nd: EGL14.EGLNativeDisplayType,
                          d: EGL14.EGLDisplay,
                          s: EGL14.EGLSurface): Task[Boolean] =
    eglExecute(nd, XorT(EGL.swapBuffers(d, s)).leftWiden[EGLError].value)

  private def aggregateRight[F[_]: Async, A, B]: Pipe2[F, A, B, (A, List[B])] =
    (fa, fb) =>
      (fa either fb)
        .mapAccumulate2(List.empty[B]) { (bs, i) =>
          i.toXor match {
            case Xor.Left(a) => (Nil, Some((a, bs.reverse)))
            case Xor.Right(b) => (b :: bs, None)
          }
        }
        .filter(_.nonEmpty)
        .map(_.get)

  private def aggregate(implicit S: Strategy)
    : Pipe[Task, List[GFX], (Long, List[GFX])] =
    q =>
      (Session.vsync through2 q)(aggregateRight).map {
        case (at, cmds) => (at, cmds.flatten.toList)
    }

  private def run(cfg: Graphics.Config, gs: List[GFX])(
      s: Graphics.State)
    : Error Xor (Graphics.State, XorT[GL.DSL, GLError, Unit]) =
    Graphics(gs).run(cfg).run(s).leftMap(s => new Error(s.toString))
 
  private def run[A](gl: GL.DSL[IliadError Xor A],
                     s: GL.State): Xor[Error, GL.State] = {

    val xor = glRunner.run(gl, s)
    xor.flatMap {
      case (nextS, xxor) => xxor.map(_ => nextS)
    }.leftMap(s => new Error(s.toString))
  }

  private def run(cfg: Graphics.Config,
                  gls: GL.State,
                  us: UniformCache.Values,
                  gs: Graphics.State): Xor[Error, GL.State] = {
    Graphics.draws(gs, us, cfg, glRunner, gls).right
    // val start = System.currentTimeMillis
    // val gl = Graphics.draws(gs, us).run(cfg).value
    // val afterDraws = System.currentTimeMillis
    // val xor = glRunner.run(gl, gls)
    // val afterRunner = System.currentTimeMillis
    // println(s"total: ${afterRunner - start}, draws: ${afterDraws - start}, runner: ${afterRunner - afterDraws}")
    // xor.flatMap {
    //   case (nextS, xxor) => xxor.map(_ => nextS)
    // }.leftMap(s => new Error(s.toString))
  }


  // private def run(cfg: Graphics.Config,
  //                 gls: GL.State,
  //                 us: UniformCache.Values,
  //                 gs: Graphics.State): Xor[Error, GL.State] = {
  //   val start = System.currentTimeMillis
  //   val gl = Graphics.draws(gs, us).run(cfg).value
  //   val afterDraws = System.currentTimeMillis
  //   val xor = glRunner.run(gl, gls)
  //   val afterRunner = System.currentTimeMillis
  //   println(s"total: ${afterRunner - start}, draws: ${afterDraws - start}, runner: ${afterRunner - afterDraws}")
  //   xor.flatMap {
  //     case (nextS, xxor) => xxor.map(_ => nextS)
  //   }.leftMap(s => new Error(s.toString))
  // }

  private def run(
    at: Long,
    us: UniformCache.State): (UniformCache.State, UniformCache.Values) =
    UniformCache.values(at).run(us).value

  val GLPipe: Pipe[Task, List[GFX], Unit] = graphics =>
    for {
      (nw, nd) <- Stream.eval(Session.session.task(EGLStrategy))
      (d, sfc, ctx) <- Stream.eval(egl(nw, nd))
      cfg <- Stream.eval(graphicsConfig)
      _ <- (graphics through aggregate(EGLStrategy))
            .mapAccumulate2(
                (Graphics.empty(cfg.graph), GL.empty(cfg.dimensions)).right[Error]
            ) { (prev, t) =>
              val (at, gs) = t
              val xor = for {
                p <- prev
                (prevGr, prevGl) = p
                start = System.currentTimeMillis
                q <- run(cfg, gs)(prevGr)
                afterGfx = System.currentTimeMillis
                (nextGr, loadCmds) = q
                midGl <- run(loadCmds.leftWiden[IliadError].value, prevGl)
                afterLoad = System.currentTimeMillis
                us = run(at, nextGr.uniformCache)
                afterUniforms = System.currentTimeMillis
                nextGl <- run(cfg, midGl, us._2, nextGr)
                afterDraws = System.currentTimeMillis
              } yield {
//                println(s"total: ${afterDraws - start}, gfx: ${afterGfx - start}, load: ${afterLoad - afterGfx}, uniforms: ${afterUniforms - afterLoad}, draws: ${afterDraws - afterUniforms}")
                (nextGr.copy(uniformCache = us._1), nextGl)
              }
              (xor, xor.bimap(Task.fail, Task.now).merge[Task[(Graphics.State, GL.State)]])
            }
            .eval
      _ <- Stream.eval(swapBuffers(nd, d, sfc))
    } yield ()
}
