package iliad
package gl

import cats._
import cats.data._
import cats.implicits._

import com.typesafe.scalalogging._

import freek._

sealed trait GLRunner {
  def run[A](dsl: GL.DSL[A], s: GL.State): Xor[GLError, (GL.State, A)]
}

object GLDebugLogRunner extends GLRunner with LazyLogging {
  
  private val interpreter = GL.runner(OpenGL.debugLog)

  def run[A](dsl: GL.DSL[A], s: GL.State): Xor[GLError, (GL.State, A)] = {
    val prg = dsl.interpret(interpreter)
    val (log, xor) = prg.run(GLES30).run(s).value.run
    log.foreach(l => logger.debug(l))
    xor
  }
}

object GLBasicRunner extends GLRunner {

  private val interpreter = GL.runner(iliad.gl.OpenGL.run)

  def run[A](dsl: GL.DSL[A], s: GL.State): Xor[GLError, (GL.State, A)] = {
    val start = System.nanoTime
    val prg = dsl.interpret(interpreter)
    val mid = System.nanoTime
    val r = prg.run(GLES30).run(s).right
    val end = System.nanoTime
    println(s"GLBasicRunner total: ${end - start}, interpret: ${mid - start}, runner: ${end - mid}")
    r
  }
}

//create a mutable runner
/** The cache and current should be mutable
  The load and draw are Kleislis
*/

import monocle._
import monocle.macros._
import monocle.function.all._
import monocle.syntax.all._
import monocle.std.map._

object MutableCacheParser extends (Cache ~> Id) {

  var state: Cache.State = _

  private val _vertexShaders: Lens[
      Cache.State,
      Map[VertexShader.Source, VertexShader.Compiled]] =
    GenLens[Cache.State](_.vertexShaders)
  private val _fragmentShaders: Lens[
      Cache.State,
      Map[FragmentShader.Source, FragmentShader.Compiled]] =
    GenLens[Cache.State](_.fragmentShaders)
  private val _programs: Lens[Cache.State,
                              Map[Program.Unlinked, Program.Linked]] =
    GenLens[Cache.State](_.programs)
  private val _vertexData: Lens[Cache.State,
                                Map[VertexData.Ref, VertexData.Loaded]] =
    GenLens[Cache.State](_.vertexData)
  private val _vertexBuffers: Lens[
      Cache.State,
      Map[VertexBuffer.Constructor, VertexBuffer.Loaded]] =
    GenLens[Cache.State](_.vertexBuffers)
  private val _elementData: Lens[Cache.State,
                                 Map[ElementData.Ref, ElementData.Loaded]] =
    GenLens[Cache.State](_.elementData)
  private val _elementBuffers: Lens[
      Cache.State,
      Map[ElementBuffer.Constructor, ElementBuffer.Loaded]] =
    GenLens[Cache.State](_.elementBuffers)
  private val _textures: Lens[Cache.State,
                              Map[Texture.Constructor, Texture.Loaded]] =
    GenLens[Cache.State](_.textures)
  private val _renderbuffers: Lens[
      Cache.State,
      Map[Renderbuffer.Constructor, Renderbuffer.Loaded]] =
    GenLens[Cache.State](_.renderbuffers)
  private val _framebuffers: Lens[
      Cache.State,
      Map[Framebuffer.Constructor, Framebuffer.Loaded]] =
    GenLens[Cache.State](_.framebuffers)
  private val _samplers: Lens[Cache.State,
                              Map[Sampler.Constructor, Sampler.Loaded]] =
    GenLens[Cache.State](_.samplers)

  def apply[A](cached: Cache[A]): Id[A] =
    cached match {
      case VertexShaderGet(vs) => state &|-> _vertexShaders ^|-> at(vs) get
      case FragmentShaderGet(fs) => state &|-> _fragmentShaders ^|-> at(fs) get
      case ProgramGet(p) => state &|-> _programs ^|-> at(p) get
      case VertexDataGet(d) => state &|-> _vertexData ^|-> at(d) get
      case VertexBufferGet(b) => state &|-> _vertexBuffers ^|-> at(b) get
      case ElementDataGet(d) => state &|-> _elementData ^|-> at(d) get
      case ElementBufferGet(b) => state &|-> _elementBuffers ^|-> at(b) get
      case TextureGet(t) => state &|-> _textures ^|-> at(t) get
      case RenderbufferGet(r) => state &|-> _renderbuffers ^|-> at(r) get
      case FramebufferGet(f) => state &|-> _framebuffers ^|-> at(f) get
      case SamplerGet(s) => state &|-> _samplers ^|-> at(s) get

      case VertexShaderPut(vs) => state = state &|-> _vertexShaders ^|-> at(vs.source) set Some(vs)
      case FragmentShaderPut(fs) => state = state &|-> _fragmentShaders ^|-> at(fs.source) set Some(fs)
      case ProgramPut(p) => state = state &|-> _programs ^|-> at(p.unlinked) set Some(p)
      case VertexDataPut(d) => state = state &|-> _vertexData ^|-> at(d.ref) set Some(d)
      case VertexBufferPut(b) => state = state &|-> _vertexBuffers ^|-> at(b.constructor) set Some(b)
      case ElementDataPut(d) => state = state &|-> _elementData ^|-> at(d.ref) set Some(d)
      case ElementBufferPut(b) => state = state &|-> _elementBuffers ^|-> at(b.constructor) set Some(b)
      case TexturePut(t) => state = state &|-> _textures ^|-> at(t.constructor) set Some(t)
      case RenderbufferPut(r) => state = state &|-> _renderbuffers ^|-> at(r.constructor) set Some(r)
      case FramebufferPut(f) => state = state &|-> _framebuffers ^|-> at(f.constructor) set Some(f)
      case SamplerPut(s) => state = state &|-> _samplers ^|-> at(s.constructor) set Some(s)
    }
}

import iliad.algebra._

object MutableCurrentParser extends (Current ~> Id) {

  var state: Current.State = _

  private val _program: Lens[Current.State, Option[Program.Linked]] =
    GenLens[Current.State](_.program)

  private val _framebuffer: Lens[Current.State, Option[Framebuffer.Loaded]] =
    GenLens[Current.State](_.framebuffer)

  private val _vertexBuffer: Lens[Current.State, Option[VertexBuffer.Loaded]] =
    GenLens[Current.State](_.vertexBuffer)

  private val _elementBuffer: Lens[Current.State, Option[ElementBuffer.Loaded]] =
    GenLens[Current.State](_.elementBuffer)

  private val _colorMask: Lens[Current.State, Option[ColorMask]] =
    GenLens[Current.State](_.colorMask)

  private val _capabilities: Lens[Current.State, Map[Capability, Boolean]] =
    GenLens[Current.State](_.capabilities)

  private val _clearColour: Lens[Current.State, Option[Vec4f]] =
    GenLens[Current.State](_.clearColour)

  private val _blendMode: Lens[Current.State, Option[BlendMode]] =
    GenLens[Current.State](_.blendMode)

  private val _blendFunction: Lens[Current.State, Option[BlendFunction]] =
    GenLens[Current.State](_.blendFunction)

  private val _viewport: Lens[Current.State, Rect[Int]] =
    GenLens[Current.State](_.viewport)

  def apply[A](current: Current[A]): Id[A] = current match {
    case CurrentProgramGet => state &|-> _program get
    case CurrentFramebufferGet => state &|-> _framebuffer get
    case CurrentVertexBufferGet => state &|-> _vertexBuffer get
    case CurrentElementBufferGet => state &|-> _elementBuffer get
    case CurrentColorMaskGet => state &|-> _colorMask get
    case CurrentCapabilityGet(c) => state &|-> _capabilities ^|-> at(c) get
    case CurrentClearColourGet => state &|-> _clearColour get
    case CurrentBlendModeGet => state &|-> _blendMode get
    case CurrentBlendFunctionGet => state &|-> _blendFunction get
    case CurrentViewportGet => state &|-> _viewport get

    case CurrentProgramSet(p) => state = state &|-> _program set Some(p)
    case CurrentFramebufferSet(f) => state = state &|-> _framebuffer set Some(f)
    case CurrentVertexBufferSet(b) => state = state &|-> _vertexBuffer set Some(b)
    case CurrentElementBufferSet(b) => state = state &|-> _elementBuffer set Some(b)
    case CurrentColorMaskSet(m) => state = state &|-> _colorMask set Some(m)
    case CurrentCapabilitySet(c, v) => state = state &|-> _capabilities ^|-> at(c) set Some(v)
    case CurrentClearColourSet(c) => state = state &|-> _clearColour set Some(c)
    case CurrentBlendModeSet(m) => state = state &|-> _blendMode set Some(m)
    case CurrentBlendFunctionSet(f) => state = state &|-> _blendFunction set Some(f)
    case CurrentViewportSet(r) => state = state &|-> _viewport set r
  }
}


object GLMutableRunner extends GLRunner {

  private def lift: Id ~> ReaderT[Id, GLES30.type, ?] = new (Id ~> ReaderT[Id, GLES30.type, ?]) {
    def apply[A](a: Id[A]): ReaderT[Id, GLES30.type, A] = ReaderT.pure(a)
  }


  def run[A](dsl: GL.DSL[A], s: GL.State): Xor[GLError, (GL.State, A)] = {
    MutableCacheParser.state = s.cache
    MutableCurrentParser.state = s.current
    val interpreter: Interpreter[GL.GL.Cop, ReaderT[Id, GLES30.type, ?]]  = 
      Load.parse(OpenGL.run) :&:
    (MutableCacheParser andThen lift) :&:
    Draw.parse(OpenGL.run) :&:
    (MutableCurrentParser andThen lift)

    val a: A = dsl.interpret(interpreter).run(GLES30)
    val next = GL.State(MutableCacheParser.state, MutableCurrentParser.state)
    (next, a).right
  }
}
