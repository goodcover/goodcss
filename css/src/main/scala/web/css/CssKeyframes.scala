package web.css

import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops.toCoercibleIdOps
import web.css.CssKeyframes.{KeyframeBlock, KeyframeSelector}

trait CssKeyframes {

  /**
    * CSS:
    * {{{
    *   @keyframes slidein {
    *      from {
    *        margin-left: 100%;
    *        width: 300%;
    *      }
    *
    *      75% {
    *        font-size: 300%;
    *        margin-left: 25%;
    *        width: 150%;
    *      }
    *
    *      to {
    *        margin-left: 0%;
    *        width: 100%;
    *      }
    *    }
    * }}}
    *
    * Scala:
    * val slidein = keyframes(
    *   from(
    *     marginLeft :- 100.percent,
    *     width :- 300.percent
    *   ),
    *   75.percent(
    *     fontSize :- 300.percent,
    *     marginLeft :- 25.percent,
    *     width :- 150.percent
    *   ),
    *   to(
    *     marginLeft :- 0.percent,
    *     width :- 100.percent
    *   )
    * )
    */
  def keyframes(blocks: KeyframeBlock*): CssKeyword =
    GoodMotion.keyframes(blocks.map {
      case (selector, css) => scope(selector.selector)(css).print
    }: _*)

  @inline implicit def percentKeyframeSelector(p: CssScalar[Percent]): KeyframeSelector =
    CssSelector(p.print).coerce[KeyframeSelector]

  val from: KeyframeSelector = CssSelector("from").coerce[KeyframeSelector]
  val to: KeyframeSelector   = CssSelector("to").coerce[KeyframeSelector]
}

object CssKeyframes {

  type KeyframeBlock = (KeyframeSelector, Css)

  @newtype case class KeyframeSelector(selector: CssSelector) {

    def apply(csss: Css*): KeyframeBlock = selector.coerce[KeyframeSelector] -> css(csss: _*)
  }
}
