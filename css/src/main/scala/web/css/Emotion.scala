package web.css

import scala.annotation.nowarn

import scalajs.js
import scalajs.js.annotation.JSImport

@js.native
@JSImport("@emotion/css", JSImport.Namespace)
private[css] object Emotion extends js.Object {
  @nowarn def css(cssStrings: String*): String = js.native

  @nowarn def cx(classNames: String*): String = js.native

  @nowarn def keyframes(cssStrings: String*): String = js.native
}

object GoodMotion {
  @inline def css(cssString: String): ClassName = ClassName(Emotion.css(cssString))
}
