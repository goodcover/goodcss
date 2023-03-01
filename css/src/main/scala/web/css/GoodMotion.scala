package web.css

import scalajs.js
import scalajs.js.annotation.JSImport

object GoodMotion {
  @inline def css(cssString: String): ClassName = ClassName(Emotion.css(cssString))

  @inline def cx(classNames: ClassName*): ClassName = ClassName(Emotion.cx(classNames.map(_.unwrap): _*))

  @inline def keyframes(cssStrings: String*): CssKeyword = CssKeyword(Emotion.keyframes(cssStrings: _*))

  @js.native
  @JSImport("@emotion/css", JSImport.Namespace)
  private object Emotion extends js.Object {
    def css(cssStrings: String*): String = js.native

    def cx(classNames: String*): String = js.native

    def keyframes(cssStrings: String*): String = js.native
  }
}
