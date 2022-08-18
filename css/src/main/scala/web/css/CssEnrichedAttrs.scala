package web.css

import slinky.core.{AttrPair, OptionalAttrPair}
import slinky.web.html._

import scalajs.js.{UndefOr => U}
import scalajs.js.JSConverters._
import OptionalAttrPair.optionToJsOption

final class RichClassName(val attr: className.type) extends AnyVal {

  @inline def :=(v: ClassName): AttrPair[_className_attr.type] =
    new AttrPair[_className_attr.type]("className", GoodMotion.cx(v))

  @inline def :=(o: Option[ClassName]): OptionalAttrPair[_className_attr.type] =
    new OptionalAttrPair[_className_attr.type]("className", optionToJsOption(o.map(GoodMotion.cx(_))))

  @inline def :=[V](v: V)(implicit ev: V => ClassName): AttrPair[_className_attr.type] = this.:=(ev(v))

  @inline def :=[V](o: Option[V])(implicit ev: V => ClassName): OptionalAttrPair[_className_attr.type] =
    this.:=(o.map(ev))
}

final class RichStyle(val attr: style.type) extends AnyVal {
  @inline def :=(v: Map[String, U[String]]) = new AttrPair[_style_attr.type]("style", v.toJSDictionary)

  @inline def :=(v: Option[Map[String, U[String]]]) =
    new OptionalAttrPair[_style_attr.type]("style", optionToJsOption(v.map(_.toJSDictionary)))
}
