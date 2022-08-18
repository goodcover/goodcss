package web.css

trait CssProps {
  val prop: CssProps = this

  val alignItems               = prop"align-items"
  val alignSelf                = prop"align-self"
  val animation                = prop"animation"
  val background               = prop"background"
  val backgroundColor          = prop"background-color"
  val backgroundImage          = prop"background-image"
  val backgroundPosition       = prop"background-position"
  val backgroundRepeat         = prop"background-repeat"
  val backgroundSize           = prop"background-size"
  val border                   = prop"border"
  val borderBottom             = prop"border-bottom"
  val borderBottomColor        = prop"border-bottom-color"
  val borderBottomLeftRadius   = prop"border-bottom-left-radius"
  val borderBottomRightRadius  = prop"border-bottom-right-radius"
  val borderColor              = prop"border-color"
  val borderLeft               = prop"border-left"
  val borderLeftColor          = prop"border-left-color"
  val borderRadius             = prop"border-radius"
  val borderRight              = prop"border-right"
  val borderRightColor         = prop"border-right-color"
  val borderStyle              = prop"border-style"
  val borderTop                = prop"border-top"
  val borderTopColor           = prop"border-top-color"
  val borderTopLeftRadius      = prop"border-top-left-radius"
  val borderTopRightRadius     = prop"border-top-right-radius"
  val borderWidth              = prop"border-width"
  val bottom                   = prop"bottom"
  val boxShadow                = prop"box-shadow"
  val boxSizing                = prop"box-sizing"
  val clear                    = prop"clear"
  val color                    = prop"color"
  val columnGap                = prop"column-gap"
  val content                  = prop"content"
  val counterIncrement         = prop"counter-increment"
  val counterReset             = prop"counter-reset"
  val cursor                   = prop"cursor"
  val display                  = prop"display"
  val filter                   = prop"filter"
  val flexBasis                = prop"flex-basis"
  val flexDirection            = prop"flex-direction"
  val flexFlow                 = prop"flex-flow"
  val flexGrow                 = prop"flex-grow"
  val flexShrink               = prop"flex-shrink"
  val flexWrap                 = prop"flex-wrap"
  val float                    = prop"float"
  val fontFamily               = prop"font-family"
  val fontFeatureSettings      = prop"font-feature-settings"
  val fontSize                 = prop"font-size"
  val fontVariant              = prop"font-variant"
  val fontWeight               = prop"font-weight"
  val gridArea                 = prop"grid-area"
  val gridColumnEnd            = prop"grid-column-end"
  val gridColumnStart          = prop"grid-column-start"
  val gridRowEnd               = prop"grid-row-end"
  val gridRowStart             = prop"grid-row-start"
  val gridTemplateAreas        = prop"grid-template-areas"
  val gridTemplateColumns      = prop"grid-template-columns"
  val gridTemplateRows         = prop"grid-template-rows"
  val height                   = prop"height"
  val justifyContent           = prop"justify-content"
  val justifySelf              = prop"justify-self"
  val left                     = prop"left"
  val letterSpacing            = prop"letter-spacing"
  val lineHeight               = prop"line-height"
  val listStyle                = prop"list-style"
  val margin                   = prop"margin"
  val marginBlock              = prop"margin-block"
  val marginBlockEnd           = prop"margin-block-end"
  val marginBlockStart         = prop"margin-block-start"
  val marginBottom             = prop"margin-bottom"
  val marginInline             = prop"margin-inline"
  val marginInlineEnd          = prop"margin-inline-end"
  val marginInlineStart        = prop"margin-inline-start"
  val marginLeft               = prop"margin-left"
  val marginRight              = prop"margin-right"
  val marginTop                = prop"margin-top"
  val maxHeight                = prop"max-height"
  val maxWidth                 = prop"max-width"
  val minHeight                = prop"min-height"
  val minWidth                 = prop"min-width"
  val opacity                  = prop"opacity"
  val outline                  = prop"outline"
  val outlineColor             = prop"outline-color"
  val outlineStyle             = prop"outline-style"
  val outlineWidth             = prop"outline-width"
  val overflow                 = prop"overflow"
  val padding                  = prop"padding"
  val paddingBlock             = prop"padding-block"
  val paddingBlockEnd          = prop"padding-block-end"
  val paddingBlockStart        = prop"padding-block-start"
  val paddingBottom            = prop"padding-bottom"
  val paddingInline            = prop"padding-inline"
  val paddingInlineEnd         = prop"padding-inline-end"
  val paddingInlineStart       = prop"padding-inline-start"
  val paddingLeft              = prop"padding-left"
  val paddingRight             = prop"padding-right"
  val paddingTop               = prop"padding-top"
  val position                 = prop"position"
  val right                    = prop"right"
  val rowGap                   = prop"row-gap"
  val textAlign                = prop"text-align"
  val textDecoration           = prop"text-decoration"
  val textDecorationThickness  = prop"text-decoration-thickness"
  val textOverflow             = prop"text-overflow"
  val textTransform            = prop"text-transform"
  val textUnderlineOffset      = prop"text-underline-offset"
  val top                      = prop"top"
  val transform                = prop"transform"
  val transition               = prop"transition"
  val transitionDuration       = prop"transition-duration"
  val transitionProperty       = CssMetaProp("transition-property")
  val transitionTimingFunction = prop"transition-timing-function"
  val verticalAlign            = prop"vertical-align"
  val visibility               = prop"visibility"
  val whiteSpace               = prop"white-space"
  val width                    = prop"width"
  val zIndex                   = prop"z-index"

  object webkit {

    val appearance = prop"-webkit-appearance"
  }

  @deprecated("To be removed after IE11 support is removed", "v1.8.7")
  object ms {
    val gridArea       = prop"-ms-grid-area"
    val gridColumn     = prop"-ms-grid-column"
    val gridColumnSpan = prop"-ms-grid-column-span"
    val gridColumns    = prop"-ms-grid-columns"
    val gridRow        = prop"-ms-grid-row"
    val gridRowSpan    = prop"-ms-grid-row-span"
    val gridRows       = prop"-ms-grid-rows"
  }

}