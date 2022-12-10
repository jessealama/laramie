#lang typed/racket/base/optional

(provide characters-by-name
         characters-index
         length-of-longest-character-ref)

(require (file "types.rkt"))

(: characters-by-name (Immutable-HashTable String named-character-ref))
(define characters-by-name (hash
  "AElig"
  (named-character-ref "AElig" '(#\Æ) "Æ")
  "AElig;"
  (named-character-ref "AElig;" '(#\Æ) "Æ")
  "AMP"
  (named-character-ref "AMP" '(#\&) "&")
  "AMP;"
  (named-character-ref "AMP;" '(#\&) "&")
  "Aacute"
  (named-character-ref "Aacute" '(#\Á) "Á")
  "Aacute;"
  (named-character-ref "Aacute;" '(#\Á) "Á")
  "Abreve;"
  (named-character-ref "Abreve;" '(#\Ă) "Ă")
  "Acirc"
  (named-character-ref "Acirc" '(#\Â) "Â")
  "Acirc;"
  (named-character-ref "Acirc;" '(#\Â) "Â")
  "Acy;"
  (named-character-ref "Acy;" '(#\А) "А")
  "Afr;"
  (named-character-ref "Afr;" '(#\𝔄) "𝔄")
  "Agrave"
  (named-character-ref "Agrave" '(#\À) "À")
  "Agrave;"
  (named-character-ref "Agrave;" '(#\À) "À")
  "Alpha;"
  (named-character-ref "Alpha;" '(#\Α) "Α")
  "Amacr;"
  (named-character-ref "Amacr;" '(#\Ā) "Ā")
  "And;"
  (named-character-ref "And;" '(#\⩓) "⩓")
  "Aogon;"
  (named-character-ref "Aogon;" '(#\Ą) "Ą")
  "Aopf;"
  (named-character-ref "Aopf;" '(#\𝔸) "𝔸")
  "ApplyFunction;"
  (named-character-ref "ApplyFunction;" '(#\u2061) "\u2061")
  "Aring"
  (named-character-ref "Aring" '(#\Å) "Å")
  "Aring;"
  (named-character-ref "Aring;" '(#\Å) "Å")
  "Ascr;"
  (named-character-ref "Ascr;" '(#\𝒜) "𝒜")
  "Assign;"
  (named-character-ref "Assign;" '(#\≔) "≔")
  "Atilde"
  (named-character-ref "Atilde" '(#\Ã) "Ã")
  "Atilde;"
  (named-character-ref "Atilde;" '(#\Ã) "Ã")
  "Auml"
  (named-character-ref "Auml" '(#\Ä) "Ä")
  "Auml;"
  (named-character-ref "Auml;" '(#\Ä) "Ä")
  "Backslash;"
  (named-character-ref "Backslash;" '(#\∖) "∖")
  "Barv;"
  (named-character-ref "Barv;" '(#\⫧) "⫧")
  "Barwed;"
  (named-character-ref "Barwed;" '(#\⌆) "⌆")
  "Bcy;"
  (named-character-ref "Bcy;" '(#\Б) "Б")
  "Because;"
  (named-character-ref "Because;" '(#\∵) "∵")
  "Bernoullis;"
  (named-character-ref "Bernoullis;" '(#\ℬ) "ℬ")
  "Beta;"
  (named-character-ref "Beta;" '(#\Β) "Β")
  "Bfr;"
  (named-character-ref "Bfr;" '(#\𝔅) "𝔅")
  "Bopf;"
  (named-character-ref "Bopf;" '(#\𝔹) "𝔹")
  "Breve;"
  (named-character-ref "Breve;" '(#\˘) "˘")
  "Bscr;"
  (named-character-ref "Bscr;" '(#\ℬ) "ℬ")
  "Bumpeq;"
  (named-character-ref "Bumpeq;" '(#\≎) "≎")
  "CHcy;"
  (named-character-ref "CHcy;" '(#\Ч) "Ч")
  "COPY"
  (named-character-ref "COPY" '(#\©) "©")
  "COPY;"
  (named-character-ref "COPY;" '(#\©) "©")
  "Cacute;"
  (named-character-ref "Cacute;" '(#\Ć) "Ć")
  "Cap;"
  (named-character-ref "Cap;" '(#\⋒) "⋒")
  "CapitalDifferentialD;"
  (named-character-ref "CapitalDifferentialD;" '(#\ⅅ) "ⅅ")
  "Cayleys;"
  (named-character-ref "Cayleys;" '(#\ℭ) "ℭ")
  "Ccaron;"
  (named-character-ref "Ccaron;" '(#\Č) "Č")
  "Ccedil"
  (named-character-ref "Ccedil" '(#\Ç) "Ç")
  "Ccedil;"
  (named-character-ref "Ccedil;" '(#\Ç) "Ç")
  "Ccirc;"
  (named-character-ref "Ccirc;" '(#\Ĉ) "Ĉ")
  "Cconint;"
  (named-character-ref "Cconint;" '(#\∰) "∰")
  "Cdot;"
  (named-character-ref "Cdot;" '(#\Ċ) "Ċ")
  "Cedilla;"
  (named-character-ref "Cedilla;" '(#\¸) "¸")
  "CenterDot;"
  (named-character-ref "CenterDot;" '(#\·) "·")
  "Cfr;"
  (named-character-ref "Cfr;" '(#\ℭ) "ℭ")
  "Chi;"
  (named-character-ref "Chi;" '(#\Χ) "Χ")
  "CircleDot;"
  (named-character-ref "CircleDot;" '(#\⊙) "⊙")
  "CircleMinus;"
  (named-character-ref "CircleMinus;" '(#\⊖) "⊖")
  "CirclePlus;"
  (named-character-ref "CirclePlus;" '(#\⊕) "⊕")
  "CircleTimes;"
  (named-character-ref "CircleTimes;" '(#\⊗) "⊗")
  "ClockwiseContourIntegral;"
  (named-character-ref "ClockwiseContourIntegral;" '(#\∲) "∲")
  "CloseCurlyDoubleQuote;"
  (named-character-ref "CloseCurlyDoubleQuote;" '(#\”) "”")
  "CloseCurlyQuote;"
  (named-character-ref "CloseCurlyQuote;" '(#\’) "’")
  "Colon;"
  (named-character-ref "Colon;" '(#\∷) "∷")
  "Colone;"
  (named-character-ref "Colone;" '(#\⩴) "⩴")
  "Congruent;"
  (named-character-ref "Congruent;" '(#\≡) "≡")
  "Conint;"
  (named-character-ref "Conint;" '(#\∯) "∯")
  "ContourIntegral;"
  (named-character-ref "ContourIntegral;" '(#\∮) "∮")
  "Copf;"
  (named-character-ref "Copf;" '(#\ℂ) "ℂ")
  "Coproduct;"
  (named-character-ref "Coproduct;" '(#\∐) "∐")
  "CounterClockwiseContourIntegral;"
  (named-character-ref "CounterClockwiseContourIntegral;" '(#\∳) "∳")
  "Cross;"
  (named-character-ref "Cross;" '(#\⨯) "⨯")
  "Cscr;"
  (named-character-ref "Cscr;" '(#\𝒞) "𝒞")
  "Cup;"
  (named-character-ref "Cup;" '(#\⋓) "⋓")
  "CupCap;"
  (named-character-ref "CupCap;" '(#\≍) "≍")
  "DD;"
  (named-character-ref "DD;" '(#\ⅅ) "ⅅ")
  "DDotrahd;"
  (named-character-ref "DDotrahd;" '(#\⤑) "⤑")
  "DJcy;"
  (named-character-ref "DJcy;" '(#\Ђ) "Ђ")
  "DScy;"
  (named-character-ref "DScy;" '(#\Ѕ) "Ѕ")
  "DZcy;"
  (named-character-ref "DZcy;" '(#\Џ) "Џ")
  "Dagger;"
  (named-character-ref "Dagger;" '(#\‡) "‡")
  "Darr;"
  (named-character-ref "Darr;" '(#\↡) "↡")
  "Dashv;"
  (named-character-ref "Dashv;" '(#\⫤) "⫤")
  "Dcaron;"
  (named-character-ref "Dcaron;" '(#\Ď) "Ď")
  "Dcy;"
  (named-character-ref "Dcy;" '(#\Д) "Д")
  "Del;"
  (named-character-ref "Del;" '(#\∇) "∇")
  "Delta;"
  (named-character-ref "Delta;" '(#\Δ) "Δ")
  "Dfr;"
  (named-character-ref "Dfr;" '(#\𝔇) "𝔇")
  "DiacriticalAcute;"
  (named-character-ref "DiacriticalAcute;" '(#\´) "´")
  "DiacriticalDot;"
  (named-character-ref "DiacriticalDot;" '(#\˙) "˙")
  "DiacriticalDoubleAcute;"
  (named-character-ref "DiacriticalDoubleAcute;" '(#\˝) "˝")
  "DiacriticalGrave;"
  (named-character-ref "DiacriticalGrave;" '(#\`) "`")
  "DiacriticalTilde;"
  (named-character-ref "DiacriticalTilde;" '(#\˜) "˜")
  "Diamond;"
  (named-character-ref "Diamond;" '(#\⋄) "⋄")
  "DifferentialD;"
  (named-character-ref "DifferentialD;" '(#\ⅆ) "ⅆ")
  "Dopf;"
  (named-character-ref "Dopf;" '(#\𝔻) "𝔻")
  "Dot;"
  (named-character-ref "Dot;" '(#\¨) "¨")
  "DotDot;"
  (named-character-ref "DotDot;" '(#\⃜) "◌⃜")
  "DotEqual;"
  (named-character-ref "DotEqual;" '(#\≐) "≐")
  "DoubleContourIntegral;"
  (named-character-ref "DoubleContourIntegral;" '(#\∯) "∯")
  "DoubleDot;"
  (named-character-ref "DoubleDot;" '(#\¨) "¨")
  "DoubleDownArrow;"
  (named-character-ref "DoubleDownArrow;" '(#\⇓) "⇓")
  "DoubleLeftArrow;"
  (named-character-ref "DoubleLeftArrow;" '(#\⇐) "⇐")
  "DoubleLeftRightArrow;"
  (named-character-ref "DoubleLeftRightArrow;" '(#\⇔) "⇔")
  "DoubleLeftTee;"
  (named-character-ref "DoubleLeftTee;" '(#\⫤) "⫤")
  "DoubleLongLeftArrow;"
  (named-character-ref "DoubleLongLeftArrow;" '(#\⟸) "⟸")
  "DoubleLongLeftRightArrow;"
  (named-character-ref "DoubleLongLeftRightArrow;" '(#\⟺) "⟺")
  "DoubleLongRightArrow;"
  (named-character-ref "DoubleLongRightArrow;" '(#\⟹) "⟹")
  "DoubleRightArrow;"
  (named-character-ref "DoubleRightArrow;" '(#\⇒) "⇒")
  "DoubleRightTee;"
  (named-character-ref "DoubleRightTee;" '(#\⊨) "⊨")
  "DoubleUpArrow;"
  (named-character-ref "DoubleUpArrow;" '(#\⇑) "⇑")
  "DoubleUpDownArrow;"
  (named-character-ref "DoubleUpDownArrow;" '(#\⇕) "⇕")
  "DoubleVerticalBar;"
  (named-character-ref "DoubleVerticalBar;" '(#\∥) "∥")
  "DownArrow;"
  (named-character-ref "DownArrow;" '(#\↓) "↓")
  "DownArrowBar;"
  (named-character-ref "DownArrowBar;" '(#\⤓) "⤓")
  "DownArrowUpArrow;"
  (named-character-ref "DownArrowUpArrow;" '(#\⇵) "⇵")
  "DownBreve;"
  (named-character-ref "DownBreve;" '(#\̑) "◌̑")
  "DownLeftRightVector;"
  (named-character-ref "DownLeftRightVector;" '(#\⥐) "⥐")
  "DownLeftTeeVector;"
  (named-character-ref "DownLeftTeeVector;" '(#\⥞) "⥞")
  "DownLeftVector;"
  (named-character-ref "DownLeftVector;" '(#\↽) "↽")
  "DownLeftVectorBar;"
  (named-character-ref "DownLeftVectorBar;" '(#\⥖) "⥖")
  "DownRightTeeVector;"
  (named-character-ref "DownRightTeeVector;" '(#\⥟) "⥟")
  "DownRightVector;"
  (named-character-ref "DownRightVector;" '(#\⇁) "⇁")
  "DownRightVectorBar;"
  (named-character-ref "DownRightVectorBar;" '(#\⥗) "⥗")
  "DownTee;"
  (named-character-ref "DownTee;" '(#\⊤) "⊤")
  "DownTeeArrow;"
  (named-character-ref "DownTeeArrow;" '(#\↧) "↧")
  "Downarrow;"
  (named-character-ref "Downarrow;" '(#\⇓) "⇓")
  "Dscr;"
  (named-character-ref "Dscr;" '(#\𝒟) "𝒟")
  "Dstrok;"
  (named-character-ref "Dstrok;" '(#\Đ) "Đ")
  "ENG;"
  (named-character-ref "ENG;" '(#\Ŋ) "Ŋ")
  "ETH"
  (named-character-ref "ETH" '(#\Ð) "Ð")
  "ETH;"
  (named-character-ref "ETH;" '(#\Ð) "Ð")
  "Eacute"
  (named-character-ref "Eacute" '(#\É) "É")
  "Eacute;"
  (named-character-ref "Eacute;" '(#\É) "É")
  "Ecaron;"
  (named-character-ref "Ecaron;" '(#\Ě) "Ě")
  "Ecirc"
  (named-character-ref "Ecirc" '(#\Ê) "Ê")
  "Ecirc;"
  (named-character-ref "Ecirc;" '(#\Ê) "Ê")
  "Ecy;"
  (named-character-ref "Ecy;" '(#\Э) "Э")
  "Edot;"
  (named-character-ref "Edot;" '(#\Ė) "Ė")
  "Efr;"
  (named-character-ref "Efr;" '(#\𝔈) "𝔈")
  "Egrave"
  (named-character-ref "Egrave" '(#\È) "È")
  "Egrave;"
  (named-character-ref "Egrave;" '(#\È) "È")
  "Element;"
  (named-character-ref "Element;" '(#\∈) "∈")
  "Emacr;"
  (named-character-ref "Emacr;" '(#\Ē) "Ē")
  "EmptySmallSquare;"
  (named-character-ref "EmptySmallSquare;" '(#\◻) "◻")
  "EmptyVerySmallSquare;"
  (named-character-ref "EmptyVerySmallSquare;" '(#\▫) "▫")
  "Eogon;"
  (named-character-ref "Eogon;" '(#\Ę) "Ę")
  "Eopf;"
  (named-character-ref "Eopf;" '(#\𝔼) "𝔼")
  "Epsilon;"
  (named-character-ref "Epsilon;" '(#\Ε) "Ε")
  "Equal;"
  (named-character-ref "Equal;" '(#\⩵) "⩵")
  "EqualTilde;"
  (named-character-ref "EqualTilde;" '(#\≂) "≂")
  "Equilibrium;"
  (named-character-ref "Equilibrium;" '(#\⇌) "⇌")
  "Escr;"
  (named-character-ref "Escr;" '(#\ℰ) "ℰ")
  "Esim;"
  (named-character-ref "Esim;" '(#\⩳) "⩳")
  "Eta;"
  (named-character-ref "Eta;" '(#\Η) "Η")
  "Euml"
  (named-character-ref "Euml" '(#\Ë) "Ë")
  "Euml;"
  (named-character-ref "Euml;" '(#\Ë) "Ë")
  "Exists;"
  (named-character-ref "Exists;" '(#\∃) "∃")
  "ExponentialE;"
  (named-character-ref "ExponentialE;" '(#\ⅇ) "ⅇ")
  "Fcy;"
  (named-character-ref "Fcy;" '(#\Ф) "Ф")
  "Ffr;"
  (named-character-ref "Ffr;" '(#\𝔉) "𝔉")
  "FilledSmallSquare;"
  (named-character-ref "FilledSmallSquare;" '(#\◼) "◼")
  "FilledVerySmallSquare;"
  (named-character-ref "FilledVerySmallSquare;" '(#\▪) "▪")
  "Fopf;"
  (named-character-ref "Fopf;" '(#\𝔽) "𝔽")
  "ForAll;"
  (named-character-ref "ForAll;" '(#\∀) "∀")
  "Fouriertrf;"
  (named-character-ref "Fouriertrf;" '(#\ℱ) "ℱ")
  "Fscr;"
  (named-character-ref "Fscr;" '(#\ℱ) "ℱ")
  "GJcy;"
  (named-character-ref "GJcy;" '(#\Ѓ) "Ѓ")
  "GT"
  (named-character-ref "GT" '(#\>) "&gt;")
  "GT;"
  (named-character-ref "GT;" '(#\>) "&gt;")
  "Gamma;"
  (named-character-ref "Gamma;" '(#\Γ) "Γ")
  "Gammad;"
  (named-character-ref "Gammad;" '(#\Ϝ) "Ϝ")
  "Gbreve;"
  (named-character-ref "Gbreve;" '(#\Ğ) "Ğ")
  "Gcedil;"
  (named-character-ref "Gcedil;" '(#\Ģ) "Ģ")
  "Gcirc;"
  (named-character-ref "Gcirc;" '(#\Ĝ) "Ĝ")
  "Gcy;"
  (named-character-ref "Gcy;" '(#\Г) "Г")
  "Gdot;"
  (named-character-ref "Gdot;" '(#\Ġ) "Ġ")
  "Gfr;"
  (named-character-ref "Gfr;" '(#\𝔊) "𝔊")
  "Gg;"
  (named-character-ref "Gg;" '(#\⋙) "⋙")
  "Gopf;"
  (named-character-ref "Gopf;" '(#\𝔾) "𝔾")
  "GreaterEqual;"
  (named-character-ref "GreaterEqual;" '(#\≥) "≥")
  "GreaterEqualLess;"
  (named-character-ref "GreaterEqualLess;" '(#\⋛) "⋛")
  "GreaterFullEqual;"
  (named-character-ref "GreaterFullEqual;" '(#\≧) "≧")
  "GreaterGreater;"
  (named-character-ref "GreaterGreater;" '(#\⪢) "⪢")
  "GreaterLess;"
  (named-character-ref "GreaterLess;" '(#\≷) "≷")
  "GreaterSlantEqual;"
  (named-character-ref "GreaterSlantEqual;" '(#\⩾) "⩾")
  "GreaterTilde;"
  (named-character-ref "GreaterTilde;" '(#\≳) "≳")
  "Gscr;"
  (named-character-ref "Gscr;" '(#\𝒢) "𝒢")
  "Gt;"
  (named-character-ref "Gt;" '(#\≫) "≫")
  "HARDcy;"
  (named-character-ref "HARDcy;" '(#\Ъ) "Ъ")
  "Hacek;"
  (named-character-ref "Hacek;" '(#\ˇ) "ˇ")
  "Hat;"
  (named-character-ref "Hat;" '(#\^) "^")
  "Hcirc;"
  (named-character-ref "Hcirc;" '(#\Ĥ) "Ĥ")
  "Hfr;"
  (named-character-ref "Hfr;" '(#\ℌ) "ℌ")
  "HilbertSpace;"
  (named-character-ref "HilbertSpace;" '(#\ℋ) "ℋ")
  "Hopf;"
  (named-character-ref "Hopf;" '(#\ℍ) "ℍ")
  "HorizontalLine;"
  (named-character-ref "HorizontalLine;" '(#\─) "─")
  "Hscr;"
  (named-character-ref "Hscr;" '(#\ℋ) "ℋ")
  "Hstrok;"
  (named-character-ref "Hstrok;" '(#\Ħ) "Ħ")
  "HumpDownHump;"
  (named-character-ref "HumpDownHump;" '(#\≎) "≎")
  "HumpEqual;"
  (named-character-ref "HumpEqual;" '(#\≏) "≏")
  "IEcy;"
  (named-character-ref "IEcy;" '(#\Е) "Е")
  "IJlig;"
  (named-character-ref "IJlig;" '(#\Ĳ) "Ĳ")
  "IOcy;"
  (named-character-ref "IOcy;" '(#\Ё) "Ё")
  "Iacute"
  (named-character-ref "Iacute" '(#\Í) "Í")
  "Iacute;"
  (named-character-ref "Iacute;" '(#\Í) "Í")
  "Icirc"
  (named-character-ref "Icirc" '(#\Î) "Î")
  "Icirc;"
  (named-character-ref "Icirc;" '(#\Î) "Î")
  "Icy;"
  (named-character-ref "Icy;" '(#\И) "И")
  "Idot;"
  (named-character-ref "Idot;" '(#\İ) "İ")
  "Ifr;"
  (named-character-ref "Ifr;" '(#\ℑ) "ℑ")
  "Igrave"
  (named-character-ref "Igrave" '(#\Ì) "Ì")
  "Igrave;"
  (named-character-ref "Igrave;" '(#\Ì) "Ì")
  "Im;"
  (named-character-ref "Im;" '(#\ℑ) "ℑ")
  "Imacr;"
  (named-character-ref "Imacr;" '(#\Ī) "Ī")
  "ImaginaryI;"
  (named-character-ref "ImaginaryI;" '(#\ⅈ) "ⅈ")
  "Implies;"
  (named-character-ref "Implies;" '(#\⇒) "⇒")
  "Int;"
  (named-character-ref "Int;" '(#\∬) "∬")
  "Integral;"
  (named-character-ref "Integral;" '(#\∫) "∫")
  "Intersection;"
  (named-character-ref "Intersection;" '(#\⋂) "⋂")
  "InvisibleComma;"
  (named-character-ref "InvisibleComma;" '(#\u2063) "\u2063")
  "InvisibleTimes;"
  (named-character-ref "InvisibleTimes;" '(#\u2062) "\u2062")
  "Iogon;"
  (named-character-ref "Iogon;" '(#\Į) "Į")
  "Iopf;"
  (named-character-ref "Iopf;" '(#\𝕀) "𝕀")
  "Iota;"
  (named-character-ref "Iota;" '(#\Ι) "Ι")
  "Iscr;"
  (named-character-ref "Iscr;" '(#\ℐ) "ℐ")
  "Itilde;"
  (named-character-ref "Itilde;" '(#\Ĩ) "Ĩ")
  "Iukcy;"
  (named-character-ref "Iukcy;" '(#\І) "І")
  "Iuml"
  (named-character-ref "Iuml" '(#\Ï) "Ï")
  "Iuml;"
  (named-character-ref "Iuml;" '(#\Ï) "Ï")
  "Jcirc;"
  (named-character-ref "Jcirc;" '(#\Ĵ) "Ĵ")
  "Jcy;"
  (named-character-ref "Jcy;" '(#\Й) "Й")
  "Jfr;"
  (named-character-ref "Jfr;" '(#\𝔍) "𝔍")
  "Jopf;"
  (named-character-ref "Jopf;" '(#\𝕁) "𝕁")
  "Jscr;"
  (named-character-ref "Jscr;" '(#\𝒥) "𝒥")
  "Jsercy;"
  (named-character-ref "Jsercy;" '(#\Ј) "Ј")
  "Jukcy;"
  (named-character-ref "Jukcy;" '(#\Є) "Є")
  "KHcy;"
  (named-character-ref "KHcy;" '(#\Х) "Х")
  "KJcy;"
  (named-character-ref "KJcy;" '(#\Ќ) "Ќ")
  "Kappa;"
  (named-character-ref "Kappa;" '(#\Κ) "Κ")
  "Kcedil;"
  (named-character-ref "Kcedil;" '(#\Ķ) "Ķ")
  "Kcy;"
  (named-character-ref "Kcy;" '(#\К) "К")
  "Kfr;"
  (named-character-ref "Kfr;" '(#\𝔎) "𝔎")
  "Kopf;"
  (named-character-ref "Kopf;" '(#\𝕂) "𝕂")
  "Kscr;"
  (named-character-ref "Kscr;" '(#\𝒦) "𝒦")
  "LJcy;"
  (named-character-ref "LJcy;" '(#\Љ) "Љ")
  "LT"
  (named-character-ref "LT" '(#\<) "&lt;")
  "LT;"
  (named-character-ref "LT;" '(#\<) "&lt;")
  "Lacute;"
  (named-character-ref "Lacute;" '(#\Ĺ) "Ĺ")
  "Lambda;"
  (named-character-ref "Lambda;" '(#\Λ) "Λ")
  "Lang;"
  (named-character-ref "Lang;" '(#\⟪) "⟪")
  "Laplacetrf;"
  (named-character-ref "Laplacetrf;" '(#\ℒ) "ℒ")
  "Larr;"
  (named-character-ref "Larr;" '(#\↞) "↞")
  "Lcaron;"
  (named-character-ref "Lcaron;" '(#\Ľ) "Ľ")
  "Lcedil;"
  (named-character-ref "Lcedil;" '(#\Ļ) "Ļ")
  "Lcy;"
  (named-character-ref "Lcy;" '(#\Л) "Л")
  "LeftAngleBracket;"
  (named-character-ref "LeftAngleBracket;" '(#\⟨) "⟨")
  "LeftArrow;"
  (named-character-ref "LeftArrow;" '(#\←) "←")
  "LeftArrowBar;"
  (named-character-ref "LeftArrowBar;" '(#\⇤) "⇤")
  "LeftArrowRightArrow;"
  (named-character-ref "LeftArrowRightArrow;" '(#\⇆) "⇆")
  "LeftCeiling;"
  (named-character-ref "LeftCeiling;" '(#\⌈) "⌈")
  "LeftDoubleBracket;"
  (named-character-ref "LeftDoubleBracket;" '(#\⟦) "⟦")
  "LeftDownTeeVector;"
  (named-character-ref "LeftDownTeeVector;" '(#\⥡) "⥡")
  "LeftDownVector;"
  (named-character-ref "LeftDownVector;" '(#\⇃) "⇃")
  "LeftDownVectorBar;"
  (named-character-ref "LeftDownVectorBar;" '(#\⥙) "⥙")
  "LeftFloor;"
  (named-character-ref "LeftFloor;" '(#\⌊) "⌊")
  "LeftRightArrow;"
  (named-character-ref "LeftRightArrow;" '(#\↔) "↔")
  "LeftRightVector;"
  (named-character-ref "LeftRightVector;" '(#\⥎) "⥎")
  "LeftTee;"
  (named-character-ref "LeftTee;" '(#\⊣) "⊣")
  "LeftTeeArrow;"
  (named-character-ref "LeftTeeArrow;" '(#\↤) "↤")
  "LeftTeeVector;"
  (named-character-ref "LeftTeeVector;" '(#\⥚) "⥚")
  "LeftTriangle;"
  (named-character-ref "LeftTriangle;" '(#\⊲) "⊲")
  "LeftTriangleBar;"
  (named-character-ref "LeftTriangleBar;" '(#\⧏) "⧏")
  "LeftTriangleEqual;"
  (named-character-ref "LeftTriangleEqual;" '(#\⊴) "⊴")
  "LeftUpDownVector;"
  (named-character-ref "LeftUpDownVector;" '(#\⥑) "⥑")
  "LeftUpTeeVector;"
  (named-character-ref "LeftUpTeeVector;" '(#\⥠) "⥠")
  "LeftUpVector;"
  (named-character-ref "LeftUpVector;" '(#\↿) "↿")
  "LeftUpVectorBar;"
  (named-character-ref "LeftUpVectorBar;" '(#\⥘) "⥘")
  "LeftVector;"
  (named-character-ref "LeftVector;" '(#\↼) "↼")
  "LeftVectorBar;"
  (named-character-ref "LeftVectorBar;" '(#\⥒) "⥒")
  "Leftarrow;"
  (named-character-ref "Leftarrow;" '(#\⇐) "⇐")
  "Leftrightarrow;"
  (named-character-ref "Leftrightarrow;" '(#\⇔) "⇔")
  "LessEqualGreater;"
  (named-character-ref "LessEqualGreater;" '(#\⋚) "⋚")
  "LessFullEqual;"
  (named-character-ref "LessFullEqual;" '(#\≦) "≦")
  "LessGreater;"
  (named-character-ref "LessGreater;" '(#\≶) "≶")
  "LessLess;"
  (named-character-ref "LessLess;" '(#\⪡) "⪡")
  "LessSlantEqual;"
  (named-character-ref "LessSlantEqual;" '(#\⩽) "⩽")
  "LessTilde;"
  (named-character-ref "LessTilde;" '(#\≲) "≲")
  "Lfr;"
  (named-character-ref "Lfr;" '(#\𝔏) "𝔏")
  "Ll;"
  (named-character-ref "Ll;" '(#\⋘) "⋘")
  "Lleftarrow;"
  (named-character-ref "Lleftarrow;" '(#\⇚) "⇚")
  "Lmidot;"
  (named-character-ref "Lmidot;" '(#\Ŀ) "Ŀ")
  "LongLeftArrow;"
  (named-character-ref "LongLeftArrow;" '(#\⟵) "⟵")
  "LongLeftRightArrow;"
  (named-character-ref "LongLeftRightArrow;" '(#\⟷) "⟷")
  "LongRightArrow;"
  (named-character-ref "LongRightArrow;" '(#\⟶) "⟶")
  "Longleftarrow;"
  (named-character-ref "Longleftarrow;" '(#\⟸) "⟸")
  "Longleftrightarrow;"
  (named-character-ref "Longleftrightarrow;" '(#\⟺) "⟺")
  "Longrightarrow;"
  (named-character-ref "Longrightarrow;" '(#\⟹) "⟹")
  "Lopf;"
  (named-character-ref "Lopf;" '(#\𝕃) "𝕃")
  "LowerLeftArrow;"
  (named-character-ref "LowerLeftArrow;" '(#\↙) "↙")
  "LowerRightArrow;"
  (named-character-ref "LowerRightArrow;" '(#\↘) "↘")
  "Lscr;"
  (named-character-ref "Lscr;" '(#\ℒ) "ℒ")
  "Lsh;"
  (named-character-ref "Lsh;" '(#\↰) "↰")
  "Lstrok;"
  (named-character-ref "Lstrok;" '(#\Ł) "Ł")
  "Lt;"
  (named-character-ref "Lt;" '(#\≪) "≪")
  "Map;"
  (named-character-ref "Map;" '(#\⤅) "⤅")
  "Mcy;"
  (named-character-ref "Mcy;" '(#\М) "М")
  "MediumSpace;"
  (named-character-ref "MediumSpace;" '(#\u205F) " ")
  "Mellintrf;"
  (named-character-ref "Mellintrf;" '(#\ℳ) "ℳ")
  "Mfr;"
  (named-character-ref "Mfr;" '(#\𝔐) "𝔐")
  "MinusPlus;"
  (named-character-ref "MinusPlus;" '(#\∓) "∓")
  "Mopf;"
  (named-character-ref "Mopf;" '(#\𝕄) "𝕄")
  "Mscr;"
  (named-character-ref "Mscr;" '(#\ℳ) "ℳ")
  "Mu;"
  (named-character-ref "Mu;" '(#\Μ) "Μ")
  "NJcy;"
  (named-character-ref "NJcy;" '(#\Њ) "Њ")
  "Nacute;"
  (named-character-ref "Nacute;" '(#\Ń) "Ń")
  "Ncaron;"
  (named-character-ref "Ncaron;" '(#\Ň) "Ň")
  "Ncedil;"
  (named-character-ref "Ncedil;" '(#\Ņ) "Ņ")
  "Ncy;"
  (named-character-ref "Ncy;" '(#\Н) "Н")
  "NegativeMediumSpace;"
  (named-character-ref "NegativeMediumSpace;" '(#\u200B) "\u200B")
  "NegativeThickSpace;"
  (named-character-ref "NegativeThickSpace;" '(#\u200B) "\u200B")
  "NegativeThinSpace;"
  (named-character-ref "NegativeThinSpace;" '(#\u200B) "\u200B")
  "NegativeVeryThinSpace;"
  (named-character-ref "NegativeVeryThinSpace;" '(#\u200B) "\u200B")
  "NestedGreaterGreater;"
  (named-character-ref "NestedGreaterGreater;" '(#\≫) "≫")
  "NestedLessLess;"
  (named-character-ref "NestedLessLess;" '(#\≪) "≪")
  "NewLine;"
  (named-character-ref "NewLine;" '(#\newline) "␊")
  "Nfr;"
  (named-character-ref "Nfr;" '(#\𝔑) "𝔑")
  "NoBreak;"
  (named-character-ref "NoBreak;" '(#\u2060) "\u2060")
  "NonBreakingSpace;"
  (named-character-ref "NonBreakingSpace;" '(#\u00A0) "&nbsp;")
  "Nopf;"
  (named-character-ref "Nopf;" '(#\ℕ) "ℕ")
  "Not;"
  (named-character-ref "Not;" '(#\⫬) "⫬")
  "NotCongruent;"
  (named-character-ref "NotCongruent;" '(#\≢) "≢")
  "NotCupCap;"
  (named-character-ref "NotCupCap;" '(#\≭) "≭")
  "NotDoubleVerticalBar;"
  (named-character-ref "NotDoubleVerticalBar;" '(#\∦) "∦")
  "NotElement;"
  (named-character-ref "NotElement;" '(#\∉) "∉")
  "NotEqual;"
  (named-character-ref "NotEqual;" '(#\≠) "≠")
  "NotEqualTilde;"
  (named-character-ref "NotEqualTilde;" '(#\≂ #\̸) "≂̸")
  "NotExists;"
  (named-character-ref "NotExists;" '(#\∄) "∄")
  "NotGreater;"
  (named-character-ref "NotGreater;" '(#\≯) "≯")
  "NotGreaterEqual;"
  (named-character-ref "NotGreaterEqual;" '(#\≱) "≱")
  "NotGreaterFullEqual;"
  (named-character-ref "NotGreaterFullEqual;" '(#\≧ #\̸) "≧̸")
  "NotGreaterGreater;"
  (named-character-ref "NotGreaterGreater;" '(#\≫ #\̸) "≫̸")
  "NotGreaterLess;"
  (named-character-ref "NotGreaterLess;" '(#\≹) "≹")
  "NotGreaterSlantEqual;"
  (named-character-ref "NotGreaterSlantEqual;" '(#\⩾ #\̸) "⩾̸")
  "NotGreaterTilde;"
  (named-character-ref "NotGreaterTilde;" '(#\≵) "≵")
  "NotHumpDownHump;"
  (named-character-ref "NotHumpDownHump;" '(#\≎ #\̸) "≎̸")
  "NotHumpEqual;"
  (named-character-ref "NotHumpEqual;" '(#\≏ #\̸) "≏̸")
  "NotLeftTriangle;"
  (named-character-ref "NotLeftTriangle;" '(#\⋪) "⋪")
  "NotLeftTriangleBar;"
  (named-character-ref "NotLeftTriangleBar;" '(#\⧏ #\̸) "⧏̸")
  "NotLeftTriangleEqual;"
  (named-character-ref "NotLeftTriangleEqual;" '(#\⋬) "⋬")
  "NotLess;"
  (named-character-ref "NotLess;" '(#\≮) "≮")
  "NotLessEqual;"
  (named-character-ref "NotLessEqual;" '(#\≰) "≰")
  "NotLessGreater;"
  (named-character-ref "NotLessGreater;" '(#\≸) "≸")
  "NotLessLess;"
  (named-character-ref "NotLessLess;" '(#\≪ #\̸) "≪̸")
  "NotLessSlantEqual;"
  (named-character-ref "NotLessSlantEqual;" '(#\⩽ #\̸) "⩽̸")
  "NotLessTilde;"
  (named-character-ref "NotLessTilde;" '(#\≴) "≴")
  "NotNestedGreaterGreater;"
  (named-character-ref "NotNestedGreaterGreater;" '(#\⪢ #\̸) "⪢̸")
  "NotNestedLessLess;"
  (named-character-ref "NotNestedLessLess;" '(#\⪡ #\̸) "⪡̸")
  "NotPrecedes;"
  (named-character-ref "NotPrecedes;" '(#\⊀) "⊀")
  "NotPrecedesEqual;"
  (named-character-ref "NotPrecedesEqual;" '(#\⪯ #\̸) "⪯̸")
  "NotPrecedesSlantEqual;"
  (named-character-ref "NotPrecedesSlantEqual;" '(#\⋠) "⋠")
  "NotReverseElement;"
  (named-character-ref "NotReverseElement;" '(#\∌) "∌")
  "NotRightTriangle;"
  (named-character-ref "NotRightTriangle;" '(#\⋫) "⋫")
  "NotRightTriangleBar;"
  (named-character-ref "NotRightTriangleBar;" '(#\⧐ #\̸) "⧐̸")
  "NotRightTriangleEqual;"
  (named-character-ref "NotRightTriangleEqual;" '(#\⋭) "⋭")
  "NotSquareSubset;"
  (named-character-ref "NotSquareSubset;" '(#\⊏ #\̸) "⊏̸")
  "NotSquareSubsetEqual;"
  (named-character-ref "NotSquareSubsetEqual;" '(#\⋢) "⋢")
  "NotSquareSuperset;"
  (named-character-ref "NotSquareSuperset;" '(#\⊐ #\̸) "⊐̸")
  "NotSquareSupersetEqual;"
  (named-character-ref "NotSquareSupersetEqual;" '(#\⋣) "⋣")
  "NotSubset;"
  (named-character-ref "NotSubset;" '(#\⊂ #\⃒) "⊂⃒")
  "NotSubsetEqual;"
  (named-character-ref "NotSubsetEqual;" '(#\⊈) "⊈")
  "NotSucceeds;"
  (named-character-ref "NotSucceeds;" '(#\⊁) "⊁")
  "NotSucceedsEqual;"
  (named-character-ref "NotSucceedsEqual;" '(#\⪰ #\̸) "⪰̸")
  "NotSucceedsSlantEqual;"
  (named-character-ref "NotSucceedsSlantEqual;" '(#\⋡) "⋡")
  "NotSucceedsTilde;"
  (named-character-ref "NotSucceedsTilde;" '(#\≿ #\̸) "≿̸")
  "NotSuperset;"
  (named-character-ref "NotSuperset;" '(#\⊃ #\⃒) "⊃⃒")
  "NotSupersetEqual;"
  (named-character-ref "NotSupersetEqual;" '(#\⊉) "⊉")
  "NotTilde;"
  (named-character-ref "NotTilde;" '(#\≁) "≁")
  "NotTildeEqual;"
  (named-character-ref "NotTildeEqual;" '(#\≄) "≄")
  "NotTildeFullEqual;"
  (named-character-ref "NotTildeFullEqual;" '(#\≇) "≇")
  "NotTildeTilde;"
  (named-character-ref "NotTildeTilde;" '(#\≉) "≉")
  "NotVerticalBar;"
  (named-character-ref "NotVerticalBar;" '(#\∤) "∤")
  "Nscr;"
  (named-character-ref "Nscr;" '(#\𝒩) "𝒩")
  "Ntilde"
  (named-character-ref "Ntilde" '(#\Ñ) "Ñ")
  "Ntilde;"
  (named-character-ref "Ntilde;" '(#\Ñ) "Ñ")
  "Nu;"
  (named-character-ref "Nu;" '(#\Ν) "Ν")
  "OElig;"
  (named-character-ref "OElig;" '(#\Œ) "Œ")
  "Oacute"
  (named-character-ref "Oacute" '(#\Ó) "Ó")
  "Oacute;"
  (named-character-ref "Oacute;" '(#\Ó) "Ó")
  "Ocirc"
  (named-character-ref "Ocirc" '(#\Ô) "Ô")
  "Ocirc;"
  (named-character-ref "Ocirc;" '(#\Ô) "Ô")
  "Ocy;"
  (named-character-ref "Ocy;" '(#\О) "О")
  "Odblac;"
  (named-character-ref "Odblac;" '(#\Ő) "Ő")
  "Ofr;"
  (named-character-ref "Ofr;" '(#\𝔒) "𝔒")
  "Ograve"
  (named-character-ref "Ograve" '(#\Ò) "Ò")
  "Ograve;"
  (named-character-ref "Ograve;" '(#\Ò) "Ò")
  "Omacr;"
  (named-character-ref "Omacr;" '(#\Ō) "Ō")
  "Omega;"
  (named-character-ref "Omega;" '(#\Ω) "Ω")
  "Omicron;"
  (named-character-ref "Omicron;" '(#\Ο) "Ο")
  "Oopf;"
  (named-character-ref "Oopf;" '(#\𝕆) "𝕆")
  "OpenCurlyDoubleQuote;"
  (named-character-ref "OpenCurlyDoubleQuote;" '(#\“) "“")
  "OpenCurlyQuote;"
  (named-character-ref "OpenCurlyQuote;" '(#\‘) "‘")
  "Or;"
  (named-character-ref "Or;" '(#\⩔) "⩔")
  "Oscr;"
  (named-character-ref "Oscr;" '(#\𝒪) "𝒪")
  "Oslash"
  (named-character-ref "Oslash" '(#\Ø) "Ø")
  "Oslash;"
  (named-character-ref "Oslash;" '(#\Ø) "Ø")
  "Otilde"
  (named-character-ref "Otilde" '(#\Õ) "Õ")
  "Otilde;"
  (named-character-ref "Otilde;" '(#\Õ) "Õ")
  "Otimes;"
  (named-character-ref "Otimes;" '(#\⨷) "⨷")
  "Ouml"
  (named-character-ref "Ouml" '(#\Ö) "Ö")
  "Ouml;"
  (named-character-ref "Ouml;" '(#\Ö) "Ö")
  "OverBar;"
  (named-character-ref "OverBar;" '(#\‾) "‾")
  "OverBrace;"
  (named-character-ref "OverBrace;" '(#\⏞) "⏞")
  "OverBracket;"
  (named-character-ref "OverBracket;" '(#\⎴) "⎴")
  "OverParenthesis;"
  (named-character-ref "OverParenthesis;" '(#\⏜) "⏜")
  "PartialD;"
  (named-character-ref "PartialD;" '(#\∂) "∂")
  "Pcy;"
  (named-character-ref "Pcy;" '(#\П) "П")
  "Pfr;"
  (named-character-ref "Pfr;" '(#\𝔓) "𝔓")
  "Phi;"
  (named-character-ref "Phi;" '(#\Φ) "Φ")
  "Pi;"
  (named-character-ref "Pi;" '(#\Π) "Π")
  "PlusMinus;"
  (named-character-ref "PlusMinus;" '(#\±) "±")
  "Poincareplane;"
  (named-character-ref "Poincareplane;" '(#\ℌ) "ℌ")
  "Popf;"
  (named-character-ref "Popf;" '(#\ℙ) "ℙ")
  "Pr;"
  (named-character-ref "Pr;" '(#\⪻) "⪻")
  "Precedes;"
  (named-character-ref "Precedes;" '(#\≺) "≺")
  "PrecedesEqual;"
  (named-character-ref "PrecedesEqual;" '(#\⪯) "⪯")
  "PrecedesSlantEqual;"
  (named-character-ref "PrecedesSlantEqual;" '(#\≼) "≼")
  "PrecedesTilde;"
  (named-character-ref "PrecedesTilde;" '(#\≾) "≾")
  "Prime;"
  (named-character-ref "Prime;" '(#\″) "″")
  "Product;"
  (named-character-ref "Product;" '(#\∏) "∏")
  "Proportion;"
  (named-character-ref "Proportion;" '(#\∷) "∷")
  "Proportional;"
  (named-character-ref "Proportional;" '(#\∝) "∝")
  "Pscr;"
  (named-character-ref "Pscr;" '(#\𝒫) "𝒫")
  "Psi;"
  (named-character-ref "Psi;" '(#\Ψ) "Ψ")
  "QUOT"
  (named-character-ref "QUOT" '(#\") "\"")
  "QUOT;"
  (named-character-ref "QUOT;" '(#\") "\"")
  "Qfr;"
  (named-character-ref "Qfr;" '(#\𝔔) "𝔔")
  "Qopf;"
  (named-character-ref "Qopf;" '(#\ℚ) "ℚ")
  "Qscr;"
  (named-character-ref "Qscr;" '(#\𝒬) "𝒬")
  "RBarr;"
  (named-character-ref "RBarr;" '(#\⤐) "⤐")
  "REG"
  (named-character-ref "REG" '(#\®) "®")
  "REG;"
  (named-character-ref "REG;" '(#\®) "®")
  "Racute;"
  (named-character-ref "Racute;" '(#\Ŕ) "Ŕ")
  "Rang;"
  (named-character-ref "Rang;" '(#\⟫) "⟫")
  "Rarr;"
  (named-character-ref "Rarr;" '(#\↠) "↠")
  "Rarrtl;"
  (named-character-ref "Rarrtl;" '(#\⤖) "⤖")
  "Rcaron;"
  (named-character-ref "Rcaron;" '(#\Ř) "Ř")
  "Rcedil;"
  (named-character-ref "Rcedil;" '(#\Ŗ) "Ŗ")
  "Rcy;"
  (named-character-ref "Rcy;" '(#\Р) "Р")
  "Re;"
  (named-character-ref "Re;" '(#\ℜ) "ℜ")
  "ReverseElement;"
  (named-character-ref "ReverseElement;" '(#\∋) "∋")
  "ReverseEquilibrium;"
  (named-character-ref "ReverseEquilibrium;" '(#\⇋) "⇋")
  "ReverseUpEquilibrium;"
  (named-character-ref "ReverseUpEquilibrium;" '(#\⥯) "⥯")
  "Rfr;"
  (named-character-ref "Rfr;" '(#\ℜ) "ℜ")
  "Rho;"
  (named-character-ref "Rho;" '(#\Ρ) "Ρ")
  "RightAngleBracket;"
  (named-character-ref "RightAngleBracket;" '(#\⟩) "⟩")
  "RightArrow;"
  (named-character-ref "RightArrow;" '(#\→) "→")
  "RightArrowBar;"
  (named-character-ref "RightArrowBar;" '(#\⇥) "⇥")
  "RightArrowLeftArrow;"
  (named-character-ref "RightArrowLeftArrow;" '(#\⇄) "⇄")
  "RightCeiling;"
  (named-character-ref "RightCeiling;" '(#\⌉) "⌉")
  "RightDoubleBracket;"
  (named-character-ref "RightDoubleBracket;" '(#\⟧) "⟧")
  "RightDownTeeVector;"
  (named-character-ref "RightDownTeeVector;" '(#\⥝) "⥝")
  "RightDownVector;"
  (named-character-ref "RightDownVector;" '(#\⇂) "⇂")
  "RightDownVectorBar;"
  (named-character-ref "RightDownVectorBar;" '(#\⥕) "⥕")
  "RightFloor;"
  (named-character-ref "RightFloor;" '(#\⌋) "⌋")
  "RightTee;"
  (named-character-ref "RightTee;" '(#\⊢) "⊢")
  "RightTeeArrow;"
  (named-character-ref "RightTeeArrow;" '(#\↦) "↦")
  "RightTeeVector;"
  (named-character-ref "RightTeeVector;" '(#\⥛) "⥛")
  "RightTriangle;"
  (named-character-ref "RightTriangle;" '(#\⊳) "⊳")
  "RightTriangleBar;"
  (named-character-ref "RightTriangleBar;" '(#\⧐) "⧐")
  "RightTriangleEqual;"
  (named-character-ref "RightTriangleEqual;" '(#\⊵) "⊵")
  "RightUpDownVector;"
  (named-character-ref "RightUpDownVector;" '(#\⥏) "⥏")
  "RightUpTeeVector;"
  (named-character-ref "RightUpTeeVector;" '(#\⥜) "⥜")
  "RightUpVector;"
  (named-character-ref "RightUpVector;" '(#\↾) "↾")
  "RightUpVectorBar;"
  (named-character-ref "RightUpVectorBar;" '(#\⥔) "⥔")
  "RightVector;"
  (named-character-ref "RightVector;" '(#\⇀) "⇀")
  "RightVectorBar;"
  (named-character-ref "RightVectorBar;" '(#\⥓) "⥓")
  "Rightarrow;"
  (named-character-ref "Rightarrow;" '(#\⇒) "⇒")
  "Ropf;"
  (named-character-ref "Ropf;" '(#\ℝ) "ℝ")
  "RoundImplies;"
  (named-character-ref "RoundImplies;" '(#\⥰) "⥰")
  "Rrightarrow;"
  (named-character-ref "Rrightarrow;" '(#\⇛) "⇛")
  "Rscr;"
  (named-character-ref "Rscr;" '(#\ℛ) "ℛ")
  "Rsh;"
  (named-character-ref "Rsh;" '(#\↱) "↱")
  "RuleDelayed;"
  (named-character-ref "RuleDelayed;" '(#\⧴) "⧴")
  "SHCHcy;"
  (named-character-ref "SHCHcy;" '(#\Щ) "Щ")
  "SHcy;"
  (named-character-ref "SHcy;" '(#\Ш) "Ш")
  "SOFTcy;"
  (named-character-ref "SOFTcy;" '(#\Ь) "Ь")
  "Sacute;"
  (named-character-ref "Sacute;" '(#\Ś) "Ś")
  "Sc;"
  (named-character-ref "Sc;" '(#\⪼) "⪼")
  "Scaron;"
  (named-character-ref "Scaron;" '(#\Š) "Š")
  "Scedil;"
  (named-character-ref "Scedil;" '(#\Ş) "Ş")
  "Scirc;"
  (named-character-ref "Scirc;" '(#\Ŝ) "Ŝ")
  "Scy;"
  (named-character-ref "Scy;" '(#\С) "С")
  "Sfr;"
  (named-character-ref "Sfr;" '(#\𝔖) "𝔖")
  "ShortDownArrow;"
  (named-character-ref "ShortDownArrow;" '(#\↓) "↓")
  "ShortLeftArrow;"
  (named-character-ref "ShortLeftArrow;" '(#\←) "←")
  "ShortRightArrow;"
  (named-character-ref "ShortRightArrow;" '(#\→) "→")
  "ShortUpArrow;"
  (named-character-ref "ShortUpArrow;" '(#\↑) "↑")
  "Sigma;"
  (named-character-ref "Sigma;" '(#\Σ) "Σ")
  "SmallCircle;"
  (named-character-ref "SmallCircle;" '(#\∘) "∘")
  "Sopf;"
  (named-character-ref "Sopf;" '(#\𝕊) "𝕊")
  "Sqrt;"
  (named-character-ref "Sqrt;" '(#\√) "√")
  "Square;"
  (named-character-ref "Square;" '(#\□) "□")
  "SquareIntersection;"
  (named-character-ref "SquareIntersection;" '(#\⊓) "⊓")
  "SquareSubset;"
  (named-character-ref "SquareSubset;" '(#\⊏) "⊏")
  "SquareSubsetEqual;"
  (named-character-ref "SquareSubsetEqual;" '(#\⊑) "⊑")
  "SquareSuperset;"
  (named-character-ref "SquareSuperset;" '(#\⊐) "⊐")
  "SquareSupersetEqual;"
  (named-character-ref "SquareSupersetEqual;" '(#\⊒) "⊒")
  "SquareUnion;"
  (named-character-ref "SquareUnion;" '(#\⊔) "⊔")
  "Sscr;"
  (named-character-ref "Sscr;" '(#\𝒮) "𝒮")
  "Star;"
  (named-character-ref "Star;" '(#\⋆) "⋆")
  "Sub;"
  (named-character-ref "Sub;" '(#\⋐) "⋐")
  "Subset;"
  (named-character-ref "Subset;" '(#\⋐) "⋐")
  "SubsetEqual;"
  (named-character-ref "SubsetEqual;" '(#\⊆) "⊆")
  "Succeeds;"
  (named-character-ref "Succeeds;" '(#\≻) "≻")
  "SucceedsEqual;"
  (named-character-ref "SucceedsEqual;" '(#\⪰) "⪰")
  "SucceedsSlantEqual;"
  (named-character-ref "SucceedsSlantEqual;" '(#\≽) "≽")
  "SucceedsTilde;"
  (named-character-ref "SucceedsTilde;" '(#\≿) "≿")
  "SuchThat;"
  (named-character-ref "SuchThat;" '(#\∋) "∋")
  "Sum;"
  (named-character-ref "Sum;" '(#\∑) "∑")
  "Sup;"
  (named-character-ref "Sup;" '(#\⋑) "⋑")
  "Superset;"
  (named-character-ref "Superset;" '(#\⊃) "⊃")
  "SupersetEqual;"
  (named-character-ref "SupersetEqual;" '(#\⊇) "⊇")
  "Supset;"
  (named-character-ref "Supset;" '(#\⋑) "⋑")
  "THORN"
  (named-character-ref "THORN" '(#\Þ) "Þ")
  "THORN;"
  (named-character-ref "THORN;" '(#\Þ) "Þ")
  "TRADE;"
  (named-character-ref "TRADE;" '(#\™) "™")
  "TSHcy;"
  (named-character-ref "TSHcy;" '(#\Ћ) "Ћ")
  "TScy;"
  (named-character-ref "TScy;" '(#\Ц) "Ц")
  "Tab;"
  (named-character-ref "Tab;" '(#\tab) "␉")
  "Tau;"
  (named-character-ref "Tau;" '(#\Τ) "Τ")
  "Tcaron;"
  (named-character-ref "Tcaron;" '(#\Ť) "Ť")
  "Tcedil;"
  (named-character-ref "Tcedil;" '(#\Ţ) "Ţ")
  "Tcy;"
  (named-character-ref "Tcy;" '(#\Т) "Т")
  "Tfr;"
  (named-character-ref "Tfr;" '(#\𝔗) "𝔗")
  "Therefore;"
  (named-character-ref "Therefore;" '(#\∴) "∴")
  "Theta;"
  (named-character-ref "Theta;" '(#\Θ) "Θ")
  "ThickSpace;"
  (named-character-ref "ThickSpace;" '(#\u205F #\u200A) "  ")
  "ThinSpace;"
  (named-character-ref "ThinSpace;" '(#\u2009) " ")
  "Tilde;"
  (named-character-ref "Tilde;" '(#\∼) "∼")
  "TildeEqual;"
  (named-character-ref "TildeEqual;" '(#\≃) "≃")
  "TildeFullEqual;"
  (named-character-ref "TildeFullEqual;" '(#\≅) "≅")
  "TildeTilde;"
  (named-character-ref "TildeTilde;" '(#\≈) "≈")
  "Topf;"
  (named-character-ref "Topf;" '(#\𝕋) "𝕋")
  "TripleDot;"
  (named-character-ref "TripleDot;" '(#\⃛) "◌⃛")
  "Tscr;"
  (named-character-ref "Tscr;" '(#\𝒯) "𝒯")
  "Tstrok;"
  (named-character-ref "Tstrok;" '(#\Ŧ) "Ŧ")
  "Uacute"
  (named-character-ref "Uacute" '(#\Ú) "Ú")
  "Uacute;"
  (named-character-ref "Uacute;" '(#\Ú) "Ú")
  "Uarr;"
  (named-character-ref "Uarr;" '(#\↟) "↟")
  "Uarrocir;"
  (named-character-ref "Uarrocir;" '(#\⥉) "⥉")
  "Ubrcy;"
  (named-character-ref "Ubrcy;" '(#\Ў) "Ў")
  "Ubreve;"
  (named-character-ref "Ubreve;" '(#\Ŭ) "Ŭ")
  "Ucirc"
  (named-character-ref "Ucirc" '(#\Û) "Û")
  "Ucirc;"
  (named-character-ref "Ucirc;" '(#\Û) "Û")
  "Ucy;"
  (named-character-ref "Ucy;" '(#\У) "У")
  "Udblac;"
  (named-character-ref "Udblac;" '(#\Ű) "Ű")
  "Ufr;"
  (named-character-ref "Ufr;" '(#\𝔘) "𝔘")
  "Ugrave"
  (named-character-ref "Ugrave" '(#\Ù) "Ù")
  "Ugrave;"
  (named-character-ref "Ugrave;" '(#\Ù) "Ù")
  "Umacr;"
  (named-character-ref "Umacr;" '(#\Ū) "Ū")
  "UnderBar;"
  (named-character-ref "UnderBar;" '(#\_) "_")
  "UnderBrace;"
  (named-character-ref "UnderBrace;" '(#\⏟) "⏟")
  "UnderBracket;"
  (named-character-ref "UnderBracket;" '(#\⎵) "⎵")
  "UnderParenthesis;"
  (named-character-ref "UnderParenthesis;" '(#\⏝) "⏝")
  "Union;"
  (named-character-ref "Union;" '(#\⋃) "⋃")
  "UnionPlus;"
  (named-character-ref "UnionPlus;" '(#\⊎) "⊎")
  "Uogon;"
  (named-character-ref "Uogon;" '(#\Ų) "Ų")
  "Uopf;"
  (named-character-ref "Uopf;" '(#\𝕌) "𝕌")
  "UpArrow;"
  (named-character-ref "UpArrow;" '(#\↑) "↑")
  "UpArrowBar;"
  (named-character-ref "UpArrowBar;" '(#\⤒) "⤒")
  "UpArrowDownArrow;"
  (named-character-ref "UpArrowDownArrow;" '(#\⇅) "⇅")
  "UpDownArrow;"
  (named-character-ref "UpDownArrow;" '(#\↕) "↕")
  "UpEquilibrium;"
  (named-character-ref "UpEquilibrium;" '(#\⥮) "⥮")
  "UpTee;"
  (named-character-ref "UpTee;" '(#\⊥) "⊥")
  "UpTeeArrow;"
  (named-character-ref "UpTeeArrow;" '(#\↥) "↥")
  "Uparrow;"
  (named-character-ref "Uparrow;" '(#\⇑) "⇑")
  "Updownarrow;"
  (named-character-ref "Updownarrow;" '(#\⇕) "⇕")
  "UpperLeftArrow;"
  (named-character-ref "UpperLeftArrow;" '(#\↖) "↖")
  "UpperRightArrow;"
  (named-character-ref "UpperRightArrow;" '(#\↗) "↗")
  "Upsi;"
  (named-character-ref "Upsi;" '(#\ϒ) "ϒ")
  "Upsilon;"
  (named-character-ref "Upsilon;" '(#\Υ) "Υ")
  "Uring;"
  (named-character-ref "Uring;" '(#\Ů) "Ů")
  "Uscr;"
  (named-character-ref "Uscr;" '(#\𝒰) "𝒰")
  "Utilde;"
  (named-character-ref "Utilde;" '(#\Ũ) "Ũ")
  "Uuml"
  (named-character-ref "Uuml" '(#\Ü) "Ü")
  "Uuml;"
  (named-character-ref "Uuml;" '(#\Ü) "Ü")
  "VDash;"
  (named-character-ref "VDash;" '(#\⊫) "⊫")
  "Vbar;"
  (named-character-ref "Vbar;" '(#\⫫) "⫫")
  "Vcy;"
  (named-character-ref "Vcy;" '(#\В) "В")
  "Vdash;"
  (named-character-ref "Vdash;" '(#\⊩) "⊩")
  "Vdashl;"
  (named-character-ref "Vdashl;" '(#\⫦) "⫦")
  "Vee;"
  (named-character-ref "Vee;" '(#\⋁) "⋁")
  "Verbar;"
  (named-character-ref "Verbar;" '(#\‖) "‖")
  "Vert;"
  (named-character-ref "Vert;" '(#\‖) "‖")
  "VerticalBar;"
  (named-character-ref "VerticalBar;" '(#\∣) "∣")
  "VerticalLine;"
  (named-character-ref "VerticalLine;" '(#\|) "|")
  "VerticalSeparator;"
  (named-character-ref "VerticalSeparator;" '(#\❘) "❘")
  "VerticalTilde;"
  (named-character-ref "VerticalTilde;" '(#\≀) "≀")
  "VeryThinSpace;"
  (named-character-ref "VeryThinSpace;" '(#\u200A) " ")
  "Vfr;"
  (named-character-ref "Vfr;" '(#\𝔙) "𝔙")
  "Vopf;"
  (named-character-ref "Vopf;" '(#\𝕍) "𝕍")
  "Vscr;"
  (named-character-ref "Vscr;" '(#\𝒱) "𝒱")
  "Vvdash;"
  (named-character-ref "Vvdash;" '(#\⊪) "⊪")
  "Wcirc;"
  (named-character-ref "Wcirc;" '(#\Ŵ) "Ŵ")
  "Wedge;"
  (named-character-ref "Wedge;" '(#\⋀) "⋀")
  "Wfr;"
  (named-character-ref "Wfr;" '(#\𝔚) "𝔚")
  "Wopf;"
  (named-character-ref "Wopf;" '(#\𝕎) "𝕎")
  "Wscr;"
  (named-character-ref "Wscr;" '(#\𝒲) "𝒲")
  "Xfr;"
  (named-character-ref "Xfr;" '(#\𝔛) "𝔛")
  "Xi;"
  (named-character-ref "Xi;" '(#\Ξ) "Ξ")
  "Xopf;"
  (named-character-ref "Xopf;" '(#\𝕏) "𝕏")
  "Xscr;"
  (named-character-ref "Xscr;" '(#\𝒳) "𝒳")
  "YAcy;"
  (named-character-ref "YAcy;" '(#\Я) "Я")
  "YIcy;"
  (named-character-ref "YIcy;" '(#\Ї) "Ї")
  "YUcy;"
  (named-character-ref "YUcy;" '(#\Ю) "Ю")
  "Yacute"
  (named-character-ref "Yacute" '(#\Ý) "Ý")
  "Yacute;"
  (named-character-ref "Yacute;" '(#\Ý) "Ý")
  "Ycirc;"
  (named-character-ref "Ycirc;" '(#\Ŷ) "Ŷ")
  "Ycy;"
  (named-character-ref "Ycy;" '(#\Ы) "Ы")
  "Yfr;"
  (named-character-ref "Yfr;" '(#\𝔜) "𝔜")
  "Yopf;"
  (named-character-ref "Yopf;" '(#\𝕐) "𝕐")
  "Yscr;"
  (named-character-ref "Yscr;" '(#\𝒴) "𝒴")
  "Yuml;"
  (named-character-ref "Yuml;" '(#\Ÿ) "Ÿ")
  "ZHcy;"
  (named-character-ref "ZHcy;" '(#\Ж) "Ж")
  "Zacute;"
  (named-character-ref "Zacute;" '(#\Ź) "Ź")
  "Zcaron;"
  (named-character-ref "Zcaron;" '(#\Ž) "Ž")
  "Zcy;"
  (named-character-ref "Zcy;" '(#\З) "З")
  "Zdot;"
  (named-character-ref "Zdot;" '(#\Ż) "Ż")
  "ZeroWidthSpace;"
  (named-character-ref "ZeroWidthSpace;" '(#\u200B) "\u200B")
  "Zeta;"
  (named-character-ref "Zeta;" '(#\Ζ) "Ζ")
  "Zfr;"
  (named-character-ref "Zfr;" '(#\ℨ) "ℨ")
  "Zopf;"
  (named-character-ref "Zopf;" '(#\ℤ) "ℤ")
  "Zscr;"
  (named-character-ref "Zscr;" '(#\𝒵) "𝒵")
  "aacute"
  (named-character-ref "aacute" '(#\á) "á")
  "aacute;"
  (named-character-ref "aacute;" '(#\á) "á")
  "abreve;"
  (named-character-ref "abreve;" '(#\ă) "ă")
  "ac;"
  (named-character-ref "ac;" '(#\∾) "∾")
  "acE;"
  (named-character-ref "acE;" '(#\∾ #\̳) "∾̳")
  "acd;"
  (named-character-ref "acd;" '(#\∿) "∿")
  "acirc"
  (named-character-ref "acirc" '(#\â) "â")
  "acirc;"
  (named-character-ref "acirc;" '(#\â) "â")
  "acute"
  (named-character-ref "acute" '(#\´) "´")
  "acute;"
  (named-character-ref "acute;" '(#\´) "´")
  "acy;"
  (named-character-ref "acy;" '(#\а) "а")
  "aelig"
  (named-character-ref "aelig" '(#\æ) "æ")
  "aelig;"
  (named-character-ref "aelig;" '(#\æ) "æ")
  "af;"
  (named-character-ref "af;" '(#\u2061) "\u2061")
  "afr;"
  (named-character-ref "afr;" '(#\𝔞) "𝔞")
  "agrave"
  (named-character-ref "agrave" '(#\à) "à")
  "agrave;"
  (named-character-ref "agrave;" '(#\à) "à")
  "alefsym;"
  (named-character-ref "alefsym;" '(#\ℵ) "ℵ")
  "aleph;"
  (named-character-ref "aleph;" '(#\ℵ) "ℵ")
  "alpha;"
  (named-character-ref "alpha;" '(#\α) "α")
  "amacr;"
  (named-character-ref "amacr;" '(#\ā) "ā")
  "amalg;"
  (named-character-ref "amalg;" '(#\⨿) "⨿")
  "amp"
  (named-character-ref "amp" '(#\&) "&")
  "amp;"
  (named-character-ref "amp;" '(#\&) "&")
  "and;"
  (named-character-ref "and;" '(#\∧) "∧")
  "andand;"
  (named-character-ref "andand;" '(#\⩕) "⩕")
  "andd;"
  (named-character-ref "andd;" '(#\⩜) "⩜")
  "andslope;"
  (named-character-ref "andslope;" '(#\⩘) "⩘")
  "andv;"
  (named-character-ref "andv;" '(#\⩚) "⩚")
  "ang;"
  (named-character-ref "ang;" '(#\∠) "∠")
  "ange;"
  (named-character-ref "ange;" '(#\⦤) "⦤")
  "angle;"
  (named-character-ref "angle;" '(#\∠) "∠")
  "angmsd;"
  (named-character-ref "angmsd;" '(#\∡) "∡")
  "angmsdaa;"
  (named-character-ref "angmsdaa;" '(#\⦨) "⦨")
  "angmsdab;"
  (named-character-ref "angmsdab;" '(#\⦩) "⦩")
  "angmsdac;"
  (named-character-ref "angmsdac;" '(#\⦪) "⦪")
  "angmsdad;"
  (named-character-ref "angmsdad;" '(#\⦫) "⦫")
  "angmsdae;"
  (named-character-ref "angmsdae;" '(#\⦬) "⦬")
  "angmsdaf;"
  (named-character-ref "angmsdaf;" '(#\⦭) "⦭")
  "angmsdag;"
  (named-character-ref "angmsdag;" '(#\⦮) "⦮")
  "angmsdah;"
  (named-character-ref "angmsdah;" '(#\⦯) "⦯")
  "angrt;"
  (named-character-ref "angrt;" '(#\∟) "∟")
  "angrtvb;"
  (named-character-ref "angrtvb;" '(#\⊾) "⊾")
  "angrtvbd;"
  (named-character-ref "angrtvbd;" '(#\⦝) "⦝")
  "angsph;"
  (named-character-ref "angsph;" '(#\∢) "∢")
  "angst;"
  (named-character-ref "angst;" '(#\Å) "Å")
  "angzarr;"
  (named-character-ref "angzarr;" '(#\⍼) "⍼")
  "aogon;"
  (named-character-ref "aogon;" '(#\ą) "ą")
  "aopf;"
  (named-character-ref "aopf;" '(#\𝕒) "𝕒")
  "ap;"
  (named-character-ref "ap;" '(#\≈) "≈")
  "apE;"
  (named-character-ref "apE;" '(#\⩰) "⩰")
  "apacir;"
  (named-character-ref "apacir;" '(#\⩯) "⩯")
  "ape;"
  (named-character-ref "ape;" '(#\≊) "≊")
  "apid;"
  (named-character-ref "apid;" '(#\≋) "≋")
  "apos;"
  (named-character-ref "apos;" '(#\') "'")
  "approx;"
  (named-character-ref "approx;" '(#\≈) "≈")
  "approxeq;"
  (named-character-ref "approxeq;" '(#\≊) "≊")
  "aring"
  (named-character-ref "aring" '(#\å) "å")
  "aring;"
  (named-character-ref "aring;" '(#\å) "å")
  "ascr;"
  (named-character-ref "ascr;" '(#\𝒶) "𝒶")
  "ast;"
  (named-character-ref "ast;" '(#\*) "*")
  "asymp;"
  (named-character-ref "asymp;" '(#\≈) "≈")
  "asympeq;"
  (named-character-ref "asympeq;" '(#\≍) "≍")
  "atilde"
  (named-character-ref "atilde" '(#\ã) "ã")
  "atilde;"
  (named-character-ref "atilde;" '(#\ã) "ã")
  "auml"
  (named-character-ref "auml" '(#\ä) "ä")
  "auml;"
  (named-character-ref "auml;" '(#\ä) "ä")
  "awconint;"
  (named-character-ref "awconint;" '(#\∳) "∳")
  "awint;"
  (named-character-ref "awint;" '(#\⨑) "⨑")
  "bNot;"
  (named-character-ref "bNot;" '(#\⫭) "⫭")
  "backcong;"
  (named-character-ref "backcong;" '(#\≌) "≌")
  "backepsilon;"
  (named-character-ref "backepsilon;" '(#\϶) "϶")
  "backprime;"
  (named-character-ref "backprime;" '(#\‵) "‵")
  "backsim;"
  (named-character-ref "backsim;" '(#\∽) "∽")
  "backsimeq;"
  (named-character-ref "backsimeq;" '(#\⋍) "⋍")
  "barvee;"
  (named-character-ref "barvee;" '(#\⊽) "⊽")
  "barwed;"
  (named-character-ref "barwed;" '(#\⌅) "⌅")
  "barwedge;"
  (named-character-ref "barwedge;" '(#\⌅) "⌅")
  "bbrk;"
  (named-character-ref "bbrk;" '(#\⎵) "⎵")
  "bbrktbrk;"
  (named-character-ref "bbrktbrk;" '(#\⎶) "⎶")
  "bcong;"
  (named-character-ref "bcong;" '(#\≌) "≌")
  "bcy;"
  (named-character-ref "bcy;" '(#\б) "б")
  "bdquo;"
  (named-character-ref "bdquo;" '(#\„) "„")
  "becaus;"
  (named-character-ref "becaus;" '(#\∵) "∵")
  "because;"
  (named-character-ref "because;" '(#\∵) "∵")
  "bemptyv;"
  (named-character-ref "bemptyv;" '(#\⦰) "⦰")
  "bepsi;"
  (named-character-ref "bepsi;" '(#\϶) "϶")
  "bernou;"
  (named-character-ref "bernou;" '(#\ℬ) "ℬ")
  "beta;"
  (named-character-ref "beta;" '(#\β) "β")
  "beth;"
  (named-character-ref "beth;" '(#\ℶ) "ℶ")
  "between;"
  (named-character-ref "between;" '(#\≬) "≬")
  "bfr;"
  (named-character-ref "bfr;" '(#\𝔟) "𝔟")
  "bigcap;"
  (named-character-ref "bigcap;" '(#\⋂) "⋂")
  "bigcirc;"
  (named-character-ref "bigcirc;" '(#\◯) "◯")
  "bigcup;"
  (named-character-ref "bigcup;" '(#\⋃) "⋃")
  "bigodot;"
  (named-character-ref "bigodot;" '(#\⨀) "⨀")
  "bigoplus;"
  (named-character-ref "bigoplus;" '(#\⨁) "⨁")
  "bigotimes;"
  (named-character-ref "bigotimes;" '(#\⨂) "⨂")
  "bigsqcup;"
  (named-character-ref "bigsqcup;" '(#\⨆) "⨆")
  "bigstar;"
  (named-character-ref "bigstar;" '(#\★) "★")
  "bigtriangledown;"
  (named-character-ref "bigtriangledown;" '(#\▽) "▽")
  "bigtriangleup;"
  (named-character-ref "bigtriangleup;" '(#\△) "△")
  "biguplus;"
  (named-character-ref "biguplus;" '(#\⨄) "⨄")
  "bigvee;"
  (named-character-ref "bigvee;" '(#\⋁) "⋁")
  "bigwedge;"
  (named-character-ref "bigwedge;" '(#\⋀) "⋀")
  "bkarow;"
  (named-character-ref "bkarow;" '(#\⤍) "⤍")
  "blacklozenge;"
  (named-character-ref "blacklozenge;" '(#\⧫) "⧫")
  "blacksquare;"
  (named-character-ref "blacksquare;" '(#\▪) "▪")
  "blacktriangle;"
  (named-character-ref "blacktriangle;" '(#\▴) "▴")
  "blacktriangledown;"
  (named-character-ref "blacktriangledown;" '(#\▾) "▾")
  "blacktriangleleft;"
  (named-character-ref "blacktriangleleft;" '(#\◂) "◂")
  "blacktriangleright;"
  (named-character-ref "blacktriangleright;" '(#\▸) "▸")
  "blank;"
  (named-character-ref "blank;" '(#\␣) "␣")
  "blk12;"
  (named-character-ref "blk12;" '(#\▒) "▒")
  "blk14;"
  (named-character-ref "blk14;" '(#\░) "░")
  "blk34;"
  (named-character-ref "blk34;" '(#\▓) "▓")
  "block;"
  (named-character-ref "block;" '(#\█) "█")
  "bne;"
  (named-character-ref "bne;" '(#\= #\⃥) "=⃥")
  "bnequiv;"
  (named-character-ref "bnequiv;" '(#\≡ #\⃥) "≡⃥")
  "bnot;"
  (named-character-ref "bnot;" '(#\⌐) "⌐")
  "bopf;"
  (named-character-ref "bopf;" '(#\𝕓) "𝕓")
  "bot;"
  (named-character-ref "bot;" '(#\⊥) "⊥")
  "bottom;"
  (named-character-ref "bottom;" '(#\⊥) "⊥")
  "bowtie;"
  (named-character-ref "bowtie;" '(#\⋈) "⋈")
  "boxDL;"
  (named-character-ref "boxDL;" '(#\╗) "╗")
  "boxDR;"
  (named-character-ref "boxDR;" '(#\╔) "╔")
  "boxDl;"
  (named-character-ref "boxDl;" '(#\╖) "╖")
  "boxDr;"
  (named-character-ref "boxDr;" '(#\╓) "╓")
  "boxH;"
  (named-character-ref "boxH;" '(#\═) "═")
  "boxHD;"
  (named-character-ref "boxHD;" '(#\╦) "╦")
  "boxHU;"
  (named-character-ref "boxHU;" '(#\╩) "╩")
  "boxHd;"
  (named-character-ref "boxHd;" '(#\╤) "╤")
  "boxHu;"
  (named-character-ref "boxHu;" '(#\╧) "╧")
  "boxUL;"
  (named-character-ref "boxUL;" '(#\╝) "╝")
  "boxUR;"
  (named-character-ref "boxUR;" '(#\╚) "╚")
  "boxUl;"
  (named-character-ref "boxUl;" '(#\╜) "╜")
  "boxUr;"
  (named-character-ref "boxUr;" '(#\╙) "╙")
  "boxV;"
  (named-character-ref "boxV;" '(#\║) "║")
  "boxVH;"
  (named-character-ref "boxVH;" '(#\╬) "╬")
  "boxVL;"
  (named-character-ref "boxVL;" '(#\╣) "╣")
  "boxVR;"
  (named-character-ref "boxVR;" '(#\╠) "╠")
  "boxVh;"
  (named-character-ref "boxVh;" '(#\╫) "╫")
  "boxVl;"
  (named-character-ref "boxVl;" '(#\╢) "╢")
  "boxVr;"
  (named-character-ref "boxVr;" '(#\╟) "╟")
  "boxbox;"
  (named-character-ref "boxbox;" '(#\⧉) "⧉")
  "boxdL;"
  (named-character-ref "boxdL;" '(#\╕) "╕")
  "boxdR;"
  (named-character-ref "boxdR;" '(#\╒) "╒")
  "boxdl;"
  (named-character-ref "boxdl;" '(#\┐) "┐")
  "boxdr;"
  (named-character-ref "boxdr;" '(#\┌) "┌")
  "boxh;"
  (named-character-ref "boxh;" '(#\─) "─")
  "boxhD;"
  (named-character-ref "boxhD;" '(#\╥) "╥")
  "boxhU;"
  (named-character-ref "boxhU;" '(#\╨) "╨")
  "boxhd;"
  (named-character-ref "boxhd;" '(#\┬) "┬")
  "boxhu;"
  (named-character-ref "boxhu;" '(#\┴) "┴")
  "boxminus;"
  (named-character-ref "boxminus;" '(#\⊟) "⊟")
  "boxplus;"
  (named-character-ref "boxplus;" '(#\⊞) "⊞")
  "boxtimes;"
  (named-character-ref "boxtimes;" '(#\⊠) "⊠")
  "boxuL;"
  (named-character-ref "boxuL;" '(#\╛) "╛")
  "boxuR;"
  (named-character-ref "boxuR;" '(#\╘) "╘")
  "boxul;"
  (named-character-ref "boxul;" '(#\┘) "┘")
  "boxur;"
  (named-character-ref "boxur;" '(#\└) "└")
  "boxv;"
  (named-character-ref "boxv;" '(#\│) "│")
  "boxvH;"
  (named-character-ref "boxvH;" '(#\╪) "╪")
  "boxvL;"
  (named-character-ref "boxvL;" '(#\╡) "╡")
  "boxvR;"
  (named-character-ref "boxvR;" '(#\╞) "╞")
  "boxvh;"
  (named-character-ref "boxvh;" '(#\┼) "┼")
  "boxvl;"
  (named-character-ref "boxvl;" '(#\┤) "┤")
  "boxvr;"
  (named-character-ref "boxvr;" '(#\├) "├")
  "bprime;"
  (named-character-ref "bprime;" '(#\‵) "‵")
  "breve;"
  (named-character-ref "breve;" '(#\˘) "˘")
  "brvbar"
  (named-character-ref "brvbar" '(#\¦) "¦")
  "brvbar;"
  (named-character-ref "brvbar;" '(#\¦) "¦")
  "bscr;"
  (named-character-ref "bscr;" '(#\𝒷) "𝒷")
  "bsemi;"
  (named-character-ref "bsemi;" '(#\⁏) "⁏")
  "bsim;"
  (named-character-ref "bsim;" '(#\∽) "∽")
  "bsime;"
  (named-character-ref "bsime;" '(#\⋍) "⋍")
  "bsol;"
  (named-character-ref "bsol;" '(#\\) "\\")
  "bsolb;"
  (named-character-ref "bsolb;" '(#\⧅) "⧅")
  "bsolhsub;"
  (named-character-ref "bsolhsub;" '(#\⟈) "⟈")
  "bull;"
  (named-character-ref "bull;" '(#\•) "•")
  "bullet;"
  (named-character-ref "bullet;" '(#\•) "•")
  "bump;"
  (named-character-ref "bump;" '(#\≎) "≎")
  "bumpE;"
  (named-character-ref "bumpE;" '(#\⪮) "⪮")
  "bumpe;"
  (named-character-ref "bumpe;" '(#\≏) "≏")
  "bumpeq;"
  (named-character-ref "bumpeq;" '(#\≏) "≏")
  "cacute;"
  (named-character-ref "cacute;" '(#\ć) "ć")
  "cap;"
  (named-character-ref "cap;" '(#\∩) "∩")
  "capand;"
  (named-character-ref "capand;" '(#\⩄) "⩄")
  "capbrcup;"
  (named-character-ref "capbrcup;" '(#\⩉) "⩉")
  "capcap;"
  (named-character-ref "capcap;" '(#\⩋) "⩋")
  "capcup;"
  (named-character-ref "capcup;" '(#\⩇) "⩇")
  "capdot;"
  (named-character-ref "capdot;" '(#\⩀) "⩀")
  "caps;"
  (named-character-ref "caps;" '(#\∩ #\︀) "∩︀")
  "caret;"
  (named-character-ref "caret;" '(#\⁁) "⁁")
  "caron;"
  (named-character-ref "caron;" '(#\ˇ) "ˇ")
  "ccaps;"
  (named-character-ref "ccaps;" '(#\⩍) "⩍")
  "ccaron;"
  (named-character-ref "ccaron;" '(#\č) "č")
  "ccedil"
  (named-character-ref "ccedil" '(#\ç) "ç")
  "ccedil;"
  (named-character-ref "ccedil;" '(#\ç) "ç")
  "ccirc;"
  (named-character-ref "ccirc;" '(#\ĉ) "ĉ")
  "ccups;"
  (named-character-ref "ccups;" '(#\⩌) "⩌")
  "ccupssm;"
  (named-character-ref "ccupssm;" '(#\⩐) "⩐")
  "cdot;"
  (named-character-ref "cdot;" '(#\ċ) "ċ")
  "cedil"
  (named-character-ref "cedil" '(#\¸) "¸")
  "cedil;"
  (named-character-ref "cedil;" '(#\¸) "¸")
  "cemptyv;"
  (named-character-ref "cemptyv;" '(#\⦲) "⦲")
  "cent"
  (named-character-ref "cent" '(#\¢) "¢")
  "cent;"
  (named-character-ref "cent;" '(#\¢) "¢")
  "centerdot;"
  (named-character-ref "centerdot;" '(#\·) "·")
  "cfr;"
  (named-character-ref "cfr;" '(#\𝔠) "𝔠")
  "chcy;"
  (named-character-ref "chcy;" '(#\ч) "ч")
  "check;"
  (named-character-ref "check;" '(#\✓) "✓")
  "checkmark;"
  (named-character-ref "checkmark;" '(#\✓) "✓")
  "chi;"
  (named-character-ref "chi;" '(#\χ) "χ")
  "cir;"
  (named-character-ref "cir;" '(#\○) "○")
  "cirE;"
  (named-character-ref "cirE;" '(#\⧃) "⧃")
  "circ;"
  (named-character-ref "circ;" '(#\ˆ) "ˆ")
  "circeq;"
  (named-character-ref "circeq;" '(#\≗) "≗")
  "circlearrowleft;"
  (named-character-ref "circlearrowleft;" '(#\↺) "↺")
  "circlearrowright;"
  (named-character-ref "circlearrowright;" '(#\↻) "↻")
  "circledR;"
  (named-character-ref "circledR;" '(#\®) "®")
  "circledS;"
  (named-character-ref "circledS;" '(#\Ⓢ) "Ⓢ")
  "circledast;"
  (named-character-ref "circledast;" '(#\⊛) "⊛")
  "circledcirc;"
  (named-character-ref "circledcirc;" '(#\⊚) "⊚")
  "circleddash;"
  (named-character-ref "circleddash;" '(#\⊝) "⊝")
  "cire;"
  (named-character-ref "cire;" '(#\≗) "≗")
  "cirfnint;"
  (named-character-ref "cirfnint;" '(#\⨐) "⨐")
  "cirmid;"
  (named-character-ref "cirmid;" '(#\⫯) "⫯")
  "cirscir;"
  (named-character-ref "cirscir;" '(#\⧂) "⧂")
  "clubs;"
  (named-character-ref "clubs;" '(#\♣) "♣")
  "clubsuit;"
  (named-character-ref "clubsuit;" '(#\♣) "♣")
  "colon;"
  (named-character-ref "colon;" '(#\:) ":")
  "colone;"
  (named-character-ref "colone;" '(#\≔) "≔")
  "coloneq;"
  (named-character-ref "coloneq;" '(#\≔) "≔")
  "comma;"
  (named-character-ref "comma;" '(#\,) ",")
  "commat;"
  (named-character-ref "commat;" '(#\@) "@")
  "comp;"
  (named-character-ref "comp;" '(#\∁) "∁")
  "compfn;"
  (named-character-ref "compfn;" '(#\∘) "∘")
  "complement;"
  (named-character-ref "complement;" '(#\∁) "∁")
  "complexes;"
  (named-character-ref "complexes;" '(#\ℂ) "ℂ")
  "cong;"
  (named-character-ref "cong;" '(#\≅) "≅")
  "congdot;"
  (named-character-ref "congdot;" '(#\⩭) "⩭")
  "conint;"
  (named-character-ref "conint;" '(#\∮) "∮")
  "copf;"
  (named-character-ref "copf;" '(#\𝕔) "𝕔")
  "coprod;"
  (named-character-ref "coprod;" '(#\∐) "∐")
  "copy"
  (named-character-ref "copy" '(#\©) "©")
  "copy;"
  (named-character-ref "copy;" '(#\©) "©")
  "copysr;"
  (named-character-ref "copysr;" '(#\℗) "℗")
  "crarr;"
  (named-character-ref "crarr;" '(#\↵) "↵")
  "cross;"
  (named-character-ref "cross;" '(#\✗) "✗")
  "cscr;"
  (named-character-ref "cscr;" '(#\𝒸) "𝒸")
  "csub;"
  (named-character-ref "csub;" '(#\⫏) "⫏")
  "csube;"
  (named-character-ref "csube;" '(#\⫑) "⫑")
  "csup;"
  (named-character-ref "csup;" '(#\⫐) "⫐")
  "csupe;"
  (named-character-ref "csupe;" '(#\⫒) "⫒")
  "ctdot;"
  (named-character-ref "ctdot;" '(#\⋯) "⋯")
  "cudarrl;"
  (named-character-ref "cudarrl;" '(#\⤸) "⤸")
  "cudarrr;"
  (named-character-ref "cudarrr;" '(#\⤵) "⤵")
  "cuepr;"
  (named-character-ref "cuepr;" '(#\⋞) "⋞")
  "cuesc;"
  (named-character-ref "cuesc;" '(#\⋟) "⋟")
  "cularr;"
  (named-character-ref "cularr;" '(#\↶) "↶")
  "cularrp;"
  (named-character-ref "cularrp;" '(#\⤽) "⤽")
  "cup;"
  (named-character-ref "cup;" '(#\∪) "∪")
  "cupbrcap;"
  (named-character-ref "cupbrcap;" '(#\⩈) "⩈")
  "cupcap;"
  (named-character-ref "cupcap;" '(#\⩆) "⩆")
  "cupcup;"
  (named-character-ref "cupcup;" '(#\⩊) "⩊")
  "cupdot;"
  (named-character-ref "cupdot;" '(#\⊍) "⊍")
  "cupor;"
  (named-character-ref "cupor;" '(#\⩅) "⩅")
  "cups;"
  (named-character-ref "cups;" '(#\∪ #\︀) "∪︀")
  "curarr;"
  (named-character-ref "curarr;" '(#\↷) "↷")
  "curarrm;"
  (named-character-ref "curarrm;" '(#\⤼) "⤼")
  "curlyeqprec;"
  (named-character-ref "curlyeqprec;" '(#\⋞) "⋞")
  "curlyeqsucc;"
  (named-character-ref "curlyeqsucc;" '(#\⋟) "⋟")
  "curlyvee;"
  (named-character-ref "curlyvee;" '(#\⋎) "⋎")
  "curlywedge;"
  (named-character-ref "curlywedge;" '(#\⋏) "⋏")
  "curren"
  (named-character-ref "curren" '(#\¤) "¤")
  "curren;"
  (named-character-ref "curren;" '(#\¤) "¤")
  "curvearrowleft;"
  (named-character-ref "curvearrowleft;" '(#\↶) "↶")
  "curvearrowright;"
  (named-character-ref "curvearrowright;" '(#\↷) "↷")
  "cuvee;"
  (named-character-ref "cuvee;" '(#\⋎) "⋎")
  "cuwed;"
  (named-character-ref "cuwed;" '(#\⋏) "⋏")
  "cwconint;"
  (named-character-ref "cwconint;" '(#\∲) "∲")
  "cwint;"
  (named-character-ref "cwint;" '(#\∱) "∱")
  "cylcty;"
  (named-character-ref "cylcty;" '(#\⌭) "⌭")
  "dArr;"
  (named-character-ref "dArr;" '(#\⇓) "⇓")
  "dHar;"
  (named-character-ref "dHar;" '(#\⥥) "⥥")
  "dagger;"
  (named-character-ref "dagger;" '(#\†) "†")
  "daleth;"
  (named-character-ref "daleth;" '(#\ℸ) "ℸ")
  "darr;"
  (named-character-ref "darr;" '(#\↓) "↓")
  "dash;"
  (named-character-ref "dash;" '(#\‐) "‐")
  "dashv;"
  (named-character-ref "dashv;" '(#\⊣) "⊣")
  "dbkarow;"
  (named-character-ref "dbkarow;" '(#\⤏) "⤏")
  "dblac;"
  (named-character-ref "dblac;" '(#\˝) "˝")
  "dcaron;"
  (named-character-ref "dcaron;" '(#\ď) "ď")
  "dcy;"
  (named-character-ref "dcy;" '(#\д) "д")
  "dd;"
  (named-character-ref "dd;" '(#\ⅆ) "ⅆ")
  "ddagger;"
  (named-character-ref "ddagger;" '(#\‡) "‡")
  "ddarr;"
  (named-character-ref "ddarr;" '(#\⇊) "⇊")
  "ddotseq;"
  (named-character-ref "ddotseq;" '(#\⩷) "⩷")
  "deg"
  (named-character-ref "deg" '(#\°) "°")
  "deg;"
  (named-character-ref "deg;" '(#\°) "°")
  "delta;"
  (named-character-ref "delta;" '(#\δ) "δ")
  "demptyv;"
  (named-character-ref "demptyv;" '(#\⦱) "⦱")
  "dfisht;"
  (named-character-ref "dfisht;" '(#\⥿) "⥿")
  "dfr;"
  (named-character-ref "dfr;" '(#\𝔡) "𝔡")
  "dharl;"
  (named-character-ref "dharl;" '(#\⇃) "⇃")
  "dharr;"
  (named-character-ref "dharr;" '(#\⇂) "⇂")
  "diam;"
  (named-character-ref "diam;" '(#\⋄) "⋄")
  "diamond;"
  (named-character-ref "diamond;" '(#\⋄) "⋄")
  "diamondsuit;"
  (named-character-ref "diamondsuit;" '(#\♦) "♦")
  "diams;"
  (named-character-ref "diams;" '(#\♦) "♦")
  "die;"
  (named-character-ref "die;" '(#\¨) "¨")
  "digamma;"
  (named-character-ref "digamma;" '(#\ϝ) "ϝ")
  "disin;"
  (named-character-ref "disin;" '(#\⋲) "⋲")
  "div;"
  (named-character-ref "div;" '(#\÷) "÷")
  "divide"
  (named-character-ref "divide" '(#\÷) "÷")
  "divide;"
  (named-character-ref "divide;" '(#\÷) "÷")
  "divideontimes;"
  (named-character-ref "divideontimes;" '(#\⋇) "⋇")
  "divonx;"
  (named-character-ref "divonx;" '(#\⋇) "⋇")
  "djcy;"
  (named-character-ref "djcy;" '(#\ђ) "ђ")
  "dlcorn;"
  (named-character-ref "dlcorn;" '(#\⌞) "⌞")
  "dlcrop;"
  (named-character-ref "dlcrop;" '(#\⌍) "⌍")
  "dollar;"
  (named-character-ref "dollar;" '(#\$) "$")
  "dopf;"
  (named-character-ref "dopf;" '(#\𝕕) "𝕕")
  "dot;"
  (named-character-ref "dot;" '(#\˙) "˙")
  "doteq;"
  (named-character-ref "doteq;" '(#\≐) "≐")
  "doteqdot;"
  (named-character-ref "doteqdot;" '(#\≑) "≑")
  "dotminus;"
  (named-character-ref "dotminus;" '(#\∸) "∸")
  "dotplus;"
  (named-character-ref "dotplus;" '(#\∔) "∔")
  "dotsquare;"
  (named-character-ref "dotsquare;" '(#\⊡) "⊡")
  "doublebarwedge;"
  (named-character-ref "doublebarwedge;" '(#\⌆) "⌆")
  "downarrow;"
  (named-character-ref "downarrow;" '(#\↓) "↓")
  "downdownarrows;"
  (named-character-ref "downdownarrows;" '(#\⇊) "⇊")
  "downharpoonleft;"
  (named-character-ref "downharpoonleft;" '(#\⇃) "⇃")
  "downharpoonright;"
  (named-character-ref "downharpoonright;" '(#\⇂) "⇂")
  "drbkarow;"
  (named-character-ref "drbkarow;" '(#\⤐) "⤐")
  "drcorn;"
  (named-character-ref "drcorn;" '(#\⌟) "⌟")
  "drcrop;"
  (named-character-ref "drcrop;" '(#\⌌) "⌌")
  "dscr;"
  (named-character-ref "dscr;" '(#\𝒹) "𝒹")
  "dscy;"
  (named-character-ref "dscy;" '(#\ѕ) "ѕ")
  "dsol;"
  (named-character-ref "dsol;" '(#\⧶) "⧶")
  "dstrok;"
  (named-character-ref "dstrok;" '(#\đ) "đ")
  "dtdot;"
  (named-character-ref "dtdot;" '(#\⋱) "⋱")
  "dtri;"
  (named-character-ref "dtri;" '(#\▿) "▿")
  "dtrif;"
  (named-character-ref "dtrif;" '(#\▾) "▾")
  "duarr;"
  (named-character-ref "duarr;" '(#\⇵) "⇵")
  "duhar;"
  (named-character-ref "duhar;" '(#\⥯) "⥯")
  "dwangle;"
  (named-character-ref "dwangle;" '(#\⦦) "⦦")
  "dzcy;"
  (named-character-ref "dzcy;" '(#\џ) "џ")
  "dzigrarr;"
  (named-character-ref "dzigrarr;" '(#\⟿) "⟿")
  "eDDot;"
  (named-character-ref "eDDot;" '(#\⩷) "⩷")
  "eDot;"
  (named-character-ref "eDot;" '(#\≑) "≑")
  "eacute"
  (named-character-ref "eacute" '(#\é) "é")
  "eacute;"
  (named-character-ref "eacute;" '(#\é) "é")
  "easter;"
  (named-character-ref "easter;" '(#\⩮) "⩮")
  "ecaron;"
  (named-character-ref "ecaron;" '(#\ě) "ě")
  "ecir;"
  (named-character-ref "ecir;" '(#\≖) "≖")
  "ecirc"
  (named-character-ref "ecirc" '(#\ê) "ê")
  "ecirc;"
  (named-character-ref "ecirc;" '(#\ê) "ê")
  "ecolon;"
  (named-character-ref "ecolon;" '(#\≕) "≕")
  "ecy;"
  (named-character-ref "ecy;" '(#\э) "э")
  "edot;"
  (named-character-ref "edot;" '(#\ė) "ė")
  "ee;"
  (named-character-ref "ee;" '(#\ⅇ) "ⅇ")
  "efDot;"
  (named-character-ref "efDot;" '(#\≒) "≒")
  "efr;"
  (named-character-ref "efr;" '(#\𝔢) "𝔢")
  "eg;"
  (named-character-ref "eg;" '(#\⪚) "⪚")
  "egrave"
  (named-character-ref "egrave" '(#\è) "è")
  "egrave;"
  (named-character-ref "egrave;" '(#\è) "è")
  "egs;"
  (named-character-ref "egs;" '(#\⪖) "⪖")
  "egsdot;"
  (named-character-ref "egsdot;" '(#\⪘) "⪘")
  "el;"
  (named-character-ref "el;" '(#\⪙) "⪙")
  "elinters;"
  (named-character-ref "elinters;" '(#\⏧) "⏧")
  "ell;"
  (named-character-ref "ell;" '(#\ℓ) "ℓ")
  "els;"
  (named-character-ref "els;" '(#\⪕) "⪕")
  "elsdot;"
  (named-character-ref "elsdot;" '(#\⪗) "⪗")
  "emacr;"
  (named-character-ref "emacr;" '(#\ē) "ē")
  "empty;"
  (named-character-ref "empty;" '(#\∅) "∅")
  "emptyset;"
  (named-character-ref "emptyset;" '(#\∅) "∅")
  "emptyv;"
  (named-character-ref "emptyv;" '(#\∅) "∅")
  "emsp13;"
  (named-character-ref "emsp13;" '(#\u2004) " ")
  "emsp14;"
  (named-character-ref "emsp14;" '(#\u2005) " ")
  "emsp;"
  (named-character-ref "emsp;" '(#\u2003) " ")
  "eng;"
  (named-character-ref "eng;" '(#\ŋ) "ŋ")
  "ensp;"
  (named-character-ref "ensp;" '(#\u2002) " ")
  "eogon;"
  (named-character-ref "eogon;" '(#\ę) "ę")
  "eopf;"
  (named-character-ref "eopf;" '(#\𝕖) "𝕖")
  "epar;"
  (named-character-ref "epar;" '(#\⋕) "⋕")
  "eparsl;"
  (named-character-ref "eparsl;" '(#\⧣) "⧣")
  "eplus;"
  (named-character-ref "eplus;" '(#\⩱) "⩱")
  "epsi;"
  (named-character-ref "epsi;" '(#\ε) "ε")
  "epsilon;"
  (named-character-ref "epsilon;" '(#\ε) "ε")
  "epsiv;"
  (named-character-ref "epsiv;" '(#\ϵ) "ϵ")
  "eqcirc;"
  (named-character-ref "eqcirc;" '(#\≖) "≖")
  "eqcolon;"
  (named-character-ref "eqcolon;" '(#\≕) "≕")
  "eqsim;"
  (named-character-ref "eqsim;" '(#\≂) "≂")
  "eqslantgtr;"
  (named-character-ref "eqslantgtr;" '(#\⪖) "⪖")
  "eqslantless;"
  (named-character-ref "eqslantless;" '(#\⪕) "⪕")
  "equals;"
  (named-character-ref "equals;" '(#\=) "=")
  "equest;"
  (named-character-ref "equest;" '(#\≟) "≟")
  "equiv;"
  (named-character-ref "equiv;" '(#\≡) "≡")
  "equivDD;"
  (named-character-ref "equivDD;" '(#\⩸) "⩸")
  "eqvparsl;"
  (named-character-ref "eqvparsl;" '(#\⧥) "⧥")
  "erDot;"
  (named-character-ref "erDot;" '(#\≓) "≓")
  "erarr;"
  (named-character-ref "erarr;" '(#\⥱) "⥱")
  "escr;"
  (named-character-ref "escr;" '(#\ℯ) "ℯ")
  "esdot;"
  (named-character-ref "esdot;" '(#\≐) "≐")
  "esim;"
  (named-character-ref "esim;" '(#\≂) "≂")
  "eta;"
  (named-character-ref "eta;" '(#\η) "η")
  "eth"
  (named-character-ref "eth" '(#\ð) "ð")
  "eth;"
  (named-character-ref "eth;" '(#\ð) "ð")
  "euml"
  (named-character-ref "euml" '(#\ë) "ë")
  "euml;"
  (named-character-ref "euml;" '(#\ë) "ë")
  "euro;"
  (named-character-ref "euro;" '(#\€) "€")
  "excl;"
  (named-character-ref "excl;" '(#\!) "!")
  "exist;"
  (named-character-ref "exist;" '(#\∃) "∃")
  "expectation;"
  (named-character-ref "expectation;" '(#\ℰ) "ℰ")
  "exponentiale;"
  (named-character-ref "exponentiale;" '(#\ⅇ) "ⅇ")
  "fallingdotseq;"
  (named-character-ref "fallingdotseq;" '(#\≒) "≒")
  "fcy;"
  (named-character-ref "fcy;" '(#\ф) "ф")
  "female;"
  (named-character-ref "female;" '(#\♀) "♀")
  "ffilig;"
  (named-character-ref "ffilig;" '(#\ﬃ) "ﬃ")
  "fflig;"
  (named-character-ref "fflig;" '(#\ﬀ) "ﬀ")
  "ffllig;"
  (named-character-ref "ffllig;" '(#\ﬄ) "ﬄ")
  "ffr;"
  (named-character-ref "ffr;" '(#\𝔣) "𝔣")
  "filig;"
  (named-character-ref "filig;" '(#\ﬁ) "ﬁ")
  "fjlig;"
  (named-character-ref "fjlig;" '(#\f #\j) "fj")
  "flat;"
  (named-character-ref "flat;" '(#\♭) "♭")
  "fllig;"
  (named-character-ref "fllig;" '(#\ﬂ) "ﬂ")
  "fltns;"
  (named-character-ref "fltns;" '(#\▱) "▱")
  "fnof;"
  (named-character-ref "fnof;" '(#\ƒ) "ƒ")
  "fopf;"
  (named-character-ref "fopf;" '(#\𝕗) "𝕗")
  "forall;"
  (named-character-ref "forall;" '(#\∀) "∀")
  "fork;"
  (named-character-ref "fork;" '(#\⋔) "⋔")
  "forkv;"
  (named-character-ref "forkv;" '(#\⫙) "⫙")
  "fpartint;"
  (named-character-ref "fpartint;" '(#\⨍) "⨍")
  "frac12"
  (named-character-ref "frac12" '(#\½) "½")
  "frac12;"
  (named-character-ref "frac12;" '(#\½) "½")
  "frac13;"
  (named-character-ref "frac13;" '(#\⅓) "⅓")
  "frac14"
  (named-character-ref "frac14" '(#\¼) "¼")
  "frac14;"
  (named-character-ref "frac14;" '(#\¼) "¼")
  "frac15;"
  (named-character-ref "frac15;" '(#\⅕) "⅕")
  "frac16;"
  (named-character-ref "frac16;" '(#\⅙) "⅙")
  "frac18;"
  (named-character-ref "frac18;" '(#\⅛) "⅛")
  "frac23;"
  (named-character-ref "frac23;" '(#\⅔) "⅔")
  "frac25;"
  (named-character-ref "frac25;" '(#\⅖) "⅖")
  "frac34"
  (named-character-ref "frac34" '(#\¾) "¾")
  "frac34;"
  (named-character-ref "frac34;" '(#\¾) "¾")
  "frac35;"
  (named-character-ref "frac35;" '(#\⅗) "⅗")
  "frac38;"
  (named-character-ref "frac38;" '(#\⅜) "⅜")
  "frac45;"
  (named-character-ref "frac45;" '(#\⅘) "⅘")
  "frac56;"
  (named-character-ref "frac56;" '(#\⅚) "⅚")
  "frac58;"
  (named-character-ref "frac58;" '(#\⅝) "⅝")
  "frac78;"
  (named-character-ref "frac78;" '(#\⅞) "⅞")
  "frasl;"
  (named-character-ref "frasl;" '(#\⁄) "⁄")
  "frown;"
  (named-character-ref "frown;" '(#\⌢) "⌢")
  "fscr;"
  (named-character-ref "fscr;" '(#\𝒻) "𝒻")
  "gE;"
  (named-character-ref "gE;" '(#\≧) "≧")
  "gEl;"
  (named-character-ref "gEl;" '(#\⪌) "⪌")
  "gacute;"
  (named-character-ref "gacute;" '(#\ǵ) "ǵ")
  "gamma;"
  (named-character-ref "gamma;" '(#\γ) "γ")
  "gammad;"
  (named-character-ref "gammad;" '(#\ϝ) "ϝ")
  "gap;"
  (named-character-ref "gap;" '(#\⪆) "⪆")
  "gbreve;"
  (named-character-ref "gbreve;" '(#\ğ) "ğ")
  "gcirc;"
  (named-character-ref "gcirc;" '(#\ĝ) "ĝ")
  "gcy;"
  (named-character-ref "gcy;" '(#\г) "г")
  "gdot;"
  (named-character-ref "gdot;" '(#\ġ) "ġ")
  "ge;"
  (named-character-ref "ge;" '(#\≥) "≥")
  "gel;"
  (named-character-ref "gel;" '(#\⋛) "⋛")
  "geq;"
  (named-character-ref "geq;" '(#\≥) "≥")
  "geqq;"
  (named-character-ref "geqq;" '(#\≧) "≧")
  "geqslant;"
  (named-character-ref "geqslant;" '(#\⩾) "⩾")
  "ges;"
  (named-character-ref "ges;" '(#\⩾) "⩾")
  "gescc;"
  (named-character-ref "gescc;" '(#\⪩) "⪩")
  "gesdot;"
  (named-character-ref "gesdot;" '(#\⪀) "⪀")
  "gesdoto;"
  (named-character-ref "gesdoto;" '(#\⪂) "⪂")
  "gesdotol;"
  (named-character-ref "gesdotol;" '(#\⪄) "⪄")
  "gesl;"
  (named-character-ref "gesl;" '(#\⋛ #\︀) "⋛︀")
  "gesles;"
  (named-character-ref "gesles;" '(#\⪔) "⪔")
  "gfr;"
  (named-character-ref "gfr;" '(#\𝔤) "𝔤")
  "gg;"
  (named-character-ref "gg;" '(#\≫) "≫")
  "ggg;"
  (named-character-ref "ggg;" '(#\⋙) "⋙")
  "gimel;"
  (named-character-ref "gimel;" '(#\ℷ) "ℷ")
  "gjcy;"
  (named-character-ref "gjcy;" '(#\ѓ) "ѓ")
  "gl;"
  (named-character-ref "gl;" '(#\≷) "≷")
  "glE;"
  (named-character-ref "glE;" '(#\⪒) "⪒")
  "gla;"
  (named-character-ref "gla;" '(#\⪥) "⪥")
  "glj;"
  (named-character-ref "glj;" '(#\⪤) "⪤")
  "gnE;"
  (named-character-ref "gnE;" '(#\≩) "≩")
  "gnap;"
  (named-character-ref "gnap;" '(#\⪊) "⪊")
  "gnapprox;"
  (named-character-ref "gnapprox;" '(#\⪊) "⪊")
  "gne;"
  (named-character-ref "gne;" '(#\⪈) "⪈")
  "gneq;"
  (named-character-ref "gneq;" '(#\⪈) "⪈")
  "gneqq;"
  (named-character-ref "gneqq;" '(#\≩) "≩")
  "gnsim;"
  (named-character-ref "gnsim;" '(#\⋧) "⋧")
  "gopf;"
  (named-character-ref "gopf;" '(#\𝕘) "𝕘")
  "grave;"
  (named-character-ref "grave;" '(#\`) "`")
  "gscr;"
  (named-character-ref "gscr;" '(#\ℊ) "ℊ")
  "gsim;"
  (named-character-ref "gsim;" '(#\≳) "≳")
  "gsime;"
  (named-character-ref "gsime;" '(#\⪎) "⪎")
  "gsiml;"
  (named-character-ref "gsiml;" '(#\⪐) "⪐")
  "gt"
  (named-character-ref "gt" '(#\>) "&gt;")
  "gt;"
  (named-character-ref "gt;" '(#\>) "&gt;")
  "gtcc;"
  (named-character-ref "gtcc;" '(#\⪧) "⪧")
  "gtcir;"
  (named-character-ref "gtcir;" '(#\⩺) "⩺")
  "gtdot;"
  (named-character-ref "gtdot;" '(#\⋗) "⋗")
  "gtlPar;"
  (named-character-ref "gtlPar;" '(#\⦕) "⦕")
  "gtquest;"
  (named-character-ref "gtquest;" '(#\⩼) "⩼")
  "gtrapprox;"
  (named-character-ref "gtrapprox;" '(#\⪆) "⪆")
  "gtrarr;"
  (named-character-ref "gtrarr;" '(#\⥸) "⥸")
  "gtrdot;"
  (named-character-ref "gtrdot;" '(#\⋗) "⋗")
  "gtreqless;"
  (named-character-ref "gtreqless;" '(#\⋛) "⋛")
  "gtreqqless;"
  (named-character-ref "gtreqqless;" '(#\⪌) "⪌")
  "gtrless;"
  (named-character-ref "gtrless;" '(#\≷) "≷")
  "gtrsim;"
  (named-character-ref "gtrsim;" '(#\≳) "≳")
  "gvertneqq;"
  (named-character-ref "gvertneqq;" '(#\≩ #\︀) "≩︀")
  "gvnE;"
  (named-character-ref "gvnE;" '(#\≩ #\︀) "≩︀")
  "hArr;"
  (named-character-ref "hArr;" '(#\⇔) "⇔")
  "hairsp;"
  (named-character-ref "hairsp;" '(#\u200A) " ")
  "half;"
  (named-character-ref "half;" '(#\½) "½")
  "hamilt;"
  (named-character-ref "hamilt;" '(#\ℋ) "ℋ")
  "hardcy;"
  (named-character-ref "hardcy;" '(#\ъ) "ъ")
  "harr;"
  (named-character-ref "harr;" '(#\↔) "↔")
  "harrcir;"
  (named-character-ref "harrcir;" '(#\⥈) "⥈")
  "harrw;"
  (named-character-ref "harrw;" '(#\↭) "↭")
  "hbar;"
  (named-character-ref "hbar;" '(#\ℏ) "ℏ")
  "hcirc;"
  (named-character-ref "hcirc;" '(#\ĥ) "ĥ")
  "hearts;"
  (named-character-ref "hearts;" '(#\♥) "♥")
  "heartsuit;"
  (named-character-ref "heartsuit;" '(#\♥) "♥")
  "hellip;"
  (named-character-ref "hellip;" '(#\…) "…")
  "hercon;"
  (named-character-ref "hercon;" '(#\⊹) "⊹")
  "hfr;"
  (named-character-ref "hfr;" '(#\𝔥) "𝔥")
  "hksearow;"
  (named-character-ref "hksearow;" '(#\⤥) "⤥")
  "hkswarow;"
  (named-character-ref "hkswarow;" '(#\⤦) "⤦")
  "hoarr;"
  (named-character-ref "hoarr;" '(#\⇿) "⇿")
  "homtht;"
  (named-character-ref "homtht;" '(#\∻) "∻")
  "hookleftarrow;"
  (named-character-ref "hookleftarrow;" '(#\↩) "↩")
  "hookrightarrow;"
  (named-character-ref "hookrightarrow;" '(#\↪) "↪")
  "hopf;"
  (named-character-ref "hopf;" '(#\𝕙) "𝕙")
  "horbar;"
  (named-character-ref "horbar;" '(#\―) "―")
  "hscr;"
  (named-character-ref "hscr;" '(#\𝒽) "𝒽")
  "hslash;"
  (named-character-ref "hslash;" '(#\ℏ) "ℏ")
  "hstrok;"
  (named-character-ref "hstrok;" '(#\ħ) "ħ")
  "hybull;"
  (named-character-ref "hybull;" '(#\⁃) "⁃")
  "hyphen;"
  (named-character-ref "hyphen;" '(#\‐) "‐")
  "iacute"
  (named-character-ref "iacute" '(#\í) "í")
  "iacute;"
  (named-character-ref "iacute;" '(#\í) "í")
  "ic;"
  (named-character-ref "ic;" '(#\u2063) "\u2063")
  "icirc"
  (named-character-ref "icirc" '(#\î) "î")
  "icirc;"
  (named-character-ref "icirc;" '(#\î) "î")
  "icy;"
  (named-character-ref "icy;" '(#\и) "и")
  "iecy;"
  (named-character-ref "iecy;" '(#\е) "е")
  "iexcl"
  (named-character-ref "iexcl" '(#\¡) "¡")
  "iexcl;"
  (named-character-ref "iexcl;" '(#\¡) "¡")
  "iff;"
  (named-character-ref "iff;" '(#\⇔) "⇔")
  "ifr;"
  (named-character-ref "ifr;" '(#\𝔦) "𝔦")
  "igrave"
  (named-character-ref "igrave" '(#\ì) "ì")
  "igrave;"
  (named-character-ref "igrave;" '(#\ì) "ì")
  "ii;"
  (named-character-ref "ii;" '(#\ⅈ) "ⅈ")
  "iiiint;"
  (named-character-ref "iiiint;" '(#\⨌) "⨌")
  "iiint;"
  (named-character-ref "iiint;" '(#\∭) "∭")
  "iinfin;"
  (named-character-ref "iinfin;" '(#\⧜) "⧜")
  "iiota;"
  (named-character-ref "iiota;" '(#\℩) "℩")
  "ijlig;"
  (named-character-ref "ijlig;" '(#\ĳ) "ĳ")
  "imacr;"
  (named-character-ref "imacr;" '(#\ī) "ī")
  "image;"
  (named-character-ref "image;" '(#\ℑ) "ℑ")
  "imagline;"
  (named-character-ref "imagline;" '(#\ℐ) "ℐ")
  "imagpart;"
  (named-character-ref "imagpart;" '(#\ℑ) "ℑ")
  "imath;"
  (named-character-ref "imath;" '(#\ı) "ı")
  "imof;"
  (named-character-ref "imof;" '(#\⊷) "⊷")
  "imped;"
  (named-character-ref "imped;" '(#\Ƶ) "Ƶ")
  "in;"
  (named-character-ref "in;" '(#\∈) "∈")
  "incare;"
  (named-character-ref "incare;" '(#\℅) "℅")
  "infin;"
  (named-character-ref "infin;" '(#\∞) "∞")
  "infintie;"
  (named-character-ref "infintie;" '(#\⧝) "⧝")
  "inodot;"
  (named-character-ref "inodot;" '(#\ı) "ı")
  "int;"
  (named-character-ref "int;" '(#\∫) "∫")
  "intcal;"
  (named-character-ref "intcal;" '(#\⊺) "⊺")
  "integers;"
  (named-character-ref "integers;" '(#\ℤ) "ℤ")
  "intercal;"
  (named-character-ref "intercal;" '(#\⊺) "⊺")
  "intlarhk;"
  (named-character-ref "intlarhk;" '(#\⨗) "⨗")
  "intprod;"
  (named-character-ref "intprod;" '(#\⨼) "⨼")
  "iocy;"
  (named-character-ref "iocy;" '(#\ё) "ё")
  "iogon;"
  (named-character-ref "iogon;" '(#\į) "į")
  "iopf;"
  (named-character-ref "iopf;" '(#\𝕚) "𝕚")
  "iota;"
  (named-character-ref "iota;" '(#\ι) "ι")
  "iprod;"
  (named-character-ref "iprod;" '(#\⨼) "⨼")
  "iquest"
  (named-character-ref "iquest" '(#\¿) "¿")
  "iquest;"
  (named-character-ref "iquest;" '(#\¿) "¿")
  "iscr;"
  (named-character-ref "iscr;" '(#\𝒾) "𝒾")
  "isin;"
  (named-character-ref "isin;" '(#\∈) "∈")
  "isinE;"
  (named-character-ref "isinE;" '(#\⋹) "⋹")
  "isindot;"
  (named-character-ref "isindot;" '(#\⋵) "⋵")
  "isins;"
  (named-character-ref "isins;" '(#\⋴) "⋴")
  "isinsv;"
  (named-character-ref "isinsv;" '(#\⋳) "⋳")
  "isinv;"
  (named-character-ref "isinv;" '(#\∈) "∈")
  "it;"
  (named-character-ref "it;" '(#\u2062) "\u2062")
  "itilde;"
  (named-character-ref "itilde;" '(#\ĩ) "ĩ")
  "iukcy;"
  (named-character-ref "iukcy;" '(#\і) "і")
  "iuml"
  (named-character-ref "iuml" '(#\ï) "ï")
  "iuml;"
  (named-character-ref "iuml;" '(#\ï) "ï")
  "jcirc;"
  (named-character-ref "jcirc;" '(#\ĵ) "ĵ")
  "jcy;"
  (named-character-ref "jcy;" '(#\й) "й")
  "jfr;"
  (named-character-ref "jfr;" '(#\𝔧) "𝔧")
  "jmath;"
  (named-character-ref "jmath;" '(#\ȷ) "ȷ")
  "jopf;"
  (named-character-ref "jopf;" '(#\𝕛) "𝕛")
  "jscr;"
  (named-character-ref "jscr;" '(#\𝒿) "𝒿")
  "jsercy;"
  (named-character-ref "jsercy;" '(#\ј) "ј")
  "jukcy;"
  (named-character-ref "jukcy;" '(#\є) "є")
  "kappa;"
  (named-character-ref "kappa;" '(#\κ) "κ")
  "kappav;"
  (named-character-ref "kappav;" '(#\ϰ) "ϰ")
  "kcedil;"
  (named-character-ref "kcedil;" '(#\ķ) "ķ")
  "kcy;"
  (named-character-ref "kcy;" '(#\к) "к")
  "kfr;"
  (named-character-ref "kfr;" '(#\𝔨) "𝔨")
  "kgreen;"
  (named-character-ref "kgreen;" '(#\ĸ) "ĸ")
  "khcy;"
  (named-character-ref "khcy;" '(#\х) "х")
  "kjcy;"
  (named-character-ref "kjcy;" '(#\ќ) "ќ")
  "kopf;"
  (named-character-ref "kopf;" '(#\𝕜) "𝕜")
  "kscr;"
  (named-character-ref "kscr;" '(#\𝓀) "𝓀")
  "lAarr;"
  (named-character-ref "lAarr;" '(#\⇚) "⇚")
  "lArr;"
  (named-character-ref "lArr;" '(#\⇐) "⇐")
  "lAtail;"
  (named-character-ref "lAtail;" '(#\⤛) "⤛")
  "lBarr;"
  (named-character-ref "lBarr;" '(#\⤎) "⤎")
  "lE;"
  (named-character-ref "lE;" '(#\≦) "≦")
  "lEg;"
  (named-character-ref "lEg;" '(#\⪋) "⪋")
  "lHar;"
  (named-character-ref "lHar;" '(#\⥢) "⥢")
  "lacute;"
  (named-character-ref "lacute;" '(#\ĺ) "ĺ")
  "laemptyv;"
  (named-character-ref "laemptyv;" '(#\⦴) "⦴")
  "lagran;"
  (named-character-ref "lagran;" '(#\ℒ) "ℒ")
  "lambda;"
  (named-character-ref "lambda;" '(#\λ) "λ")
  "lang;"
  (named-character-ref "lang;" '(#\⟨) "⟨")
  "langd;"
  (named-character-ref "langd;" '(#\⦑) "⦑")
  "langle;"
  (named-character-ref "langle;" '(#\⟨) "⟨")
  "lap;"
  (named-character-ref "lap;" '(#\⪅) "⪅")
  "laquo"
  (named-character-ref "laquo" '(#\«) "«")
  "laquo;"
  (named-character-ref "laquo;" '(#\«) "«")
  "larr;"
  (named-character-ref "larr;" '(#\←) "←")
  "larrb;"
  (named-character-ref "larrb;" '(#\⇤) "⇤")
  "larrbfs;"
  (named-character-ref "larrbfs;" '(#\⤟) "⤟")
  "larrfs;"
  (named-character-ref "larrfs;" '(#\⤝) "⤝")
  "larrhk;"
  (named-character-ref "larrhk;" '(#\↩) "↩")
  "larrlp;"
  (named-character-ref "larrlp;" '(#\↫) "↫")
  "larrpl;"
  (named-character-ref "larrpl;" '(#\⤹) "⤹")
  "larrsim;"
  (named-character-ref "larrsim;" '(#\⥳) "⥳")
  "larrtl;"
  (named-character-ref "larrtl;" '(#\↢) "↢")
  "lat;"
  (named-character-ref "lat;" '(#\⪫) "⪫")
  "latail;"
  (named-character-ref "latail;" '(#\⤙) "⤙")
  "late;"
  (named-character-ref "late;" '(#\⪭) "⪭")
  "lates;"
  (named-character-ref "lates;" '(#\⪭ #\︀) "⪭︀")
  "lbarr;"
  (named-character-ref "lbarr;" '(#\⤌) "⤌")
  "lbbrk;"
  (named-character-ref "lbbrk;" '(#\❲) "❲")
  "lbrace;"
  (named-character-ref "lbrace;" '(#\{) "{")
  "lbrack;"
  (named-character-ref "lbrack;" '(#\[) "[")
  "lbrke;"
  (named-character-ref "lbrke;" '(#\⦋) "⦋")
  "lbrksld;"
  (named-character-ref "lbrksld;" '(#\⦏) "⦏")
  "lbrkslu;"
  (named-character-ref "lbrkslu;" '(#\⦍) "⦍")
  "lcaron;"
  (named-character-ref "lcaron;" '(#\ľ) "ľ")
  "lcedil;"
  (named-character-ref "lcedil;" '(#\ļ) "ļ")
  "lceil;"
  (named-character-ref "lceil;" '(#\⌈) "⌈")
  "lcub;"
  (named-character-ref "lcub;" '(#\{) "{")
  "lcy;"
  (named-character-ref "lcy;" '(#\л) "л")
  "ldca;"
  (named-character-ref "ldca;" '(#\⤶) "⤶")
  "ldquo;"
  (named-character-ref "ldquo;" '(#\“) "“")
  "ldquor;"
  (named-character-ref "ldquor;" '(#\„) "„")
  "ldrdhar;"
  (named-character-ref "ldrdhar;" '(#\⥧) "⥧")
  "ldrushar;"
  (named-character-ref "ldrushar;" '(#\⥋) "⥋")
  "ldsh;"
  (named-character-ref "ldsh;" '(#\↲) "↲")
  "le;"
  (named-character-ref "le;" '(#\≤) "≤")
  "leftarrow;"
  (named-character-ref "leftarrow;" '(#\←) "←")
  "leftarrowtail;"
  (named-character-ref "leftarrowtail;" '(#\↢) "↢")
  "leftharpoondown;"
  (named-character-ref "leftharpoondown;" '(#\↽) "↽")
  "leftharpoonup;"
  (named-character-ref "leftharpoonup;" '(#\↼) "↼")
  "leftleftarrows;"
  (named-character-ref "leftleftarrows;" '(#\⇇) "⇇")
  "leftrightarrow;"
  (named-character-ref "leftrightarrow;" '(#\↔) "↔")
  "leftrightarrows;"
  (named-character-ref "leftrightarrows;" '(#\⇆) "⇆")
  "leftrightharpoons;"
  (named-character-ref "leftrightharpoons;" '(#\⇋) "⇋")
  "leftrightsquigarrow;"
  (named-character-ref "leftrightsquigarrow;" '(#\↭) "↭")
  "leftthreetimes;"
  (named-character-ref "leftthreetimes;" '(#\⋋) "⋋")
  "leg;"
  (named-character-ref "leg;" '(#\⋚) "⋚")
  "leq;"
  (named-character-ref "leq;" '(#\≤) "≤")
  "leqq;"
  (named-character-ref "leqq;" '(#\≦) "≦")
  "leqslant;"
  (named-character-ref "leqslant;" '(#\⩽) "⩽")
  "les;"
  (named-character-ref "les;" '(#\⩽) "⩽")
  "lescc;"
  (named-character-ref "lescc;" '(#\⪨) "⪨")
  "lesdot;"
  (named-character-ref "lesdot;" '(#\⩿) "⩿")
  "lesdoto;"
  (named-character-ref "lesdoto;" '(#\⪁) "⪁")
  "lesdotor;"
  (named-character-ref "lesdotor;" '(#\⪃) "⪃")
  "lesg;"
  (named-character-ref "lesg;" '(#\⋚ #\︀) "⋚︀")
  "lesges;"
  (named-character-ref "lesges;" '(#\⪓) "⪓")
  "lessapprox;"
  (named-character-ref "lessapprox;" '(#\⪅) "⪅")
  "lessdot;"
  (named-character-ref "lessdot;" '(#\⋖) "⋖")
  "lesseqgtr;"
  (named-character-ref "lesseqgtr;" '(#\⋚) "⋚")
  "lesseqqgtr;"
  (named-character-ref "lesseqqgtr;" '(#\⪋) "⪋")
  "lessgtr;"
  (named-character-ref "lessgtr;" '(#\≶) "≶")
  "lesssim;"
  (named-character-ref "lesssim;" '(#\≲) "≲")
  "lfisht;"
  (named-character-ref "lfisht;" '(#\⥼) "⥼")
  "lfloor;"
  (named-character-ref "lfloor;" '(#\⌊) "⌊")
  "lfr;"
  (named-character-ref "lfr;" '(#\𝔩) "𝔩")
  "lg;"
  (named-character-ref "lg;" '(#\≶) "≶")
  "lgE;"
  (named-character-ref "lgE;" '(#\⪑) "⪑")
  "lhard;"
  (named-character-ref "lhard;" '(#\↽) "↽")
  "lharu;"
  (named-character-ref "lharu;" '(#\↼) "↼")
  "lharul;"
  (named-character-ref "lharul;" '(#\⥪) "⥪")
  "lhblk;"
  (named-character-ref "lhblk;" '(#\▄) "▄")
  "ljcy;"
  (named-character-ref "ljcy;" '(#\љ) "љ")
  "ll;"
  (named-character-ref "ll;" '(#\≪) "≪")
  "llarr;"
  (named-character-ref "llarr;" '(#\⇇) "⇇")
  "llcorner;"
  (named-character-ref "llcorner;" '(#\⌞) "⌞")
  "llhard;"
  (named-character-ref "llhard;" '(#\⥫) "⥫")
  "lltri;"
  (named-character-ref "lltri;" '(#\◺) "◺")
  "lmidot;"
  (named-character-ref "lmidot;" '(#\ŀ) "ŀ")
  "lmoust;"
  (named-character-ref "lmoust;" '(#\⎰) "⎰")
  "lmoustache;"
  (named-character-ref "lmoustache;" '(#\⎰) "⎰")
  "lnE;"
  (named-character-ref "lnE;" '(#\≨) "≨")
  "lnap;"
  (named-character-ref "lnap;" '(#\⪉) "⪉")
  "lnapprox;"
  (named-character-ref "lnapprox;" '(#\⪉) "⪉")
  "lne;"
  (named-character-ref "lne;" '(#\⪇) "⪇")
  "lneq;"
  (named-character-ref "lneq;" '(#\⪇) "⪇")
  "lneqq;"
  (named-character-ref "lneqq;" '(#\≨) "≨")
  "lnsim;"
  (named-character-ref "lnsim;" '(#\⋦) "⋦")
  "loang;"
  (named-character-ref "loang;" '(#\⟬) "⟬")
  "loarr;"
  (named-character-ref "loarr;" '(#\⇽) "⇽")
  "lobrk;"
  (named-character-ref "lobrk;" '(#\⟦) "⟦")
  "longleftarrow;"
  (named-character-ref "longleftarrow;" '(#\⟵) "⟵")
  "longleftrightarrow;"
  (named-character-ref "longleftrightarrow;" '(#\⟷) "⟷")
  "longmapsto;"
  (named-character-ref "longmapsto;" '(#\⟼) "⟼")
  "longrightarrow;"
  (named-character-ref "longrightarrow;" '(#\⟶) "⟶")
  "looparrowleft;"
  (named-character-ref "looparrowleft;" '(#\↫) "↫")
  "looparrowright;"
  (named-character-ref "looparrowright;" '(#\↬) "↬")
  "lopar;"
  (named-character-ref "lopar;" '(#\⦅) "⦅")
  "lopf;"
  (named-character-ref "lopf;" '(#\𝕝) "𝕝")
  "loplus;"
  (named-character-ref "loplus;" '(#\⨭) "⨭")
  "lotimes;"
  (named-character-ref "lotimes;" '(#\⨴) "⨴")
  "lowast;"
  (named-character-ref "lowast;" '(#\∗) "∗")
  "lowbar;"
  (named-character-ref "lowbar;" '(#\_) "_")
  "loz;"
  (named-character-ref "loz;" '(#\◊) "◊")
  "lozenge;"
  (named-character-ref "lozenge;" '(#\◊) "◊")
  "lozf;"
  (named-character-ref "lozf;" '(#\⧫) "⧫")
  "lpar;"
  (named-character-ref "lpar;" '(#\() "(")
  "lparlt;"
  (named-character-ref "lparlt;" '(#\⦓) "⦓")
  "lrarr;"
  (named-character-ref "lrarr;" '(#\⇆) "⇆")
  "lrcorner;"
  (named-character-ref "lrcorner;" '(#\⌟) "⌟")
  "lrhar;"
  (named-character-ref "lrhar;" '(#\⇋) "⇋")
  "lrhard;"
  (named-character-ref "lrhard;" '(#\⥭) "⥭")
  "lrm;"
  (named-character-ref "lrm;" '(#\u200E) "\u200E")
  "lrtri;"
  (named-character-ref "lrtri;" '(#\⊿) "⊿")
  "lsaquo;"
  (named-character-ref "lsaquo;" '(#\‹) "‹")
  "lscr;"
  (named-character-ref "lscr;" '(#\𝓁) "𝓁")
  "lsh;"
  (named-character-ref "lsh;" '(#\↰) "↰")
  "lsim;"
  (named-character-ref "lsim;" '(#\≲) "≲")
  "lsime;"
  (named-character-ref "lsime;" '(#\⪍) "⪍")
  "lsimg;"
  (named-character-ref "lsimg;" '(#\⪏) "⪏")
  "lsqb;"
  (named-character-ref "lsqb;" '(#\[) "[")
  "lsquo;"
  (named-character-ref "lsquo;" '(#\‘) "‘")
  "lsquor;"
  (named-character-ref "lsquor;" '(#\‚) "‚")
  "lstrok;"
  (named-character-ref "lstrok;" '(#\ł) "ł")
  "lt"
  (named-character-ref "lt" '(#\<) "&lt;")
  "lt;"
  (named-character-ref "lt;" '(#\<) "&lt;")
  "ltcc;"
  (named-character-ref "ltcc;" '(#\⪦) "⪦")
  "ltcir;"
  (named-character-ref "ltcir;" '(#\⩹) "⩹")
  "ltdot;"
  (named-character-ref "ltdot;" '(#\⋖) "⋖")
  "lthree;"
  (named-character-ref "lthree;" '(#\⋋) "⋋")
  "ltimes;"
  (named-character-ref "ltimes;" '(#\⋉) "⋉")
  "ltlarr;"
  (named-character-ref "ltlarr;" '(#\⥶) "⥶")
  "ltquest;"
  (named-character-ref "ltquest;" '(#\⩻) "⩻")
  "ltrPar;"
  (named-character-ref "ltrPar;" '(#\⦖) "⦖")
  "ltri;"
  (named-character-ref "ltri;" '(#\◃) "◃")
  "ltrie;"
  (named-character-ref "ltrie;" '(#\⊴) "⊴")
  "ltrif;"
  (named-character-ref "ltrif;" '(#\◂) "◂")
  "lurdshar;"
  (named-character-ref "lurdshar;" '(#\⥊) "⥊")
  "luruhar;"
  (named-character-ref "luruhar;" '(#\⥦) "⥦")
  "lvertneqq;"
  (named-character-ref "lvertneqq;" '(#\≨ #\︀) "≨︀")
  "lvnE;"
  (named-character-ref "lvnE;" '(#\≨ #\︀) "≨︀")
  "mDDot;"
  (named-character-ref "mDDot;" '(#\∺) "∺")
  "macr"
  (named-character-ref "macr" '(#\¯) "¯")
  "macr;"
  (named-character-ref "macr;" '(#\¯) "¯")
  "male;"
  (named-character-ref "male;" '(#\♂) "♂")
  "malt;"
  (named-character-ref "malt;" '(#\✠) "✠")
  "maltese;"
  (named-character-ref "maltese;" '(#\✠) "✠")
  "map;"
  (named-character-ref "map;" '(#\↦) "↦")
  "mapsto;"
  (named-character-ref "mapsto;" '(#\↦) "↦")
  "mapstodown;"
  (named-character-ref "mapstodown;" '(#\↧) "↧")
  "mapstoleft;"
  (named-character-ref "mapstoleft;" '(#\↤) "↤")
  "mapstoup;"
  (named-character-ref "mapstoup;" '(#\↥) "↥")
  "marker;"
  (named-character-ref "marker;" '(#\▮) "▮")
  "mcomma;"
  (named-character-ref "mcomma;" '(#\⨩) "⨩")
  "mcy;"
  (named-character-ref "mcy;" '(#\м) "м")
  "mdash;"
  (named-character-ref "mdash;" '(#\—) "—")
  "measuredangle;"
  (named-character-ref "measuredangle;" '(#\∡) "∡")
  "mfr;"
  (named-character-ref "mfr;" '(#\𝔪) "𝔪")
  "mho;"
  (named-character-ref "mho;" '(#\℧) "℧")
  "micro"
  (named-character-ref "micro" '(#\µ) "µ")
  "micro;"
  (named-character-ref "micro;" '(#\µ) "µ")
  "mid;"
  (named-character-ref "mid;" '(#\∣) "∣")
  "midast;"
  (named-character-ref "midast;" '(#\*) "*")
  "midcir;"
  (named-character-ref "midcir;" '(#\⫰) "⫰")
  "middot"
  (named-character-ref "middot" '(#\·) "·")
  "middot;"
  (named-character-ref "middot;" '(#\·) "·")
  "minus;"
  (named-character-ref "minus;" '(#\−) "−")
  "minusb;"
  (named-character-ref "minusb;" '(#\⊟) "⊟")
  "minusd;"
  (named-character-ref "minusd;" '(#\∸) "∸")
  "minusdu;"
  (named-character-ref "minusdu;" '(#\⨪) "⨪")
  "mlcp;"
  (named-character-ref "mlcp;" '(#\⫛) "⫛")
  "mldr;"
  (named-character-ref "mldr;" '(#\…) "…")
  "mnplus;"
  (named-character-ref "mnplus;" '(#\∓) "∓")
  "models;"
  (named-character-ref "models;" '(#\⊧) "⊧")
  "mopf;"
  (named-character-ref "mopf;" '(#\𝕞) "𝕞")
  "mp;"
  (named-character-ref "mp;" '(#\∓) "∓")
  "mscr;"
  (named-character-ref "mscr;" '(#\𝓂) "𝓂")
  "mstpos;"
  (named-character-ref "mstpos;" '(#\∾) "∾")
  "mu;"
  (named-character-ref "mu;" '(#\μ) "μ")
  "multimap;"
  (named-character-ref "multimap;" '(#\⊸) "⊸")
  "mumap;"
  (named-character-ref "mumap;" '(#\⊸) "⊸")
  "nGg;"
  (named-character-ref "nGg;" '(#\⋙ #\̸) "⋙̸")
  "nGt;"
  (named-character-ref "nGt;" '(#\≫ #\⃒) "≫⃒")
  "nGtv;"
  (named-character-ref "nGtv;" '(#\≫ #\̸) "≫̸")
  "nLeftarrow;"
  (named-character-ref "nLeftarrow;" '(#\⇍) "⇍")
  "nLeftrightarrow;"
  (named-character-ref "nLeftrightarrow;" '(#\⇎) "⇎")
  "nLl;"
  (named-character-ref "nLl;" '(#\⋘ #\̸) "⋘̸")
  "nLt;"
  (named-character-ref "nLt;" '(#\≪ #\⃒) "≪⃒")
  "nLtv;"
  (named-character-ref "nLtv;" '(#\≪ #\̸) "≪̸")
  "nRightarrow;"
  (named-character-ref "nRightarrow;" '(#\⇏) "⇏")
  "nVDash;"
  (named-character-ref "nVDash;" '(#\⊯) "⊯")
  "nVdash;"
  (named-character-ref "nVdash;" '(#\⊮) "⊮")
  "nabla;"
  (named-character-ref "nabla;" '(#\∇) "∇")
  "nacute;"
  (named-character-ref "nacute;" '(#\ń) "ń")
  "nang;"
  (named-character-ref "nang;" '(#\∠ #\⃒) "∠⃒")
  "nap;"
  (named-character-ref "nap;" '(#\≉) "≉")
  "napE;"
  (named-character-ref "napE;" '(#\⩰ #\̸) "⩰̸")
  "napid;"
  (named-character-ref "napid;" '(#\≋ #\̸) "≋̸")
  "napos;"
  (named-character-ref "napos;" '(#\ŉ) "ŉ")
  "napprox;"
  (named-character-ref "napprox;" '(#\≉) "≉")
  "natur;"
  (named-character-ref "natur;" '(#\♮) "♮")
  "natural;"
  (named-character-ref "natural;" '(#\♮) "♮")
  "naturals;"
  (named-character-ref "naturals;" '(#\ℕ) "ℕ")
  "nbsp"
  (named-character-ref "nbsp" '(#\u00A0) "&nbsp;")
  "nbsp;"
  (named-character-ref "nbsp;" '(#\u00A0) "&nbsp;")
  "nbump;"
  (named-character-ref "nbump;" '(#\≎ #\̸) "≎̸")
  "nbumpe;"
  (named-character-ref "nbumpe;" '(#\≏ #\̸) "≏̸")
  "ncap;"
  (named-character-ref "ncap;" '(#\⩃) "⩃")
  "ncaron;"
  (named-character-ref "ncaron;" '(#\ň) "ň")
  "ncedil;"
  (named-character-ref "ncedil;" '(#\ņ) "ņ")
  "ncong;"
  (named-character-ref "ncong;" '(#\≇) "≇")
  "ncongdot;"
  (named-character-ref "ncongdot;" '(#\⩭ #\̸) "⩭̸")
  "ncup;"
  (named-character-ref "ncup;" '(#\⩂) "⩂")
  "ncy;"
  (named-character-ref "ncy;" '(#\н) "н")
  "ndash;"
  (named-character-ref "ndash;" '(#\–) "–")
  "ne;"
  (named-character-ref "ne;" '(#\≠) "≠")
  "neArr;"
  (named-character-ref "neArr;" '(#\⇗) "⇗")
  "nearhk;"
  (named-character-ref "nearhk;" '(#\⤤) "⤤")
  "nearr;"
  (named-character-ref "nearr;" '(#\↗) "↗")
  "nearrow;"
  (named-character-ref "nearrow;" '(#\↗) "↗")
  "nedot;"
  (named-character-ref "nedot;" '(#\≐ #\̸) "≐̸")
  "nequiv;"
  (named-character-ref "nequiv;" '(#\≢) "≢")
  "nesear;"
  (named-character-ref "nesear;" '(#\⤨) "⤨")
  "nesim;"
  (named-character-ref "nesim;" '(#\≂ #\̸) "≂̸")
  "nexist;"
  (named-character-ref "nexist;" '(#\∄) "∄")
  "nexists;"
  (named-character-ref "nexists;" '(#\∄) "∄")
  "nfr;"
  (named-character-ref "nfr;" '(#\𝔫) "𝔫")
  "ngE;"
  (named-character-ref "ngE;" '(#\≧ #\̸) "≧̸")
  "nge;"
  (named-character-ref "nge;" '(#\≱) "≱")
  "ngeq;"
  (named-character-ref "ngeq;" '(#\≱) "≱")
  "ngeqq;"
  (named-character-ref "ngeqq;" '(#\≧ #\̸) "≧̸")
  "ngeqslant;"
  (named-character-ref "ngeqslant;" '(#\⩾ #\̸) "⩾̸")
  "nges;"
  (named-character-ref "nges;" '(#\⩾ #\̸) "⩾̸")
  "ngsim;"
  (named-character-ref "ngsim;" '(#\≵) "≵")
  "ngt;"
  (named-character-ref "ngt;" '(#\≯) "≯")
  "ngtr;"
  (named-character-ref "ngtr;" '(#\≯) "≯")
  "nhArr;"
  (named-character-ref "nhArr;" '(#\⇎) "⇎")
  "nharr;"
  (named-character-ref "nharr;" '(#\↮) "↮")
  "nhpar;"
  (named-character-ref "nhpar;" '(#\⫲) "⫲")
  "ni;"
  (named-character-ref "ni;" '(#\∋) "∋")
  "nis;"
  (named-character-ref "nis;" '(#\⋼) "⋼")
  "nisd;"
  (named-character-ref "nisd;" '(#\⋺) "⋺")
  "niv;"
  (named-character-ref "niv;" '(#\∋) "∋")
  "njcy;"
  (named-character-ref "njcy;" '(#\њ) "њ")
  "nlArr;"
  (named-character-ref "nlArr;" '(#\⇍) "⇍")
  "nlE;"
  (named-character-ref "nlE;" '(#\≦ #\̸) "≦̸")
  "nlarr;"
  (named-character-ref "nlarr;" '(#\↚) "↚")
  "nldr;"
  (named-character-ref "nldr;" '(#\‥) "‥")
  "nle;"
  (named-character-ref "nle;" '(#\≰) "≰")
  "nleftarrow;"
  (named-character-ref "nleftarrow;" '(#\↚) "↚")
  "nleftrightarrow;"
  (named-character-ref "nleftrightarrow;" '(#\↮) "↮")
  "nleq;"
  (named-character-ref "nleq;" '(#\≰) "≰")
  "nleqq;"
  (named-character-ref "nleqq;" '(#\≦ #\̸) "≦̸")
  "nleqslant;"
  (named-character-ref "nleqslant;" '(#\⩽ #\̸) "⩽̸")
  "nles;"
  (named-character-ref "nles;" '(#\⩽ #\̸) "⩽̸")
  "nless;"
  (named-character-ref "nless;" '(#\≮) "≮")
  "nlsim;"
  (named-character-ref "nlsim;" '(#\≴) "≴")
  "nlt;"
  (named-character-ref "nlt;" '(#\≮) "≮")
  "nltri;"
  (named-character-ref "nltri;" '(#\⋪) "⋪")
  "nltrie;"
  (named-character-ref "nltrie;" '(#\⋬) "⋬")
  "nmid;"
  (named-character-ref "nmid;" '(#\∤) "∤")
  "nopf;"
  (named-character-ref "nopf;" '(#\𝕟) "𝕟")
  "not"
  (named-character-ref "not" '(#\¬) "¬")
  "not;"
  (named-character-ref "not;" '(#\¬) "¬")
  "notin;"
  (named-character-ref "notin;" '(#\∉) "∉")
  "notinE;"
  (named-character-ref "notinE;" '(#\⋹ #\̸) "⋹̸")
  "notindot;"
  (named-character-ref "notindot;" '(#\⋵ #\̸) "⋵̸")
  "notinva;"
  (named-character-ref "notinva;" '(#\∉) "∉")
  "notinvb;"
  (named-character-ref "notinvb;" '(#\⋷) "⋷")
  "notinvc;"
  (named-character-ref "notinvc;" '(#\⋶) "⋶")
  "notni;"
  (named-character-ref "notni;" '(#\∌) "∌")
  "notniva;"
  (named-character-ref "notniva;" '(#\∌) "∌")
  "notnivb;"
  (named-character-ref "notnivb;" '(#\⋾) "⋾")
  "notnivc;"
  (named-character-ref "notnivc;" '(#\⋽) "⋽")
  "npar;"
  (named-character-ref "npar;" '(#\∦) "∦")
  "nparallel;"
  (named-character-ref "nparallel;" '(#\∦) "∦")
  "nparsl;"
  (named-character-ref "nparsl;" '(#\⫽ #\⃥) "⫽⃥")
  "npart;"
  (named-character-ref "npart;" '(#\∂ #\̸) "∂̸")
  "npolint;"
  (named-character-ref "npolint;" '(#\⨔) "⨔")
  "npr;"
  (named-character-ref "npr;" '(#\⊀) "⊀")
  "nprcue;"
  (named-character-ref "nprcue;" '(#\⋠) "⋠")
  "npre;"
  (named-character-ref "npre;" '(#\⪯ #\̸) "⪯̸")
  "nprec;"
  (named-character-ref "nprec;" '(#\⊀) "⊀")
  "npreceq;"
  (named-character-ref "npreceq;" '(#\⪯ #\̸) "⪯̸")
  "nrArr;"
  (named-character-ref "nrArr;" '(#\⇏) "⇏")
  "nrarr;"
  (named-character-ref "nrarr;" '(#\↛) "↛")
  "nrarrc;"
  (named-character-ref "nrarrc;" '(#\⤳ #\̸) "⤳̸")
  "nrarrw;"
  (named-character-ref "nrarrw;" '(#\↝ #\̸) "↝̸")
  "nrightarrow;"
  (named-character-ref "nrightarrow;" '(#\↛) "↛")
  "nrtri;"
  (named-character-ref "nrtri;" '(#\⋫) "⋫")
  "nrtrie;"
  (named-character-ref "nrtrie;" '(#\⋭) "⋭")
  "nsc;"
  (named-character-ref "nsc;" '(#\⊁) "⊁")
  "nsccue;"
  (named-character-ref "nsccue;" '(#\⋡) "⋡")
  "nsce;"
  (named-character-ref "nsce;" '(#\⪰ #\̸) "⪰̸")
  "nscr;"
  (named-character-ref "nscr;" '(#\𝓃) "𝓃")
  "nshortmid;"
  (named-character-ref "nshortmid;" '(#\∤) "∤")
  "nshortparallel;"
  (named-character-ref "nshortparallel;" '(#\∦) "∦")
  "nsim;"
  (named-character-ref "nsim;" '(#\≁) "≁")
  "nsime;"
  (named-character-ref "nsime;" '(#\≄) "≄")
  "nsimeq;"
  (named-character-ref "nsimeq;" '(#\≄) "≄")
  "nsmid;"
  (named-character-ref "nsmid;" '(#\∤) "∤")
  "nspar;"
  (named-character-ref "nspar;" '(#\∦) "∦")
  "nsqsube;"
  (named-character-ref "nsqsube;" '(#\⋢) "⋢")
  "nsqsupe;"
  (named-character-ref "nsqsupe;" '(#\⋣) "⋣")
  "nsub;"
  (named-character-ref "nsub;" '(#\⊄) "⊄")
  "nsubE;"
  (named-character-ref "nsubE;" '(#\⫅ #\̸) "⫅̸")
  "nsube;"
  (named-character-ref "nsube;" '(#\⊈) "⊈")
  "nsubset;"
  (named-character-ref "nsubset;" '(#\⊂ #\⃒) "⊂⃒")
  "nsubseteq;"
  (named-character-ref "nsubseteq;" '(#\⊈) "⊈")
  "nsubseteqq;"
  (named-character-ref "nsubseteqq;" '(#\⫅ #\̸) "⫅̸")
  "nsucc;"
  (named-character-ref "nsucc;" '(#\⊁) "⊁")
  "nsucceq;"
  (named-character-ref "nsucceq;" '(#\⪰ #\̸) "⪰̸")
  "nsup;"
  (named-character-ref "nsup;" '(#\⊅) "⊅")
  "nsupE;"
  (named-character-ref "nsupE;" '(#\⫆ #\̸) "⫆̸")
  "nsupe;"
  (named-character-ref "nsupe;" '(#\⊉) "⊉")
  "nsupset;"
  (named-character-ref "nsupset;" '(#\⊃ #\⃒) "⊃⃒")
  "nsupseteq;"
  (named-character-ref "nsupseteq;" '(#\⊉) "⊉")
  "nsupseteqq;"
  (named-character-ref "nsupseteqq;" '(#\⫆ #\̸) "⫆̸")
  "ntgl;"
  (named-character-ref "ntgl;" '(#\≹) "≹")
  "ntilde"
  (named-character-ref "ntilde" '(#\ñ) "ñ")
  "ntilde;"
  (named-character-ref "ntilde;" '(#\ñ) "ñ")
  "ntlg;"
  (named-character-ref "ntlg;" '(#\≸) "≸")
  "ntriangleleft;"
  (named-character-ref "ntriangleleft;" '(#\⋪) "⋪")
  "ntrianglelefteq;"
  (named-character-ref "ntrianglelefteq;" '(#\⋬) "⋬")
  "ntriangleright;"
  (named-character-ref "ntriangleright;" '(#\⋫) "⋫")
  "ntrianglerighteq;"
  (named-character-ref "ntrianglerighteq;" '(#\⋭) "⋭")
  "nu;"
  (named-character-ref "nu;" '(#\ν) "ν")
  "num;"
  (named-character-ref "num;" '(#\#) "#")
  "numero;"
  (named-character-ref "numero;" '(#\№) "№")
  "numsp;"
  (named-character-ref "numsp;" '(#\u2007) " ")
  "nvDash;"
  (named-character-ref "nvDash;" '(#\⊭) "⊭")
  "nvHarr;"
  (named-character-ref "nvHarr;" '(#\⤄) "⤄")
  "nvap;"
  (named-character-ref "nvap;" '(#\≍ #\⃒) "≍⃒")
  "nvdash;"
  (named-character-ref "nvdash;" '(#\⊬) "⊬")
  "nvge;"
  (named-character-ref "nvge;" '(#\≥ #\⃒) "≥⃒")
  "nvgt;"
  (named-character-ref "nvgt;" '(#\> #\⃒) "&gt;⃒")
  "nvinfin;"
  (named-character-ref "nvinfin;" '(#\⧞) "⧞")
  "nvlArr;"
  (named-character-ref "nvlArr;" '(#\⤂) "⤂")
  "nvle;"
  (named-character-ref "nvle;" '(#\≤ #\⃒) "≤⃒")
  "nvlt;"
  (named-character-ref "nvlt;" '(#\< #\⃒) "&lt;⃒")
  "nvltrie;"
  (named-character-ref "nvltrie;" '(#\⊴ #\⃒) "⊴⃒")
  "nvrArr;"
  (named-character-ref "nvrArr;" '(#\⤃) "⤃")
  "nvrtrie;"
  (named-character-ref "nvrtrie;" '(#\⊵ #\⃒) "⊵⃒")
  "nvsim;"
  (named-character-ref "nvsim;" '(#\∼ #\⃒) "∼⃒")
  "nwArr;"
  (named-character-ref "nwArr;" '(#\⇖) "⇖")
  "nwarhk;"
  (named-character-ref "nwarhk;" '(#\⤣) "⤣")
  "nwarr;"
  (named-character-ref "nwarr;" '(#\↖) "↖")
  "nwarrow;"
  (named-character-ref "nwarrow;" '(#\↖) "↖")
  "nwnear;"
  (named-character-ref "nwnear;" '(#\⤧) "⤧")
  "oS;"
  (named-character-ref "oS;" '(#\Ⓢ) "Ⓢ")
  "oacute"
  (named-character-ref "oacute" '(#\ó) "ó")
  "oacute;"
  (named-character-ref "oacute;" '(#\ó) "ó")
  "oast;"
  (named-character-ref "oast;" '(#\⊛) "⊛")
  "ocir;"
  (named-character-ref "ocir;" '(#\⊚) "⊚")
  "ocirc"
  (named-character-ref "ocirc" '(#\ô) "ô")
  "ocirc;"
  (named-character-ref "ocirc;" '(#\ô) "ô")
  "ocy;"
  (named-character-ref "ocy;" '(#\о) "о")
  "odash;"
  (named-character-ref "odash;" '(#\⊝) "⊝")
  "odblac;"
  (named-character-ref "odblac;" '(#\ő) "ő")
  "odiv;"
  (named-character-ref "odiv;" '(#\⨸) "⨸")
  "odot;"
  (named-character-ref "odot;" '(#\⊙) "⊙")
  "odsold;"
  (named-character-ref "odsold;" '(#\⦼) "⦼")
  "oelig;"
  (named-character-ref "oelig;" '(#\œ) "œ")
  "ofcir;"
  (named-character-ref "ofcir;" '(#\⦿) "⦿")
  "ofr;"
  (named-character-ref "ofr;" '(#\𝔬) "𝔬")
  "ogon;"
  (named-character-ref "ogon;" '(#\˛) "˛")
  "ograve"
  (named-character-ref "ograve" '(#\ò) "ò")
  "ograve;"
  (named-character-ref "ograve;" '(#\ò) "ò")
  "ogt;"
  (named-character-ref "ogt;" '(#\⧁) "⧁")
  "ohbar;"
  (named-character-ref "ohbar;" '(#\⦵) "⦵")
  "ohm;"
  (named-character-ref "ohm;" '(#\Ω) "Ω")
  "oint;"
  (named-character-ref "oint;" '(#\∮) "∮")
  "olarr;"
  (named-character-ref "olarr;" '(#\↺) "↺")
  "olcir;"
  (named-character-ref "olcir;" '(#\⦾) "⦾")
  "olcross;"
  (named-character-ref "olcross;" '(#\⦻) "⦻")
  "oline;"
  (named-character-ref "oline;" '(#\‾) "‾")
  "olt;"
  (named-character-ref "olt;" '(#\⧀) "⧀")
  "omacr;"
  (named-character-ref "omacr;" '(#\ō) "ō")
  "omega;"
  (named-character-ref "omega;" '(#\ω) "ω")
  "omicron;"
  (named-character-ref "omicron;" '(#\ο) "ο")
  "omid;"
  (named-character-ref "omid;" '(#\⦶) "⦶")
  "ominus;"
  (named-character-ref "ominus;" '(#\⊖) "⊖")
  "oopf;"
  (named-character-ref "oopf;" '(#\𝕠) "𝕠")
  "opar;"
  (named-character-ref "opar;" '(#\⦷) "⦷")
  "operp;"
  (named-character-ref "operp;" '(#\⦹) "⦹")
  "oplus;"
  (named-character-ref "oplus;" '(#\⊕) "⊕")
  "or;"
  (named-character-ref "or;" '(#\∨) "∨")
  "orarr;"
  (named-character-ref "orarr;" '(#\↻) "↻")
  "ord;"
  (named-character-ref "ord;" '(#\⩝) "⩝")
  "order;"
  (named-character-ref "order;" '(#\ℴ) "ℴ")
  "orderof;"
  (named-character-ref "orderof;" '(#\ℴ) "ℴ")
  "ordf"
  (named-character-ref "ordf" '(#\ª) "ª")
  "ordf;"
  (named-character-ref "ordf;" '(#\ª) "ª")
  "ordm"
  (named-character-ref "ordm" '(#\º) "º")
  "ordm;"
  (named-character-ref "ordm;" '(#\º) "º")
  "origof;"
  (named-character-ref "origof;" '(#\⊶) "⊶")
  "oror;"
  (named-character-ref "oror;" '(#\⩖) "⩖")
  "orslope;"
  (named-character-ref "orslope;" '(#\⩗) "⩗")
  "orv;"
  (named-character-ref "orv;" '(#\⩛) "⩛")
  "oscr;"
  (named-character-ref "oscr;" '(#\ℴ) "ℴ")
  "oslash"
  (named-character-ref "oslash" '(#\ø) "ø")
  "oslash;"
  (named-character-ref "oslash;" '(#\ø) "ø")
  "osol;"
  (named-character-ref "osol;" '(#\⊘) "⊘")
  "otilde"
  (named-character-ref "otilde" '(#\õ) "õ")
  "otilde;"
  (named-character-ref "otilde;" '(#\õ) "õ")
  "otimes;"
  (named-character-ref "otimes;" '(#\⊗) "⊗")
  "otimesas;"
  (named-character-ref "otimesas;" '(#\⨶) "⨶")
  "ouml"
  (named-character-ref "ouml" '(#\ö) "ö")
  "ouml;"
  (named-character-ref "ouml;" '(#\ö) "ö")
  "ovbar;"
  (named-character-ref "ovbar;" '(#\⌽) "⌽")
  "par;"
  (named-character-ref "par;" '(#\∥) "∥")
  "para"
  (named-character-ref "para" '(#\¶) "¶")
  "para;"
  (named-character-ref "para;" '(#\¶) "¶")
  "parallel;"
  (named-character-ref "parallel;" '(#\∥) "∥")
  "parsim;"
  (named-character-ref "parsim;" '(#\⫳) "⫳")
  "parsl;"
  (named-character-ref "parsl;" '(#\⫽) "⫽")
  "part;"
  (named-character-ref "part;" '(#\∂) "∂")
  "pcy;"
  (named-character-ref "pcy;" '(#\п) "п")
  "percnt;"
  (named-character-ref "percnt;" '(#\%) "%")
  "period;"
  (named-character-ref "period;" '(#\.) ".")
  "permil;"
  (named-character-ref "permil;" '(#\‰) "‰")
  "perp;"
  (named-character-ref "perp;" '(#\⊥) "⊥")
  "pertenk;"
  (named-character-ref "pertenk;" '(#\‱) "‱")
  "pfr;"
  (named-character-ref "pfr;" '(#\𝔭) "𝔭")
  "phi;"
  (named-character-ref "phi;" '(#\φ) "φ")
  "phiv;"
  (named-character-ref "phiv;" '(#\ϕ) "ϕ")
  "phmmat;"
  (named-character-ref "phmmat;" '(#\ℳ) "ℳ")
  "phone;"
  (named-character-ref "phone;" '(#\☎) "☎")
  "pi;"
  (named-character-ref "pi;" '(#\π) "π")
  "pitchfork;"
  (named-character-ref "pitchfork;" '(#\⋔) "⋔")
  "piv;"
  (named-character-ref "piv;" '(#\ϖ) "ϖ")
  "planck;"
  (named-character-ref "planck;" '(#\ℏ) "ℏ")
  "planckh;"
  (named-character-ref "planckh;" '(#\ℎ) "ℎ")
  "plankv;"
  (named-character-ref "plankv;" '(#\ℏ) "ℏ")
  "plus;"
  (named-character-ref "plus;" '(#\+) "+")
  "plusacir;"
  (named-character-ref "plusacir;" '(#\⨣) "⨣")
  "plusb;"
  (named-character-ref "plusb;" '(#\⊞) "⊞")
  "pluscir;"
  (named-character-ref "pluscir;" '(#\⨢) "⨢")
  "plusdo;"
  (named-character-ref "plusdo;" '(#\∔) "∔")
  "plusdu;"
  (named-character-ref "plusdu;" '(#\⨥) "⨥")
  "pluse;"
  (named-character-ref "pluse;" '(#\⩲) "⩲")
  "plusmn"
  (named-character-ref "plusmn" '(#\±) "±")
  "plusmn;"
  (named-character-ref "plusmn;" '(#\±) "±")
  "plussim;"
  (named-character-ref "plussim;" '(#\⨦) "⨦")
  "plustwo;"
  (named-character-ref "plustwo;" '(#\⨧) "⨧")
  "pm;"
  (named-character-ref "pm;" '(#\±) "±")
  "pointint;"
  (named-character-ref "pointint;" '(#\⨕) "⨕")
  "popf;"
  (named-character-ref "popf;" '(#\𝕡) "𝕡")
  "pound"
  (named-character-ref "pound" '(#\£) "£")
  "pound;"
  (named-character-ref "pound;" '(#\£) "£")
  "pr;"
  (named-character-ref "pr;" '(#\≺) "≺")
  "prE;"
  (named-character-ref "prE;" '(#\⪳) "⪳")
  "prap;"
  (named-character-ref "prap;" '(#\⪷) "⪷")
  "prcue;"
  (named-character-ref "prcue;" '(#\≼) "≼")
  "pre;"
  (named-character-ref "pre;" '(#\⪯) "⪯")
  "prec;"
  (named-character-ref "prec;" '(#\≺) "≺")
  "precapprox;"
  (named-character-ref "precapprox;" '(#\⪷) "⪷")
  "preccurlyeq;"
  (named-character-ref "preccurlyeq;" '(#\≼) "≼")
  "preceq;"
  (named-character-ref "preceq;" '(#\⪯) "⪯")
  "precnapprox;"
  (named-character-ref "precnapprox;" '(#\⪹) "⪹")
  "precneqq;"
  (named-character-ref "precneqq;" '(#\⪵) "⪵")
  "precnsim;"
  (named-character-ref "precnsim;" '(#\⋨) "⋨")
  "precsim;"
  (named-character-ref "precsim;" '(#\≾) "≾")
  "prime;"
  (named-character-ref "prime;" '(#\′) "′")
  "primes;"
  (named-character-ref "primes;" '(#\ℙ) "ℙ")
  "prnE;"
  (named-character-ref "prnE;" '(#\⪵) "⪵")
  "prnap;"
  (named-character-ref "prnap;" '(#\⪹) "⪹")
  "prnsim;"
  (named-character-ref "prnsim;" '(#\⋨) "⋨")
  "prod;"
  (named-character-ref "prod;" '(#\∏) "∏")
  "profalar;"
  (named-character-ref "profalar;" '(#\⌮) "⌮")
  "profline;"
  (named-character-ref "profline;" '(#\⌒) "⌒")
  "profsurf;"
  (named-character-ref "profsurf;" '(#\⌓) "⌓")
  "prop;"
  (named-character-ref "prop;" '(#\∝) "∝")
  "propto;"
  (named-character-ref "propto;" '(#\∝) "∝")
  "prsim;"
  (named-character-ref "prsim;" '(#\≾) "≾")
  "prurel;"
  (named-character-ref "prurel;" '(#\⊰) "⊰")
  "pscr;"
  (named-character-ref "pscr;" '(#\𝓅) "𝓅")
  "psi;"
  (named-character-ref "psi;" '(#\ψ) "ψ")
  "puncsp;"
  (named-character-ref "puncsp;" '(#\u2008) " ")
  "qfr;"
  (named-character-ref "qfr;" '(#\𝔮) "𝔮")
  "qint;"
  (named-character-ref "qint;" '(#\⨌) "⨌")
  "qopf;"
  (named-character-ref "qopf;" '(#\𝕢) "𝕢")
  "qprime;"
  (named-character-ref "qprime;" '(#\⁗) "⁗")
  "qscr;"
  (named-character-ref "qscr;" '(#\𝓆) "𝓆")
  "quaternions;"
  (named-character-ref "quaternions;" '(#\ℍ) "ℍ")
  "quatint;"
  (named-character-ref "quatint;" '(#\⨖) "⨖")
  "quest;"
  (named-character-ref "quest;" '(#\?) "?")
  "questeq;"
  (named-character-ref "questeq;" '(#\≟) "≟")
  "quot"
  (named-character-ref "quot" '(#\") "\"")
  "quot;"
  (named-character-ref "quot;" '(#\") "\"")
  "rAarr;"
  (named-character-ref "rAarr;" '(#\⇛) "⇛")
  "rArr;"
  (named-character-ref "rArr;" '(#\⇒) "⇒")
  "rAtail;"
  (named-character-ref "rAtail;" '(#\⤜) "⤜")
  "rBarr;"
  (named-character-ref "rBarr;" '(#\⤏) "⤏")
  "rHar;"
  (named-character-ref "rHar;" '(#\⥤) "⥤")
  "race;"
  (named-character-ref "race;" '(#\∽ #\̱) "∽̱")
  "racute;"
  (named-character-ref "racute;" '(#\ŕ) "ŕ")
  "radic;"
  (named-character-ref "radic;" '(#\√) "√")
  "raemptyv;"
  (named-character-ref "raemptyv;" '(#\⦳) "⦳")
  "rang;"
  (named-character-ref "rang;" '(#\⟩) "⟩")
  "rangd;"
  (named-character-ref "rangd;" '(#\⦒) "⦒")
  "range;"
  (named-character-ref "range;" '(#\⦥) "⦥")
  "rangle;"
  (named-character-ref "rangle;" '(#\⟩) "⟩")
  "raquo"
  (named-character-ref "raquo" '(#\») "»")
  "raquo;"
  (named-character-ref "raquo;" '(#\») "»")
  "rarr;"
  (named-character-ref "rarr;" '(#\→) "→")
  "rarrap;"
  (named-character-ref "rarrap;" '(#\⥵) "⥵")
  "rarrb;"
  (named-character-ref "rarrb;" '(#\⇥) "⇥")
  "rarrbfs;"
  (named-character-ref "rarrbfs;" '(#\⤠) "⤠")
  "rarrc;"
  (named-character-ref "rarrc;" '(#\⤳) "⤳")
  "rarrfs;"
  (named-character-ref "rarrfs;" '(#\⤞) "⤞")
  "rarrhk;"
  (named-character-ref "rarrhk;" '(#\↪) "↪")
  "rarrlp;"
  (named-character-ref "rarrlp;" '(#\↬) "↬")
  "rarrpl;"
  (named-character-ref "rarrpl;" '(#\⥅) "⥅")
  "rarrsim;"
  (named-character-ref "rarrsim;" '(#\⥴) "⥴")
  "rarrtl;"
  (named-character-ref "rarrtl;" '(#\↣) "↣")
  "rarrw;"
  (named-character-ref "rarrw;" '(#\↝) "↝")
  "ratail;"
  (named-character-ref "ratail;" '(#\⤚) "⤚")
  "ratio;"
  (named-character-ref "ratio;" '(#\∶) "∶")
  "rationals;"
  (named-character-ref "rationals;" '(#\ℚ) "ℚ")
  "rbarr;"
  (named-character-ref "rbarr;" '(#\⤍) "⤍")
  "rbbrk;"
  (named-character-ref "rbbrk;" '(#\❳) "❳")
  "rbrace;"
  (named-character-ref "rbrace;" '(#\}) "}")
  "rbrack;"
  (named-character-ref "rbrack;" '(#\]) "]")
  "rbrke;"
  (named-character-ref "rbrke;" '(#\⦌) "⦌")
  "rbrksld;"
  (named-character-ref "rbrksld;" '(#\⦎) "⦎")
  "rbrkslu;"
  (named-character-ref "rbrkslu;" '(#\⦐) "⦐")
  "rcaron;"
  (named-character-ref "rcaron;" '(#\ř) "ř")
  "rcedil;"
  (named-character-ref "rcedil;" '(#\ŗ) "ŗ")
  "rceil;"
  (named-character-ref "rceil;" '(#\⌉) "⌉")
  "rcub;"
  (named-character-ref "rcub;" '(#\}) "}")
  "rcy;"
  (named-character-ref "rcy;" '(#\р) "р")
  "rdca;"
  (named-character-ref "rdca;" '(#\⤷) "⤷")
  "rdldhar;"
  (named-character-ref "rdldhar;" '(#\⥩) "⥩")
  "rdquo;"
  (named-character-ref "rdquo;" '(#\”) "”")
  "rdquor;"
  (named-character-ref "rdquor;" '(#\”) "”")
  "rdsh;"
  (named-character-ref "rdsh;" '(#\↳) "↳")
  "real;"
  (named-character-ref "real;" '(#\ℜ) "ℜ")
  "realine;"
  (named-character-ref "realine;" '(#\ℛ) "ℛ")
  "realpart;"
  (named-character-ref "realpart;" '(#\ℜ) "ℜ")
  "reals;"
  (named-character-ref "reals;" '(#\ℝ) "ℝ")
  "rect;"
  (named-character-ref "rect;" '(#\▭) "▭")
  "reg"
  (named-character-ref "reg" '(#\®) "®")
  "reg;"
  (named-character-ref "reg;" '(#\®) "®")
  "rfisht;"
  (named-character-ref "rfisht;" '(#\⥽) "⥽")
  "rfloor;"
  (named-character-ref "rfloor;" '(#\⌋) "⌋")
  "rfr;"
  (named-character-ref "rfr;" '(#\𝔯) "𝔯")
  "rhard;"
  (named-character-ref "rhard;" '(#\⇁) "⇁")
  "rharu;"
  (named-character-ref "rharu;" '(#\⇀) "⇀")
  "rharul;"
  (named-character-ref "rharul;" '(#\⥬) "⥬")
  "rho;"
  (named-character-ref "rho;" '(#\ρ) "ρ")
  "rhov;"
  (named-character-ref "rhov;" '(#\ϱ) "ϱ")
  "rightarrow;"
  (named-character-ref "rightarrow;" '(#\→) "→")
  "rightarrowtail;"
  (named-character-ref "rightarrowtail;" '(#\↣) "↣")
  "rightharpoondown;"
  (named-character-ref "rightharpoondown;" '(#\⇁) "⇁")
  "rightharpoonup;"
  (named-character-ref "rightharpoonup;" '(#\⇀) "⇀")
  "rightleftarrows;"
  (named-character-ref "rightleftarrows;" '(#\⇄) "⇄")
  "rightleftharpoons;"
  (named-character-ref "rightleftharpoons;" '(#\⇌) "⇌")
  "rightrightarrows;"
  (named-character-ref "rightrightarrows;" '(#\⇉) "⇉")
  "rightsquigarrow;"
  (named-character-ref "rightsquigarrow;" '(#\↝) "↝")
  "rightthreetimes;"
  (named-character-ref "rightthreetimes;" '(#\⋌) "⋌")
  "ring;"
  (named-character-ref "ring;" '(#\˚) "˚")
  "risingdotseq;"
  (named-character-ref "risingdotseq;" '(#\≓) "≓")
  "rlarr;"
  (named-character-ref "rlarr;" '(#\⇄) "⇄")
  "rlhar;"
  (named-character-ref "rlhar;" '(#\⇌) "⇌")
  "rlm;"
  (named-character-ref "rlm;" '(#\u200F) "\u200F")
  "rmoust;"
  (named-character-ref "rmoust;" '(#\⎱) "⎱")
  "rmoustache;"
  (named-character-ref "rmoustache;" '(#\⎱) "⎱")
  "rnmid;"
  (named-character-ref "rnmid;" '(#\⫮) "⫮")
  "roang;"
  (named-character-ref "roang;" '(#\⟭) "⟭")
  "roarr;"
  (named-character-ref "roarr;" '(#\⇾) "⇾")
  "robrk;"
  (named-character-ref "robrk;" '(#\⟧) "⟧")
  "ropar;"
  (named-character-ref "ropar;" '(#\⦆) "⦆")
  "ropf;"
  (named-character-ref "ropf;" '(#\𝕣) "𝕣")
  "roplus;"
  (named-character-ref "roplus;" '(#\⨮) "⨮")
  "rotimes;"
  (named-character-ref "rotimes;" '(#\⨵) "⨵")
  "rpar;"
  (named-character-ref "rpar;" '(#\)) ")")
  "rpargt;"
  (named-character-ref "rpargt;" '(#\⦔) "⦔")
  "rppolint;"
  (named-character-ref "rppolint;" '(#\⨒) "⨒")
  "rrarr;"
  (named-character-ref "rrarr;" '(#\⇉) "⇉")
  "rsaquo;"
  (named-character-ref "rsaquo;" '(#\›) "›")
  "rscr;"
  (named-character-ref "rscr;" '(#\𝓇) "𝓇")
  "rsh;"
  (named-character-ref "rsh;" '(#\↱) "↱")
  "rsqb;"
  (named-character-ref "rsqb;" '(#\]) "]")
  "rsquo;"
  (named-character-ref "rsquo;" '(#\’) "’")
  "rsquor;"
  (named-character-ref "rsquor;" '(#\’) "’")
  "rthree;"
  (named-character-ref "rthree;" '(#\⋌) "⋌")
  "rtimes;"
  (named-character-ref "rtimes;" '(#\⋊) "⋊")
  "rtri;"
  (named-character-ref "rtri;" '(#\▹) "▹")
  "rtrie;"
  (named-character-ref "rtrie;" '(#\⊵) "⊵")
  "rtrif;"
  (named-character-ref "rtrif;" '(#\▸) "▸")
  "rtriltri;"
  (named-character-ref "rtriltri;" '(#\⧎) "⧎")
  "ruluhar;"
  (named-character-ref "ruluhar;" '(#\⥨) "⥨")
  "rx;"
  (named-character-ref "rx;" '(#\℞) "℞")
  "sacute;"
  (named-character-ref "sacute;" '(#\ś) "ś")
  "sbquo;"
  (named-character-ref "sbquo;" '(#\‚) "‚")
  "sc;"
  (named-character-ref "sc;" '(#\≻) "≻")
  "scE;"
  (named-character-ref "scE;" '(#\⪴) "⪴")
  "scap;"
  (named-character-ref "scap;" '(#\⪸) "⪸")
  "scaron;"
  (named-character-ref "scaron;" '(#\š) "š")
  "sccue;"
  (named-character-ref "sccue;" '(#\≽) "≽")
  "sce;"
  (named-character-ref "sce;" '(#\⪰) "⪰")
  "scedil;"
  (named-character-ref "scedil;" '(#\ş) "ş")
  "scirc;"
  (named-character-ref "scirc;" '(#\ŝ) "ŝ")
  "scnE;"
  (named-character-ref "scnE;" '(#\⪶) "⪶")
  "scnap;"
  (named-character-ref "scnap;" '(#\⪺) "⪺")
  "scnsim;"
  (named-character-ref "scnsim;" '(#\⋩) "⋩")
  "scpolint;"
  (named-character-ref "scpolint;" '(#\⨓) "⨓")
  "scsim;"
  (named-character-ref "scsim;" '(#\≿) "≿")
  "scy;"
  (named-character-ref "scy;" '(#\с) "с")
  "sdot;"
  (named-character-ref "sdot;" '(#\⋅) "⋅")
  "sdotb;"
  (named-character-ref "sdotb;" '(#\⊡) "⊡")
  "sdote;"
  (named-character-ref "sdote;" '(#\⩦) "⩦")
  "seArr;"
  (named-character-ref "seArr;" '(#\⇘) "⇘")
  "searhk;"
  (named-character-ref "searhk;" '(#\⤥) "⤥")
  "searr;"
  (named-character-ref "searr;" '(#\↘) "↘")
  "searrow;"
  (named-character-ref "searrow;" '(#\↘) "↘")
  "sect"
  (named-character-ref "sect" '(#\§) "§")
  "sect;"
  (named-character-ref "sect;" '(#\§) "§")
  "semi;"
  (named-character-ref "semi;" '(#\;) ";")
  "seswar;"
  (named-character-ref "seswar;" '(#\⤩) "⤩")
  "setminus;"
  (named-character-ref "setminus;" '(#\∖) "∖")
  "setmn;"
  (named-character-ref "setmn;" '(#\∖) "∖")
  "sext;"
  (named-character-ref "sext;" '(#\✶) "✶")
  "sfr;"
  (named-character-ref "sfr;" '(#\𝔰) "𝔰")
  "sfrown;"
  (named-character-ref "sfrown;" '(#\⌢) "⌢")
  "sharp;"
  (named-character-ref "sharp;" '(#\♯) "♯")
  "shchcy;"
  (named-character-ref "shchcy;" '(#\щ) "щ")
  "shcy;"
  (named-character-ref "shcy;" '(#\ш) "ш")
  "shortmid;"
  (named-character-ref "shortmid;" '(#\∣) "∣")
  "shortparallel;"
  (named-character-ref "shortparallel;" '(#\∥) "∥")
  "shy"
  (named-character-ref "shy" '(#\u00AD) "\u00AD")
  "shy;"
  (named-character-ref "shy;" '(#\u00AD) "\u00AD")
  "sigma;"
  (named-character-ref "sigma;" '(#\σ) "σ")
  "sigmaf;"
  (named-character-ref "sigmaf;" '(#\ς) "ς")
  "sigmav;"
  (named-character-ref "sigmav;" '(#\ς) "ς")
  "sim;"
  (named-character-ref "sim;" '(#\∼) "∼")
  "simdot;"
  (named-character-ref "simdot;" '(#\⩪) "⩪")
  "sime;"
  (named-character-ref "sime;" '(#\≃) "≃")
  "simeq;"
  (named-character-ref "simeq;" '(#\≃) "≃")
  "simg;"
  (named-character-ref "simg;" '(#\⪞) "⪞")
  "simgE;"
  (named-character-ref "simgE;" '(#\⪠) "⪠")
  "siml;"
  (named-character-ref "siml;" '(#\⪝) "⪝")
  "simlE;"
  (named-character-ref "simlE;" '(#\⪟) "⪟")
  "simne;"
  (named-character-ref "simne;" '(#\≆) "≆")
  "simplus;"
  (named-character-ref "simplus;" '(#\⨤) "⨤")
  "simrarr;"
  (named-character-ref "simrarr;" '(#\⥲) "⥲")
  "slarr;"
  (named-character-ref "slarr;" '(#\←) "←")
  "smallsetminus;"
  (named-character-ref "smallsetminus;" '(#\∖) "∖")
  "smashp;"
  (named-character-ref "smashp;" '(#\⨳) "⨳")
  "smeparsl;"
  (named-character-ref "smeparsl;" '(#\⧤) "⧤")
  "smid;"
  (named-character-ref "smid;" '(#\∣) "∣")
  "smile;"
  (named-character-ref "smile;" '(#\⌣) "⌣")
  "smt;"
  (named-character-ref "smt;" '(#\⪪) "⪪")
  "smte;"
  (named-character-ref "smte;" '(#\⪬) "⪬")
  "smtes;"
  (named-character-ref "smtes;" '(#\⪬ #\︀) "⪬︀")
  "softcy;"
  (named-character-ref "softcy;" '(#\ь) "ь")
  "sol;"
  (named-character-ref "sol;" '(#\/) "/")
  "solb;"
  (named-character-ref "solb;" '(#\⧄) "⧄")
  "solbar;"
  (named-character-ref "solbar;" '(#\⌿) "⌿")
  "sopf;"
  (named-character-ref "sopf;" '(#\𝕤) "𝕤")
  "spades;"
  (named-character-ref "spades;" '(#\♠) "♠")
  "spadesuit;"
  (named-character-ref "spadesuit;" '(#\♠) "♠")
  "spar;"
  (named-character-ref "spar;" '(#\∥) "∥")
  "sqcap;"
  (named-character-ref "sqcap;" '(#\⊓) "⊓")
  "sqcaps;"
  (named-character-ref "sqcaps;" '(#\⊓ #\︀) "⊓︀")
  "sqcup;"
  (named-character-ref "sqcup;" '(#\⊔) "⊔")
  "sqcups;"
  (named-character-ref "sqcups;" '(#\⊔ #\︀) "⊔︀")
  "sqsub;"
  (named-character-ref "sqsub;" '(#\⊏) "⊏")
  "sqsube;"
  (named-character-ref "sqsube;" '(#\⊑) "⊑")
  "sqsubset;"
  (named-character-ref "sqsubset;" '(#\⊏) "⊏")
  "sqsubseteq;"
  (named-character-ref "sqsubseteq;" '(#\⊑) "⊑")
  "sqsup;"
  (named-character-ref "sqsup;" '(#\⊐) "⊐")
  "sqsupe;"
  (named-character-ref "sqsupe;" '(#\⊒) "⊒")
  "sqsupset;"
  (named-character-ref "sqsupset;" '(#\⊐) "⊐")
  "sqsupseteq;"
  (named-character-ref "sqsupseteq;" '(#\⊒) "⊒")
  "squ;"
  (named-character-ref "squ;" '(#\□) "□")
  "square;"
  (named-character-ref "square;" '(#\□) "□")
  "squarf;"
  (named-character-ref "squarf;" '(#\▪) "▪")
  "squf;"
  (named-character-ref "squf;" '(#\▪) "▪")
  "srarr;"
  (named-character-ref "srarr;" '(#\→) "→")
  "sscr;"
  (named-character-ref "sscr;" '(#\𝓈) "𝓈")
  "ssetmn;"
  (named-character-ref "ssetmn;" '(#\∖) "∖")
  "ssmile;"
  (named-character-ref "ssmile;" '(#\⌣) "⌣")
  "sstarf;"
  (named-character-ref "sstarf;" '(#\⋆) "⋆")
  "star;"
  (named-character-ref "star;" '(#\☆) "☆")
  "starf;"
  (named-character-ref "starf;" '(#\★) "★")
  "straightepsilon;"
  (named-character-ref "straightepsilon;" '(#\ϵ) "ϵ")
  "straightphi;"
  (named-character-ref "straightphi;" '(#\ϕ) "ϕ")
  "strns;"
  (named-character-ref "strns;" '(#\¯) "¯")
  "sub;"
  (named-character-ref "sub;" '(#\⊂) "⊂")
  "subE;"
  (named-character-ref "subE;" '(#\⫅) "⫅")
  "subdot;"
  (named-character-ref "subdot;" '(#\⪽) "⪽")
  "sube;"
  (named-character-ref "sube;" '(#\⊆) "⊆")
  "subedot;"
  (named-character-ref "subedot;" '(#\⫃) "⫃")
  "submult;"
  (named-character-ref "submult;" '(#\⫁) "⫁")
  "subnE;"
  (named-character-ref "subnE;" '(#\⫋) "⫋")
  "subne;"
  (named-character-ref "subne;" '(#\⊊) "⊊")
  "subplus;"
  (named-character-ref "subplus;" '(#\⪿) "⪿")
  "subrarr;"
  (named-character-ref "subrarr;" '(#\⥹) "⥹")
  "subset;"
  (named-character-ref "subset;" '(#\⊂) "⊂")
  "subseteq;"
  (named-character-ref "subseteq;" '(#\⊆) "⊆")
  "subseteqq;"
  (named-character-ref "subseteqq;" '(#\⫅) "⫅")
  "subsetneq;"
  (named-character-ref "subsetneq;" '(#\⊊) "⊊")
  "subsetneqq;"
  (named-character-ref "subsetneqq;" '(#\⫋) "⫋")
  "subsim;"
  (named-character-ref "subsim;" '(#\⫇) "⫇")
  "subsub;"
  (named-character-ref "subsub;" '(#\⫕) "⫕")
  "subsup;"
  (named-character-ref "subsup;" '(#\⫓) "⫓")
  "succ;"
  (named-character-ref "succ;" '(#\≻) "≻")
  "succapprox;"
  (named-character-ref "succapprox;" '(#\⪸) "⪸")
  "succcurlyeq;"
  (named-character-ref "succcurlyeq;" '(#\≽) "≽")
  "succeq;"
  (named-character-ref "succeq;" '(#\⪰) "⪰")
  "succnapprox;"
  (named-character-ref "succnapprox;" '(#\⪺) "⪺")
  "succneqq;"
  (named-character-ref "succneqq;" '(#\⪶) "⪶")
  "succnsim;"
  (named-character-ref "succnsim;" '(#\⋩) "⋩")
  "succsim;"
  (named-character-ref "succsim;" '(#\≿) "≿")
  "sum;"
  (named-character-ref "sum;" '(#\∑) "∑")
  "sung;"
  (named-character-ref "sung;" '(#\♪) "♪")
  "sup1"
  (named-character-ref "sup1" '(#\¹) "¹")
  "sup1;"
  (named-character-ref "sup1;" '(#\¹) "¹")
  "sup2"
  (named-character-ref "sup2" '(#\²) "²")
  "sup2;"
  (named-character-ref "sup2;" '(#\²) "²")
  "sup3"
  (named-character-ref "sup3" '(#\³) "³")
  "sup3;"
  (named-character-ref "sup3;" '(#\³) "³")
  "sup;"
  (named-character-ref "sup;" '(#\⊃) "⊃")
  "supE;"
  (named-character-ref "supE;" '(#\⫆) "⫆")
  "supdot;"
  (named-character-ref "supdot;" '(#\⪾) "⪾")
  "supdsub;"
  (named-character-ref "supdsub;" '(#\⫘) "⫘")
  "supe;"
  (named-character-ref "supe;" '(#\⊇) "⊇")
  "supedot;"
  (named-character-ref "supedot;" '(#\⫄) "⫄")
  "suphsol;"
  (named-character-ref "suphsol;" '(#\⟉) "⟉")
  "suphsub;"
  (named-character-ref "suphsub;" '(#\⫗) "⫗")
  "suplarr;"
  (named-character-ref "suplarr;" '(#\⥻) "⥻")
  "supmult;"
  (named-character-ref "supmult;" '(#\⫂) "⫂")
  "supnE;"
  (named-character-ref "supnE;" '(#\⫌) "⫌")
  "supne;"
  (named-character-ref "supne;" '(#\⊋) "⊋")
  "supplus;"
  (named-character-ref "supplus;" '(#\⫀) "⫀")
  "supset;"
  (named-character-ref "supset;" '(#\⊃) "⊃")
  "supseteq;"
  (named-character-ref "supseteq;" '(#\⊇) "⊇")
  "supseteqq;"
  (named-character-ref "supseteqq;" '(#\⫆) "⫆")
  "supsetneq;"
  (named-character-ref "supsetneq;" '(#\⊋) "⊋")
  "supsetneqq;"
  (named-character-ref "supsetneqq;" '(#\⫌) "⫌")
  "supsim;"
  (named-character-ref "supsim;" '(#\⫈) "⫈")
  "supsub;"
  (named-character-ref "supsub;" '(#\⫔) "⫔")
  "supsup;"
  (named-character-ref "supsup;" '(#\⫖) "⫖")
  "swArr;"
  (named-character-ref "swArr;" '(#\⇙) "⇙")
  "swarhk;"
  (named-character-ref "swarhk;" '(#\⤦) "⤦")
  "swarr;"
  (named-character-ref "swarr;" '(#\↙) "↙")
  "swarrow;"
  (named-character-ref "swarrow;" '(#\↙) "↙")
  "swnwar;"
  (named-character-ref "swnwar;" '(#\⤪) "⤪")
  "szlig"
  (named-character-ref "szlig" '(#\ß) "ß")
  "szlig;"
  (named-character-ref "szlig;" '(#\ß) "ß")
  "target;"
  (named-character-ref "target;" '(#\⌖) "⌖")
  "tau;"
  (named-character-ref "tau;" '(#\τ) "τ")
  "tbrk;"
  (named-character-ref "tbrk;" '(#\⎴) "⎴")
  "tcaron;"
  (named-character-ref "tcaron;" '(#\ť) "ť")
  "tcedil;"
  (named-character-ref "tcedil;" '(#\ţ) "ţ")
  "tcy;"
  (named-character-ref "tcy;" '(#\т) "т")
  "tdot;"
  (named-character-ref "tdot;" '(#\⃛) "◌⃛")
  "telrec;"
  (named-character-ref "telrec;" '(#\⌕) "⌕")
  "tfr;"
  (named-character-ref "tfr;" '(#\𝔱) "𝔱")
  "there4;"
  (named-character-ref "there4;" '(#\∴) "∴")
  "therefore;"
  (named-character-ref "therefore;" '(#\∴) "∴")
  "theta;"
  (named-character-ref "theta;" '(#\θ) "θ")
  "thetasym;"
  (named-character-ref "thetasym;" '(#\ϑ) "ϑ")
  "thetav;"
  (named-character-ref "thetav;" '(#\ϑ) "ϑ")
  "thickapprox;"
  (named-character-ref "thickapprox;" '(#\≈) "≈")
  "thicksim;"
  (named-character-ref "thicksim;" '(#\∼) "∼")
  "thinsp;"
  (named-character-ref "thinsp;" '(#\u2009) " ")
  "thkap;"
  (named-character-ref "thkap;" '(#\≈) "≈")
  "thksim;"
  (named-character-ref "thksim;" '(#\∼) "∼")
  "thorn"
  (named-character-ref "thorn" '(#\þ) "þ")
  "thorn;"
  (named-character-ref "thorn;" '(#\þ) "þ")
  "tilde;"
  (named-character-ref "tilde;" '(#\˜) "˜")
  "times"
  (named-character-ref "times" '(#\×) "×")
  "times;"
  (named-character-ref "times;" '(#\×) "×")
  "timesb;"
  (named-character-ref "timesb;" '(#\⊠) "⊠")
  "timesbar;"
  (named-character-ref "timesbar;" '(#\⨱) "⨱")
  "timesd;"
  (named-character-ref "timesd;" '(#\⨰) "⨰")
  "tint;"
  (named-character-ref "tint;" '(#\∭) "∭")
  "toea;"
  (named-character-ref "toea;" '(#\⤨) "⤨")
  "top;"
  (named-character-ref "top;" '(#\⊤) "⊤")
  "topbot;"
  (named-character-ref "topbot;" '(#\⌶) "⌶")
  "topcir;"
  (named-character-ref "topcir;" '(#\⫱) "⫱")
  "topf;"
  (named-character-ref "topf;" '(#\𝕥) "𝕥")
  "topfork;"
  (named-character-ref "topfork;" '(#\⫚) "⫚")
  "tosa;"
  (named-character-ref "tosa;" '(#\⤩) "⤩")
  "tprime;"
  (named-character-ref "tprime;" '(#\‴) "‴")
  "trade;"
  (named-character-ref "trade;" '(#\™) "™")
  "triangle;"
  (named-character-ref "triangle;" '(#\▵) "▵")
  "triangledown;"
  (named-character-ref "triangledown;" '(#\▿) "▿")
  "triangleleft;"
  (named-character-ref "triangleleft;" '(#\◃) "◃")
  "trianglelefteq;"
  (named-character-ref "trianglelefteq;" '(#\⊴) "⊴")
  "triangleq;"
  (named-character-ref "triangleq;" '(#\≜) "≜")
  "triangleright;"
  (named-character-ref "triangleright;" '(#\▹) "▹")
  "trianglerighteq;"
  (named-character-ref "trianglerighteq;" '(#\⊵) "⊵")
  "tridot;"
  (named-character-ref "tridot;" '(#\◬) "◬")
  "trie;"
  (named-character-ref "trie;" '(#\≜) "≜")
  "triminus;"
  (named-character-ref "triminus;" '(#\⨺) "⨺")
  "triplus;"
  (named-character-ref "triplus;" '(#\⨹) "⨹")
  "trisb;"
  (named-character-ref "trisb;" '(#\⧍) "⧍")
  "tritime;"
  (named-character-ref "tritime;" '(#\⨻) "⨻")
  "trpezium;"
  (named-character-ref "trpezium;" '(#\⏢) "⏢")
  "tscr;"
  (named-character-ref "tscr;" '(#\𝓉) "𝓉")
  "tscy;"
  (named-character-ref "tscy;" '(#\ц) "ц")
  "tshcy;"
  (named-character-ref "tshcy;" '(#\ћ) "ћ")
  "tstrok;"
  (named-character-ref "tstrok;" '(#\ŧ) "ŧ")
  "twixt;"
  (named-character-ref "twixt;" '(#\≬) "≬")
  "twoheadleftarrow;"
  (named-character-ref "twoheadleftarrow;" '(#\↞) "↞")
  "twoheadrightarrow;"
  (named-character-ref "twoheadrightarrow;" '(#\↠) "↠")
  "uArr;"
  (named-character-ref "uArr;" '(#\⇑) "⇑")
  "uHar;"
  (named-character-ref "uHar;" '(#\⥣) "⥣")
  "uacute"
  (named-character-ref "uacute" '(#\ú) "ú")
  "uacute;"
  (named-character-ref "uacute;" '(#\ú) "ú")
  "uarr;"
  (named-character-ref "uarr;" '(#\↑) "↑")
  "ubrcy;"
  (named-character-ref "ubrcy;" '(#\ў) "ў")
  "ubreve;"
  (named-character-ref "ubreve;" '(#\ŭ) "ŭ")
  "ucirc"
  (named-character-ref "ucirc" '(#\û) "û")
  "ucirc;"
  (named-character-ref "ucirc;" '(#\û) "û")
  "ucy;"
  (named-character-ref "ucy;" '(#\у) "у")
  "udarr;"
  (named-character-ref "udarr;" '(#\⇅) "⇅")
  "udblac;"
  (named-character-ref "udblac;" '(#\ű) "ű")
  "udhar;"
  (named-character-ref "udhar;" '(#\⥮) "⥮")
  "ufisht;"
  (named-character-ref "ufisht;" '(#\⥾) "⥾")
  "ufr;"
  (named-character-ref "ufr;" '(#\𝔲) "𝔲")
  "ugrave"
  (named-character-ref "ugrave" '(#\ù) "ù")
  "ugrave;"
  (named-character-ref "ugrave;" '(#\ù) "ù")
  "uharl;"
  (named-character-ref "uharl;" '(#\↿) "↿")
  "uharr;"
  (named-character-ref "uharr;" '(#\↾) "↾")
  "uhblk;"
  (named-character-ref "uhblk;" '(#\▀) "▀")
  "ulcorn;"
  (named-character-ref "ulcorn;" '(#\⌜) "⌜")
  "ulcorner;"
  (named-character-ref "ulcorner;" '(#\⌜) "⌜")
  "ulcrop;"
  (named-character-ref "ulcrop;" '(#\⌏) "⌏")
  "ultri;"
  (named-character-ref "ultri;" '(#\◸) "◸")
  "umacr;"
  (named-character-ref "umacr;" '(#\ū) "ū")
  "uml"
  (named-character-ref "uml" '(#\¨) "¨")
  "uml;"
  (named-character-ref "uml;" '(#\¨) "¨")
  "uogon;"
  (named-character-ref "uogon;" '(#\ų) "ų")
  "uopf;"
  (named-character-ref "uopf;" '(#\𝕦) "𝕦")
  "uparrow;"
  (named-character-ref "uparrow;" '(#\↑) "↑")
  "updownarrow;"
  (named-character-ref "updownarrow;" '(#\↕) "↕")
  "upharpoonleft;"
  (named-character-ref "upharpoonleft;" '(#\↿) "↿")
  "upharpoonright;"
  (named-character-ref "upharpoonright;" '(#\↾) "↾")
  "uplus;"
  (named-character-ref "uplus;" '(#\⊎) "⊎")
  "upsi;"
  (named-character-ref "upsi;" '(#\υ) "υ")
  "upsih;"
  (named-character-ref "upsih;" '(#\ϒ) "ϒ")
  "upsilon;"
  (named-character-ref "upsilon;" '(#\υ) "υ")
  "upuparrows;"
  (named-character-ref "upuparrows;" '(#\⇈) "⇈")
  "urcorn;"
  (named-character-ref "urcorn;" '(#\⌝) "⌝")
  "urcorner;"
  (named-character-ref "urcorner;" '(#\⌝) "⌝")
  "urcrop;"
  (named-character-ref "urcrop;" '(#\⌎) "⌎")
  "uring;"
  (named-character-ref "uring;" '(#\ů) "ů")
  "urtri;"
  (named-character-ref "urtri;" '(#\◹) "◹")
  "uscr;"
  (named-character-ref "uscr;" '(#\𝓊) "𝓊")
  "utdot;"
  (named-character-ref "utdot;" '(#\⋰) "⋰")
  "utilde;"
  (named-character-ref "utilde;" '(#\ũ) "ũ")
  "utri;"
  (named-character-ref "utri;" '(#\▵) "▵")
  "utrif;"
  (named-character-ref "utrif;" '(#\▴) "▴")
  "uuarr;"
  (named-character-ref "uuarr;" '(#\⇈) "⇈")
  "uuml"
  (named-character-ref "uuml" '(#\ü) "ü")
  "uuml;"
  (named-character-ref "uuml;" '(#\ü) "ü")
  "uwangle;"
  (named-character-ref "uwangle;" '(#\⦧) "⦧")
  "vArr;"
  (named-character-ref "vArr;" '(#\⇕) "⇕")
  "vBar;"
  (named-character-ref "vBar;" '(#\⫨) "⫨")
  "vBarv;"
  (named-character-ref "vBarv;" '(#\⫩) "⫩")
  "vDash;"
  (named-character-ref "vDash;" '(#\⊨) "⊨")
  "vangrt;"
  (named-character-ref "vangrt;" '(#\⦜) "⦜")
  "varepsilon;"
  (named-character-ref "varepsilon;" '(#\ϵ) "ϵ")
  "varkappa;"
  (named-character-ref "varkappa;" '(#\ϰ) "ϰ")
  "varnothing;"
  (named-character-ref "varnothing;" '(#\∅) "∅")
  "varphi;"
  (named-character-ref "varphi;" '(#\ϕ) "ϕ")
  "varpi;"
  (named-character-ref "varpi;" '(#\ϖ) "ϖ")
  "varpropto;"
  (named-character-ref "varpropto;" '(#\∝) "∝")
  "varr;"
  (named-character-ref "varr;" '(#\↕) "↕")
  "varrho;"
  (named-character-ref "varrho;" '(#\ϱ) "ϱ")
  "varsigma;"
  (named-character-ref "varsigma;" '(#\ς) "ς")
  "varsubsetneq;"
  (named-character-ref "varsubsetneq;" '(#\⊊ #\︀) "⊊︀")
  "varsubsetneqq;"
  (named-character-ref "varsubsetneqq;" '(#\⫋ #\︀) "⫋︀")
  "varsupsetneq;"
  (named-character-ref "varsupsetneq;" '(#\⊋ #\︀) "⊋︀")
  "varsupsetneqq;"
  (named-character-ref "varsupsetneqq;" '(#\⫌ #\︀) "⫌︀")
  "vartheta;"
  (named-character-ref "vartheta;" '(#\ϑ) "ϑ")
  "vartriangleleft;"
  (named-character-ref "vartriangleleft;" '(#\⊲) "⊲")
  "vartriangleright;"
  (named-character-ref "vartriangleright;" '(#\⊳) "⊳")
  "vcy;"
  (named-character-ref "vcy;" '(#\в) "в")
  "vdash;"
  (named-character-ref "vdash;" '(#\⊢) "⊢")
  "vee;"
  (named-character-ref "vee;" '(#\∨) "∨")
  "veebar;"
  (named-character-ref "veebar;" '(#\⊻) "⊻")
  "veeeq;"
  (named-character-ref "veeeq;" '(#\≚) "≚")
  "vellip;"
  (named-character-ref "vellip;" '(#\⋮) "⋮")
  "verbar;"
  (named-character-ref "verbar;" '(#\|) "|")
  "vert;"
  (named-character-ref "vert;" '(#\|) "|")
  "vfr;"
  (named-character-ref "vfr;" '(#\𝔳) "𝔳")
  "vltri;"
  (named-character-ref "vltri;" '(#\⊲) "⊲")
  "vnsub;"
  (named-character-ref "vnsub;" '(#\⊂ #\⃒) "⊂⃒")
  "vnsup;"
  (named-character-ref "vnsup;" '(#\⊃ #\⃒) "⊃⃒")
  "vopf;"
  (named-character-ref "vopf;" '(#\𝕧) "𝕧")
  "vprop;"
  (named-character-ref "vprop;" '(#\∝) "∝")
  "vrtri;"
  (named-character-ref "vrtri;" '(#\⊳) "⊳")
  "vscr;"
  (named-character-ref "vscr;" '(#\𝓋) "𝓋")
  "vsubnE;"
  (named-character-ref "vsubnE;" '(#\⫋ #\︀) "⫋︀")
  "vsubne;"
  (named-character-ref "vsubne;" '(#\⊊ #\︀) "⊊︀")
  "vsupnE;"
  (named-character-ref "vsupnE;" '(#\⫌ #\︀) "⫌︀")
  "vsupne;"
  (named-character-ref "vsupne;" '(#\⊋ #\︀) "⊋︀")
  "vzigzag;"
  (named-character-ref "vzigzag;" '(#\⦚) "⦚")
  "wcirc;"
  (named-character-ref "wcirc;" '(#\ŵ) "ŵ")
  "wedbar;"
  (named-character-ref "wedbar;" '(#\⩟) "⩟")
  "wedge;"
  (named-character-ref "wedge;" '(#\∧) "∧")
  "wedgeq;"
  (named-character-ref "wedgeq;" '(#\≙) "≙")
  "weierp;"
  (named-character-ref "weierp;" '(#\℘) "℘")
  "wfr;"
  (named-character-ref "wfr;" '(#\𝔴) "𝔴")
  "wopf;"
  (named-character-ref "wopf;" '(#\𝕨) "𝕨")
  "wp;"
  (named-character-ref "wp;" '(#\℘) "℘")
  "wr;"
  (named-character-ref "wr;" '(#\≀) "≀")
  "wreath;"
  (named-character-ref "wreath;" '(#\≀) "≀")
  "wscr;"
  (named-character-ref "wscr;" '(#\𝓌) "𝓌")
  "xcap;"
  (named-character-ref "xcap;" '(#\⋂) "⋂")
  "xcirc;"
  (named-character-ref "xcirc;" '(#\◯) "◯")
  "xcup;"
  (named-character-ref "xcup;" '(#\⋃) "⋃")
  "xdtri;"
  (named-character-ref "xdtri;" '(#\▽) "▽")
  "xfr;"
  (named-character-ref "xfr;" '(#\𝔵) "𝔵")
  "xhArr;"
  (named-character-ref "xhArr;" '(#\⟺) "⟺")
  "xharr;"
  (named-character-ref "xharr;" '(#\⟷) "⟷")
  "xi;"
  (named-character-ref "xi;" '(#\ξ) "ξ")
  "xlArr;"
  (named-character-ref "xlArr;" '(#\⟸) "⟸")
  "xlarr;"
  (named-character-ref "xlarr;" '(#\⟵) "⟵")
  "xmap;"
  (named-character-ref "xmap;" '(#\⟼) "⟼")
  "xnis;"
  (named-character-ref "xnis;" '(#\⋻) "⋻")
  "xodot;"
  (named-character-ref "xodot;" '(#\⨀) "⨀")
  "xopf;"
  (named-character-ref "xopf;" '(#\𝕩) "𝕩")
  "xoplus;"
  (named-character-ref "xoplus;" '(#\⨁) "⨁")
  "xotime;"
  (named-character-ref "xotime;" '(#\⨂) "⨂")
  "xrArr;"
  (named-character-ref "xrArr;" '(#\⟹) "⟹")
  "xrarr;"
  (named-character-ref "xrarr;" '(#\⟶) "⟶")
  "xscr;"
  (named-character-ref "xscr;" '(#\𝓍) "𝓍")
  "xsqcup;"
  (named-character-ref "xsqcup;" '(#\⨆) "⨆")
  "xuplus;"
  (named-character-ref "xuplus;" '(#\⨄) "⨄")
  "xutri;"
  (named-character-ref "xutri;" '(#\△) "△")
  "xvee;"
  (named-character-ref "xvee;" '(#\⋁) "⋁")
  "xwedge;"
  (named-character-ref "xwedge;" '(#\⋀) "⋀")
  "yacute"
  (named-character-ref "yacute" '(#\ý) "ý")
  "yacute;"
  (named-character-ref "yacute;" '(#\ý) "ý")
  "yacy;"
  (named-character-ref "yacy;" '(#\я) "я")
  "ycirc;"
  (named-character-ref "ycirc;" '(#\ŷ) "ŷ")
  "ycy;"
  (named-character-ref "ycy;" '(#\ы) "ы")
  "yen"
  (named-character-ref "yen" '(#\¥) "¥")
  "yen;"
  (named-character-ref "yen;" '(#\¥) "¥")
  "yfr;"
  (named-character-ref "yfr;" '(#\𝔶) "𝔶")
  "yicy;"
  (named-character-ref "yicy;" '(#\ї) "ї")
  "yopf;"
  (named-character-ref "yopf;" '(#\𝕪) "𝕪")
  "yscr;"
  (named-character-ref "yscr;" '(#\𝓎) "𝓎")
  "yucy;"
  (named-character-ref "yucy;" '(#\ю) "ю")
  "yuml"
  (named-character-ref "yuml" '(#\ÿ) "ÿ")
  "yuml;"
  (named-character-ref "yuml;" '(#\ÿ) "ÿ")
  "zacute;"
  (named-character-ref "zacute;" '(#\ź) "ź")
  "zcaron;"
  (named-character-ref "zcaron;" '(#\ž) "ž")
  "zcy;"
  (named-character-ref "zcy;" '(#\з) "з")
  "zdot;"
  (named-character-ref "zdot;" '(#\ż) "ż")
  "zeetrf;"
  (named-character-ref "zeetrf;" '(#\ℨ) "ℨ")
  "zeta;"
  (named-character-ref "zeta;" '(#\ζ) "ζ")
  "zfr;"
  (named-character-ref "zfr;" '(#\𝔷) "𝔷")
  "zhcy;"
  (named-character-ref "zhcy;" '(#\ж) "ж")
  "zigrarr;"
  (named-character-ref "zigrarr;" '(#\⇝) "⇝")
  "zopf;"
  (named-character-ref "zopf;" '(#\𝕫) "𝕫")
  "zscr;"
  (named-character-ref "zscr;" '(#\𝓏) "𝓏")
  "zwj;"
  (named-character-ref "zwj;" '(#\u200D) "\u200D")
  "zwnj;"
  (named-character-ref "zwnj;" '(#\u200C) "\u200C")))

(: characters-index (Immutable-HashTable Char (Listof String)))
(define characters-index
  (hash #\A
        '("Auml"
         "Auml;"
         "Atilde"
         "Atilde;"
         "Assign;"
         "Ascr;"
         "Aring"
         "Aring;"
         "ApplyFunction;"
         "Aopf;"
         "Aogon;"
         "And;"
         "AMP"
         "AMP;"
         "Amacr;"
         "Alpha;"
         "Agrave"
         "Agrave;"
         "Afr;"
         "AElig"
         "AElig;"
         "Acy;"
         "Acirc"
         "Acirc;"
         "Abreve;"
         "Aacute"
         "Aacute;")
        #\B
        '("Bumpeq;"
         "Bscr;"
         "Breve;"
         "Bopf;"
         "Bfr;"
         "Beta;"
         "Bernoullis;"
         "Because;"
         "Bcy;"
         "Barwed;"
         "Barv;"
         "Backslash;")
        #\C
        '("CupCap;"
         "Cup;"
         "Cscr;"
         "Cross;"
         "CounterClockwiseContourIntegral;"
         "COPY"
         "COPY;"
         "Coproduct;"
         "Copf;"
         "ContourIntegral;"
         "Conint;"
         "Congruent;"
         "Colone;"
         "Colon;"
         "CloseCurlyQuote;"
         "CloseCurlyDoubleQuote;"
         "ClockwiseContourIntegral;"
         "CircleTimes;"
         "CirclePlus;"
         "CircleMinus;"
         "CircleDot;"
         "Chi;"
         "CHcy;"
         "Cfr;"
         "CenterDot;"
         "Cedilla;"
         "Cdot;"
         "Cconint;"
         "Ccirc;"
         "Ccedil"
         "Ccedil;"
         "Ccaron;"
         "Cayleys;"
         "CapitalDifferentialD;"
         "Cap;"
         "Cacute;")
        #\D
        '("DZcy;"
         "Dstrok;"
         "DScy;"
         "Dscr;"
         "DownTeeArrow;"
         "DownTee;"
         "DownRightVectorBar;"
         "DownRightVector;"
         "DownRightTeeVector;"
         "DownLeftVectorBar;"
         "DownLeftVector;"
         "DownLeftTeeVector;"
         "DownLeftRightVector;"
         "DownBreve;"
         "DownArrowUpArrow;"
         "DownArrowBar;"
         "Downarrow;"
         "DownArrow;"
         "DoubleVerticalBar;"
         "DoubleUpDownArrow;"
         "DoubleUpArrow;"
         "DoubleRightTee;"
         "DoubleRightArrow;"
         "DoubleLongRightArrow;"
         "DoubleLongLeftRightArrow;"
         "DoubleLongLeftArrow;"
         "DoubleLeftTee;"
         "DoubleLeftRightArrow;"
         "DoubleLeftArrow;"
         "DoubleDownArrow;"
         "DoubleDot;"
         "DoubleContourIntegral;"
         "DotEqual;"
         "DotDot;"
         "Dot;"
         "Dopf;"
         "DJcy;"
         "DifferentialD;"
         "Diamond;"
         "DiacriticalTilde;"
         "DiacriticalGrave;"
         "DiacriticalDoubleAcute;"
         "DiacriticalDot;"
         "DiacriticalAcute;"
         "Dfr;"
         "Delta;"
         "Del;"
         "DDotrahd;"
         "DD;"
         "Dcy;"
         "Dcaron;"
         "Dashv;"
         "Darr;"
         "Dagger;")
        #\E
        '("ExponentialE;"
         "Exists;"
         "Euml"
         "Euml;"
         "ETH"
         "ETH;"
         "Eta;"
         "Esim;"
         "Escr;"
         "Equilibrium;"
         "EqualTilde;"
         "Equal;"
         "Epsilon;"
         "Eopf;"
         "Eogon;"
         "ENG;"
         "EmptyVerySmallSquare;"
         "EmptySmallSquare;"
         "Emacr;"
         "Element;"
         "Egrave"
         "Egrave;"
         "Efr;"
         "Edot;"
         "Ecy;"
         "Ecirc"
         "Ecirc;"
         "Ecaron;"
         "Eacute"
         "Eacute;")
        #\F
        '("Fscr;"
         "Fouriertrf;"
         "ForAll;"
         "Fopf;"
         "FilledVerySmallSquare;"
         "FilledSmallSquare;"
         "Ffr;"
         "Fcy;")
        #\G
        '("Gt;"
         "GT"
         "GT;"
         "Gscr;"
         "GreaterTilde;"
         "GreaterSlantEqual;"
         "GreaterLess;"
         "GreaterGreater;"
         "GreaterFullEqual;"
         "GreaterEqualLess;"
         "GreaterEqual;"
         "Gopf;"
         "GJcy;"
         "Gg;"
         "Gfr;"
         "Gdot;"
         "Gcy;"
         "Gcirc;"
         "Gcedil;"
         "Gbreve;"
         "Gammad;"
         "Gamma;")
        #\H
        '("HumpEqual;"
         "HumpDownHump;"
         "Hstrok;"
         "Hscr;"
         "HorizontalLine;"
         "Hopf;"
         "HilbertSpace;"
         "Hfr;"
         "Hcirc;"
         "Hat;"
         "HARDcy;"
         "Hacek;")
        #\I
        '("Iuml"
         "Iuml;"
         "Iukcy;"
         "Itilde;"
         "Iscr;"
         "Iota;"
         "Iopf;"
         "Iogon;"
         "IOcy;"
         "InvisibleTimes;"
         "InvisibleComma;"
         "Intersection;"
         "Integral;"
         "Int;"
         "Implies;"
         "ImaginaryI;"
         "Imacr;"
         "Im;"
         "IJlig;"
         "Igrave"
         "Igrave;"
         "Ifr;"
         "IEcy;"
         "Idot;"
         "Icy;"
         "Icirc"
         "Icirc;"
         "Iacute"
         "Iacute;")
        #\J '("Jukcy;" "Jsercy;" "Jscr;" "Jopf;" "Jfr;" "Jcy;" "Jcirc;")
        #\K
        '("Kscr;" "Kopf;" "KJcy;" "KHcy;" "Kfr;" "Kcy;" "Kcedil;" "Kappa;")
        #\L
        '("Lt;"
         "LT"
         "LT;"
         "Lstrok;"
         "Lsh;"
         "Lscr;"
         "LowerRightArrow;"
         "LowerLeftArrow;"
         "Lopf;"
         "Longrightarrow;"
         "LongRightArrow;"
         "Longleftrightarrow;"
         "LongLeftRightArrow;"
         "Longleftarrow;"
         "LongLeftArrow;"
         "Lmidot;"
         "Lleftarrow;"
         "Ll;"
         "LJcy;"
         "Lfr;"
         "LessTilde;"
         "LessSlantEqual;"
         "LessLess;"
         "LessGreater;"
         "LessFullEqual;"
         "LessEqualGreater;"
         "LeftVectorBar;"
         "LeftVector;"
         "LeftUpVectorBar;"
         "LeftUpVector;"
         "LeftUpTeeVector;"
         "LeftUpDownVector;"
         "LeftTriangleEqual;"
         "LeftTriangleBar;"
         "LeftTriangle;"
         "LeftTeeVector;"
         "LeftTeeArrow;"
         "LeftTee;"
         "LeftRightVector;"
         "Leftrightarrow;"
         "LeftRightArrow;"
         "LeftFloor;"
         "LeftDownVectorBar;"
         "LeftDownVector;"
         "LeftDownTeeVector;"
         "LeftDoubleBracket;"
         "LeftCeiling;"
         "LeftArrowRightArrow;"
         "LeftArrowBar;"
         "Leftarrow;"
         "LeftArrow;"
         "LeftAngleBracket;"
         "Lcy;"
         "Lcedil;"
         "Lcaron;"
         "Larr;"
         "Laplacetrf;"
         "Lang;"
         "Lambda;"
         "Lacute;")
        #\M
        '("Mu;"
         "Mscr;"
         "Mopf;"
         "MinusPlus;"
         "Mfr;"
         "Mellintrf;"
         "MediumSpace;"
         "Mcy;"
         "Map;")
        #\N
        '("Nu;"
         "Ntilde"
         "Ntilde;"
         "Nscr;"
         "NotVerticalBar;"
         "NotTildeTilde;"
         "NotTildeFullEqual;"
         "NotTildeEqual;"
         "NotTilde;"
         "NotSupersetEqual;"
         "NotSuperset;"
         "NotSucceedsTilde;"
         "NotSucceedsSlantEqual;"
         "NotSucceedsEqual;"
         "NotSucceeds;"
         "NotSubsetEqual;"
         "NotSubset;"
         "NotSquareSupersetEqual;"
         "NotSquareSuperset;"
         "NotSquareSubsetEqual;"
         "NotSquareSubset;"
         "NotRightTriangleEqual;"
         "NotRightTriangleBar;"
         "NotRightTriangle;"
         "NotReverseElement;"
         "NotPrecedesSlantEqual;"
         "NotPrecedesEqual;"
         "NotPrecedes;"
         "NotNestedLessLess;"
         "NotNestedGreaterGreater;"
         "NotLessTilde;"
         "NotLessSlantEqual;"
         "NotLessLess;"
         "NotLessGreater;"
         "NotLessEqual;"
         "NotLess;"
         "NotLeftTriangleEqual;"
         "NotLeftTriangleBar;"
         "NotLeftTriangle;"
         "NotHumpEqual;"
         "NotHumpDownHump;"
         "NotGreaterTilde;"
         "NotGreaterSlantEqual;"
         "NotGreaterLess;"
         "NotGreaterGreater;"
         "NotGreaterFullEqual;"
         "NotGreaterEqual;"
         "NotGreater;"
         "NotExists;"
         "NotEqualTilde;"
         "NotEqual;"
         "NotElement;"
         "NotDoubleVerticalBar;"
         "NotCupCap;"
         "NotCongruent;"
         "Not;"
         "Nopf;"
         "NonBreakingSpace;"
         "NoBreak;"
         "NJcy;"
         "Nfr;"
         "NewLine;"
         "NestedLessLess;"
         "NestedGreaterGreater;"
         "NegativeVeryThinSpace;"
         "NegativeThinSpace;"
         "NegativeThickSpace;"
         "NegativeMediumSpace;"
         "Ncy;"
         "Ncedil;"
         "Ncaron;"
         "Nacute;")
        #\O
        '("OverParenthesis;"
         "OverBracket;"
         "OverBrace;"
         "OverBar;"
         "Ouml"
         "Ouml;"
         "Otimes;"
         "Otilde"
         "Otilde;"
         "Oslash"
         "Oslash;"
         "Oscr;"
         "Or;"
         "OpenCurlyQuote;"
         "OpenCurlyDoubleQuote;"
         "Oopf;"
         "Omicron;"
         "Omega;"
         "Omacr;"
         "Ograve"
         "Ograve;"
         "Ofr;"
         "OElig;"
         "Odblac;"
         "Ocy;"
         "Ocirc"
         "Ocirc;"
         "Oacute"
         "Oacute;")
        #\P
        '("Psi;"
         "Pscr;"
         "Proportional;"
         "Proportion;"
         "Product;"
         "Prime;"
         "PrecedesTilde;"
         "PrecedesSlantEqual;"
         "PrecedesEqual;"
         "Precedes;"
         "Pr;"
         "Popf;"
         "Poincareplane;"
         "PlusMinus;"
         "Pi;"
         "Phi;"
         "Pfr;"
         "Pcy;"
         "PartialD;")
        #\R
        '("RuleDelayed;"
         "Rsh;"
         "Rscr;"
         "Rrightarrow;"
         "RoundImplies;"
         "Ropf;"
         "RightVectorBar;"
         "RightVector;"
         "RightUpVectorBar;"
         "RightUpVector;"
         "RightUpTeeVector;"
         "RightUpDownVector;"
         "RightTriangleEqual;"
         "RightTriangleBar;"
         "RightTriangle;"
         "RightTeeVector;"
         "RightTeeArrow;"
         "RightTee;"
         "RightFloor;"
         "RightDownVectorBar;"
         "RightDownVector;"
         "RightDownTeeVector;"
         "RightDoubleBracket;"
         "RightCeiling;"
         "RightArrowLeftArrow;"
         "RightArrowBar;"
         "Rightarrow;"
         "RightArrow;"
         "RightAngleBracket;"
         "Rho;"
         "Rfr;"
         "ReverseUpEquilibrium;"
         "ReverseEquilibrium;"
         "ReverseElement;"
         "REG"
         "REG;"
         "Re;"
         "Rcy;"
         "Rcedil;"
         "Rcaron;"
         "RBarr;"
         "Rarrtl;"
         "Rarr;"
         "Rang;"
         "Racute;")
        #\S
        '("Supset;"
         "SupersetEqual;"
         "Superset;"
         "Sup;"
         "Sum;"
         "SuchThat;"
         "SucceedsTilde;"
         "SucceedsSlantEqual;"
         "SucceedsEqual;"
         "Succeeds;"
         "SubsetEqual;"
         "Subset;"
         "Sub;"
         "Star;"
         "Sscr;"
         "SquareUnion;"
         "SquareSupersetEqual;"
         "SquareSuperset;"
         "SquareSubsetEqual;"
         "SquareSubset;"
         "SquareIntersection;"
         "Square;"
         "Sqrt;"
         "Sopf;"
         "SOFTcy;"
         "SmallCircle;"
         "Sigma;"
         "ShortUpArrow;"
         "ShortRightArrow;"
         "ShortLeftArrow;"
         "ShortDownArrow;"
         "SHcy;"
         "SHCHcy;"
         "Sfr;"
         "Scy;"
         "Scirc;"
         "Scedil;"
         "Scaron;"
         "Sc;"
         "Sacute;")
        #\T
        '("Tstrok;"
         "TSHcy;"
         "TScy;"
         "Tscr;"
         "TripleDot;"
         "TRADE;"
         "Topf;"
         "TildeTilde;"
         "TildeFullEqual;"
         "TildeEqual;"
         "Tilde;"
         "THORN"
         "THORN;"
         "ThinSpace;"
         "ThickSpace;"
         "Theta;"
         "Therefore;"
         "Tfr;"
         "Tcy;"
         "Tcedil;"
         "Tcaron;"
         "Tau;"
         "Tab;")
        #\U
        '("Uuml"
         "Uuml;"
         "Utilde;"
         "Uscr;"
         "Uring;"
         "UpTeeArrow;"
         "UpTee;"
         "Upsilon;"
         "Upsi;"
         "UpperRightArrow;"
         "UpperLeftArrow;"
         "UpEquilibrium;"
         "Updownarrow;"
         "UpDownArrow;"
         "UpArrowDownArrow;"
         "UpArrowBar;"
         "Uparrow;"
         "UpArrow;"
         "Uopf;"
         "Uogon;"
         "UnionPlus;"
         "Union;"
         "UnderParenthesis;"
         "UnderBracket;"
         "UnderBrace;"
         "UnderBar;"
         "Umacr;"
         "Ugrave"
         "Ugrave;"
         "Ufr;"
         "Udblac;"
         "Ucy;"
         "Ucirc"
         "Ucirc;"
         "Ubreve;"
         "Ubrcy;"
         "Uarrocir;"
         "Uarr;"
         "Uacute"
         "Uacute;")
        #\V
        '("Vvdash;"
         "Vscr;"
         "Vopf;"
         "Vfr;"
         "VeryThinSpace;"
         "VerticalTilde;"
         "VerticalSeparator;"
         "VerticalLine;"
         "VerticalBar;"
         "Vert;"
         "Verbar;"
         "Vee;"
         "Vdashl;"
         "Vdash;"
         "VDash;"
         "Vcy;"
         "Vbar;")
        #\W '("Wscr;" "Wopf;" "Wfr;" "Wedge;" "Wcirc;")
        #\X '("Xscr;" "Xopf;" "Xi;" "Xfr;")
        #\Y
        '("Yuml;"
         "YUcy;"
         "Yscr;"
         "Yopf;"
         "YIcy;"
         "Yfr;"
         "Ycy;"
         "Ycirc;"
         "YAcy;"
         "Yacute"
         "Yacute;")
        #\Z
        '("Zscr;"
         "Zopf;"
         "ZHcy;"
         "Zfr;"
         "Zeta;"
         "ZeroWidthSpace;"
         "Zdot;"
         "Zcy;"
         "Zcaron;"
         "Zacute;")
        #\a
        '("awint;"
         "awconint;"
         "auml"
         "auml;"
         "atilde"
         "atilde;"
         "asympeq;"
         "asymp;"
         "ast;"
         "ascr;"
         "aring"
         "aring;"
         "approxeq;"
         "approx;"
         "apos;"
         "apid;"
         "ape;"
         "apE;"
         "apacir;"
         "ap;"
         "aopf;"
         "aogon;"
         "angzarr;"
         "angst;"
         "angsph;"
         "angrtvbd;"
         "angrtvb;"
         "angrt;"
         "angmsdah;"
         "angmsdag;"
         "angmsdaf;"
         "angmsdae;"
         "angmsdad;"
         "angmsdac;"
         "angmsdab;"
         "angmsdaa;"
         "angmsd;"
         "angle;"
         "ange;"
         "ang;"
         "andv;"
         "andslope;"
         "andd;"
         "andand;"
         "and;"
         "amp"
         "amp;"
         "amalg;"
         "amacr;"
         "alpha;"
         "aleph;"
         "alefsym;"
         "agrave"
         "agrave;"
         "afr;"
         "af;"
         "aelig"
         "aelig;"
         "acy;"
         "acute"
         "acute;"
         "acirc"
         "acirc;"
         "acE;"
         "acd;"
         "ac;"
         "abreve;"
         "aacute"
         "aacute;")
        #\b
        '("bumpeq;"
         "bumpe;"
         "bumpE;"
         "bump;"
         "bullet;"
         "bull;"
         "bsolhsub;"
         "bsolb;"
         "bsol;"
         "bsime;"
         "bsim;"
         "bsemi;"
         "bscr;"
         "brvbar"
         "brvbar;"
         "breve;"
         "bprime;"
         "boxvr;"
         "boxvR;"
         "boxVr;"
         "boxVR;"
         "boxvl;"
         "boxvL;"
         "boxVl;"
         "boxVL;"
         "boxvh;"
         "boxvH;"
         "boxVh;"
         "boxVH;"
         "boxv;"
         "boxV;"
         "boxur;"
         "boxuR;"
         "boxUr;"
         "boxUR;"
         "boxul;"
         "boxuL;"
         "boxUl;"
         "boxUL;"
         "boxtimes;"
         "boxplus;"
         "boxminus;"
         "boxhu;"
         "boxhU;"
         "boxHu;"
         "boxHU;"
         "boxhd;"
         "boxhD;"
         "boxHd;"
         "boxHD;"
         "boxh;"
         "boxH;"
         "boxdr;"
         "boxdR;"
         "boxDr;"
         "boxDR;"
         "boxdl;"
         "boxdL;"
         "boxDl;"
         "boxDL;"
         "boxbox;"
         "bowtie;"
         "bottom;"
         "bot;"
         "bopf;"
         "bnot;"
         "bNot;"
         "bnequiv;"
         "bne;"
         "block;"
         "blk34;"
         "blk14;"
         "blk12;"
         "blank;"
         "blacktriangleright;"
         "blacktriangleleft;"
         "blacktriangledown;"
         "blacktriangle;"
         "blacksquare;"
         "blacklozenge;"
         "bkarow;"
         "bigwedge;"
         "bigvee;"
         "biguplus;"
         "bigtriangleup;"
         "bigtriangledown;"
         "bigstar;"
         "bigsqcup;"
         "bigotimes;"
         "bigoplus;"
         "bigodot;"
         "bigcup;"
         "bigcirc;"
         "bigcap;"
         "bfr;"
         "between;"
         "beth;"
         "beta;"
         "bernou;"
         "bepsi;"
         "bemptyv;"
         "because;"
         "becaus;"
         "bdquo;"
         "bcy;"
         "bcong;"
         "bbrktbrk;"
         "bbrk;"
         "barwedge;"
         "barwed;"
         "barvee;"
         "backsimeq;"
         "backsim;"
         "backprime;"
         "backepsilon;"
         "backcong;")
        #\c
        '("cylcty;"
         "cwint;"
         "cwconint;"
         "cuwed;"
         "cuvee;"
         "curvearrowright;"
         "curvearrowleft;"
         "curren"
         "curren;"
         "curlywedge;"
         "curlyvee;"
         "curlyeqsucc;"
         "curlyeqprec;"
         "curarrm;"
         "curarr;"
         "cups;"
         "cupor;"
         "cupdot;"
         "cupcup;"
         "cupcap;"
         "cupbrcap;"
         "cup;"
         "cularrp;"
         "cularr;"
         "cuesc;"
         "cuepr;"
         "cudarrr;"
         "cudarrl;"
         "ctdot;"
         "csupe;"
         "csup;"
         "csube;"
         "csub;"
         "cscr;"
         "cross;"
         "crarr;"
         "copysr;"
         "copy"
         "copy;"
         "coprod;"
         "copf;"
         "conint;"
         "congdot;"
         "cong;"
         "complexes;"
         "complement;"
         "compfn;"
         "comp;"
         "commat;"
         "comma;"
         "coloneq;"
         "colone;"
         "colon;"
         "clubsuit;"
         "clubs;"
         "cirscir;"
         "cirmid;"
         "cirfnint;"
         "cire;"
         "cirE;"
         "circledS;"
         "circledR;"
         "circleddash;"
         "circledcirc;"
         "circledast;"
         "circlearrowright;"
         "circlearrowleft;"
         "circeq;"
         "circ;"
         "cir;"
         "chi;"
         "checkmark;"
         "check;"
         "chcy;"
         "cfr;"
         "centerdot;"
         "cent"
         "cent;"
         "cemptyv;"
         "cedil"
         "cedil;"
         "cdot;"
         "ccupssm;"
         "ccups;"
         "ccirc;"
         "ccedil"
         "ccedil;"
         "ccaron;"
         "ccaps;"
         "caron;"
         "caret;"
         "caps;"
         "capdot;"
         "capcup;"
         "capcap;"
         "capbrcup;"
         "capand;"
         "cap;"
         "cacute;")
        #\d
        '("dzigrarr;"
         "dzcy;"
         "dwangle;"
         "duhar;"
         "duarr;"
         "dtrif;"
         "dtri;"
         "dtdot;"
         "dstrok;"
         "dsol;"
         "dscy;"
         "dscr;"
         "drcrop;"
         "drcorn;"
         "drbkarow;"
         "downharpoonright;"
         "downharpoonleft;"
         "downdownarrows;"
         "downarrow;"
         "doublebarwedge;"
         "dotsquare;"
         "dotplus;"
         "dotminus;"
         "doteqdot;"
         "doteq;"
         "dot;"
         "dopf;"
         "dollar;"
         "dlcrop;"
         "dlcorn;"
         "djcy;"
         "divonx;"
         "divideontimes;"
         "divide"
         "divide;"
         "div;"
         "disin;"
         "digamma;"
         "die;"
         "diams;"
         "diamondsuit;"
         "diamond;"
         "diam;"
         "dharr;"
         "dharl;"
         "dHar;"
         "dfr;"
         "dfisht;"
         "demptyv;"
         "delta;"
         "deg"
         "deg;"
         "ddotseq;"
         "ddarr;"
         "ddagger;"
         "dd;"
         "dcy;"
         "dcaron;"
         "dblac;"
         "dbkarow;"
         "dashv;"
         "dash;"
         "darr;"
         "dArr;"
         "daleth;"
         "dagger;")
        #\e
        '("exponentiale;"
         "expectation;"
         "exist;"
         "excl;"
         "euro;"
         "euml"
         "euml;"
         "eth"
         "eth;"
         "eta;"
         "esim;"
         "esdot;"
         "escr;"
         "erDot;"
         "erarr;"
         "eqvparsl;"
         "equivDD;"
         "equiv;"
         "equest;"
         "equals;"
         "eqslantless;"
         "eqslantgtr;"
         "eqsim;"
         "eqcolon;"
         "eqcirc;"
         "epsiv;"
         "epsilon;"
         "epsi;"
         "eplus;"
         "eparsl;"
         "epar;"
         "eopf;"
         "eogon;"
         "ensp;"
         "eng;"
         "emsp14;"
         "emsp13;"
         "emsp;"
         "emptyv;"
         "emptyset;"
         "empty;"
         "emacr;"
         "elsdot;"
         "els;"
         "ell;"
         "elinters;"
         "el;"
         "egsdot;"
         "egs;"
         "egrave"
         "egrave;"
         "eg;"
         "efr;"
         "efDot;"
         "ee;"
         "edot;"
         "eDot;"
         "eDDot;"
         "ecy;"
         "ecolon;"
         "ecirc"
         "ecirc;"
         "ecir;"
         "ecaron;"
         "easter;"
         "eacute"
         "eacute;")
        #\f
        '("fscr;"
         "frown;"
         "frasl;"
         "frac78;"
         "frac58;"
         "frac56;"
         "frac45;"
         "frac38;"
         "frac35;"
         "frac34"
         "frac34;"
         "frac25;"
         "frac23;"
         "frac18;"
         "frac16;"
         "frac15;"
         "frac14"
         "frac14;"
         "frac13;"
         "frac12"
         "frac12;"
         "fpartint;"
         "forkv;"
         "fork;"
         "forall;"
         "fopf;"
         "fnof;"
         "fltns;"
         "fllig;"
         "flat;"
         "fjlig;"
         "filig;"
         "ffr;"
         "ffllig;"
         "fflig;"
         "ffilig;"
         "female;"
         "fcy;"
         "fallingdotseq;")
        #\g
        '("gvnE;"
         "gvertneqq;"
         "gtrsim;"
         "gtrless;"
         "gtreqqless;"
         "gtreqless;"
         "gtrdot;"
         "gtrarr;"
         "gtrapprox;"
         "gtquest;"
         "gtlPar;"
         "gtdot;"
         "gtcir;"
         "gtcc;"
         "gt"
         "gt;"
         "gsiml;"
         "gsime;"
         "gsim;"
         "gscr;"
         "grave;"
         "gopf;"
         "gnsim;"
         "gneqq;"
         "gneq;"
         "gne;"
         "gnE;"
         "gnapprox;"
         "gnap;"
         "glj;"
         "glE;"
         "gla;"
         "gl;"
         "gjcy;"
         "gimel;"
         "ggg;"
         "gg;"
         "gfr;"
         "gesles;"
         "gesl;"
         "gesdotol;"
         "gesdoto;"
         "gesdot;"
         "gescc;"
         "ges;"
         "geqslant;"
         "geqq;"
         "geq;"
         "gel;"
         "gEl;"
         "ge;"
         "gE;"
         "gdot;"
         "gcy;"
         "gcirc;"
         "gbreve;"
         "gap;"
         "gammad;"
         "gamma;"
         "gacute;")
        #\h
        '("hyphen;"
         "hybull;"
         "hstrok;"
         "hslash;"
         "hscr;"
         "horbar;"
         "hopf;"
         "hookrightarrow;"
         "hookleftarrow;"
         "homtht;"
         "hoarr;"
         "hkswarow;"
         "hksearow;"
         "hfr;"
         "hercon;"
         "hellip;"
         "heartsuit;"
         "hearts;"
         "hcirc;"
         "hbar;"
         "harrw;"
         "harrcir;"
         "harr;"
         "hArr;"
         "hardcy;"
         "hamilt;"
         "half;"
         "hairsp;")
        #\i
        '("iuml"
         "iuml;"
         "iukcy;"
         "itilde;"
         "it;"
         "isinv;"
         "isinsv;"
         "isins;"
         "isinE;"
         "isindot;"
         "isin;"
         "iscr;"
         "iquest"
         "iquest;"
         "iprod;"
         "iota;"
         "iopf;"
         "iogon;"
         "iocy;"
         "intprod;"
         "intlarhk;"
         "intercal;"
         "integers;"
         "intcal;"
         "int;"
         "inodot;"
         "infintie;"
         "infin;"
         "incare;"
         "in;"
         "imped;"
         "imof;"
         "imath;"
         "imagpart;"
         "imagline;"
         "image;"
         "imacr;"
         "ijlig;"
         "iiota;"
         "iinfin;"
         "iiint;"
         "iiiint;"
         "ii;"
         "igrave"
         "igrave;"
         "ifr;"
         "iff;"
         "iexcl"
         "iexcl;"
         "iecy;"
         "icy;"
         "icirc"
         "icirc;"
         "ic;"
         "iacute"
         "iacute;")
        #\j
        '("jukcy;" "jsercy;" "jscr;" "jopf;" "jmath;" "jfr;" "jcy;" "jcirc;")
        #\k
        '("kscr;"
         "kopf;"
         "kjcy;"
         "khcy;"
         "kgreen;"
         "kfr;"
         "kcy;"
         "kcedil;"
         "kappav;"
         "kappa;")
        #\l
        '("lvnE;"
         "lvertneqq;"
         "luruhar;"
         "lurdshar;"
         "ltrPar;"
         "ltrif;"
         "ltrie;"
         "ltri;"
         "ltquest;"
         "ltlarr;"
         "ltimes;"
         "lthree;"
         "ltdot;"
         "ltcir;"
         "ltcc;"
         "lt"
         "lt;"
         "lstrok;"
         "lsquor;"
         "lsquo;"
         "lsqb;"
         "lsimg;"
         "lsime;"
         "lsim;"
         "lsh;"
         "lscr;"
         "lsaquo;"
         "lrtri;"
         "lrm;"
         "lrhard;"
         "lrhar;"
         "lrcorner;"
         "lrarr;"
         "lparlt;"
         "lpar;"
         "lozf;"
         "lozenge;"
         "loz;"
         "lowbar;"
         "lowast;"
         "lotimes;"
         "loplus;"
         "lopf;"
         "lopar;"
         "looparrowright;"
         "looparrowleft;"
         "longrightarrow;"
         "longmapsto;"
         "longleftrightarrow;"
         "longleftarrow;"
         "lobrk;"
         "loarr;"
         "loang;"
         "lnsim;"
         "lneqq;"
         "lneq;"
         "lne;"
         "lnE;"
         "lnapprox;"
         "lnap;"
         "lmoustache;"
         "lmoust;"
         "lmidot;"
         "lltri;"
         "llhard;"
         "llcorner;"
         "llarr;"
         "ll;"
         "ljcy;"
         "lhblk;"
         "lharul;"
         "lharu;"
         "lhard;"
         "lHar;"
         "lgE;"
         "lg;"
         "lfr;"
         "lfloor;"
         "lfisht;"
         "lesssim;"
         "lessgtr;"
         "lesseqqgtr;"
         "lesseqgtr;"
         "lessdot;"
         "lessapprox;"
         "lesges;"
         "lesg;"
         "lesdotor;"
         "lesdoto;"
         "lesdot;"
         "lescc;"
         "les;"
         "leqslant;"
         "leqq;"
         "leq;"
         "leg;"
         "lEg;"
         "leftthreetimes;"
         "leftrightsquigarrow;"
         "leftrightharpoons;"
         "leftrightarrows;"
         "leftrightarrow;"
         "leftleftarrows;"
         "leftharpoonup;"
         "leftharpoondown;"
         "leftarrowtail;"
         "leftarrow;"
         "le;"
         "lE;"
         "ldsh;"
         "ldrushar;"
         "ldrdhar;"
         "ldquor;"
         "ldquo;"
         "ldca;"
         "lcy;"
         "lcub;"
         "lceil;"
         "lcedil;"
         "lcaron;"
         "lbrkslu;"
         "lbrksld;"
         "lbrke;"
         "lbrack;"
         "lbrace;"
         "lbbrk;"
         "lbarr;"
         "lBarr;"
         "lates;"
         "late;"
         "latail;"
         "lAtail;"
         "lat;"
         "larrtl;"
         "larrsim;"
         "larrpl;"
         "larrlp;"
         "larrhk;"
         "larrfs;"
         "larrbfs;"
         "larrb;"
         "larr;"
         "lArr;"
         "laquo"
         "laquo;"
         "lap;"
         "langle;"
         "langd;"
         "lang;"
         "lambda;"
         "lagran;"
         "laemptyv;"
         "lacute;"
         "lAarr;")
        #\m
        '("mumap;"
         "multimap;"
         "mu;"
         "mstpos;"
         "mscr;"
         "mp;"
         "mopf;"
         "models;"
         "mnplus;"
         "mldr;"
         "mlcp;"
         "minusdu;"
         "minusd;"
         "minusb;"
         "minus;"
         "middot"
         "middot;"
         "midcir;"
         "midast;"
         "mid;"
         "micro"
         "micro;"
         "mho;"
         "mfr;"
         "measuredangle;"
         "mDDot;"
         "mdash;"
         "mcy;"
         "mcomma;"
         "marker;"
         "mapstoup;"
         "mapstoleft;"
         "mapstodown;"
         "mapsto;"
         "map;"
         "maltese;"
         "malt;"
         "male;"
         "macr"
         "macr;")
        #\n
        '("nwnear;"
         "nwarrow;"
         "nwarr;"
         "nwArr;"
         "nwarhk;"
         "nvsim;"
         "nvrtrie;"
         "nvrArr;"
         "nvltrie;"
         "nvlt;"
         "nvle;"
         "nvlArr;"
         "nvinfin;"
         "nvHarr;"
         "nvgt;"
         "nvge;"
         "nvdash;"
         "nvDash;"
         "nVdash;"
         "nVDash;"
         "nvap;"
         "numsp;"
         "numero;"
         "num;"
         "nu;"
         "ntrianglerighteq;"
         "ntriangleright;"
         "ntrianglelefteq;"
         "ntriangleleft;"
         "ntlg;"
         "ntilde"
         "ntilde;"
         "ntgl;"
         "nsupseteqq;"
         "nsupseteq;"
         "nsupset;"
         "nsupe;"
         "nsupE;"
         "nsup;"
         "nsucceq;"
         "nsucc;"
         "nsubseteqq;"
         "nsubseteq;"
         "nsubset;"
         "nsube;"
         "nsubE;"
         "nsub;"
         "nsqsupe;"
         "nsqsube;"
         "nspar;"
         "nsmid;"
         "nsimeq;"
         "nsime;"
         "nsim;"
         "nshortparallel;"
         "nshortmid;"
         "nscr;"
         "nsce;"
         "nsccue;"
         "nsc;"
         "nrtrie;"
         "nrtri;"
         "nrightarrow;"
         "nRightarrow;"
         "nrarrw;"
         "nrarrc;"
         "nrarr;"
         "nrArr;"
         "npreceq;"
         "nprec;"
         "npre;"
         "nprcue;"
         "npr;"
         "npolint;"
         "npart;"
         "nparsl;"
         "nparallel;"
         "npar;"
         "notnivc;"
         "notnivb;"
         "notniva;"
         "notni;"
         "notinvc;"
         "notinvb;"
         "notinva;"
         "notinE;"
         "notindot;"
         "notin;"
         "not"
         "not;"
         "nopf;"
         "nmid;"
         "nLtv;"
         "nltrie;"
         "nltri;"
         "nlt;"
         "nLt;"
         "nlsim;"
         "nLl;"
         "nless;"
         "nles;"
         "nleqslant;"
         "nleqq;"
         "nleq;"
         "nleftrightarrow;"
         "nLeftrightarrow;"
         "nleftarrow;"
         "nLeftarrow;"
         "nle;"
         "nlE;"
         "nldr;"
         "nlarr;"
         "nlArr;"
         "njcy;"
         "niv;"
         "nisd;"
         "nis;"
         "ni;"
         "nhpar;"
         "nharr;"
         "nhArr;"
         "nGtv;"
         "ngtr;"
         "ngt;"
         "nGt;"
         "ngsim;"
         "nGg;"
         "nges;"
         "ngeqslant;"
         "ngeqq;"
         "ngeq;"
         "nge;"
         "ngE;"
         "nfr;"
         "nexists;"
         "nexist;"
         "nesim;"
         "nesear;"
         "nequiv;"
         "nedot;"
         "nearrow;"
         "nearr;"
         "neArr;"
         "nearhk;"
         "ne;"
         "ndash;"
         "ncy;"
         "ncup;"
         "ncongdot;"
         "ncong;"
         "ncedil;"
         "ncaron;"
         "ncap;"
         "nbumpe;"
         "nbump;"
         "nbsp"
         "nbsp;"
         "naturals;"
         "natural;"
         "natur;"
         "napprox;"
         "napos;"
         "napid;"
         "napE;"
         "nap;"
         "nang;"
         "nacute;"
         "nabla;")
        #\o
        '("ovbar;"
         "ouml"
         "ouml;"
         "otimesas;"
         "otimes;"
         "otilde"
         "otilde;"
         "osol;"
         "oslash"
         "oslash;"
         "oscr;"
         "oS;"
         "orv;"
         "orslope;"
         "oror;"
         "origof;"
         "ordm"
         "ordm;"
         "ordf"
         "ordf;"
         "orderof;"
         "order;"
         "ord;"
         "orarr;"
         "or;"
         "oplus;"
         "operp;"
         "opar;"
         "oopf;"
         "ominus;"
         "omid;"
         "omicron;"
         "omega;"
         "omacr;"
         "olt;"
         "oline;"
         "olcross;"
         "olcir;"
         "olarr;"
         "oint;"
         "ohm;"
         "ohbar;"
         "ogt;"
         "ograve"
         "ograve;"
         "ogon;"
         "ofr;"
         "ofcir;"
         "oelig;"
         "odsold;"
         "odot;"
         "odiv;"
         "odblac;"
         "odash;"
         "ocy;"
         "ocirc"
         "ocirc;"
         "ocir;"
         "oast;"
         "oacute"
         "oacute;")
        #\p
        '("puncsp;"
         "psi;"
         "pscr;"
         "prurel;"
         "prsim;"
         "propto;"
         "prop;"
         "profsurf;"
         "profline;"
         "profalar;"
         "prod;"
         "prnsim;"
         "prnE;"
         "prnap;"
         "primes;"
         "prime;"
         "precsim;"
         "precnsim;"
         "precneqq;"
         "precnapprox;"
         "preceq;"
         "preccurlyeq;"
         "precapprox;"
         "prec;"
         "pre;"
         "prE;"
         "prcue;"
         "prap;"
         "pr;"
         "pound"
         "pound;"
         "popf;"
         "pointint;"
         "pm;"
         "plustwo;"
         "plussim;"
         "plusmn"
         "plusmn;"
         "pluse;"
         "plusdu;"
         "plusdo;"
         "pluscir;"
         "plusb;"
         "plusacir;"
         "plus;"
         "plankv;"
         "planckh;"
         "planck;"
         "piv;"
         "pitchfork;"
         "pi;"
         "phone;"
         "phmmat;"
         "phiv;"
         "phi;"
         "pfr;"
         "pertenk;"
         "perp;"
         "permil;"
         "period;"
         "percnt;"
         "pcy;"
         "part;"
         "parsl;"
         "parsim;"
         "parallel;"
         "para"
         "para;"
         "par;")
        #\q
        '("quot"
         "quot;"
         "questeq;"
         "quest;"
         "quatint;"
         "quaternions;"
         "qscr;"
         "qprime;"
         "qopf;"
         "qint;"
         "qfr;")
        #\r
        '("rx;"
         "ruluhar;"
         "rtriltri;"
         "rtrif;"
         "rtrie;"
         "rtri;"
         "rtimes;"
         "rthree;"
         "rsquor;"
         "rsquo;"
         "rsqb;"
         "rsh;"
         "rscr;"
         "rsaquo;"
         "rrarr;"
         "rppolint;"
         "rpargt;"
         "rpar;"
         "rotimes;"
         "roplus;"
         "ropf;"
         "ropar;"
         "robrk;"
         "roarr;"
         "roang;"
         "rnmid;"
         "rmoustache;"
         "rmoust;"
         "rlm;"
         "rlhar;"
         "rlarr;"
         "risingdotseq;"
         "ring;"
         "rightthreetimes;"
         "rightsquigarrow;"
         "rightrightarrows;"
         "rightleftharpoons;"
         "rightleftarrows;"
         "rightharpoonup;"
         "rightharpoondown;"
         "rightarrowtail;"
         "rightarrow;"
         "rhov;"
         "rho;"
         "rharul;"
         "rharu;"
         "rhard;"
         "rHar;"
         "rfr;"
         "rfloor;"
         "rfisht;"
         "reg"
         "reg;"
         "rect;"
         "reals;"
         "realpart;"
         "realine;"
         "real;"
         "rdsh;"
         "rdquor;"
         "rdquo;"
         "rdldhar;"
         "rdca;"
         "rcy;"
         "rcub;"
         "rceil;"
         "rcedil;"
         "rcaron;"
         "rbrkslu;"
         "rbrksld;"
         "rbrke;"
         "rbrack;"
         "rbrace;"
         "rbbrk;"
         "rbarr;"
         "rBarr;"
         "rationals;"
         "ratio;"
         "ratail;"
         "rAtail;"
         "rarrw;"
         "rarrtl;"
         "rarrsim;"
         "rarrpl;"
         "rarrlp;"
         "rarrhk;"
         "rarrfs;"
         "rarrc;"
         "rarrbfs;"
         "rarrb;"
         "rarrap;"
         "rarr;"
         "rArr;"
         "raquo"
         "raquo;"
         "rangle;"
         "range;"
         "rangd;"
         "rang;"
         "raemptyv;"
         "radic;"
         "racute;"
         "race;"
         "rAarr;")
        #\s
        '("szlig"
         "szlig;"
         "swnwar;"
         "swarrow;"
         "swarr;"
         "swArr;"
         "swarhk;"
         "supsup;"
         "supsub;"
         "supsim;"
         "supsetneqq;"
         "supsetneq;"
         "supseteqq;"
         "supseteq;"
         "supset;"
         "supplus;"
         "supne;"
         "supnE;"
         "supmult;"
         "suplarr;"
         "suphsub;"
         "suphsol;"
         "supedot;"
         "supe;"
         "supE;"
         "supdsub;"
         "supdot;"
         "sup3"
         "sup3;"
         "sup2"
         "sup2;"
         "sup1"
         "sup1;"
         "sup;"
         "sung;"
         "sum;"
         "succsim;"
         "succnsim;"
         "succneqq;"
         "succnapprox;"
         "succeq;"
         "succcurlyeq;"
         "succapprox;"
         "succ;"
         "subsup;"
         "subsub;"
         "subsim;"
         "subsetneqq;"
         "subsetneq;"
         "subseteqq;"
         "subseteq;"
         "subset;"
         "subrarr;"
         "subplus;"
         "subne;"
         "subnE;"
         "submult;"
         "subedot;"
         "sube;"
         "subE;"
         "subdot;"
         "sub;"
         "strns;"
         "straightphi;"
         "straightepsilon;"
         "starf;"
         "star;"
         "sstarf;"
         "ssmile;"
         "ssetmn;"
         "sscr;"
         "srarr;"
         "squf;"
         "squarf;"
         "square;"
         "squ;"
         "sqsupseteq;"
         "sqsupset;"
         "sqsupe;"
         "sqsup;"
         "sqsubseteq;"
         "sqsubset;"
         "sqsube;"
         "sqsub;"
         "sqcups;"
         "sqcup;"
         "sqcaps;"
         "sqcap;"
         "spar;"
         "spadesuit;"
         "spades;"
         "sopf;"
         "solbar;"
         "solb;"
         "sol;"
         "softcy;"
         "smtes;"
         "smte;"
         "smt;"
         "smile;"
         "smid;"
         "smeparsl;"
         "smashp;"
         "smallsetminus;"
         "slarr;"
         "simrarr;"
         "simplus;"
         "simne;"
         "simlE;"
         "siml;"
         "simgE;"
         "simg;"
         "simeq;"
         "sime;"
         "simdot;"
         "sim;"
         "sigmav;"
         "sigmaf;"
         "sigma;"
         "shy"
         "shy;"
         "shortparallel;"
         "shortmid;"
         "shcy;"
         "shchcy;"
         "sharp;"
         "sfrown;"
         "sfr;"
         "sext;"
         "setmn;"
         "setminus;"
         "seswar;"
         "semi;"
         "sect"
         "sect;"
         "searrow;"
         "searr;"
         "seArr;"
         "searhk;"
         "sdote;"
         "sdotb;"
         "sdot;"
         "scy;"
         "scsim;"
         "scpolint;"
         "scnsim;"
         "scnE;"
         "scnap;"
         "scirc;"
         "scedil;"
         "sce;"
         "scE;"
         "sccue;"
         "scaron;"
         "scap;"
         "sc;"
         "sbquo;"
         "sacute;")
        #\t
        '("twoheadrightarrow;"
         "twoheadleftarrow;"
         "twixt;"
         "tstrok;"
         "tshcy;"
         "tscy;"
         "tscr;"
         "trpezium;"
         "tritime;"
         "trisb;"
         "triplus;"
         "triminus;"
         "trie;"
         "tridot;"
         "trianglerighteq;"
         "triangleright;"
         "triangleq;"
         "trianglelefteq;"
         "triangleleft;"
         "triangledown;"
         "triangle;"
         "trade;"
         "tprime;"
         "tosa;"
         "topfork;"
         "topf;"
         "topcir;"
         "topbot;"
         "top;"
         "toea;"
         "tint;"
         "timesd;"
         "timesbar;"
         "timesb;"
         "times"
         "times;"
         "tilde;"
         "thorn"
         "thorn;"
         "thksim;"
         "thkap;"
         "thinsp;"
         "thicksim;"
         "thickapprox;"
         "thetav;"
         "thetasym;"
         "theta;"
         "therefore;"
         "there4;"
         "tfr;"
         "telrec;"
         "tdot;"
         "tcy;"
         "tcedil;"
         "tcaron;"
         "tbrk;"
         "tau;"
         "target;")
        #\u
        '("uwangle;"
         "uuml"
         "uuml;"
         "uuarr;"
         "utrif;"
         "utri;"
         "utilde;"
         "utdot;"
         "uscr;"
         "urtri;"
         "uring;"
         "urcrop;"
         "urcorner;"
         "urcorn;"
         "upuparrows;"
         "upsilon;"
         "upsih;"
         "upsi;"
         "uplus;"
         "upharpoonright;"
         "upharpoonleft;"
         "updownarrow;"
         "uparrow;"
         "uopf;"
         "uogon;"
         "uml"
         "uml;"
         "umacr;"
         "ultri;"
         "ulcrop;"
         "ulcorner;"
         "ulcorn;"
         "uhblk;"
         "uharr;"
         "uharl;"
         "uHar;"
         "ugrave"
         "ugrave;"
         "ufr;"
         "ufisht;"
         "udhar;"
         "udblac;"
         "udarr;"
         "ucy;"
         "ucirc"
         "ucirc;"
         "ubreve;"
         "ubrcy;"
         "uarr;"
         "uArr;"
         "uacute"
         "uacute;")
        #\v
        '("vzigzag;"
         "vsupne;"
         "vsupnE;"
         "vsubne;"
         "vsubnE;"
         "vscr;"
         "vrtri;"
         "vprop;"
         "vopf;"
         "vnsup;"
         "vnsub;"
         "vltri;"
         "vfr;"
         "vert;"
         "verbar;"
         "vellip;"
         "veeeq;"
         "veebar;"
         "vee;"
         "vdash;"
         "vDash;"
         "vcy;"
         "vBarv;"
         "vBar;"
         "vartriangleright;"
         "vartriangleleft;"
         "vartheta;"
         "varsupsetneqq;"
         "varsupsetneq;"
         "varsubsetneqq;"
         "varsubsetneq;"
         "varsigma;"
         "varrho;"
         "varr;"
         "vArr;"
         "varpropto;"
         "varpi;"
         "varphi;"
         "varnothing;"
         "varkappa;"
         "varepsilon;"
         "vangrt;")
        #\w
        '("wscr;"
         "wreath;"
         "wr;"
         "wp;"
         "wopf;"
         "wfr;"
         "weierp;"
         "wedgeq;"
         "wedge;"
         "wedbar;"
         "wcirc;")
        #\x
        '("xwedge;"
         "xvee;"
         "xutri;"
         "xuplus;"
         "xsqcup;"
         "xscr;"
         "xrarr;"
         "xrArr;"
         "xotime;"
         "xoplus;"
         "xopf;"
         "xodot;"
         "xnis;"
         "xmap;"
         "xlarr;"
         "xlArr;"
         "xi;"
         "xharr;"
         "xhArr;"
         "xfr;"
         "xdtri;"
         "xcup;"
         "xcirc;"
         "xcap;")
        #\y
        '("yuml"
         "yuml;"
         "yucy;"
         "yscr;"
         "yopf;"
         "yicy;"
         "yfr;"
         "yen"
         "yen;"
         "ycy;"
         "ycirc;"
         "yacy;"
         "yacute"
         "yacute;")
        #\z
        '("zwnj;"
         "zwj;"
         "zscr;"
         "zopf;"
         "zigrarr;"
         "zhcy;"
         "zfr;"
         "zeta;"
         "zeetrf;"
         "zdot;"
         "zcy;"
         "zcaron;"
         "zacute;")))

(define length-of-longest-character-ref 32)
