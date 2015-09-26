" Vim syntax file
" Language: RxPebble
" Maintainer: Edmund Noble

if exists("b:current_syntax")
  finish
endif

syn match rxpType '[a-z]+'

syn keyword rxpSignal signal nextgroup=rxpType skipwhite
syn keyword rxpIntro intro nextgroup=introBlock skipwhite
syn keyword rxpCurve linear ease_in_out ease_in ease_out
syn keyword rxpStage stage
syn keyword rxpForever forever
syn keyword rxpAnimate animate
syn keyword rxpThen then
syn keyword rxpFrom from
syn keyword rxpTo to

syn match rxpNumber '[-+]?\d\+'
syn match rxpDuration '\d\+ms'

syn keyword rxpFor for nextgroup=rxDuration skipwhite
syn keyword rxpAfter after nextgroup=rxDuration skipwhite

syn region block start="{" end="}" fold transparent


hi def link rxpDuration  Constant
hi def link rxpSignal    Statement
hi def link rxpCurve     Constant
hi def link rxpAnimate   Statement
hi def link rxpStage     Statement
hi def link rxpIntro     Statement
hi def link rxpForever   Statement
hi def link rxpThen      Statement
hi def link rxpType      Type
hi def link rxpFor       Function
hi def link rxpAfter     Function
hi def link rxpFrom      Keyword
hi def link rxpTo      Keyword
let b:current_syntax = "rxp"
