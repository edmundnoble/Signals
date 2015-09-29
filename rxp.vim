" Vim syntax file
" Language: RxPebble
" Maintainer: Edmund Noble

if exists("b:current_syntax")
  finish
endif

syn match rxpSignalName /\<[_A-Za-z0-9$]\+\>/ contained
syn match rxpSignalDeclaration /\<[_A-Za-z0-9$]\+\>/ contained nextgroup=rxpSignalName

syn keyword rxpSignal signal nextgroup=rxpSignalDeclaration skipwhite
syn keyword rxpIntro intro
syn keyword rxpCurve linear ease_in_out ease_in ease_out
syn keyword rxpStage stage
syn keyword rxpForever forever
syn keyword rxpAnimate animate
syn keyword rxpThen then
syn keyword rxpFrom from
syn keyword rxpTo to
syn keyword rxpTemp temp
syn keyword rxpFunction function
syn keyword rxpType type
syn match rxpLayerDeclaration /\<[_A-Za-z0-9$]\+\>/ contained
syn keyword rxpLayer layer nextgroup=rxpLayerDeclaration skipwhite
syn match rxpFunctionArrow "=>"

syn match rxpNumber '[-+]?\d\+'
syn match rxpDuration '\d\+ms'

syn keyword rxpFor for nextgroup=rxDuration skipwhite
syn keyword rxpAfter after nextgroup=rxDuration skipwhite

syn keyword rxpPebbleSDKSpecialTypes GColor GColor8 GSize GRect GPoint Layer
hi link rxpPebbleSDKSpecialTypes Type
"syn region stageBlock start="{" end="}" fold transparent contains=introBlock


hi def link rxpDuration  Constant
hi def link rxpSignal    Statement
hi def link rxpCurve     Constant
hi def link rxpAnimate   Statement
hi def link rxpStage     Statement
hi def link rxpIntro     Statement
hi def link rxpForever   Statement
hi def link rxpThen      Statement
hi def link rxpSignalDeclaration      Type
hi def link rxpSignalName Identifier
hi def link rxpFor       Keyword
hi def link rxpAfter     Keyword
hi def link rxpFrom      Keyword
hi def link rxpTo        Keyword
hi def link rxpLayer     Keyword
hi def link rxpTemp      Keyword
hi def link rxpFunction  Keyword
hi def link rxpType      Keyword
hi def link rxpLayerDeclaration Identifier
hi def link rxpFunctionArrow Keyword

let b:current_syntax = "rxp"
