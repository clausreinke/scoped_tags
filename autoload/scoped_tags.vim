
" these functions should go in autoload; build language-specific
" functionality on top, or just instantiate the DefaultKeyBindings
" (WARNING: with the possible exception of the tags file format,
"           nothing here is considered stable yet!-)

" Author:  Claus Reinke
" project: https://github.com/clausreinke/scoped_tags

" TODO: - separate Javascript-specific from general functionality
"       - where should the key definitions go?
"       - special case .members and assignments (less likely to be unique)
"       - what about multi-file matches in CycleTagOccurrence?
"       - completion

" handy, but optional key bindings
" TODO: user-specified leaders
function! scoped_tags#DefaultKeyBindings()
  " goto tag definitions for .member (not currently unique)
  " (just a shorthand for {Visual}g])
  map _g :exe ":tselect .".expand("<cword>")<cr>

  " goto tag definition for .member (not currently unique)
  map _. :call scoped_tags#GotoTagDefinition(".".expand("<cword>"))<cr>

  " goto tag definition for assignment (not currently unique)
  map _= :call scoped_tags#GotoTagDefinition("=".expand("<cword>"))<cr>
  " cycle forward/backward through assignments to scoped tag
  map _=* :call scoped_tags#CycleTagOccurrence("=".expand("<cword>"),'')<cr>
  map _=# :call scoped_tags#CycleTagOccurrence("=".expand("<cword>"),'b')<cr>

  " goto tag definition in scope
  map _] :call scoped_tags#GotoTagDefinition(expand("<cword>"))<cr>
  " cycle forward/backward through tag occurrences in scope
  map _* :call scoped_tags#CycleTagOccurrence(expand("<cword>"),'')<cr>
  map _# :call scoped_tags#CycleTagOccurrence(expand("<cword>"),'b')<cr>
endfunction

" cycle through occurrences of id, forward or backward, from cursor position
" (analogue to * and #, but scoped)
" NOTE: search patterns for assignments cover more variations than assignment 
"         tags right now, so we keep looking even if no assignment tag found
function! scoped_tags#CycleTagOccurrence(id,direction)
  try
    let [tag,rangePat] = scoped_tags#FindTagInScope(a:id[0]=='=' ? a:id[1:] : a:id,1)
    set hlsearch
    let @/ = rangePat.scoped_tags#TagSearchPattern(a:id)
    call search(@/,a:direction)
    let v:searchforward = a:direction=='' ? 1 : 0
  catch /.*/
    echo v:exception
  endtry
endfunction

" goto definition of id, if one is in scope at cursor position
" (analogue to ctrl-], but scoped)
function! scoped_tags#GotoTagDefinition(id)
  try
    let [tag,rangePat] = scoped_tags#FindTagInScope(a:id,0)
    normal m'
    if bufexists(tag['filename'])
      exe "buffer ".tag['filename']
    else
      exe "edit ".tag['filename']
    endif
    call cursor(tag['lineno'],1)
    set hlsearch
    let @/ = rangePat . scoped_tags#TagSearchPattern(a:id)
    call search(@/)
  catch /.*/
    echo v:exception
  endtry
endfunction

" search pattern for .selectors, =assignments, or regular ids 
" (allowing for Javascript-specific variations, such as 
"   _.selector, _['selector'], { selector: _ }
"   assign = _, assign[_] =, assign._ = 
"   )
" TODO: - scoped search patterns will match in comments and strings
"       - member search pattern will match in ternary (_?member:_)
"         and isolated strings 'member'
"       - assignment search pattern for id will match with 'container.id ='
"         (can we rule this out without losing valid matches, in declaration
"          lists or nested assignments?)
"       - regexp-based assignment search patterns are quite hopeless, really
"         (given that anything can be part of string selectors)
function! scoped_tags#TagSearchPattern(id)
  return ( a:id[0]=='.' 
       \ ? "\\(['\"\\.]".a:id[1:].'\|'.a:id[1:].'\s*:\)'
       \ : a:id[0]=='='
       \   ? '\<'.a:id[1:].'\>[[:space:]\.\[\]"''[:alnum:]_]*=\([^=]\|$\)'
       \   : '\<'.a:id.'\>' )
endfunction

" find tag definition for id, if one is in scope at cursor position
" return tag info and a range pattern marking tag's scope
" NOTE: - we could drop tags after current line
"         (assumes 'cmd' is line no, ignores hoisting)
"       - we could drop tags shadowed by later assignments
"         (what about conditional assignments, though?
"          also, assignment is just an approximation of modification!
"          other operations modify inplace, as well)
"       => separate declaration tags from assignment tags
function! scoped_tags#FindTagInScope(id,local)
  let tagsForId = taglist("^".a:id."$")
  " normalize slashes, to help comparisons
  for tagForId in tagsForId
    let tagForId['filename'] = substitute(tagForId['filename'],'\\','/','g')
  endfor

  let tagsInScope = filter(copy(tagsForId),
                        \ "scoped_tags#Inscope(expand('%:p'),line('.'),col('.'),v:val)")
  if tagsInScope==[] | throw "tag ".a:id." not found" | endif

  " sort matching tags by scope nesting
  call sort(tagsInScope,"scoped_tags#SmallerScope")
  " drop shadowed scopes
  call filter(tagsInScope,'v:val["scope"]==tagsInScope[0]["scope"]')
  let l = len(tagsInScope)
  let which = 0
  " choose tag
  " TODO: what to do if l>'lines'?
  if l>1
    let prompt = 'multiple tags for '.a:id.' ('.expand('%').' line '.line('.').')'
    let which  = scoped_tags#Inputlist(prompt,tagsInScope)
    if (which<0) || (which>=l)
      throw "no tag selected"
    endif
  endif
  let tag = tagsInScope[which]

  if a:local
    let tagsForIdFiltered = filter(copy(tagsForId),'v:val["filename"]==tag["filename"]')
  else
    let tagsForIdFiltered = filter(copy(tagsForId),'expand("%:p")=~v:val["filename"]')
  endif

  " eliminate nested scopes (where current tag is shadowed) from range!
  let nestedRanges = filter(copy(tagsForIdFiltered),'scoped_tags#Nested(tag,v:val)')
  let nestedRangePat = join(map(nestedRanges,'scoped_tags#ExcludeRange(v:val)'),'\&')

  " range pattern for tag scope
  let rangePat = scoped_tags#InRange(tag)
  return [tag,rangePat.'\('.nestedRangePat.'\)']
endfunction

" wrapper for inputlist, to handle prompt, line numbers, and indexing
function! scoped_tags#Inputlist(label,l)
  let lineno = '(exists("v:val[\"lineno\"]") ? "lineno:".v:val["lineno"] : "")'
  let l = map(copy(a:l),'v:val["filename"]." ".v:val["cmd"]." ".'.lineno)
  let i = 0
  for item in l
    let l[i] = '('.(i+1).'): '.item
    let i = i+1
  endfor
  return inputlist([a:label] + l) - 1
endfunction

" calculate range pattern for tag, 
" (matches where tag is in scope, ignoring shadowing by nested scopes)
function! scoped_tags#InRange(tag)
  let range = scoped_tags#Range(a:tag)
  return (range=={}) ? '' : '\(\(\%>'.range['startline'].'l'
                         \   .'\|\%'.range['startline'].'l'.'\%>'.(range['startpos']-1).'c\)'
                         \ .'\&'
                         \   .'\(\%<'.range['endline'].'l'
                         \   .'\|\%'.range['endline'].'l'.'\%<'.(range['endpos']+1).'c\)'
                         \ .'\)'
endfunction

" NOTE: we only call exclude for nested (non-global) ranges!
" calculate range pattern to exclude the scope of tag
" (matches where tag is not in scope)
function! scoped_tags#ExcludeRange(tag)
  let range = scoped_tags#Range(a:tag)
  return '\(\%<'.range['startline'].'l'
      \ .'\|'.'\%'.range['startline'].'l'.'\%<'.range['startpos'].'c'
      \ .'\|'.'\%'.range['endline'].'l'.'\%>'.range['endpos'].'c'
      \ .'\|'.'\%>'.range['endline'].'l\)'
endfunction

" test whether tag2's scope is properly nested in tag1's
" (global or equal scopes are never nested)
function! scoped_tags#Nested(tag1,tag2)
  let tagRange1 = scoped_tags#Range(a:tag1)
  let tagRange2 = scoped_tags#Range(a:tag2)
  return (tagRange2!={}) && ( (tagRange1=={}) ||
       \ (  ( (tagRange1['startline']<tagRange2['startline'])
       \    ||( (tagRange1['startline']==tagRange2['startline'])
       \      &&(tagRange1['startpos']<tagRange2['startpos'])))
       \ && ( (tagRange1['endline']>tagRange2['endline'])
       \    ||( (tagRange1['endline']==tagRange2['endline'])
       \      &&(tagRange1['endpos']>tagRange2['endpos']))) ) )
endfunction

" tag2 has a smaller scope if it is nested in tag1's
" NOTE: used only for sorting tags that are in scope (modulo shadowing)
"       at cursor position, so nesting can be used for ordering
function! scoped_tags#SmallerScope(tag1,tag2)
  return scoped_tags#Nested(a:tag1,a:tag2) ? 1 : a:tag1==a:tag2 ? 0 : -1
endfunction

" test if tag in scope in file f, line l at column c
" NOTE: assumes that global scope extends to any file
function! scoped_tags#Inscope(f,l,c,tag)
  let range = scoped_tags#Range(a:tag)
  return ( (range=={}) || ((a:f=~a:tag['filename']) && ( ( (range['startline']<a:l)
                      \   ||((range['startline']==a:l) && (range['startpos']<a:c)))
                      \ &&( (a:l<range['endline'])
                      \   ||((range['endline']==a:l) && (range['endpos']>a:c))) )) )
endfunction

" extract range data from tag attribute
function! scoped_tags#Range(tag)
  let ml = matchlist(a:tag['scope'],'\(\d\+\):\(\d\+\)-\(\d\+\):\(\d\+\)')
  return ml==[] ? {} : {'startline':str2nr(ml[1],10)
                     \ ,'startpos': str2nr(ml[2],10)
                     \ ,'endline':  str2nr(ml[3],10)
                     \ ,'endpos':   str2nr(ml[4],10) }
endfunction
