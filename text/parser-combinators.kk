/*----------------------------------------------------------------------------
   Copyright 2024, Koka-Community Authors

   Licensed under the MIT License ("The License"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/
module parser-combinators
import std/core/sslice
import std/data/hashset
import std/data/hashmap
import std/data/hash

pub alias parser<e, a> = (int) -> <parse|e> a

pub effect parse
   ctl fail( msg : string ) : a
   //fun get-state() : s
   //fun set-state( s : s ) : ()
   fun current-input() : sslice
   fun current-index() : int
   fun update-index( i : int) : ()
   fun advance-input( n : int, a : a ) : a
   ctl pick() : bool
   fun is-cached( tag : string, index : int ) : bool
   ctl depend( tag : string, index : int ) : (a, int)
   fun add-result( tag : string, index : int, result : a, end : int ) : ()

value struct memo-state<e, a>
   deps : list<(((a, int)) -> <pure|e> ())>
   vals : hash-set<(a, int)>

fun pair/hash(pair : (a, int), seed : int64) : int
  pair.snd.hash(seed)
// clear continuations when leaving fixpoint

fun pair/(==)(pair-a : (a, int), pair-b : (a, int)) : bool
  pair-a.snd == pair-b.snd

pub type parse-result<a>
  ParseOk( result : a)
  ParseFail( msg : string )

pub fun parse-result/show( r : parse-result<a>, ?show-a : a -> string ) : string
  match r
    ParseOk(x) -> ["ParseOk(", show-a(x), ")"].join("")
    ParseFail(msg) -> ["ParseFail(", msg, ")"].join("")

pub fun run-parser<a>( input : sslice, initial-state : s, p : parser<e, a> ) : e parse-result<a> 
  var index := 0
  var state := initial-state
  var cache := thread/hash-map()
  handle p
    return(x)
      ParseOk(x)
    fun current-input()
      input.advance(index)
    fun current-index()
      index
    fun update-index( i : int ) 
      index := i
    fun advance-input(n, a)
      index := index + n
      a
    brk fail(msg)
      ParseFail(msg)
    ctl pick()
      val save = input
      match resume(True)
        ParseOk(input1) -> ParseOk(input1)
        err1 ->
          input := save
          match resume(False)
            ParseOk(input2) -> ParseOk(input2)
            _err2 -> err1
    fun is-cached( tag, i ) 
      match cache.get(tag)
        Just(index-map) ->
          match index-map.get(i)
            Just(_) -> True
            _ ->        
              cache := cache.insert(tag, index-map.insert(i, Memo-state([], thread/hash-set())))
              False
        _ -> 
          val index-map = thread/hash-map()
          cache := cache.insert(tag, index-map.insert(start, Memo-state([], thread/hash-set())))
          False
    ctl depend( tag, i )
      match cache.get(tag)
        Just(index-map) ->
          match index-map.get(i)
            Just(Memo-state(deps, vals)) ->
              cache := cache.insert( tag, index-map.insert(i, Memo-state(Cons(fn(r) resume(r), deps), vals)))
              vals.list.foreach(resume)
              Nothing
            Nothing -> Nothing
        Nothing -> Nothing
    fun add-result( tag, i, result, end )
      match cache.get(tag)
        Just(index-map) ->
          match index-map.get(i)
            Just(Memo-state(deps, vals)) ->
              val new = (result, end)
              if vals.hashset/contains(new) then ()
              else 
                cache := cache.insert(tag, index-map.insert(i, Memo-state(deps, vals.insert(new, ?when-resize = hashset/when-resize, ?resizer = hashset/resizer)), ?when-resize = hashmap/when-resize, ?resizer = hashmap/resizer), ?when-resize = hashmap/when-resize, ?resizer = hashmap/resizer)
                deps.list/foreach(fn(res) { res(new); ()})
            Nothing -> ()
        Nothing -> ()
     /*fun get-state()
       state
     fun set-state(new-state)
       state := new-state*/
     
pub fun parser/choose( ps : list<parser<e, a>> ) : <parse|e> a
   match ps
      Nil -> fail("no further choices")
      Cons(p, Nil) -> p(current-index())
      Cons(p, ps) -> if pick() then p(current-index) else choose(ps)

pub fun parser/eof() : parse ()
   if current-input().count() == 0
      then return(())
      else fail("expected end of input")

pub fun parser/any() : parse char
   match current-input().count()
      0 -> fail("expected any character")
      _ -> match current-input().string.head-char
         Just(c) -> advance-input(1, c)
         Nothing -> fail("expected any character")

pub fun parser/char( c : char ) : parse char
   match current-input().string.head-char
      Just(c1) -> if c1 == c then advance-input(1, c1) else fail(["expected: ", c.show, ", found: ", c1.show].join(""))
      Nothing -> fail("unexpected end of input")

pub fun parser/string( s : string ) : parse string
   s.foreach fn (c) {
      parser/char(c)
      ()
   }
   s

/*value struct fixpoint-key
   tag : string
   index : int

fun hash( key : fixpoint-key, seed : int64 ) : int
   [key.tag.string/hash(seed), key.index.int/hash(seed)].hash(fn (x, seed1) { x }, ?seed = seed) 
*/



//pub fun parser/fixpoint( p : parser<e, a> ) : <parse|e> a
   



pub fun main()
   match run-parser(slice("a"), (), fn () { eof() })
      ParseOk(c) -> c.println
      ParseFail(e) -> e.println