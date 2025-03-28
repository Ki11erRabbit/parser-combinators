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
   fun cache( tag : string, node : node<a> ) : ()
   fun fetch-cache ( tag : string ) : maybe<node<a>>

type node<a>
   Positions(set : hash-set<(a, int)>)
   Continuations(list : list<parser<e, a>>)


// clear continuations when leaving fixpoint

pub type parse-result<a>
   ParseOk( result : a)
   ParseFail( msg : string )

pub fun parse-result/show( r : parse-result<a>, ?show-a : a -> string ) : string
   match r
      ParseOk(x) -> ["ParseOk(", show-a(x), ")"].join("")
      ParseFail(msg) -> ["ParseFail(", msg, ")"].join("")

pub fun run-parser( input : sslice, initial-state : s, p : parser<e, a> ) : e parse-result<a> 
   var index := 0
   var state := initial-state
   var map := thread/hash-map()
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
     fun cache( tag : string, node : node<a> )
       map := map.insert(tag, node)
     fun fetch-cache ( tag : string )
       map.get( tag )
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



pub fun parser/fixpoint( p : parser<e, a> ) : <parse|e> a
   



pub fun main()
   match run-parser(slice("a"), (), fn () { eof() })
      ParseOk(c) -> c.println
      ParseFail(e) -> e.println