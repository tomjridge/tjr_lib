(** Interface to [tjr_lib_core] package functionality. *)

(** {2 Prelude} *)

include Util

module Int_ = Int_

module List_ = List_

module Iter = Iter
include Iter

module String_ = String_


(** {2 Extras} *)

module Tjr_map = Tjr_map

module Exhaustive_testing = Exhaustive_testing

module Gensym = Gensym

module Global = Global

module Tjr_store = Tjr_store
type fstore = Tjr_store.fstore (* available after open Tjr_lib_core *)

module Args_ = Args_

module Singleton_type = Singleton_type

type ('a,'b) sng = ('a,'b) Singleton_type.sng
let dest_sng = Singleton_type.dest_sng

module Tjr_lru = Tjr_lru
type ('k,'v,'t) lru_ops = ('k,'v,'t) Tjr_lru.lru_ops

(* module Lru_two_gen = Lru_two_gen *)

module Lru_two_gen = Lru_two_gen_v3
module Lru_two_gen_v3 = Lru_two_gen_v3

(* deprecated
module Bimap = Bimap
type ('x,'y,'t) bimap_ops = ('x,'y,'t) Bimap.bimap_ops
*)

module Intexable = Intexable

module Linear = Linear

module Seq_obj = Seq_obj

let seq_f = Seq_obj.seq_f

(* module Init_ref = Init_ref *)

(** {2 Logging and test control} *)

module Logger = Logger

module Log = Log

module Test = Test

module Tjr_show = Tjr_show
